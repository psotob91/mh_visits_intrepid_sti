sti_model_norway <- function(country, mh_category, p , q, data, 
                             shock_exclude = TRUE, mean_cond = TRUE, 
                             dum_outlier = TRUE) {
  
  phi <- p
  theta <- q
  
  data |> 
    arrange(country, mh_category, time) |> 
    filter(To == 1) |> 
    group_by(country, mh_category) |> 
    summarize(start_time_of_pandemic = min(time)) |> 
    ungroup() -> indexTo
  
  # Subset data by country and disease group of interest
  mh_visitsm <- data %>%
    filter(country == .env$country) %>%
    filter(mh_category == .env$mh_category) %>%
    arrange(country, mh_category, time) |>    #Sort by time (important for autocorrelation checking!!)
    mutate(july = if_else(month == 7, 1, 0))
  
  indexTo |> 
    filter(mh_category == .env$mh_category) |> 
    filter(country == .env$country) |> 
    pull(start_time_of_pandemic) -> tpand
  
  mh_visitsm$tpand <- rep(tpand, length.out = nrow(mh_visitsm))
  
  if (shock_exclude == TRUE) {
    
    mh_visitsm <- mh_visitsm |> 
      mutate(time_adjusted = time - tpand, 
             time_to_interaction = (time - tpand) * To) |> 
      filter(Norway_Feb21 == 0)
   
    if (dum_outlier == TRUE) {
      glm_model <- glm(
        numerator ~ 1 + 
          time + 
          To + 
          I(time - tpand):To +
          harmonic(month, 2, 12) + 
          july +
          offset(log(denominator)), 
        data = mh_visitsm, 
        family = quasipoisson)
    } else {
      glm_model <- glm(
        numerator ~ 1 + 
          time + 
          To + 
          I(time - tpand):To +
          harmonic(month, 2, 12) +
          offset(log(denominator)), 
        data = mh_visitsm, 
        family = quasipoisson)
    }
    
    
  } else {
    
    mh_visitsm <- mh_visitsm |> 
      mutate(time_adjusted = time - tpand, 
             time_to_interaction = (time - tpand) * To)
   
    if (dum_outlier == TRUE) {
      glm_model <- glm(
        numerator ~ 1 + 
          time + 
          To + 
          I(time - tpand):To +
          harmonic(month, 2, 12) +
          july + 
          offset(log(denominator)), 
        data = mh_visitsm, 
        family = quasipoisson)
    } else {
      glm_model <- glm(
        numerator ~ 1 + 
          time + 
          To + 
          I(time - tpand):To +
          harmonic(month, 2, 12) +
          offset(log(denominator)), 
        data = mh_visitsm, 
        family = quasipoisson)
    }

    
  }

  rst <- rstudent(glm_model)
  arima_mod <- auto.arima(rst)
  # acf_plot <- acf(rst)
  # pacf_plot <- pacf(rst)
  
  auto_data <- mh_visitsm |> 
    dplyr::select(time) |> 
    mutate(rst = rst) |> 
    as_tsibble(index = time)
  
  auto_data |> 
    ACF(rst) |> 
    autoplot() + 
    scale_y_continuous(limits = c(-1, 1)) + 
    theme_minimal() + 
    labs(title = "Autocorr. of glm's residuals") -> acf_plot
  
  auto_data |> 
    PACF(rst) |> 
    autoplot() + 
    scale_y_continuous(limits = c(-1, 1)) + 
    theme_minimal() -> pacf_plot
  
  auto_plot <- acf_plot / pacf_plot

  if (shock_exclude == TRUE) {
    
    glmm_model <- glmmPQL(
      numerator ~ 1 + 
        time + 
        To + 
        I(time - tpand):To +
        harmonic(month, 2, 12) +
        offset(log(denominator)), 
      random = ~ time|country,
      correlation = corARMA(form = ~ time, p = phi, q = theta), 
      data = mh_visitsm, 
      family = poisson)
    
  } else { 
    
    glmm_model <- glmmPQL(
      numerator ~ 1 + 
        time + 
        To + 
        I(time - tpand):To +
        harmonic(month, 2, 12) +
        offset(log(denominator)), 
      random = ~ time|country,
      correlation = corARMA(form = ~ time, p = phi, q = theta), 
      data = mh_visitsm, 
      family = poisson)
    
    }

  # Calculating Predicted values:
  # For the contrafactual trend (PREPANDEMIC TREND)
  # New dataframe for with all To as 0 (as it were only pre-pandemic)
  bd_contrafact <- mh_visitsm |>  
    dplyr::select(numerator, time, tpand, To, month, Norway_Feb21, 
                  denominator, country, year, date) |> 
    mutate(`log(denominator)` = log(denominator), 
           To = 0)
  
  bd_impact <- mh_visitsm |>  
    dplyr::select(numerator, time, tpand, To, month, Norway_Feb21,
                  denominator, country, year, date) |> 
    mutate(`log(denominator)` = log(denominator)) |> 
    filter(To == 1)
  
  bd_contrafact |> 
    bind_rows(bd_impact) |>  
    mutate(rateobs = numerator / denominator) -> bd_its_all
  
  bd_its_all |> 
    mutate(ypred_s = predict(glmm_model, 
                             type = "response", 
                             newdata = bd_its_all,
                             allow.new.levels = TRUE,
                             level = 0), 
           ratepred_s = ypred_s / exp(`log(denominator)`)) -> bd_its_effect_s 
  
  if (mean_cond == TRUE) {
    
    bd_its_all |> 
      mutate(`log(denominator)` = mean(`log(denominator)`), 
             month = mean(month)) -> bd_its_all 
    
  } else {
    
    bd_its_all |> 
      mutate(`log(denominator)` = mean(`log(denominator)`), 
             month = mean(month), 
             Norway_Feb21 = mean(Norway_Feb21)) -> bd_its_all 
  }
  
  
  bd_its_all |> 
    mutate(ypred_t = predict(glmm_model, 
                             type = "response", 
                             newdata = bd_its_all,
                             allow.new.levels = TRUE,
                             level = 0), 
           ratepred_t = ypred_t / exp(`log(denominator)`)) |> 
    dplyr::select(ypred_t, ratepred_t) -> bd_its_effect_t
  
  bd_its_effect_s |> 
    bind_cols(bd_its_effect_t) -> bd_its_effect  
  
  plot_diag <- diag_glmm(mh_visitsm, glmm_model) 
  
  model_list <- list(mod = glmm_model, 
                     bd.plot = bd_its_effect, 
                     dx.plot = plot_diag, 
                     glm = glm_model, 
                     arima = arima_mod, 
                     auto_plot = auto_plot)
  
  return(model_list)
}
