diag_glmm <- function(data, model) {
  # Function for Diagnostic's Regression
  tam <- 1
  
  dx_data <- data |> 
    mutate(norm_res = residuals(model, 
                                type = "normalized"), 
           fitted = fitted(model))
  
  dx_data |> 
    ggplot(aes(x = fitted, y = norm_res)) + 
    geom_point(size = tam) + 
    geom_hline(yintercept = 0, 
               linetype = "dashed", 
               color = "red") + 
    geom_smooth() + 
    theme_minimal() + 
    labs(x = "Fitted values", 
         y = "Normalized residuals") -> fit_res
  
  dx_data |> 
    ggplot(aes(sample = norm_res)) + 
    stat_qq_band() + 
    stat_qq_line() + 
    stat_qq_point(size = tam) + 
    labs(x = "Theoretical Quantiles", 
         y = "Sample Quantiles") + 
    theme_minimal() -> qq_res
  
  dx_data |> 
    ggplot(aes(x = time, y = norm_res)) + 
    geom_point(size = tam) + 
    geom_hline(yintercept = 0, 
               color = "red", 
               linetype = "dashed") + 
    geom_smooth() + 
    theme_minimal() + 
    labs(x = "Time", y = "Normalized residuals") -> res_time
  
  dx_data |> 
    summarise(max_fit = max(exp(fitted)), 
              max_num = max(numerator), 
              min_fit = min(exp(fitted)), 
              min_num = min(numerator)) |> 
    summarise(min = min(min_fit, min_num), 
              max = max(max_fit, max_num)) -> minmax
  
  dx_data |> 
    ggplot(aes(x = exp(fitted), y = numerator)) + 
    geom_point(size = tam) + 
    geom_hline(yintercept = 0, 
               linetype = "dashed", 
               color = "red") + 
    geom_abline(slope = 1, 
                intercept = 0, 
                color = "red", 
                linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Fitted values", 
         y = "Response values") + 
    coord_cartesian(
      x = c(minmax$min, minmax$max),
      y = c(minmax$min, minmax$max)
    ) -> fit_y
  
  dx_data <- dx_data |> 
    as_tsibble(index = time)
  
  dx_data |> 
    ACF(norm_res) |> 
    autoplot() + 
    scale_y_continuous(limits = c(-1, 1)) + 
    theme_minimal() -> acf_plot
  
  dx_data |> 
    PACF(norm_res) |> 
    autoplot() + 
    scale_y_continuous(limits = c(-1, 1)) + 
    theme_minimal() -> pacf_plot
  
  dx_data |> 
    ggplot(aes(x = month, y = norm_res)) + 
    geom_point(size = tam) + 
    geom_hline(yintercept = 0, 
               color = "red", 
               linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Month", y = "Normalized residuals") -> res_month
  
  (fit_res | res_time) / (acf_plot | pacf_plot) / 
    (fit_y | qq_res) -> dx_res_plot
  
  return(dx_res_plot)
}