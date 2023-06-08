sti_effect_ploter <- function(model, 
                              alfa = 0.1, 
                              alfa_ribbon = 0.1, 
                              color_point = "#002D72", 
                              color_line = "#002D72", 
                              color_ribbon_ex = "skyblue", 
                              color_ribbon_def = "orange", 
                              color_interrupt = "#99B933", 
                              mindate = "2018-01-01", 
                              maxdate = "2022-01-01") {
  
  mindate <- as.Date(mindate) 
  maxdate <- as.Date(maxdate)
  
  int_time <- mean(mod_full$bd.plot$tpand)
  
  int <- mod_full$bd.plot |> 
    filter(time == int_time, To == 1) |> 
    pull(date)
  
  mod_full$bd.plot |> 
    mutate(
      rate_fact = case_when(
        To == 0 & time <= int_time ~ ratepred_t, 
        To == 1 ~ ratepred_t,
        TRUE ~ as.numeric(NA)
      ), 
      rate_ctf = case_when(
        To == 0 & time >= int_time ~ ratepred_t, 
        TRUE ~ as.numeric(NA)
      ),  
      rate_fact_s = case_when(
        To == 0 & time <= int_time ~ ratepred_s, 
        To == 1 ~ ratepred_s,
        TRUE ~ as.numeric(NA)
      ), 
      rate_ctf_s = case_when(
        To == 0 & time >= int_time ~ ratepred_s, 
        TRUE ~ as.numeric(NA)
      )
    ) |> 
    arrange(To, time) -> datos
  
  datos_filt1 <- datos |> 
    filter(time >= int_time, To == 0) |> 
    dplyr::select(date, rate_ctf_s)
  
  datos_filt2 <- datos |> 
    filter(time >= int_time, To == 1) |> 
    dplyr::select(date, rate_fact_s)
  
  datos_rib <- datos_filt1 |> 
    left_join(datos_filt2, by = "date") |> 
    mutate(dif_rate = if_else(rate_ctf_s < rate_fact_s,
                              "Excess of visits", 
                              "Deficit of visits"))
  
  datos |> 
    ggplot() + 
    geom_point(aes(x = date, y = rateobs * 1000, color = "Observed rate"), 
               fill = color_point) + 
    geom_line(aes(x = date, 
                  y = ratepred_s * 1000, 
                  group = To, 
                  color = "Seasonal trends"), 
              alpha = alfa) + 
    geom_line(aes(x = date, 
                  y = rate_fact * 1000, 
                  group = To, 
                  color = "Pandemic trend")) + 
    geom_line(aes(x = date, 
                  y = rate_ctf * 1000, 
                  group = To, 
                  color = "Prepandemic trend"), 
              linetype = "dashed") + 
    geom_vline(xintercept = int, 
               linetype = "dashed", 
               color = color_interrupt) + 
    geom_ribbon(data = datos_rib, 
                mapping = aes(x = date, 
                              ymin = rate_fact_s * 1000, 
                              ymax = rate_ctf_s * 1000,
                              fill = dif_rate),
                alpha = alfa_ribbon) +
    scale_fill_manual(values =
                        c("Excess of visits" = color_ribbon_ex,
                          "Deficit of visits" = color_ribbon_def)) + 
    scale_x_date(limits = c(mindate, maxdate), 
                 breaks = "1 year", 
                 expand = expansion(mult = c(0.01, 0.01)),
                 labels = date_format("%Y"), 
                 date_minor_breaks = "6 month") + 
    scale_color_manual(values = c("Observed rate" = color_point, 
                                  "Prepandemic trend" = color_line, 
                                  "Pandemic trend" = color_line, 
                                  "Seasonal trends" = color_line)) +
    guides(color = guide_legend(order = 1, reverse = TRUE),
           fill = guide_legend(order = 2, reverse = TRUE)) + 
    theme_classic() + 
    labs(y = "Incidence Rates \n (numbers per 1000 habitants)", 
         x = "Time", 
         title = countryc[i], 
         fill = "", 
         colour = "") + 
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom", 
          legend.title = element_blank()) -> plot
  
  return(plot)
}
