sti_plotter_pre <- function(countryd, mh_categoryd, blind = TRUE, data) {

  # Obtaining the number of time where the "pandemic" initiate
  data |> 
    arrange(country, mh_category, time) |> 
    filter(pre_post == 1) |> 
    group_by(country, mh_category) |> 
    summarize(start_time_of_pandemic = min(time)) |> 
    ungroup() -> index
  
  index |> 
    filter(country == countryd, mh_category == mh_categoryd) |> 
    pull(start_time_of_pandemic) -> time_pand
  
  data |> 
    filter(country == countryd, mh_category == mh_categoryd, 
           time == time_pand) |> 
    pull(date) -> tpand_date
  
  if (blind == TRUE) {
    data |> 
      filter(country == countryd, mh_category == mh_categoryd) |> 
      filter(time < time_pand) -> data_filtered
  } else {
    data |> 
      filter(country == countryd, mh_category == mh_categoryd) -> data_filtered
  }
  
  max_y <- max(data_filtered$rate) 
  min_y <- min(data_filtered$rate) 
  # min_y <- 0
  n <- dim(data_filtered)[1]
  k <- (max_y - min_y) / (log2(n) + 1) # Sturges's rule
  
  data_filtered |> 
    ggplot(aes(x = date, y = rate)) + 
    geom_point(shape = 4, color = cbPalette[6]) +  
    labs(x = "", 
         y = paste("Incidence Rate of ", mh_categoryd, "\n in " , 
                   countryd, " per 10,000 habitants")) + 
    theme_classic() +
    scale_x_date(limits = c(min_tdate, max_tdate),
                 breaks = "1 month", 
                 expand = expansion(mult = c(0.01, 0.01)),
                 labels = date_format("%b-%Y"), 
                 date_minor_breaks = "1 month") + 
    scale_y_continuous(breaks = round(seq(min_y, max_y, k), 1), 
                       minor_breaks = round(seq(min_y, max_y, k), 1)) + 
    geom_vline(xintercept = tpand_date, linetype = 2, color = cbPalette[7]) +
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), 
          panel.grid.major.y = element_line(color = cbPalette[1], 
                                            linewidth = 1, 
                                            linetype = 0.001), 
          panel.grid.minor.y = element_line(color = cbPalette[1], 
                                            linewidth = 0.001, 
                                            linetype = 1)) -> plot
  
  return(plot)
}