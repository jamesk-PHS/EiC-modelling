


library(fpp2)

plot_this_ts <- function(measure){

  summarised_data <- data_extract |> 
    filter(measure_id == measure) |> 
    group_by(measure_date_my) |> 
    summarise(calc_rate = mean(calc_rate, na.rm = T))
  
  min <- min(summarised_data$measure_date_my)
  year <- year(min)
  month <- month(min)
  
  data_to_plot <- ts(summarised_data$calc_rate, 
                     start = c(year, month),
                     frequency = 12)
  
  data <- ses(data_to_plot, h=6)
  
  plot <- autoplot(data) +
    autolayer(fitted(data), series="Fitted") +
    labs(title = str_glue("TS for {measure}"))

  return(plot)
  
}


plot_this_ts("IFR")




decompose_this_ts <- function(measure){
  
  summarised_data <- data_extract |> 
    filter(measure_id == measure) |> 
    group_by(measure_date_my) |> 
    summarise(calc_rate = mean(calc_rate, na.rm = T)) |> 
    filter(measure_date_my >= (max(measure_date_my) - months(24)))
  
  min <- min(summarised_data$measure_date_my)
  year <- year(min)
  month <- month(min)

  plot <- ts(summarised_data$calc_rate,
             start = c(year, month),
             frequency = 12) |> 
    decompose("additive") |> 
    autoplot() +
    labs(title = str_glue("Decom analsys for {measure}"))
  
  return(plot)
  
}


measures <- unique(data_extract$measure_id)[!str_detect(unique(data_extract$measure_id), "COM|SSC|SNGA")]

decompose_this_ts("PTA")

for(measure in measures){
  
  plot <- decompose_this_ts(measure)
  
  print(plot)
  
  readline()
}

