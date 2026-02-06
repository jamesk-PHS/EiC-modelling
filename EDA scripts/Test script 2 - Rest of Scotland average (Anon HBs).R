



library(tidyverse)
setwd("/conf/EIC/Personal/James/Checks on data/2025-10-09 QI scoping")





data_extract <- read.csv("Overview TDE - data table (1).csv", fileEncoding = "UTF-16LE",
  sep = "\t", header = T) %>%
  as_tibble() |> 
  janitor::clean_names() |> 
  mutate(measure_date_my = dmy(measure_date_my),
         across(9:11, as.numeric)) |>
  filter(hb_name != "PUBLIC HEALTH SCOTLAND")

num <- 1


plot_vs_Scotland <- function(health_board, measure){
  
  HB_level_data <- data_extract |> 
    filter(hb_name == health_board,
           measure_id == measure) |> 
    group_by(measure_date_my) |>
    summarise(calc_rate = mean(calc_rate, na.rm = T),
              hb_name = "Example health board")
  
  Scotland_level_data <-  data_extract |> 
    filter(hb_name != health_board,
           measure_id == measure) |>
    group_by(measure_date_my) |>
    summarise(calc_rate = mean(calc_rate, na.rm = T),
              hb_name = "Rest of Scotland average")
  
  data_to_plot <- rbind(HB_level_data, Scotland_level_data)
  
  
  plot <- ggplot(data = data_to_plot , 
                 mapping = aes(measure_date_my, calc_rate, colour = hb_name, group = hb_name)) + 
    geom_line() +
    scale_y_continuous(labels = scales::label_percent(), limits= c(0, 1)) +
    scale_color_manual(values = c("#3F3685", "#CAC6D1")) + 
    labs(title = str_glue("HB comparison for {measure}"))
  
  ggsave(str_glue("Example plots/Plot {num} {measure}.png"), plot, 
         width = 3000,
         height = 1500,
         units = "px")

    
  num <<- num + 1
  
}


function_inputs <- data_extract |> 
  distinct(hb_name, measure_id)

map2(function_inputs$hb_name[1], function_inputs$measure_id[1:12], plot_vs_Scotland)



plot_vs_Scotland("FFN3")
