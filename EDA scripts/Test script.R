

library(tidyverse)

# Folder where the data extract from tableau and outputs are to be saved
data_path <- '/conf/EIC/Data Submission/Submission Reports/Revised reports/data/'

submissions_raw <- read.csv(paste0(
  data_path, "submission_extracts/1-sub_reports_raw_extract.csv"), fileEncoding = "UTF-16LE",
  sep = "\t", header = T) %>%
  as_tibble() |> 
  select(-X) |>
  janitor::clean_names() |>
  # Fix date format (currently has time added, so taking only dmy characters) and
  # converting it to lubridate format
  mutate(measure_date = dmy(str_sub(measure_date, 1, 10))) |>
  rename(hb_code = health_board_code_9_curr) |> 
  filter(health_board_name != "PUBLIC HEALTH SCOTLAND")


submissions_raw |> 
  filter(measure_id == "AEW") |> 
  mutate(across(starts_with("User_"), as.double)) |> 
  group_by(health_board_name, measure_date) |> 
  summarise(user_data_1 = sum(user_data_1),
            user_data_2 = sum(user_data_2)) |> 
  mutate(calc_rate = user_data_1/user_data_2,
         colour_by = ifelse(health_board_name == "NHS AYRSHIRE & ARRAN", TRUE, FALSE)) |> 
  ggplot(aes(measure_date, calc_rate, colour = colour_by, group = health_board_name)) + 
  geom_line() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_color_manual(values = c("#CAC6D1", "#3F3685")) 

plotly::ggplotly()



plot_vs_Scotland <- function(measure){

HB_level_data <- submissions_raw |> 
	filter(health_board_name == health_board,
		measure_id == measure) |> 
  mutate(calc_rate = as.numeric(user_data_1)/as.numeric(user_data_2)) |> 
  select(measure_date, calc_rate, health_board_name) |> 
  group_by(measure_date) |>
  summarise(calc_rate = mean(calc_rate, na.rm = T),
            health_board_name = health_board_name)

Scotland_level_data <-  submissions_raw |> 
	filter(health_board_name != health_board,
		measure_id == measure) |>
  mutate(calc_rate = as.numeric(user_data_1)/as.numeric(user_data_2)) |> 
	group_by(measure_date) |>
	summarise(calc_rate = mean(calc_rate, na.rm = T),
		health_board_name = "Rest of Scotland average")

data_to_plot <- rbind(HB_level_data, Scotland_level_data)


plot <- ggplot(data = data_to_plot , 
		mapping = aes(measure_date, calc_rate, colour = health_board_name, group = health_board_name)) + 
  geom_line() +
  scale_y_continuous(labels = scales::label_percent(), limits= c(0, 1)) +
  scale_color_manual(values = c("#3F3685", "#CAC6D1"))

return(plot)

}

health_board <- "NHS AYRSHIRE & ARRAN"


plot_vs_Scotland("FFN3")
