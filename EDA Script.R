

library(tidyverse)
library(patchwork)


setwd("/conf/EIC/Personal/James/Checks on data/2025-11-11 Modelling")


data <- read.csv("Data/Overview TDE - data table (3).csv",
         fileEncoding = "UTF-16LE",
         sep = "\t", header = T) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(hb_name != "PUBLIC HEALTH SCOTLAND")



ref_files <- list.files("/conf/EIC/Data Submission/Reference Files/Loaded", 
                        pattern = "EIC_NURSEFAMMEAS_[0-9]{8}.csv", full.names = TRUE) |>  
  max() |> 
  read_csv(skip = 1) |> 
  janitor::clean_names()


adult_inpatient <- ref_files |> 
  filter(nurse_family == "Adult_Inpatient") |> 
  select(measure_id) |> 
  distinct() |> 
  pull()


data |> 
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(8:38) |> 
  mutate(across(everything(), as.numeric),
         across(everything(), ~!is.na(.x)),
         row_num = row_number(),
         .before = 1) |>
  pivot_longer(cols = c(2:32)) |> 
  ggplot(aes(name, row_num, fill = value)) + 
  geom_raster()


data |> 
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(8:38) |> 
  mutate(across(everything(), as.numeric),
         across(everything(), ~!is.na(.x)),
         row_num = row_number(),
         .before = 1) |>
  pivot_longer(cols = c(2:32)) |> 
  group_by(name, value) |> 
  count() |> 
  ggplot(aes(name, n, fill = value)) + 
  geom_col()

data |> 
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(8:14) |> 
  select(-c("COM")) |> 
  mutate(across(everything(), as.numeric)) |> 
  na.omit()



data |> 
  mutate(measure_date_my = dmy(measure_date_my),
         across(c(9:11), as.numeric)) |> 
  group_by(location_name, measure_id, measure_date_my) |> 
  summarise(numerator = sum(numerator, na.rm = T),
            denominator = sum(denominator, na.rm = T)) |> 
  mutate(calc_rate = numerator / denominator) |> 
  ungroup() |> 
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(3:33) |> 
  mutate(across(everything(), ~!is.na(.x)),
         row_num = row_number(),
         .before = 1) |>
  pivot_longer(cols = c(2:32)) |> 
  ggplot(aes(name, row_num, fill = value)) + 
  geom_raster()


data |> 
  mutate(measure_date_my = dmy(measure_date_my),
         across(c(9:11), as.numeric)) |> 
  group_by(measure_id, measure_date_my) |> 
  summarise(numerator = sum(numerator, na.rm = T),
            denominator = sum(denominator, na.rm = T)) |> 
  mutate(calc_rate = numerator / denominator) |> 
  ungroup() |> 
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(3:33) |> 
  mutate(across(everything(), ~!is.na(.x)),
         row_num = row_number(),
         .before = 1) |>
  pivot_longer(cols = c(2:32)) |> 
  ggplot(aes(name, row_num, fill = value)) + 
  geom_raster()
  







data |> 
  select(-c(numerator, denominator)) |> 
  filter(measure_id %in% adult_inpatient) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  select(8:26) |> 
  mutate(across(everything(), as.numeric),
         across(everything(), ~!is.na(.x)),
         row_num = row_number(),
         .before = 1) |>
  pivot_longer(cols = c(2:20)) |> 
  ggplot(aes(name, row_num, fill = value)) + 
  geom_raster()

  

adult_inpatient_data <- data |> 
  select(-c(numerator, denominator)) |> 
  filter(measure_id %in% adult_inpatient) |> 
  pivot_wider(names_from = "measure_id", values_from = "calc_rate") |> 
  mutate(across(c(8:26), as.numeric),
         row_num = row_number(),
         .before = 1)




adult_inpatient_data |> 
  filter(AEW <= 1, 
         EWF <= 1) |> 
  ggplot(aes(AEW, EWF)) +
  geom_point() + 
  geom_smooth(method = "lm")



adult_inpatient_data |> 
  filter(IFR <= 200, 
         PUR <= 200) |> 
  ggplot(aes(IFR, PUR, colour = hb_name)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "smooth", se = F)






adult_inpatient_data |> 
  #filter(FFN1 <= 1, 
  #       FFN2 <= 1) |> 
  ggplot(aes(FFN1, FFN2, colour = hb_name)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = F)

plotly::ggplotly()

adult_inpatient_data |> 
  #filter(FFN1 <= 1, 
  #       FFN2 <= 1) |> 
  ggplot(aes(FFN1, FFN3, colour = hb_name)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = F)

plotly::ggplotly()

adult_inpatient_data |> 
  #filter(FFN1 <= 1, 
  #       FFN2 <= 1) |> 
  ggplot(aes(FFN2, FFN3, colour = hb_name)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = F)

plotly::ggplotly()









adult_inpatient_data |> 
  filter(SSUEO <= 1, 
         SSUBA <= 1) |> 
  ggplot(aes(log(SSUEO), SSUBA, colour = hb_name)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = F)

plotly::ggplotly()


GGally::ggpairs(adult_inpatient_data[c(9, 11:23)])





