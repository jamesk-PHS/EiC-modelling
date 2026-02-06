




library(fpp3)




data <- read.csv("Data/Overview TDE - data table (3).csv",
                 fileEncoding = "UTF-16LE",
                 sep = "\t", header = T) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(hb_name != "PUBLIC HEALTH SCOTLAND")


# 3. Prepping data for modelling ---------------------------------------------

IFR_split_data <- data |> 
  filter(measure_id == "IFR") |>
  dplyr::select(-calc_rate) |> 
  pivot_wider(names_from = measure_id, values_from = numerator) |>  
  mutate(across(c(8:9), as.numeric)) |>  
  rename(OBD = denominator)

# Let's see the distribution for IFR
data |> 
  filter(measure_id == "IFR") |>
  mutate(across(c(9:11), as.numeric)) |> 
  ggplot(aes(numerator)) +
  geom_histogram()


other_data <- data |> 
  filter(measure_id != "IFR") |>
  dplyr::select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = measure_id, values_from = calc_rate) |>  
  mutate(across(c(9:37), as.numeric))


modelling_data <- left_join(IFR_split_data, other_data) |> 
  dplyr::select(c(sub_location_code, measure_date_my, IFR, OBD, VAC, PTA, SSUBA, SSUEO)) |>  
  na.omit() |> 
  # Removing outlier/high leverage points
  filter(if_all(c(6:8), ~. <= 1),
         if_all(c(6:8), ~. >= 0)) |> 
  mutate(measure_date_my =dmy(measure_date_my)) |> 
  arrange(sub_location_code, measure_date_my)



ACF <- modelling_data %>% 
  group_by(sub_location_code) %>%
  summarise(ac = list(acf(IFR, lag.max = 6, plot = FALSE))) |> 
  ungroup() 



c(ACF$ac[[1]]$lag)


ACF <- map_df(c(1:nrow(ACF)), function(x){
  
  tibble(sub_location_code = ACF$sub_location_code[[x]],
         lags = c(ACF$ac[[x]]$lag),
         ac_lags = c(ACF$ac[[x]]$acf)
  )
  
  
  
})


hist(ACF$ac_lags)

ACF |> 
  filter(lags != 0) |> 
  ggplot(aes(ac_lags)) + 
  geom_histogram()

ACF |> 
  filter(lags != 0) |> 
  ggplot(aes(ac_lags, fill = as.factor(lags))) + 
  geom_histogram(show.legend = F) + 
  facet_wrap(~lags)





ACF |> 
  filter(lags != 0,
         !is.na(ac_lags)) |> 
  summarise(mean = mean(ac_lags, na.rm = T),
            sd = sd(ac_lags, na.rm = T),
            n = n()) |> 
  mutate(lower_CI = mean - 1.96*sd/sqrt(n),
         upper_CI = mean + 1.96*sd/sqrt(n),
         zero_in_bounds = lower_CI <= 0 & 0 <= upper_CI)

t.test(filter(ACF, lags != 0 & !is.na(ac_lags))$ac_lags)


ACF |> 
  filter(lags != 0,
         !is.na(ac_lags)) |> 
  group_by(lags) |> 
  summarise(mean = mean(ac_lags, na.rm = T),
            sd = sd(ac_lags, na.rm = T),
            n = n()) |> 
  mutate(lower_CI = mean - 1.96*sd/sqrt(n),
         upper_CI = mean + 1.96*sd/sqrt(n),
         zero_in_bounds = lower_CI <= 0 & 0 <= upper_CI)

ACF |> 
  filter(lags != 0,
         !is.na(ac_lags)) |> 
  group_by(sub_location_code) |> 
  summarise(mean = mean(ac_lags, na.rm = T),
            sd = sd(ac_lags, na.rm = T),
            n = n()) |> 
  mutate(lower_CI = mean - 1.96*sd/sqrt(n),
         upper_CI = mean + 1.96*sd/sqrt(n),
         zero_in_bounds = lower_CI <= 0 & 0 <= upper_CI) |>
  group_by(zero_in_bounds) |> 
  summarise(n = n())




ACF |> 
  filter(lags != 0,
         !is.na(ac_lags)) |> 
  group_by(sub_location_code) |> 
  summarise(mean = mean(ac_lags, na.rm = T),
            sd = sd(ac_lags, na.rm = T),
            n = n()) |> 
  mutate(lower_CI = mean - 1.96*sd/sqrt(n),
         upper_CI = mean + 1.96*sd/sqrt(n),
         zero_in_bounds = lower_CI <= 0 & 0 <= upper_CI) |> 
  ungroup() |> 
  filter(zero_in_bounds == FALSE) |> 
  group_by(sub_location_code) |> 
  summarise(n = n()) |>
  arrange(desc(n))

