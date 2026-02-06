
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(broom)
library(boot)



# Loading data ------------------------------------------------------------

data <- read.csv("Data/Overview TDE - data table (3).csv",
                 fileEncoding = "UTF-16LE",
                 sep = "\t", header = T) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(hb_name != "PUBLIC HEALTH SCOTLAND")


# Prepping data for modelling ---------------------------------------------

IFR_split_data <- data |> 
  filter(measure_id == "IFR") |>
  select(-calc_rate) |> 
  pivot_wider(names_from = measure_id, values_from = numerator) |>  
  mutate(across(c(8:9), as.numeric)) |>  
  rename(OBD = denominator)

# Let's see the distribution for IFR
data |> 
  filter(measure_id == "IFR") |>
  mutate(across(c(9:11), as.numeric)) |> 
  ggplot(aes(numerator)) +
  geom_histogram()


testthat::test_that("Poissoin regression is relevant?", {
  
  testthat::expect_equal(mean(IFR_split_data$IFR, na.rm = T), var(IFR_split_data$IFR, na.rm = T))
  
})





other_data <- data |> 
  filter(measure_id != "IFR") |>
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = measure_id, values_from = calc_rate) |>  
  mutate(across(c(9:37), as.numeric))


modelling_data <- left_join(IFR_split_data, other_data) |> 
  select(c("OBD", "IFR", "PTA", "SSUBA", "SSUEO", "VAC")) |>  
  na.omit()



# Overview ---------------------------------------------------------------

GGally::ggpairs(modelling_data)

# Visualize the correlation matrix
corrplot::corrplot(cor(modelling_data), method = "circle")


#   Class imbalance check -------------------------------------------------

modelling_data |> 
  select(IFR) |> 
  group_by(IFR) |> 
  count() |>
  ungroup() |> 
  mutate(prop = n/sum(n))




# Modelling ---------------------------------------------------------------

map(seq(5), function(x){
  
  assign(str_glue("train_{x}"),
         tibble(),
         envir = globalenv())
  
  
  assign(str_glue("test_{x}"),
         tibble(),
         envir = globalenv())
  
  
  
})


map(seq(5), function(x){

## 75% of the sample size
  smp_size <- floor(0.75 * nrow(modelling_data))
  
  ## set the seed to make your partition reproducible
  set.seed(x)
  
  train_ind <- sample(seq_len(nrow(modelling_data)), size = smp_size)
  
  
  
  assign(str_glue("train_{x}"),
         modelling_data[train_ind, ],
         envir = globalenv())
  
  assign(str_glue("test_{x}"),
         modelling_data[-train_ind, ],
         envir = globalenv())
  
  
})


test <- list(test_1, test_2, test_3, test_4, test_5) 
train <- list(train_1, train_2, train_3, train_4, train_5) 

rm(test_1, test_2, test_3, test_4, test_5,
   train_1, train_2, train_3, train_4, train_5)



# Poisson regression ------------------------------------------------------

map(train, function(x){
  
  
  model <- glm(formula = "IFR ~  OBD + PTA + SSUBA + SSUEO + VAC",
      family = "quasipoisson",
      data = trainp[x])
  
  tidy(model)
  glance(model)
  summary(model)
  
  
})
  

train[1]
  





