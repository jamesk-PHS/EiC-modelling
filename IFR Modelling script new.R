

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
  

other_data <- data |> 
  filter(measure_id != "IFR") |>
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = measure_id, values_from = calc_rate) |>  
  mutate(across(c(9:37), as.numeric))


modelling_data <- left_join(IFR_split_data, other_data) |> 
  select(c("OBD", "IFR", "PTA", "SSUBA", "SSUEO", "VAC")) |>  
  na.omit()


#   Class imbalance check -------------------------------------------------

modelling_data |> 
  select(IFR) |> 
  group_by(IFR) |> 
  count() |>
  ungroup() |> 
  mutate(prop = n/sum(n))




# Modelling ---------------------------------------------------------------

## 75% of the sample size
smp_size <- floor(0.75 * nrow(modelling_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(modelling_data)), size = smp_size)

train <- modelling_data[train_ind, ]
test <- modelling_data[-train_ind, ]



# Logistic regression -----------------------------------------------------

model_1 <- glm(formula = "IFR ~  OBD + PTA + SSUBA + SSUEO + VAC",
               family = "binomial",
               data = train |> 
                  mutate(IFR = if_else(IFR == 0, 0, 1))
               )

tidy(model_1)
glance(model_1)
summary(model_1)


# Poisson regression ------------------------------------------------------

model_2 <- glm(formula = "IFR ~  OBD + PTA + SSUBA + SSUEO + VAC",
               family = "poisson",
               data = train)

tidy(model_2)
glance(model_2)
summary(model_2)




model_3 <- glm(formula = IFR ~  OBD + PTA + SSUBA + SSUEO + VAC,
               family = "Gamma", 
               data = train)

tidy(model_2)
glance(model_2)
summary(model_1)



