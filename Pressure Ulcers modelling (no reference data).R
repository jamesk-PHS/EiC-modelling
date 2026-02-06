

# 1. Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(broom)
library(boot)
library(MASS)
library(lme4)
library(glmmTMB)
library(pscl)

# 2. Loading data ------------------------------------------------------------

data <- read.csv("Data/Overview TDE - data table (3).csv",
                 fileEncoding = "UTF-16LE",
                 sep = "\t", header = T) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(hb_name != "PUBLIC HEALTH SCOTLAND") |> 
  mutate(measure_date_my = dmy(measure_date_my),
         sub_location_code = str_to_upper(sub_location_code))


# 3. Prepping data for modelling ---------------------------------------------

PUR_split_data <- data |> 
  filter(measure_id == "PUR") |>
  dplyr::select(-calc_rate) |> 
  pivot_wider(names_from = measure_id, values_from = numerator) |>  
  mutate(across(c(8:9), as.numeric)) |>  
  rename(OBD = denominator)

# Let's see the distribution for PUR
data |> 
  filter(measure_id == "PUR") |>
  mutate(across(c(9:11), as.numeric)) |> 
  ggplot(aes(numerator, fill = if_else(numerator == 0, TRUE, FALSE))) +
  geom_histogram(show.legend = F)


other_data <- data |> 
  filter(measure_id != "PUR") |>
  dplyr::select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = measure_id, values_from = calc_rate) |>  
  mutate(across(c(9:37), as.numeric))


modelling_data <- left_join(PUR_split_data, other_data) |> 
  dplyr::select(PUR, OBD, VAC, PTA, SSUBA, SSUEO) |>  
  na.omit() |> 
  # Removing outlier/high leverage points
  filter(if_all(c(4:6), ~. <= 1),
         if_all(c(4:6), ~. >= 0)) |> 
  mutate(across(c(3:6), ~.x*100)) |>  
  na.omit()

# 4. Poisson regression assumptions ------------------------------------------

testthat::test_that("Mean and variance terms are equal",{
  
  testthat::expect_equal(mean(PUR_split_data$PUR, na.rm = T), var(PUR_split_data$PUR, na.rm = T))
  
})

# Fails. Not ideal

## Multi-colinearity 
GGally::ggpairs(dplyr::select(modelling_data, 2:6))

# Visualize the correlation matrix
corrplot::corrplot(cor(dplyr::select(modelling_data, 2:6)), method = "circle")

# Eek, not great either.


# Let's look at the VIF of a test model to see if it's going to be problematic


test_nb_model <- glm.nb(formula = "PUR ~  OBD + PTA + SSUBA + SSUEO + VAC",
                        data = filter(modelling_data, PUR != 0))

testthat::test_that(
  "There is no multicollinearity", {
    testthat::expect_true(
      all(car::vif(test_nb_model) < 5)
    )
  })


test_logistic_model <- glm(formula = "PUR ~  OBD + PTA + SSUBA + SSUEO + VAC",
                           family = "binomial",
                           data = mutate(modelling_data, PUR = if_else(PUR > 0, 1, 0)))

testthat::test_that(
  "There is no multicollinearity", {
    testthat::expect_true(
      all(car::vif(test_logistic_model) < 5)
    )
  })


# Looks like it's fine. All values are VERY close to 1 so these data are almost
# ideal. 


## 4.1 Outcome ----

# Okay, so, given the greater variance within the falls data, we're stuck with 
# quasi poisson or negative binomial (NB) models. We could also look into 
# Gamma regression with epsilon adjustments but, 

# I'll also need to examine zero-inflated p

# We could also look into Gamma regression usibg a slight epsilon adjustment
# of .Machine$double.eps



# 5. Modelling ---------------------------------------------


## 75% of the sample size
smp_size <- floor(0.75 * nrow(modelling_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(modelling_data)), size = smp_size)

train <- modelling_data[train_ind, ]
test <- modelling_data[-train_ind, ]


# Negative binomial GLMM using the function glm.nb()
base_model_backward_selection <- glm.nb(PUR ~  OBD + VAC + PTA + SSUBA + SSUEO,
                                        data = train)


tidy(base_model_backward_selection)
glance(base_model_backward_selection)
summary(base_model_backward_selection)


zeroinfl_nb_model <- pscl::zeroinfl(PUR ~ . | .,
                                    data = train, dist = "negbin")

base::summary(zeroinfl_nb_model)
pscl::pR2(zeroinfl_nb_model)
AIC(zeroinfl_nb_model)


pscl::vuong(base_model_backward_selection, zeroinfl_nb_model)


## Hurdle model:


hurdle_nb_model <- pscl::hurdle(PUR ~ . | .,
                                data = train, dist = "negbin")

base::summary(hurdle_nb_model)
pscl::pR2(hurdle_nb_model)


AIC(hurdle_nb_model)


pscl::vuong(zeroinfl_nb_model, hurdle_nb_model)
pscl::vuong(base_model_backward_selection, hurdle_nb_model)

AIC(base_model_backward_selection, zeroinfl_nb_model, hurdle_nb_model)

# Hurlde beats it out narrowly.

pscl::pR2(zeroinfl_nb_model)
pscl::pR2(hurdle_nb_model)


fm <- list("ZINB" = zeroinfl_nb_model, "Hurdle-NB" = hurdle_nb_model)
sapply(fm[1:2], function(x) round(x$coefficients$count, digits = 3))
sapply(fm[1:2], function(x) round(exp(x$coefficients$zero), digits = 3))



count_predictions <- ggeffects::ggpredict(hurdle_nb_model, type = "count")
zero_probs <- ggeffects::ggpredict(hurdle_nb_model, type = "zi_prob")


rbind(
  map_df(names(count_predictions), \(x){tibble(count_predictions[[x]]) |> mutate(type = "counts")}),
  map_df(names(zero_probs), \(x){tibble(zero_probs[[x]]) |> mutate(type = "zeros")})
  
) |> 
  #bind_cols(x = nurse_fam_only$x,
  #          predicted = nurse_fam_only$predicted,
  #          std.error = nurse_fam_only$std.error,
  #          conf.low = nurse_fam_only$conf.low,
  #          conf.high = nurse_fam_only$conf.high,
  #          group = nurse_fam_only$group) |> 
  ggplot() +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, predicted, fill = group, ymax = conf.high, ymin = conf.low, alpha = 0.2), show.legend = F) +
  facet_wrap(type~group, scales = "free")





count_predictions <- ggeffects::ggpredict(hurdle_nb_model, type = "count")
zero_probs <- ggeffects::ggpredict(hurdle_nb_model, type = "zi_prob")
nurse_fam_only <- ggeffects::ggpredict(hurdle_nb_model, term = "nursefamily", type = "zi_prob")
nurse_count_only <- ggeffects::ggpredict(hurdle_nb_model, term = "fam_count", type = "zi_prob")


map_df(c(1:3), \(x){
  
  predict(hurdle_nb_model, 
          newdata = tibble(OBD = 469.69,
                           VAC = 11.51,
                           PTA = 35.76,
                           SSUBA = 16.92,
                           SSUEO = 2.74,
                           nursefamily = "Adult_Inpatient",
                           fam_count = x),
          interval = "confidence")
  
})









bind_cols(x = nurse_fam_only$x,
          predicted = nurse_fam_only$predicted,
          std.error = nurse_fam_only$std.error,
          conf.low = nurse_fam_only$conf.low,
          conf.high = nurse_fam_only$conf.high,
          group = nurse_fam_only$group) |> 
  ggplot() +
  geom_col(aes(fct_reorder(x, predicted, .desc = T), predicted, fill = x))










