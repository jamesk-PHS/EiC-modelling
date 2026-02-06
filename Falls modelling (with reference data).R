

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


# Reading in some reference data for nurse family assignment:

reference_data <- list.files("/conf/EIC/Data Submission/Reference Files/Loaded", 
                             pattern = "EIC_NURSEFAMSUBLOC_[0-9]{8}.csv",
                             full.names = TRUE) |> 
  max() |> 
  read_csv(, skip = 1) |> 
  janitor::clean_names() |> 
  mutate(across(contains("date"), dmy),
         endate = replace_na(endate, today()),  
         across(contains("date"), ~ floor_date(.x, unit = "month")),
         endate = endate + months(1),
         startdate = if_else(startdate > today(), today(), startdate),
         subcode = str_to_upper(subcode))


reference_data <- map_df(c(1:nrow(reference_data)), function(x){
  
  tibble(subcode = reference_data$subcode[x],
         nursefamily = reference_data$nursefamily[x],
         dates = seq(from = reference_data$startdate[x],
                     to = reference_data$endate[x],
                     by = "month"))
})






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
  dplyr::select(sub_location_code, measure_date_my, IFR, OBD, VAC, PTA, SSUBA, SSUEO) |>  
  na.omit() |> 
  # Removing outlier/high leverage points
  filter(if_all(c(6:8), ~. <= 1),
         if_all(c(6:8), ~. >= 0)) |> 
  mutate(across(c(5:8), ~.x*100)) |> 
  left_join(reference_data, by = c("sub_location_code" = "subcode", 
                                   "measure_date_my" = "dates"),
            relationship = "many-to-many") |> 
  na.omit()


modelling_data |> 
  ggplot(aes(IFR, fill = nursefamily)) + 
  geom_histogram(position = "dodge")

# Quite a class imbalance for specialist dementia and pediatric wards. 
# Let's look to bootstrap them a touch so we can beef up the data slightly and 
# model with more accuracy for rare events


data_to_bootstrap <- modelling_data |> 
  filter(!nursefamily %in% c("Adult_Inpatient", "Mental_Health_Inpatient"))

set.seed(123)

modelling_data <- data_to_bootstrap |> 
  group_by(nursefamily) |> 
  slice_sample(n = 1000, replace = T) |> 
  rbind(modelling_data) |> 
  ungroup() |> 
  dplyr::select(-measure_date_my)


modelling_data |> 
  ggplot(aes(IFR, fill = nursefamily)) + 
  geom_histogram(position = "dodge")

# Much better!


# 4. Poisson regression assumptions ------------------------------------------

testthat::test_that("Mean and variance terms are equal",{
  
  testthat::expect_equal(mean(IFR_split_data$IFR, na.rm = T), var(IFR_split_data$IFR, na.rm = T))
  
})

# Fails. Not ideal

## Multi-colinearity 
GGally::ggpairs(dplyr::select(modelling_data, 3:8))

# Visualize the correlation matrix
corrplot::corrplot(cor(dplyr::select(modelling_data, 3:7)), method = "circle")

# Eek, not great either.


# Let's look at the VIF of a test model to see if it's going to be problematic

test_nb_model <- glm.nb(formula = "IFR ~  OBD + PTA + SSUBA + SSUEO + VAC",
                        data = filter(modelling_data, IFR != 0))

testthat::test_that(
  "There is no multicollinearity", {
    testthat::expect_true(
      all(car::vif(test_nb_model) < 5)
    )
  })



test_logistic_model <- glm(formula = "IFR ~  OBD + PTA + SSUBA + SSUEO + VAC",
                           family = "binomial",
                           data = mutate(modelling_data, IFR = if_else(IFR > 0, 1, 0)))

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

modelling_data <- modelling_data |>
  group_by(sub_location_code) |> 
  mutate(fam_count = n_distinct(nursefamily)) |>
  ungroup(sub_location_code) |> 
  dplyr::select(-sub_location_code) |> 
  mutate(nursefamily = as.factor(nursefamily))


## 75% of the sample size
smp_size <- floor(0.75 * nrow(modelling_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(modelling_data)), size = smp_size)

train <- modelling_data[train_ind, ]
test <- modelling_data[-train_ind, ]


# Negative binomial GLMM using the function glm.nb()
base_model_backward_selection <- glm.nb(IFR ~  OBD + VAC + PTA + SSUBA + SSUEO + nursefamily + fam_count,
                                        data = train)


tidy(base_model_backward_selection)
glance(base_model_backward_selection)
summary(base_model_backward_selection)


zeroinfl_nb_model_ref_data <- pscl::zeroinfl(IFR ~ OBD + VAC + PTA + SSUBA + SSUEO | 
                                               OBD + VAC + PTA + SSUBA + SSUEO + nursefamily + fam_count,
                                             data = train, dist = "negbin")

base::summary(zeroinfl_nb_model_ref_data)
pscl::pR2(zeroinfl_nb_model_ref_data)
AIC(zeroinfl_nb_model_ref_data)


pscl::vuong(base_model_backward_selection, zeroinfl_nb_model_ref_data)


## Hurdle model:


hurdle_nb_model_ref_data <- pscl::hurdle(IFR ~  OBD + VAC + PTA + SSUBA + SSUEO | 
                                           OBD + VAC + PTA + SSUBA + SSUEO + nursefamily + fam_count, 
                                         data = train, 
                                         dist = "negbin")

base::summary(hurdle_nb_model_ref_data)
pscl::pR2(hurdle_nb_model_ref_data)


AIC(hurdle_nb_model_ref_data)


pscl::vuong(zeroinfl_nb_model_ref_data, hurdle_nb_model_ref_data)
pscl::vuong(base_model_backward_selection, hurdle_nb_model_ref_data)

AIC(base_model_backward_selection, zeroinfl_nb_model_ref_data, hurdle_nb_model_ref_data)

# Hurlde beats it out narrowly.

pscl::pR2(zeroinfl_nb_model_ref_data)
pscl::pR2(hurdle_nb_model_ref_data)


fm <- list("ZINB" = zeroinfl_nb_model_ref_data, "Hurdle-NB" = hurdle_nb_model_ref_data)
sapply(fm[1:2], function(x) round(x$coefficients$count, digits = 3))
sapply(fm[1:2], function(x) round(exp(x$coefficients$zero), digits = 3))



count_predictions <- ggeffects::ggpredict(hurdle_nb_model_ref_data, type = "count")
zero_probs <- ggeffects::ggpredict(hurdle_nb_model_ref_data, type = "zi_prob")
nurse_fam_only <- ggeffects::ggpredict(hurdle_nb_model_ref_data, term = "nursefamily", type = "zi_prob")



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





count_predictions <- ggeffects::ggpredict(hurdle_nb_model_ref_data, type = "count")
zero_probs <- ggeffects::ggpredict(hurdle_nb_model_ref_data, type = "zi_prob")
nurse_fam_only <- ggeffects::ggpredict(hurdle_nb_model_ref_data, term = "nursefamily", type = "zi_prob")
nurse_count_only <- ggeffects::ggpredict(hurdle_nb_model_ref_data, term = "fam_count", type = "zi_prob")


map_df(c(1:3), \(x){
  
  predict(hurdle_nb_model_ref_data, 
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










