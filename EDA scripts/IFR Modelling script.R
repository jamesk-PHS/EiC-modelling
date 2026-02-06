
library(tidyverse)
library(patchwork)
library(broom)
library(patchwork)
library(boot)


setwd("/conf/EIC/Personal/James/Checks on data/2025-11-11 Modelling")


data <- read.csv("Data/Overview TDE - data table (3).csv",
                 fileEncoding = "UTF-16LE",
                 sep = "\t", header = T) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(hb_name != "PUBLIC HEALTH SCOTLAND")


IFR_split_data <- data |> 
  filter(measure_id == "IFR") |>
  select(-calc_rate) |> 
  pivot_wider(names_from = measure_id, values_from = numerator) |>  
  mutate(across(c(8:9), as.numeric)) |>  
  rename(OBD = denominator)


other_data <- data |> 
  filter(measure_id != "IFR") |>
  select(-c(numerator, denominator)) |> 
  pivot_wider(names_from = measure_id, values_from = calc_rate) |>  
  mutate(across(c(9:37), as.numeric))


modelling_data <- left_join(IFR_split_data, other_data) |> 
  select(c("OBD", "IFR", "PTA", "SSUBA", "SSUEO", "VAC")) |>  
  na.omit()



variables <- colnames(modelling_data[c(1, 3:6)])
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+")
  tmp <- paste0("IFR~", tmp)
  formulas[[i]] <- tmp
}
formulas <- unlist(formulas)


## 75% of the sample size
smp_size <- floor(0.75 * nrow(modelling_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(modelling_data)), size = smp_size)

train <- modelling_data[train_ind, ]
test <- modelling_data[-train_ind, ]

results <- tibble()


#job::job({
#  map(1:5, function(x){
#    
#    
#    model <- glm(formulas[x],
#                 family = "poisson", 
#                 data = train)
#    
##    summary <- summary(model)
#    
#    prediction <- cbind(test,
#                        IFR_pred = signif(predict.glm(model, newdata = test, type = "response"), 1)) |> 
#      mutate(diff = IFR - IFR_pred)
#    
#    
#    tibble <- tibble(formula = x,
#                     stat_aic = summary$aic,
#                     residuals = list(residuals(model)),
#                     null_deviance = summary$null.deviance,
#                     residual_deviance = summary$deviance.resid,
#                     deviance_ratio = null_deviance/residual_deviance,
#                     iterations = summary$iter,
#                     predictions = list(prediction$diff)
#    )
#    
#    results <<- append(results, tibble)              
#    
#    
#  })
#  
#})



job::job({
  map(1:length(formulas), function(x){
    
    
    model <- glm(formulas[x],
                 family = "poisson", 
                 data = train)
    
    tibble <- tibble(formula = formulas[x],
                     null_deviance = glance(model)$null.deviance,
                     residual_deviance = glance(model)$deviance,
                     AIC_stat = broom::glance(model)$AIC) |> 
                     mutate(deviance_ratio = 1 - residual_deviance/null_deviance)
    
    results <<- rbind(results, tibble)              
    
    
  })
  
})

results |> 
  ggplot() +
  geom_line(aes(c(1:31), AIC_stat), colour = "green") +
  geom_smooth(aes(c(1:31), AIC_stat), colour = "green")
  #geom_line(aes(c(1:31), deviance_ratio), colour = "red") +
  #geom_smooth(aes(c(1:31), deviance_ratio), colour = "red") 


plotly::ggplotly()



results

residual_results <- tibble()

job::job({
  map(1:length(formulas), function(x){
    
    
    model <- glm(formulas[x],
                 family = "poisson", 
                 data = train)
    
    tibble <- tibble(formula = x,
                     residuals = list(residuals(model))
    )
    
    residual_results <<- rbind(residual_results, tibble)              
    
    
  })
  
})

library(ggridges)

residual_results |>
  unnest() |> 
  ggplot(aes(residuals, formula, group = formula, fill = factor(formula))) + 
  ggridges::geom_density_ridges2() +
  geom_vline(aes(xintercept = 0, fill = "black", linewidth = 2)) +
  xlim(-5, 5)

residual_results |>
  unnest() |> 
  ggplot(aes(formula, residuals, group = formula, fill = factor(formula))) + 
  geom_boxplot() +
  geom_hline(aes(yintercept = 0, clour = "black"))

residual_results |>
  unnest() |> 
  ggplot(aes(sample = residuals)) + 
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~formula)
  

residual_results |>
  unnest() |> 
  group_by(formula) |> 
  summarise(mean = mean(residuals)) |> 
  print(n = 31)



prediction_results <- tibble()

job::job({
  map(1:length(formulas), function(x){
    
    
    model <- glm(formulas[x],
                 family = "poisson", 
                 data = train)
    
    prediction <- cbind(test,
                        IFR_pred = floor(predict.glm(model, newdata = test, type = "response"))) |> 
                        mutate(diff = IFR - IFR_pred)
    
    tibble <- tibble(formula = formulas[x],
                     predictions = list(prediction$diff)) 
    
    prediction_results <<- rbind(prediction_results, tibble)              
    
    
  })
  
})



prediction_results |> 
  unnest() |> 
  ggplot(aes(predictions, formula, group = formula, fill = factor(formula))) + 
  ggridges::geom_density_ridges2() +
  geom_vline(aes(xintercept = 0, fill = "black", linewidth = 2)) +
  xlim(-5, 5)


prediction_results |> 
  unnest() |> 
  ggplot(aes(predictions, formula, group = formula, fill = factor(formula))) + 
  geom_boxplot(show.legend =  F) +
  geom_vline(aes(xintercept = 0, fill = "black"))
