



# Hurdle model prediction




test <- bind_cols(rename(test, IFR_actual = IFR), IFR_predict = predict(hurdle_nb_model, newdata = rename(test, IFR_actual = IFR), interval = "confidence", level = 0.95))



test |> 
  mutate(IFR_predict = floor(IFR_predict),
         residuals = IFR_actual - IFR_predict) |> 
  ggplot(aes(residuals)) + 
  geom_histogram()


test |> 
  mutate(IFR_predict = floor(IFR_predict)) |> 
  pivot_longer(cols = starts_with("IFR"), names_to = "type", values_to = "value") |> 
  ggplot(aes(value, fill = type)) + 
  geom_density(position = "dodge")



hrdle_RMSE <- sqrt(mean((test$IFR_actual - test$IFR_predict)^2))








# Zero-inflated prediction
test <- modelling_data[-train_ind, ]


test <- bind_cols(rename(test, IFR_actual = IFR), IFR_predict = predict(zeroinfl_nb_model, rename(test, IFR_actual = IFR), type = "response", interval = "confidence"))



test |> 
  mutate(IFR_predict = floor(IFR_predict),
         residuals = IFR_actual - IFR_predict) |> 
  ggplot(aes(residuals)) + 
  geom_histogram()


test |> 
  mutate(IFR_predict = floor(IFR_predict)) |> 
  pivot_longer(cols = starts_with("IFR"), names_to = "type", values_to = "value") |> 
  ggplot(aes(value, fill = type)) + 
  geom_density(position = "dodge")





zero_infl_RMSE <- sqrt(mean((test$IFR_actual - test$IFR_predict)^2))




hrdle_RMSE
zero_infl_RMSE


