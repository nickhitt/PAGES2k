### Hindcasting

x <- all_temps %>%
  select(-Time, -global_temp, -North.America.Pollen.Temperature, -North.America.Trees.Temperature)

predicted_test <- all_temps %>%
  select(global_temp) %>%
  mutate(y_pred_lm = predict(model_lm, x),
         #y_pred_nnet = predict(model_nnet, x))
         y_pred_rf = predict(model_rf, x),
         y_pred_svml = predict(model_svml, x),
         y_pred_xgbl = predict(model_xgbl, x))


time <- seq(2003,2003-(nrow(predicted_test)-1), -1)

predicted_test$time <- time

var_names <- names(predicted_test)

predicted_test_long <- reshape(predicted_test, direction ="long", 
                          varying = var_names[1:length(var_names)-1], 
                          v.names = "temperature",
                          timevar = "Model",
                          times = var_names[1:length(var_names)-1])

all_models <- predicted_test_long %>%
  ggplot(aes(time,temperature)) +
  geom_point(data = subset(predicted_test_long, Model != "global_temp"), aes(colour = Model)) +
  geom_line(data = subset(predicted_test_long, Model != "global_temp"), aes(colour = Model)) +
  geom_point(data = subset(predicted_test_long, Model == "global_temp"), aes(color = "black"),
             color = "black") +
  geom_line(data = subset(predicted_test_long, Model == "global_temp"), aes(color = "black"),
            color = "black") 


all_models