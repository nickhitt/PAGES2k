### Visualising

time <- seq(2003,2003-(nrow(predicted)-1), -1)

predicted$time <- time

predicted <- predicted 

var_names <- names(predicted)

predicted_long <- reshape(predicted, direction ="long", 
                          varying = var_names[1:length(var_names)-1], 
                          v.names = "temperature",
                          timevar = "Model",
                          times = var_names[1:length(var_names)-1])

all_models <- predicted_long %>%
  ggplot(aes(time,temperature, color = Model)) +
  geom_point() +
  geom_line() 
  

all_models
