### Fitting Models

x <- to_train_training %>%
  select(-global_temp, -North.America.Pollen.Temperature, -North.America.Trees.Temperature)

y <- to_train_training$global_temp

preProcess <- c("knnImpute", "center", "scale", "pca")

# LM

model_lm <- train(
  y = y,
  x = x,
  method = "lm",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = preProcess
)

#Random Forest

model_rf <- train(
  y = y,
  x = x,
  method = "ranger",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = preProcess
)

#xgBoost Linear

model_xgbl <- train(
  y = y,
  x = x,
  method = "xgbLinear",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = preProcess
)

#SVM Linear

model_svml <- train(
  y = y,
  x = x,
  method = "svmLinear",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = preProcess
)

#Nueral Network

model_nnet <- train(
  y = y,
  x = x,
  method = "nnet",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = preProcess
)

#### Predicting Models

y_pred_lm <- predict(model_lm, x)
y_pred_rf <- predict(model_rf, x)
y_pred_xgbl <- predict(model_xgbl, x)
y_pred_svml <- predict(model_svml, x)
y_pred_nnet <- predict(model_nnet, x)

predicted <- data.frame(y, y_pred_lm, y_pred_rf,y_pred_xgbl, y_pred_svml,y_pred_nnet)
