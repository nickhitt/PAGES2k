### Fitting Models

set.seed(42)

x <- to_train %>%
  select(-global_temp, -North.America.Pollen.Temperature, -North.America.Trees.Temperature)

y <- to_train$global_temp

# LM

model_lm <- train(
  y = y,
  x = x,
  method = "lm",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = "knnImpute"
)

#Random Forest

model_rf <- train(
  y = y,
  x = x,
  method = "ranger",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = "knnImpute"
)

#xgBoost Linear

model_xgbl <- train(
  y = y,
  x = x,
  method = "xgbLinear",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = "knnImpute"
)

#SVM Linear

model_svml <- train(
  y = y,
  x = x,
  method = "svmLinear",
  #tuneGrid = grid,
  trControl = myControl,
  preProcess = "knnImpute"
)

#### Predicting Models

y_pred_lm <- predict(model_lm, x)
y_pred_rf <- predict(model_rf, x)
y_pred_xgbl <- predict(model_xgbl, x)
y_pred_svml <- predict(model_svml, x)

predicted <- data.frame(y, y_pred_lm, y_pred_rf,y_pred_xgbl, y_pred_svml)
