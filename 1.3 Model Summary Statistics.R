#### Model Summary Statistics

# On training data
models_list <- list(
  lm = model_lm,
  rf = model_rf,
  xgbl = model_xgbl,
  svm = model_svml,
  nnet = model_nnet)

resamps <- resamples(models_list)

bwplot(resamps, metric = "RMSE")

bwplot(resamps, metric = "Rsquared")

bwplot(resamps, metric = "MAE")

# On testing data

x_test <- to_train_test %>%
  select(-global_temp)

predicted_test <- to_train_test %>%
  select(global_temp) %>%
  mutate(y_pred_lm = predict(model_lm, x_test),
         y_pred_rf = predict(model_rf, x_test),
         y_pred_svml = predict(model_svml, x_test),
         y_pred_xgbl = predict(model_xgbl, x_test))



