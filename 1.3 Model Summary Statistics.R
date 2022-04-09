#### Model Summary Statistics

models_list <- list(
  lm = model_lm,
  rf = model_rf,
  xgbl = model_xgbl,
  svm = model_svml
)

resamps <- resamples(models_list)

bwplot(resamps, metric = "RMSE")

bwplot(resamps, metric = "Rsquared")

bwplot(resamps, metric = "MAE")
