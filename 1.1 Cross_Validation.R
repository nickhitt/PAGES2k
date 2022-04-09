############### Pre-processing

####### Changing all data to numeric

all_temps <- data.frame(lapply(all_temps,as.numeric))

####### Creating Cross Validation Folds

# First slice data where we have global temps

to_train <-all_temps %>%
  filter(!is.na(global_temp)) %>%
  select(-Time)

folds <- createFolds(to_train$global_temp, k = 5)

myControl <- trainControl(verboseIter = TRUE, savePredictions = TRUE, index = folds)
