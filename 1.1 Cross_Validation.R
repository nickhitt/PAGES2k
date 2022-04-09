############### Pre-processing

####### Changing all data to numeric

all_temps <- data.frame(lapply(all_temps,as.numeric))

####### Creating Cross Validation Folds

set.seed(42)

# First slice data where we have global temps

to_train <-all_temps %>%
  filter(!is.na(global_temp)) %>%
  select(-Time)

to_train_training <- sample(nrow(to_train), round(0.8*nrow(to_train)),
                                     replace = FALSE)
to_train_test <- to_train[-to_train_training,]
to_train_training <- to_train[to_train_training,]


folds <- createFolds(to_train_training$global_temp, k = 5)

myControl <- trainControl(verboseIter = TRUE, savePredictions = TRUE, index = folds)
