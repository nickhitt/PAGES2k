## Data Clean

headers <- names(all_recons[[2]])

data <- data.frame(all_recons[[2]][[1]])
for (i in 2:length(all_recons[[2]])){
  data[,i] <- all_recons[[2]][[i]]
}

colnames(data) <- headers

data <- data %>%
  dplyr::slice(2:length(spine[,1]),)



time <- data.frame(seq(1,2002,1))

colnames(time) <- c("Time")


