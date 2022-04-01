## Data Clean

headers <- names(all_recons[[2]])

spine <- data.frame(all_recons[[2]][[1]])
for (i in 2:length(all_recons[[2]])){
  spine[,i] <- all_recons[[2]][[i]]
}

colnames(spine) <- headers

spine <- spine %>%
  dplyr::slice(2:length(spine[,1]),)