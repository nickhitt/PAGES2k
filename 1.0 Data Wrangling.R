## Data Clean

headers <- names(all_recons[[2]]) ## pulling out list names

data <- data.frame(all_recons[[2]][[1]]) # pulling first record
for (i in 2:length(all_recons[[2]])){ #pulling all lists into DF
  data[,i] <- all_recons[[2]][[i]]
}

colnames(data) <- headers # renaming columns

data <- data %>% # getting rid of unit measurement
  dplyr::slice(2:length(data[,1]),)

time <- data.frame(seq(1,2003,1)) # generating spine vector
colnames(time) <- c("Time") # renaming

ind1 <- c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30) # indexes time and temperature data for each region

data <- data[,ind1] # pulling out the time and temperature data for each region

names <- c("Time", "Antarctic Temperature",
           "Time", "Arctic Temperature",
           "Time", "Asia Temperature",
           "Time", "Australasia Temperature",
           "Time", "Europe Temperature",
           "Time", "North America Pollen Temperature",
           "Time", "North America Trees Temperature",
           "Time", "South America Temperature")

colnames(data) <- names # applying names
#data <- apply(data,MARGIN = c(1,2),FUN = as.numeric) # formatting data

for (i in seq(1,length(ind1),2)){
  dat1 <- select(data,i,i+1) %>%
    mutate(Time = as.numeric(Time))
  time <- left_join(time,dat1,copy=TRUE, by = c("Time"))
}

wide_records_even <- time
