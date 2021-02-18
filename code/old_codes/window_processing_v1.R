
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
# library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

train_reader <- function(file, mask=FALSE){
  tmp <- readRDS(file) %>%
    group_by(unique_id, zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    group_by(unique_id) %>%
    mutate(value = value/max(value)) %>%
    ungroup() 
  
  if(mask == TRUE){
    tmp %>%
      select(unique_id, species_id, tscore, FreqHz, t_min, t_max, f_min, f_max, time, value, zts) %>%
      group_by(unique_id) %>%
      mutate(value = ifelse(FreqHz < f_min | FreqHz > f_max, 0, value)) %>%
      mutate(value = ifelse(time < t_min | time > t_max, 0, value)) %>%
      ungroup() %>%
      mutate(unique_id = paste0(unique_id, "_m")) %>%
      select(-f_min, -f_max)
  } else {
    tmp %>%
      select(unique_id, species_id, tscore, FreqHz, t_min, t_max, time, value, zts)
  }
  }


files <- list.files("output/processed_data_whole2", pattern="data", full.names = TRUE)
files <- files[!grepl("dataX|dataY",files)]
keys <- list.files("output/processed_data_whole2", pattern="key", full.names = TRUE)
key <- do.call(rbind, lapply(1:length(keys), function(x) readRDS(keys[x])))

for(i in 1:length(files)){
  print(files[i])
  data <- rbind(train_reader(files[i], mask=FALSE),
                train_reader(files[i], mask=TRUE))

  key <- readRDS(keys[i])

  data <- data %>%
    # mutate(time = as.numeric(as.character(time))) %>%
    mutate(unique_id2 = case_when(time >= 0 & time < 20 ~ paste0(unique_id, "_f1"),
                                  time >= 20 & time < 40 ~ paste0(unique_id, "_f2"),
                                  time >= 40 & time < 60 ~ paste0(unique_id, "_f3"))) %>%
    mutate(tscore = ifelse(time > t_min & time < t_max, tscore, 0)) %>%
    group_by(unique_id2) %>%
    mutate(maxScore = max(tscore) >0) %>%
    filter(maxScore == TRUE) %>%
    select(-t_min, -t_max, -time, -maxScore)

  train_x <- sample(key$unique_id, nrow(key)*.90)
  train_y <- key[!(key$unique_id %in% train_x),]$unique_id

  data_x <- data[data$unique_id %in% c(train_x, paste0(train_x, "_m")),]
  data_y <- data[data$unique_id %in% c(train_y, paste0(train_y, "_m")),]

  data_x <- data_x %>%
    mutate(unique_id = unique_id2)

  data_y <- data_y %>%
    mutate(unique_id = unique_id2)

  data_x <- data_x[,1:ncol(data_x)-1]
  data_y <- data_y[,1:ncol(data_y)-1]

  saveRDS(data_x, paste0(files[i], "_dataX.RDS"))
  saveRDS(data_y, paste0(files[i], "_dataY.RDS"))
  }

##########################################################################
rm(list = setdiff(ls(), lsf.str()))
##########################################################################


