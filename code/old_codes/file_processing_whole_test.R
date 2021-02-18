library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

### Import data keys
## False positives
train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false_positive") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  mutate(species_id = as.numeric(as.character(species_id))) %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

## True positives
train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true_positive") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  # mutate(species_id = as.numeric(as.character(species_id)))  %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

### Filter out some lone positive samples
# train_tp <- train_tp %>%
#   filter(!(species_id == 5 & duration > 3)) %>%
#   filter(!(species_id == 13 & duration > 3)) %>%
#   mutate(species_id = ifelse(species_id == 20 & duration > 3.5, species_id + 0.4, species_id))

# train_fp <- train_fp %>%
#   # filter(!(species_id == 5 & duration > 3)) %>%
#   # filter(!(species_id == 13 & duration > 3)) %>%
#   mutate(species_id = ifelse(species_id == 20 & duration > 3.5, species_id + 0.4, species_id))

# train_fp <- train_fp %>%
#   filter(species_id == 20) %>%
#   mutate(species_id = 20 + 0.4) %>%
#   rbind(., train_fp)

train_tp <- train_tp %>%
  mutate(score1 = duration/60) %>%
  mutate(score2 = (f_max-f_min)/max(train_tp$f_max)) %>%
  mutate(score3 = score1*score2) %>%
  mutate(tscore = score3/max(score3))

train_fp <- train_fp %>%
  mutate(score1 = duration/60) %>%
  mutate(score2 = (f_max-f_min)/max(train_tp$f_max)) %>%
  mutate(score3 = score1*score2)  %>%
  mutate(tscore = -score3/max(score3))


#### Loop

species_list <- sort(unique(train_tp$species_id))

for(j in 1:length(species_list)){
  
  positives <- train_tp[train_tp$species_id == species_list[j],]
  # negatives <- train_fp[train_fp$species_id == species_list[j],]
  # sub_negs <- sample(negatives$unique_id, nrow(positives))
  # negatives <- negatives %>% filter(unique_id %in% sub_negs)
  
  # key <- rbind(positives, negatives)
  key <- rbind(positives)
  key <- key[sample(nrow(key)),]
  
  res_list <- vector('list', nrow(key))
  
  for(i in 1:length(res_list)){

    print(paste0(species_list[j]," ____ " , i))
    tmp_key <- key[i,]
    res <- specmaker(tmp_key, low_f = 50, high_f = 8500, window_multiplier=4)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() %>%
      mutate(value = (value - min(value)) / (max(value) - min(value)))  %>%
      mutate(zts = seqt - min(seqt)) %>%
      group_by(zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value))
    
    res <- res %>% 
      mutate(unique_id = ifelse(time > 0 & time <= 20, paste0(unique_id, "_f1"), unique_id)) %>%
      mutate(unique_id = ifelse(time > 20 & time <= 40, paste0(unique_id, "_f2"), unique_id)) %>%
      mutate(unique_id = ifelse(time > 40 & time <= 60, paste0(unique_id, "_f3"), unique_id)) %>%
      mutate(score1 = ifelse(time > t_min & time < t_max, score1, 0)) %>%
      group_by(unique_id) %>%
      mutate(maxScore = max(score1) >0 ) %>%
      filter(maxScore == TRUE)
    
    res_list[[i]] <- res
    rm(res)
  }
  print("summarizing")
  res_list <- do.call(rbind, res_list)
  rowss <- res_list %>%
    group_by(unique_id) %>%
    summarise(n = n())
  print("slicing")
  res_list <- res_list %>%
    group_by(unique_id) %>%
    slice(1:min(rowss$n)) %>%
    mutate(zts = seqt - min(seqt)) %>%
    ungroup()
  print("writing file")
  
  saveRDS(res_list, paste0("output/processed_data_whole1/train_species_", species_list[j], "_data.RDS"))
  saveRDS(key, paste0("output/processed_data_whole1/train_species_", species_list[j], "_key.RDS"))
  }
