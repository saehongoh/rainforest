library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")


train_fp <- read.csv("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/data/train_fp.csv") %>% mutate(cate = "false_positive") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=4)) %>%
  mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  mutate(species_id = as.numeric(as.character(species_id))) %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

train_tp <- read.csv("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/data/train_tp.csv") %>% mutate(cate = "true_positive") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=4)) %>%
  mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  mutate(species_id = as.numeric(as.character(species_id)))  %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

species_list <- sort(unique(train_tp$species_id))
species_list <- species_list[18:length(species_list)]

for(j in 1:length(species_list)){
  
  positives <- train_tp[train_tp$species_id == species_list[j],]
  negatives <- train_fp[train_fp$species_id == species_list[j],]
  sub_negs <- sample(negatives$unique_id, nrow(positives)*5)
  negatives <- negatives %>%
    filter(unique_id %in% sub_negs)
  
  if(all(max(c(positives$duration, negatives$duration)) == c(positives$duration, negatives$duration))){
    print("durations are the same size")
  } else {
    print("using average duration of positive signals")
    mean_duration <- mean(positives$duration)
    positives$t_max <- ifelse(positives$duration < mean_duration, 
                              positives$t_max + (mean_duration - positives$duration),
                              positives$t_max - (positives$duration - mean_duration))
    positives$duration <- positives$t_max - positives$t_min
    
    negatives$t_max <- ifelse(negatives$duration < mean_duration, 
                              negatives$t_max + (mean_duration - negatives$duration),
                              negatives$t_max - (negatives$duration - mean_duration))
    negatives$duration <- negatives$t_max - negatives$t_min
  }
  
  key <- rbind(positives, negatives)
  key <- key[sample(nrow(key)),]
  
  res_list <- vector('list', nrow(key))
  
  for(i in 1:length(res_list)){

    print(paste0(species_list[j]," ____ " , i))
    tmp_key <- key[i,]
    res <- specmaker(tmp_key, low_f = 50, high_f = 8500)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() %>%
      mutate(value = (value - min(value)) / (max(value) - min(value)))
    
    # res %>%
    #   ggplot(aes(x=time, y=FreqHz, col=value)) +
    #   geom_tile() +
    #   scale_color_viridis_c() +
    #   geom_vline(xintercept=tmp_key$t_min, col="red") +
    #   geom_vline(xintercept=tmp_key$t_max, col="red")

    if(tmp_key$cate == "true_positive"){
      print("processing positive samples")
      p1 <- res %>% filter(time >= (tmp_key$t_min-tmp_key$duration*0.15) & time <= (tmp_key$t_max-tmp_key$duration*0.15)) %>%
        mutate(unique_id = paste0(unique_id, "_m15"))
      p2 <- res %>% filter(time >= (tmp_key$t_min-tmp_key$duration*0.1) & time <= (tmp_key$t_max-tmp_key$duration*0.1)) %>%
        mutate(unique_id = paste0(unique_id, "_m1"))
      p3 <- res %>% filter(time >= (tmp_key$t_min-tmp_key$duration*0.05) & time <= (tmp_key$t_max-tmp_key$duration*0.05)) %>%
        mutate(unique_id = paste0(unique_id, "_m05"))
      p4 <- res %>% filter(time >= tmp_key$t_min & time <= tmp_key$t_max) %>%
        mutate(unique_id = paste0(unique_id, "_neu"))
      p5 <- res %>% filter(time >= (tmp_key$t_min+tmp_key$duration*0.05) & time <= (tmp_key$t_max+tmp_key$duration*0.05)) %>%
        mutate(unique_id = paste0(unique_id, "_p05"))
      p6 <- res %>% filter(time >= (tmp_key$t_min+tmp_key$duration*0.1) & time <= (tmp_key$t_max+tmp_key$duration*0.1)) %>%
        mutate(unique_id = paste0(unique_id, "_p1"))
      p7 <- res %>% filter(time >= (tmp_key$t_min+tmp_key$duration*0.15) & time <= (tmp_key$t_max+tmp_key$duration*0.15)) %>%
        mutate(unique_id = paste0(unique_id, "_p15"))
      
      res_list[[i]] <- rbind(p1, p2, p3, p4, p5, p6, p7) 

      rm(res)
    } else if(tmp_key$cate == "false_positive"){
      print("processing negative samples")
      res_list[[i]] <- res %>% 
        filter(time >= tmp_key$t_min & time <= tmp_key$t_max) %>%
        mutate(unique_id = paste0(unique_id, "_neu")) 
      rm(res)
    }
  }
  res_list <- do.call(rbind, res_list)
  print("summarizing")
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
  saveRDS(res_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN_ver4/train_species_", species_list[j], "_data.RDS"))
  saveRDS(key, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN_ver4/train_species_", species_list[j], "_key.RDS"))
  }
