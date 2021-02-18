library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr)
require(reshape2)

specmaker <- function(tmp_key, low_f, high_f) {
  # define path to audio file
  fin = paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/", tmp_key$file_id)
  
  # read in audio file
  data = readWave(fin)
  
  # extract signal
  snd = data@left
  
  # determine duration
  dur = length(snd) / data@samp.rate
  
  # determine sample rate
  fs = data@samp.rate
  
  # demean to remove DC offset
  snd = snd - mean(snd)
  
  # plot waveform
  # plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
  
  # number of points to use for the fft
  nfft = 1024
  # window size (in points)
  window = 256
  # overlap (in points)
  overlap = 128
  
  # create spectrogram
  spec = specgram(
    x = snd,
    n = nfft,
    Fs = fs,
    window = window,
    overlap = overlap
  )
  # discard phase information
  P = abs(spec$S)
  
  # normalize
  P = P / max(P)
  
  # convert to dB
  P = 10 * log10(P)
  
  # config time axis
  t = spec$t
  
  time_key <-
    data.frame(variable = paste0("X", seq(1, length(t), 1)), 
               seqt = seq(1, length(t), 1), 
               time = round(t, digits=4))
  
  tmp <- data.frame(FreqHz = spec$f, (P)) %>%
    filter(FreqHz > low_f & FreqHz < high_f) %>%
    melt(., id.var = "FreqHz") %>%
    left_join(., time_key, by = "variable")
  tmp <- cbind(tmp_key, tmp, row.names = NULL) 
  
  return(tmp)
}

# ######################## Train X
# key1 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/pos_x.RDS")[[2]] %>%
#   mutate(duration = round(t_max - t_min, digits=4))
# 
# key2 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/neg_x.RDS")[[2]] %>%
#   mutate(duration = round(t_max - t_min, digits=4))
# 
# key <- rbind(key1, key2) %>%
#   mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
#          cat2 = ifelse(cate == "false_positive", 1, 0))
# key <- key[sample(nrow(key)),]
# 
# key <- key %>%
#   mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
#   mutate(species_id = ifelse(songtype_id ==4, paste0(species_id,".",songtype_id), species_id)) %>%
#   mutate(species_id = as.numeric(as.character(species_id)))
# 
# species_list <- sort(unique(key$species_id))
# # species_list <- species_list[19:length(species_list)]
# species_list <- species_list[26]
# 
# for(j in 1:length(species_list)) {
#   tmp <- key %>%
#     filter(species_id == species_list[j])
# 
#   if(nrow(tmp) > 150 & max(tmp$duration) > 4){
#     print("toomuch")
#     negs <- tmp %>%
#       filter(cate == "false_positive") %>%
#       sample_n(., 4*length(which(tmp$cate == "true_positive")))
#     tmp <- rbind(tmp %>% filter(cate=="true_positive"), negs)
#     tmp <- tmp[sample(nrow(tmp)),]
#     pos_list <- vector('list', nrow(tmp))
#     max_duration = max(tmp$duration)
# 
#     for (i in 1:nrow(tmp)) {
#       print(paste("trainX",j, i))
#       tmp_key <- tmp[i,]
#       res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
# 
#       if (tmp_key$duration != max_duration) {
#         bi_diff <- (max_duration - tmp_key$duration) / 2
#         res <- res  %>%
#           filter(time >= (tmp_key$t_min - bi_diff) &
#                    time <= ( (tmp_key$t_max -3)+ bi_diff))
#       } else {
#         res <- res %>%
#           filter(time >= tmp_key$t_min & time <= (tmp_key$t_max -3))
#       }
#       # print(dim(res))
# 
#       pos_list[[i]] <- res %>%
#         group_by(FreqHz) %>%
#         mutate(value = value - median(value)) %>%
#         ungroup() %>%
#         mutate(value = (value - min(value)) / (max(value) - min(value)))
#     }
# 
#   } else {
#     print("a-ok")
# 
#   pos_list <- vector('list', nrow(tmp))
#   max_duration = max(tmp$duration)
# 
#   for (i in 1:nrow(tmp)) {
#     print(paste("trainX",j, i))
#     tmp_key <- tmp[i,]
#     res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
# 
#     if (tmp_key$duration != max_duration) {
#       bi_diff <- (max_duration - tmp_key$duration) / 2
#       res <- res  %>%
#         filter(time >= (tmp_key$t_min - bi_diff) &
#                  time <= (tmp_key$t_max + bi_diff))
#     } else {
#       res <- res %>%
#         filter(time >= tmp_key$t_min & time <= tmp_key$t_max)
#     }
#     # print(dim(res))
# 
#     pos_list[[i]] <- res %>%
#       group_by(FreqHz) %>%
#       mutate(value = value - median(value)) %>%
#       ungroup() %>%
#       mutate(value = (value - min(value)) / (max(value) - min(value)))
#   }
#   }
#   pos_list <- do.call(rbind, pos_list)
#   print("summarizing")
#   rowss <- pos_list %>%
#     group_by(recording_id) %>%
#     summarise(n = n())
#   print("slicing")
#   pos_list <- pos_list %>%
#     group_by(recording_id) %>%
#     slice(1:min(rowss$n))
#   print("writing file")
#   saveRDS(pos_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN/trainX_species_", species_list[j], "_data.RDS"))
#   # saveRDS(tmp,paste0("output/CNN/species_", species_list[j], "_key.RDS"))
# }
# 
# ######################## Train Y
# key1 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/pos_y.RDS")[[2]] %>%
#   mutate(duration = round(t_max - t_min, digits=4)) 
# 
# key2 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/neg_y.RDS")[[2]] %>%
#   mutate(duration = round(t_max - t_min, digits=4)) 
# 
# key <- rbind(key1, key2) %>%
#   mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
#          cat2 = ifelse(cate == "false_positive", 1, 0))
# key <- key[sample(nrow(key)),]
# 
# key <- key %>%
#   mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
#   mutate(species_id = ifelse(songtype_id ==4, paste0(species_id,".",songtype_id), species_id)) %>%
#   mutate(species_id = as.numeric(as.character(species_id)))
# 
# 
# species_list <- sort(unique(key$species_id))
# species_list <- species_list[26]
# 
# for(j in 1:length(species_list)) {
#   tmp <- key %>%
#     filter(species_id == species_list[j]) 
#   
#   if(nrow(tmp) > 150 & max(tmp$duration) > 4){
#     print("toomuch")
#     negs <- tmp %>%
#       filter(cate == "false_positive") %>%
#       sample_n(., 4*length(which(tmp$cate == "true_positive")))
#     tmp <- rbind(tmp %>% filter(cate=="true_positive"), negs) 
#     tmp <- tmp[sample(nrow(tmp)),]
#     pos_list <- vector('list', nrow(tmp))
#     max_duration = max(tmp$duration)
#     
#     for (i in 1:nrow(tmp)) {
#       print(paste("trainY",j, i))
#       tmp_key <- tmp[i,]
#       res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
#       
#       if (tmp_key$duration != max_duration) {
#         bi_diff <- (max_duration - tmp_key$duration) / 2
#         res <- res  %>%
#           filter(time >= (tmp_key$t_min - bi_diff) &
#                    time <= ( (tmp_key$t_max -2)+ bi_diff))
#       } else {
#         res <- res %>%
#           filter(time >= tmp_key$t_min & time <= (tmp_key$t_max -2))
#       }
#       # print(dim(res))
#       
#       pos_list[[i]] <- res %>%
#         group_by(FreqHz) %>%
#         mutate(value = value - median(value)) %>%
#         ungroup() %>%
#         mutate(value = (value - min(value)) / (max(value) - min(value)))
#     }
#     
#   } else {
#     print("a-ok")
#     
#     pos_list <- vector('list', nrow(tmp))
#     max_duration = max(tmp$duration)
#     
#     for (i in 1:nrow(tmp)) {
#       print(paste("trainX",j, i))
#       tmp_key <- tmp[i,]
#       res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
#       
#       if (tmp_key$duration != max_duration) {
#         bi_diff <- (max_duration - tmp_key$duration) / 2
#         res <- res  %>%
#           filter(time >= (tmp_key$t_min - bi_diff) &
#                    time <= (tmp_key$t_max + bi_diff))
#       } else {
#         res <- res %>%
#           filter(time >= tmp_key$t_min & time <= tmp_key$t_max)
#       }
#       # print(dim(res))
#       
#       pos_list[[i]] <- res %>%
#         group_by(FreqHz) %>%
#         mutate(value = value - median(value)) %>%
#         ungroup() %>%
#         mutate(value = (value - min(value)) / (max(value) - min(value)))
#     }
#   }
#   pos_list <- do.call(rbind, pos_list)
#   print("summarizing")
#   rowss <- pos_list %>%
#     group_by(recording_id) %>%
#     summarise(n = n())
#   print("slicing")
#   pos_list <- pos_list %>%
#     group_by(recording_id) %>%
#     slice(1:min(rowss$n))
#   print("writing file")
#   saveRDS(pos_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN/trainY_species_", species_list[j], "_data.RDS"))
#   # saveRDS(tmp,paste0("output/CNN/species_", species_list[j], "_key.RDS"))
# }


######################## Train X Random regions

key1 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/pos_x.RDS")[[2]] %>%
  mutate(duration = round(t_max - t_min, digits=4))

key2 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/neg_x.RDS")[[2]] %>%
  mutate(duration = round(t_max - t_min, digits=4))

key <- rbind(key1, key2) %>%
  mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
         cat2 = ifelse(cate == "false_positive", 1, 0))
key <- key[sample(nrow(key)),]

key <- key %>%
  mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  mutate(species_id = ifelse(songtype_id ==4, paste0(species_id,".",songtype_id), species_id)) %>%
  mutate(species_id = as.numeric(as.character(species_id)))

species_list <- sort(unique(key$species_id))

for(j in 1:length(species_list)) {
  tmp <- key %>%
    filter(species_id == species_list[j])
  
  if(nrow(tmp) > 150 & max(tmp$duration) > 4){
    print("toomuch")
    negs <- tmp %>%
      filter(cate == "false_positive") %>%
      sample_n(., 4*length(which(tmp$cate == "true_positive")))
    tmp <- rbind(tmp %>% filter(cate=="true_positive"), negs)
    tmp <- tmp[sample(nrow(tmp)),]
    pos_list <- vector('list', nrow(tmp))
    max_duration = max(tmp$duration)
    
    for (i in 1:nrow(tmp)) {
      print(paste("trainX",j, i))
      tmp_key <- tmp[i,]
      res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
      
      times <- unique(res$time)
      times <- times[times < 50]
      start_time <- sample(times, 1)
      
      if (tmp_key$duration != max_duration) {
        bi_diff <- (max_duration - tmp_key$duration) / 2
        res <- res  %>%
          filter(time >= (start_time - bi_diff) &
                   time <= (start_time+tmp_key$duration -3) + bi_diff)
      } else {
        res <- res %>%
          filter(time >= start_time & time <= (start_time + tmp_key$duration -3))
      }
      # print(dim(res))
      
      pos_list[[i]] <- res %>%
        group_by(FreqHz) %>%
        mutate(value = value - median(value)) %>%
        ungroup() %>%
        mutate(value = (value - min(value)) / (max(value) - min(value)))
    }
    
  } else {
    print("a-ok")
    
    pos_list <- vector('list', nrow(tmp))
    max_duration = max(tmp$duration)
    
    for (i in 1:nrow(tmp)) {
      print(paste("trainX",j, i))
      tmp_key <- tmp[i,]
      res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
      
      times <- unique(res$time)
      times <- times[times < 50]
      start_time <- sample(times, 1)
      
      if (tmp_key$duration != max_duration) {
        bi_diff <- (max_duration - tmp_key$duration) / 2
        res <- res  %>%
          filter(time >= (start_time - bi_diff) &
                   time <= (start_time+tmp_key$duration) + bi_diff)
      } else {
        res <- res %>%
          filter(time >= start_time & time <= (start_time + tmp_key$duration))
      }
      # print(dim(res))
      
      pos_list[[i]] <- res %>%
        group_by(FreqHz) %>%
        mutate(value = value - median(value)) %>%
        ungroup() %>%
        mutate(value = (value - min(value)) / (max(value) - min(value)))
    }
  }
  pos_list <- do.call(rbind, pos_list)
  print("summarizing")
  rowss <- pos_list %>%
    group_by(recording_id) %>%
    summarise(n = n())
  print("slicing")
  pos_list <- pos_list %>%
    group_by(recording_id) %>%
    slice(1:min(rowss$n))
  print("writing file")
  saveRDS(pos_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN/trainX_species_", species_list[j], "_rand.RDS"))
  # saveRDS(tmp,paste0("output/CNN/species_", species_list[j], "_key.RDS"))
}

######################## Train Y
key1 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/pos_y.RDS")[[2]] %>%
  mutate(duration = round(t_max - t_min, digits=4)) 

key2 <- readRDS("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/neg_y.RDS")[[2]] %>%
  mutate(duration = round(t_max - t_min, digits=4)) 

key <- rbind(key1, key2) %>%
  mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
         cat2 = ifelse(cate == "false_positive", 1, 0))
key <- key[sample(nrow(key)),]

key <- key %>%
  mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  mutate(species_id = ifelse(songtype_id ==4, paste0(species_id,".",songtype_id), species_id)) %>%
  mutate(species_id = as.numeric(as.character(species_id)))


species_list <- sort(unique(key$species_id))
species_list <- species_list[26]

for(j in 1:length(species_list)) {
  tmp <- key %>%
    filter(species_id == species_list[j]) 
  
  if(nrow(tmp) > 150 & max(tmp$duration) > 4){
    print("toomuch")
    negs <- tmp %>%
      filter(cate == "false_positive") %>%
      sample_n(., 4*length(which(tmp$cate == "true_positive")))
    tmp <- rbind(tmp %>% filter(cate=="true_positive"), negs)
    tmp <- tmp[sample(nrow(tmp)),]
    pos_list <- vector('list', nrow(tmp))
    max_duration = max(tmp$duration)
    
    for (i in 1:nrow(tmp)) {
      print(paste("trainX",j, i))
      tmp_key <- tmp[i,]
      res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
      
      times <- unique(res$time)
      times <- times[times < 50]
      start_time <- sample(times, 1)
      
      if (tmp_key$duration != max_duration) {
        bi_diff <- (max_duration - tmp_key$duration) / 2
        res <- res  %>%
          filter(time >= (start_time - bi_diff) &
                   time <= (start_time+tmp_key$duration -3) + bi_diff)
      } else {
        res <- res %>%
          filter(time >= start_time & time <= (start_time + tmp_key$duration -3))
      }
      # print(dim(res))
      
      pos_list[[i]] <- res %>%
        group_by(FreqHz) %>%
        mutate(value = value - median(value)) %>%
        ungroup() %>%
        mutate(value = (value - min(value)) / (max(value) - min(value)))
    }
    
  } else {
    print("a-ok")
    
    pos_list <- vector('list', nrow(tmp))
    max_duration = max(tmp$duration)
    
    for (i in 1:nrow(tmp)) {
      print(paste("trainX",j, i))
      tmp_key <- tmp[i,]
      res <- specmaker(tmp_key, low_f = 50, high_f = 10000)
      
      times <- unique(res$time)
      times <- times[times < 50]
      start_time <- sample(times, 1)
      
      if (tmp_key$duration != max_duration) {
        bi_diff <- (max_duration - tmp_key$duration) / 2
        res <- res  %>%
          filter(time >= (start_time - bi_diff) &
                   time <= (start_time+tmp_key$duration) + bi_diff)
      } else {
        res <- res %>%
          filter(time >= start_time & time <= (start_time + tmp_key$duration))
      }
      # print(dim(res))
      
      pos_list[[i]] <- res %>%
        group_by(FreqHz) %>%
        mutate(value = value - median(value)) %>%
        ungroup() %>%
        mutate(value = (value - min(value)) / (max(value) - min(value)))
    }
  }
  pos_list <- do.call(rbind, pos_list)
  print("summarizing")
  rowss <- pos_list %>%
    group_by(recording_id) %>%
    summarise(n = n())
  print("slicing")
  pos_list <- pos_list %>%
    group_by(recording_id) %>%
    slice(1:min(rowss$n))
  print("writing file")
  saveRDS(pos_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN/trainY_species_", species_list[j], "_rand.RDS"))
  # saveRDS(tmp,paste0("output/CNN/species_", species_list[j], "_key.RDS"))
}