library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr)
require(reshape2)

specmaker <- function(tmp_key, low_f, high_f) {
  # define path to audio file
  fin = paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/", tmp_key$file_id)
  data = readWave(fin)
  snd = data@left
  dur = length(snd) / data@samp.rate
  fs = data@samp.rate
  snd = snd - mean(snd)
  
  # plot waveform
  # plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
  
  # number of points to use for the fft
  nfft = 1024
  # window size (in points)
  window = 256*2
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
# species_list <- species_list[5:length(species_list)]

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
    res <- specmaker(tmp_key, low_f = 50, high_f = 9000)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() %>%
      mutate(value = (value - min(value)) / (max(value) - min(value)))
    # 
    # res %>%
    #   ggplot(aes(x=time, y=FreqHz, col=value)) +
    #   geom_tile() + 
    #   scale_color_gradient2(low = ("white"), mid = "grey90", midpoint = 0.50, high = ("red")) 
    
    if(tmp_key$cate == "true_positive"){
      print("processing positive samples")
      p1 <- res %>% filter(time >= (tmp_key$t_min-tmp_key$duration*0.2) & time <= (tmp_key$t_max-tmp_key$duration*0.2)) %>%
        mutate(unique_id = paste0(unique_id, "_m2"))
      p2 <- res %>% filter(time >= (tmp_key$t_min-tmp_key$duration*0.1) & time <= (tmp_key$t_max-tmp_key$duration*0.1)) %>%
        mutate(unique_id = paste0(unique_id, "_m1"))
      p3 <- res %>% filter(time >= tmp_key$t_min & time <= tmp_key$t_max) %>%
        mutate(unique_id = paste0(unique_id, "_neu"))
      p4 <- res %>% filter(time >= (tmp_key$t_min+tmp_key$duration*0.1) & time <= (tmp_key$t_max+tmp_key$duration*0.1)) %>%
        mutate(unique_id = paste0(unique_id, "_p1"))
      p5 <- res %>% filter(time >= (tmp_key$t_min+tmp_key$duration*0.2) & time <= (tmp_key$t_max+tmp_key$duration*0.2)) %>%
        mutate(unique_id = paste0(unique_id, "_p2"))
      
      res_list[[i]] <- rbind(p1, p2, p3, p4, p5) 
        # %>%
        # group_by(unique_id, FreqHz) %>%
        # mutate(value = value - median(value)) %>%
        # ungroup() %>%
        # mutate(value = (value - min(value)) / (max(value) - min(value)))
      rm(res)
    } else if(tmp_key$cate == "false_positive"){
      print("processing negative samples")
      res_list[[i]] <- res %>% 
        filter(time >= tmp_key$t_min & time <= tmp_key$t_max) %>%
        mutate(unique_id = paste0(unique_id, "_neu")) 
        # %>%
        # group_by(unique_id, FreqHz) %>%
        # mutate(value = value - median(value)) %>%
        # ungroup() %>%
        # mutate(value = (value - min(value)) / (max(value) - min(value)))
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
  saveRDS(res_list, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN_ver3/train_species_", species_list[j], "_data.RDS"))
  saveRDS(key, paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/output/CNN_ver3/train_species_", species_list[j], "_key.RDS"))
  }
