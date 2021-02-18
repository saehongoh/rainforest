##################################################
### Import .wav files as data.frames 
##################################################

specmaker <- function(tmp_key, low_f, high_f, window_multiplier=3) {
  # define path to audio file
  # fin = paste0("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/", tmp_key$file_id)
  fin = paste0(tmp_key)
  data = tuneR::readWave(fin)
  snd = data@left
  dur = length(snd) / data@samp.rate
  fs = data@samp.rate
  snd = snd - mean(snd)
  
  # plot waveform
  # plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
  
  # number of points to use for the fft
  nfft = 1024
  # window size (in points)
  window = 256*window_multiplier
  # overlap (in points)
  overlap = 128
  
  # create spectrogram
  spec = signal::specgram(
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

##############################################
### Makes matrices for keras input
##############################################

keras_prepper <- function(input_data){
  tmp <- input_data  %>%
    mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
           cat2 = ifelse(cate == "false_positive", 1, 0)) %>%
    group_by(unique_id) %>%
    mutate(zts = seqt - min(seqt)) %>%
    ungroup() %>%
    select(unique_id, cat1, cat2, FreqHz, value, zts)
  
  freq_bin <- data.frame(FreqHz = unique(tmp$FreqHz), FreqBin = cut(unique(tmp$FreqHz), 4, labels = c(1,2,3,4)))
  
  f1 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin == 1) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin == 2) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz)
  
  
  f3 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin == 3) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz) 
  
  f4 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin == 4) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f2 %>% filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f3 %>% filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f4 %>% filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  output_x_lab <- f1 %>% filter(zts == 0) %>% select(cat2) %>% as.matrix() %>% keras::to_categorical() 
  return(list(output_x, output_x_lab))
}

##############################################
### Makes matrices for keras input
##############################################


keras_prepper_v2 <- function(input_data){
  tmp <- input_data  %>%
    mutate(cat1 = ifelse(cate == "true_positive", 1, 0),
           cat2 = ifelse(cate == "false_positive", 1, 0)) %>%
    group_by(unique_id) %>%
    mutate(zts = seqt - min(seqt)) %>%
    ungroup() %>%
    select(unique_id, cat1, cat2, FreqHz, value, zts)
  
  freq_bin <- data.frame(FreqHz = unique(tmp$FreqHz), 
                         FreqBin1 = cut(unique(tmp$FreqHz), 4, labels = c(1,1,2,2)),
                         FreqBin2 = cut(unique(tmp$FreqHz), 4, labels = c(0,3,3,0)))
  
  f1 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz)
  
  f3 <- tmp %>%
    left_join(., freq_bin, by="FreqHz") %>%
    dplyr::filter(FreqBin2 == 3) %>%
    dcast(unique_id + cat1 + cat2 + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  output_x_lab <- f1 %>% dplyr::filter(zts == 0) %>% select(cat2) %>% as.matrix() %>% keras::to_categorical() 
  return(list(output_x, output_x_lab))
}

##############################################
### Makes matrices for test input
##############################################

test_keras_prepper_v2 <- function(tmp, train_dim){
  
  train_dim <- unlist(train_dim)
  y_dim <- train_dim[3]
  zts <- as.numeric(as.character(unique(tmp$zts)))
  max_zt <- max(unique(tmp$zts))
  
  frames <-  data.frame(zts = zts, 
                        breaks1 = cut(zts, include.lowest=TRUE, breaks=seq(0, max_zt, y_dim-1)), 
                        breaks2 = cut(zts, include.lowest=TRUE, breaks=seq(floor(y_dim*0.333), floor(y_dim*0.333) + max_zt, y_dim-1)), 
                        breaks3 = cut(zts, include.lowest=TRUE, breaks=seq(floor(y_dim*0.667), floor(y_dim*0.667) + max_zt, y_dim-1))) %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, as.character(x))) %>%
    melt(., id.vars=c("zts")) %>%
    filter(value != 0) %>%
    mutate(frame_id = paste0(variable, "_", value)) %>%
    select(-variable, -value) %>%
    mutate(zts = as.numeric(as.character(zts)))
  
  tmp <- tmp %>%
    left_join(frames, ., by ="zts") %>%
    group_by(frame_id, FreqHz) %>%
    mutate(zts2 = 1:n()) %>%
    ungroup() %>%
    mutate(zts = as.numeric(as.character(zts2)))
  
  freq_bin <- data.frame(FreqHz = unique(tmp$FreqHz), 
                         FreqBin1 = cut(unique(tmp$FreqHz), 4, labels = c(1,1,2,2)),
                         FreqBin2 = cut(unique(tmp$FreqHz), 4, labels = c(0,3,3,0)))
  tmp <- tmp %>%
    select(frame_id, zts, value, FreqHz) %>%
    left_join(., freq_bin, by="FreqHz") 
  
  f1 <- tmp %>%
    filter(FreqBin1 == 1) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    filter(FreqBin1 == 2) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    filter(FreqBin2 == 3) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix(),
         f2 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix(),
         f3 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  if(all(dim(output_x)[-1] != unlist(train_dim)[-1])){print("dimensions don't match")}
  return(list(output_x))
}

##############################################
### Makes matrices for test input
##############################################

test_reader <- function(test_file_wav, window_multiplier=2){
  specmaker(test_file_wav, low_f = 50, high_f = 8500, window_multiplier = window_multiplier)  %>%
    group_by(FreqHz) %>%
    mutate(value = value - mean(value)) %>%
    ungroup() %>%
    mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
    mutate(zts = seqt - min(seqt) + 1) %>%
    ungroup() %>%
    group_by(zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) 
}