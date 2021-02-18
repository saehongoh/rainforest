
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
# library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

train_reader <- function(file){
  readRDS(file) %>%
    group_by(unique_id, zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    group_by(unique_id) %>%
    mutate(value = value/max(value)) %>%
    ungroup() %>%
    select(unique_id, species_id, tscore, FreqHz, value, zts)
  }


keras_whole <- function(data){
  freq_bin <- data.frame(FreqHz = unique(data$FreqHz), 
                         FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,1,2,2)),
                         FreqBin2 = cut(unique(data$FreqHz), 4, labels = c(0,3,3,0)))
  
  tmp <- data  %>%
    left_join(., freq_bin, by="FreqHz")
  
  f1 <- tmp %>%
    dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    dplyr::filter(FreqBin2 == 3) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -cat1, -cat2, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  
  output_x_lab <- f1 %>% 
    dplyr::filter(zts == 0) %>% 
    select(unique_id, species_id, tscore) %>%
    mutate(tscore = as.numeric(as.character(tscore))) %>%
    mutate(species_id = paste0("s", species_id)) %>%
    dcast(unique_id ~ species_id, value.var = "tscore") %>%
    select(-unique_id) %>%
    as.matrix()
  return(list(output_x, output_x_lab))
}


# max_iter = 25
# files1 <- list.files("output/processed_data_set1", pattern="data", full.names = TRUE)
files <- list.files("output/processed_data_whole1", pattern="data", full.names = TRUE)
keys <- list.files("output/processed_data_whole1", pattern="key", full.names = TRUE)

# species <- unlist(strsplit(files[i],"_"))[[5]]
print("reading in files")
data <- do.call(rbind, lapply(1:length(files), function(x) train_reader(files[x])))
key <- do.call(rbind, lapply(1:length(keys), function(x) readRDS(keys[x])))

train_x <- key %>%
  group_by(species_id) %>%
  mutate(n=n()) %>%
  sample_n(., floor(n*.90))

train_y <- key[!(key$unique_id %in% train_x$unique_id),]

data_x <- data[data$unique_id %in% train_x$unique_id,]
data_y <- data[data$unique_id %in% train_y$unique_id,]

print("preparing for keras")

tmp <- keras_whole(data_x)
train_x <- tmp[[1]]
train_x_lab <- tmp[[2]]

tmp <- keras_whole(data_y)
train_y <- tmp[[1]]
train_y_lab <- tmp[[2]]

print("saving backup files")
saveRDS(train_x, "output/whole_train_x.RDS")
saveRDS(train_x_lab, "output/whole_train_x_lab.RDS")
saveRDS(train_y, "output/whole_train_y.RDS")
saveRDS(train_y_lab, "output/whole_train_y_lab.RDS")

# max_iter = 1
# 
# #### Model iterations
# iter_list <- vector('list', max_iter)
# j=1
# 
# repeat {
#   
#   model <- keras_model_sequential()
#   
#   model %>% 
#     layer_conv_2d(input_shape = c(dim(train_x)[-1]),
#                   filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     # layer_conv_2d(filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_max_pooling_2d(pool_size = c(2,2) ) %>%   #--------Max Pooling
#     layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
#     layer_dropout(rate = 0.60) %>%
#     layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dropout(rate = 0.50) %>%
#     layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dropout(rate = 0.50) %>%
#     layer_activation(activation = 'relu') %>%
#     layer_flatten() %>%
#     # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dense(units = 2, activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
#     # layer_dense(units = 2, activation = 'sigmoid') %>%
#     compile(
#       loss = 'categorical_crossentropy', 
#       metrics = c("accuracy"), 
#       optimizer = optimizer_adam(lr=1e-4)
#     )
#   
#   model %>%  fit(train_x, train_x_lab, batch_size = 10, epochs = 100, shuffle = TRUE, 
#                  validation_split=0.20,
#                  verbose=0,
#                  callbacks=list(callback_early_stopping(patience=5, verbose=1)))
#   
#   eval_x <- model %>% evaluate(train_x, train_x_lab)
#   eval_y <- model %>% evaluate(train_y, train_y_lab)
#   if(j==max_iter){break}
#   j = j + 1
# }
