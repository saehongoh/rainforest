
# library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

####

max_iter = 25
files <- list.files("output/CNN_ver4", pattern="data", full.names = TRUE)
# long_durations <- as.character(c(12, 2, 20, 22, 23, 23.4, 6, 8, 9))
# files <- files[c(1:4,6:13,16,20:22,24)]
# files <- rev(files)
files <- files[6:7]

for(i in 1:length(files)){
  
  data <- readRDS(files[i])
  
  data <- data %>%
    group_by(unique_id, zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value))
  
  species <- unlist(strsplit(files[i],"_"))[[4]]
  # 
  # long_durations <- as.character(c(12, 2, 20, 22, 23, 23.4, 6, 8, 9))
  # 
  # if(species %in% long_durations){
  #   quantiles <- quantile(data$zts, seq(0,1,0.2))
  #   data <- data %>%
  #     filter(zts >= quantiles[[2]] & zts <= quantiles[[5]])
  #   }
  
  #### Model iterations
  iter_list <- vector('list', max_iter)
  j=1
  
  repeat {
    print(paste(species," ____ ", j))
    
    data_x <- data %>%
      filter(unique_id %in% sample(unique(data$unique_id), length(unique(data$unique_id))*0.9))
    
    data_y <- data %>%
      filter(!(unique_id %in% data_x$unique_id))

    tmp <- keras_prepper(data_x)
    train_x <- tmp[[1]]
    train_x_lab <- tmp[[2]]
    
    tmp <- keras_prepper(data_y)
    train_y <- tmp[[1]]
    train_y_lab <- tmp[[2]]

    train_sum <- data_x %>%
      select(unique_id, cate) %>%
      distinct() %>%
      group_by(cate) %>%
      summarise(n=n(), .groups = 'drop')
    
    test_sum <- data_y %>%
      select(unique_id, cate) %>%
      distinct() %>%
      group_by(cate) %>%
      summarise(n=n(), .groups = 'drop')
    
    res_table <- data.frame(file_name = files[i],species_id=species)
    res_table$dim_x <- list(dim(train_x))
    res_table$dim_y <- list(dim(train_y))
    res_table$train_positive <- train_sum$n[2]
    res_table$train_negatve <- train_sum$n[2]
    res_table$test_positive <- test_sum$n[2]
    res_table$test_negatve <- test_sum$n[2]
    
    rm(tmp)
    rm(data_y)
    rm(data_x)
    #########
    
    model <- keras_model_sequential()
    
    model %>% 
      layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                    filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      # layer_conv_2d(filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_max_pooling_2d(pool_size = c(2,2) ) %>%   #--------Max Pooling
      layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
      layer_dropout(rate = 0.60) %>%
      layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_dropout(rate = 0.50) %>%
      layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_dropout(rate = 0.50) %>%
      layer_activation(activation = 'relu') %>%
      layer_flatten() %>%
      # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
      layer_dense(units = 2, activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
      # layer_dense(units = 2, activation = 'sigmoid') %>%
      compile(
        loss = 'categorical_crossentropy',
        metrics = c("accuracy"), 
        optimizer = optimizer_adam(lr=1e-4)
      )
    
    model %>%  fit(train_x, train_x_lab, batch_size = 10, epochs = 100, shuffle = TRUE, 
                   validation_split=0.20,
                   verbose=0,
                   callbacks=list(callback_early_stopping(patience=5, verbose=1)))
    
    eval_x <- model %>% evaluate(train_x, train_x_lab)
    eval_y <- model %>% evaluate(train_y, train_y_lab)
    
    file_name = paste0("models_ver3/species_", res_table$species_id, "_iter_", j+50, ".h5")
    
    iter_list[[j]] <- cbind(res_table, data.frame(iter = j+50, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                                 test_loss = eval_y[[1]], test_accuracy = eval_y[[2]], results_name = file_name))
    
    model %>% save_model_hdf5(file_name)
    rm(model)
    # tmp <- readRDS("/Users/eoh/Documents/R_projects/rainforest/models_ver3/species_0_res.RDS")
    tmp <- data.frame(do.call(rbind, iter_list))
    good_early_stop = length(which(tmp$test_accuracy >= 0.945))
    # bad_early_stop = length(which(tmp$test_accuracy <= 0.80))
    print(paste0("average accuracy thus far _____ ", round(mean(tmp$test_accuracy), digits=2)))
    if(good_early_stop == 3 | j == max_iter){break}
    # if(j >= 10 & bad_early_stop >= 10){break}
    
    j = j + 1
    
  }
  
  export <- do.call(rbind, iter_list)
  saveRDS(export, paste0("models_ver3/species_", res_table$species_id,"_res3.RDS"))
  
}

