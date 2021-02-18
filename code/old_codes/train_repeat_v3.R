
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

####

max_iter = 25
files <- list.files("output/processed_data_set3", pattern="data", full.names = TRUE)
# files1 <- files[c(5, 14:15, 17:19, 23:26)]
# files2 <- files[!(files %in% files1)]
# files <- c(files1[6:length(files1)], files2)
files <- files[6]
for(i in 1:length(files)){
  
  data <- readRDS(files[i])
  
  data <- data %>%
    group_by(unique_id, zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value))  %>%
    group_by(unique_id) %>%
    mutate(value = value/max(value)) %>%
    ungroup()
  
  species <- unlist(strsplit(files[i],"_"))[[5]]

  # drop_aug <- as.character(c(2, 6, 8, 9, 12, 20, 22, 23, 23.4))
  # 
  # if(species %in% drop_aug){
  #   print("special case; using max 5% augmented data")
  #   data <- data %>% filter(grepl(paste(c("neu","m05","p05"), collapse="|"), unique_id))
  #   # data <- data %>% filter(grepl(paste(c("neu"), collapse="|"), unique_id))
  #   }
  
  #### Model iterations
  iter_list <- vector('list', max_iter)
  j=1
  
  repeat {
    print(paste(species," ____ ", j))
    
    pos_ids <- (unique(data[data$cate == "true_positive",]$unique_id))
    neg_ids <- (unique(data[data$cate == "false_positive",]$unique_id))
    
    if(length(pos_ids) > length(neg_ids)){
      toMatch <- paste(c("_neu","_p05","_m05"), collapse = "|")
      pos_ids <- c(pos_ids[grepl(toMatch,pos_ids)],
                   sample(pos_ids[!grepl(toMatch,pos_ids)], length(neg_ids) - length(pos_ids[grepl(toMatch,pos_ids)])))
    }
    
    data_tmp <- data %>%
      filter(unique_id %in% pos_ids | unique_id %in% neg_ids) 
    
    data_x <- data_tmp %>%
      filter(unique_id %in% sample(pos_ids, length(pos_ids)*0.9) | unique_id %in% sample(neg_ids, length(neg_ids)*0.9))
    
    data_y <- data_tmp %>%
      filter(!(unique_id %in% data_x$unique_id))
    
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

    tmp <- keras_prepper(data_x)
    train_x <- tmp[[1]]
    train_x_lab <- tmp[[2]]
    
    tmp <- keras_prepper(data_y)
    train_y <- tmp[[1]]
    train_y_lab <- tmp[[2]]
    
    res_table <- data.frame(file_name = files[i],species_id=species)
    res_table$dim_x <- list(dim(train_x))
    res_table$dim_y <- list(dim(train_y))
    res_table$train_positive <- train_sum$n[2]
    res_table$train_negatve <- train_sum$n[1]
    res_table$test_positive <- test_sum$n[2]
    res_table$test_negatve <- test_sum$n[1]
    
    rm(tmp)
    rm(data_y)
    rm(data_x)
    #########
    
    model <- keras_model_sequential()
    
    model %>% 
      layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                    filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
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
      layer_dense(units = 2, activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
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
    
    file_name = paste0("output/trained_models_set/species_", res_table$species_id, "_iter_", j, ".h5")
    
    iter_list[[j]] <- cbind(res_table, data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                                 test_loss = eval_y[[1]], test_accuracy = eval_y[[2]], results_name = file_name))
    
    model %>% save_model_hdf5(file_name)
    rm(model)

    tmp <- data.frame(do.call(rbind, iter_list))
    good_early_stop = length(which(tmp$test_accuracy >= 0.97))
    # bad_early_stop = length(which(tmp$test_accuracy <= 0.80))
    print(paste0("average accuracy thus far _____ ", round(mean(tmp$test_accuracy), digits=2)))
    if(good_early_stop == 3){break}
    if(j == max_iter){break}
    # if(j >= 10 & bad_early_stop >= 10){break}
    
    j = j + 1
    
  }
  print("finishing up")
  export <- do.call(rbind, iter_list)
  saveRDS(export, paste0("output/trained_models_set4/species_", res_table$species_id,"_res.RDS"))
  
}

