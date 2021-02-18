
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

####

files <- list.files("output/processed_data_set4", pattern="data", full.names = TRUE)
# files1 <- files[c(5, 14:15, 17:19, 23:26)]
# files2 <- files[!(files %in% files1)]
# files <- c(files1[6:length(files1)], files2)
# files <- files[6]

keys <- list.files("output/trained_models_set5/", pattern="res", full.names=TRUE)
res_tmp <- do.call(rbind, lapply(1:length(keys), function(x) readRDS(keys[x])))

res_tmp$ver <- do.call(rbind, strsplit(res_tmp$results_name,"_"))[,4]

test <- res_tmp %>%
  filter(ver == "V2") %>%
  mutate(total_test = test_positive + test_negatve) %>%
  mutate(wrong1 = (total_test-1)/total_test) %>%
  mutate(wrong2 = (total_test-2)/total_test) %>%
  mutate(wrong3 = (total_test-3)/total_test) %>%
  # mutate(over98 = test_accuracy >= 0.979,
  #        over96 = test_accuracy < 0.979 & test_accuracy >= 0.959) %>%
   group_by(file_name, wrong1, wrong2, wrong3, species_id) %>%
   summarise(max_acc = max(test_accuracy),
             n_at_wrong1 = length(which(test_accuracy >= wrong1)),
             n_at_wrong2 = length(which(test_accuracy >= wrong2 & test_accuracy < wrong1)),
             n_at_wrong3 = length(which(test_accuracy >= wrong3 & test_accuracy < wrong2)),
             max_iter = max(iter))

passed_species_to_omit1 <- test[test$n_at_wrong1 >= 2,]$file_name
passed_species_to_omit2 <- test[test$n_at_wrong2 >= 2 & test$n_at_wrong1 >= 1,]$file_name
# passed_species_to_omit3 <- test[test$n_at_wrong3 >= 3 & test$n_at_wrong2 >= 2,]$file_name

print(paste("omitting species ____ ", unique(c(passed_species_to_omit1, passed_species_to_omit2))))

files <- files[!(files %in% unique(c(passed_species_to_omit1, passed_species_to_omit2)))]

max_iter = 7

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
  
  
  # #### Model iterations V1
  # iter_list <- vector('list', max_iter)
  # j=1
  # 
  # repeat {
  #   print(paste(species," ____ V1 ____ ", j))
  #   
  #   pos_ids <- (unique(data[data$cate == "true_positive",]$unique_id))
  #   negs_to_sample <- length(which(grepl("neu",pos_ids)))*5
  #   neg_ids <- sample(unique(data[data$cate == "false_positive",]$unique_id), negs_to_sample)
  #   
  #   if(length(pos_ids) > length(neg_ids)){
  #     toMatch <- paste(c("_neu","_p05","_m05"), collapse = "|")
  #     pos_ids <- c(pos_ids[grepl(toMatch,pos_ids)],
  #                  sample(pos_ids[!grepl(toMatch,pos_ids)], length(neg_ids) - length(pos_ids[grepl(toMatch,pos_ids)])))
  #   }
  #   
  #   data_tmp <- data %>%
  #     filter(unique_id %in% pos_ids | unique_id %in% neg_ids) 
  #   
  #   data_x <- data_tmp %>%
  #     filter(unique_id %in% sample(pos_ids, length(pos_ids)*0.9) | unique_id %in% sample(neg_ids, length(neg_ids)*0.9))
  #   
  #   data_y <- data_tmp %>%
  #     filter(!(unique_id %in% data_x$unique_id))
  #   
  #   train_sum <- data_x %>%
  #     select(unique_id, cate) %>%
  #     distinct() %>%
  #     group_by(cate) %>%
  #     summarise(n=n(), .groups = 'drop')
  #   
  #   test_sum <- data_y %>%
  #     select(unique_id, cate) %>%
  #     distinct() %>%
  #     group_by(cate) %>%
  #     summarise(n=n(), .groups = 'drop')
  # 
  #   tmp <- keras_prepper(data_x)
  #   train_x <- tmp[[1]]
  #   train_x_lab <- tmp[[2]]
  #   
  #   tmp <- keras_prepper(data_y)
  #   train_y <- tmp[[1]]
  #   train_y_lab <- tmp[[2]]
  #   
  #   res_table <- data.frame(file_name = files[i],species_id=species)
  #   res_table$dim_x <- list(dim(train_x))
  #   res_table$dim_y <- list(dim(train_y))
  #   res_table$train_positive <- train_sum$n[2]
  #   res_table$train_negatve <- train_sum$n[1]
  #   res_table$test_positive <- test_sum$n[2]
  #   res_table$test_negatve <- test_sum$n[1]
  #   
  #   rm(tmp)
  #   rm(data_y)
  #   rm(data_x)
  #   #########
  #   
  #   model <- keras_model_sequential()
  #   
  #   model %>% 
  #     layer_conv_2d(input_shape = c(dim(train_x)[-1]),
  #                   filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
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
  #     layer_dense(units = 2, activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
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
  #   
  #   file_name = paste0("output/trained_models_set5/species_V1_", res_table$species_id, "_iter_", j, ".h5")
  #   
  #   iter_list[[j]] <- cbind(res_table, data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
  #                                test_loss = eval_y[[1]], test_accuracy = eval_y[[2]], results_name = file_name))
  #   
  #   model %>% save_model_hdf5(file_name)
  #   rm(model)
  # 
  #   tmp <- data.frame(do.call(rbind, iter_list))
  #   good_early_stop1 = length(which(tmp$test_accuracy >= 0.969))
  #   good_early_stop2 = length(which(tmp$test_accuracy >= 0.949))
  #   # bad_early_stop = length(which(tmp$test_accuracy <= 0.80))
  #   print(paste0("average accuracy thus far _____ ", round(mean(tmp$test_accuracy), digits=2)))
  #   if(good_early_stop1 == 2){break}
  #   if(good_early_stop2 == 2 & good_early_stop1 == 1){break}
  #   if(j == max_iter){break}
  # 
  #   j = j + 1
  #   
  # }
  # 
  # print("finishing up")
  # export <- do.call(rbind, iter_list)
  # saveRDS(export, paste0("output/trained_models_set5/species_V1_", res_table$species_id, "_res.RDS"))
  
  #### Model iterations V2
  iter_list <- vector('list', max_iter)
  j=1
  
  repeat {
    print(paste(species," ____ V2 ____ ", j))
    
    pos_ids <- (unique(data[data$cate == "true_positive",]$unique_id))
    negs_to_sample <- length(which(grepl("neu",pos_ids)))*5
    neg_ids <- sample(unique(data[data$cate == "false_positive",]$unique_id), negs_to_sample)
    
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
    
    tmp <- keras_prepper_v2(data_x)
    train_x <- tmp[[1]]
    train_x_lab <- tmp[[2]]
    
    tmp <- keras_prepper_v2(data_y)
    train_y <- tmp[[1]]
    train_y_lab <- tmp[[2]]
    
    res_table <- data.frame(file_name = files[i],species_id = species)
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
    
    iter_k = j + test[test$species_id == species,]$max_iter
    
    file_name = paste0("output/trained_models_set5/species_V2_", res_table$species_id, "_iter_", iter_k, ".h5")
    
    iter_list[[j]] <- cbind(res_table, data.frame(iter = iter_k, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                                                  test_loss = eval_y[[1]], test_accuracy = eval_y[[2]], results_name = file_name))
    
    model %>% save_model_hdf5(file_name)
    rm(model)
    
    tmp <- data.frame(do.call(rbind, iter_list))
    
    
    good_early_stop1 = length(which(tmp$test_accuracy >= test[test$species_id == species,]$wrong1))
    good_early_stop2 = length(which(tmp$test_accuracy >= test[test$species_id == species,]$wrong2))
    # bad_early_stop = length(which(tmp$test_accuracy <= 0.80))
    print(paste0("average accuracy thus far _____ ", round(mean(tmp$test_accuracy), digits=2)))
    if(good_early_stop1 >= 2){break}
    if(good_early_stop2 >= 2 & good_early_stop1 >= 1){break}
    if(j == max_iter){break}
    
    j = j + 1
    
  }
  
  print("finishing up")
  export <- do.call(rbind, iter_list)
  saveRDS(export, paste0("output/trained_models_set5/species_V2_", res_table$species_id, "_",paste0(min(export$iter), "-", max(export$iter)), "_res.RDS"))
  
}

