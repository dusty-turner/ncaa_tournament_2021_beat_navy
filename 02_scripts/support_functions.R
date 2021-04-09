library(tidymodels) 
library(vip)

conflicted::conflict_prefer(name = "pluck", winner = "purrr", losers = "rvest")
conflicted::conflict_prefer(name = "filter", winner = "dplyr", losers = "stats")

register_model <- function(current_model_data){
  
  registry <- read_csv("model_registry.csv")

  model_check <-
  current_model_data %>% 
    inner_join(registry)
  
  if(nrow(model_check) == 0){
    message("This is a new model")
  } else if(nrow(model_check) == 1){
     stop(str_c("This model has been done before with an ", model_check$performance_measure, " of ", round(model_check$model_performance,6)))
  }

  return(registry)
  
}


register_make_execute_evaluate <- function(data = stats, 
                                           type_of_model = "random_forest",
                                           mode = "classification",
                                           # mode = "regression",
                                           regression_formula = formula(win ~ ARPI + ABPI + BRPI + BBPI),
                                           # regression_formula = formula(win_by ~ ARPI + ABPI + BRPI + BBPI),
                                           grid_size = 1,
                                           prediction_data = stats_final,
                                           notes = "current best model make into regression",
                                           submission_mode = F
                                           ){
    
current_model_data <- tibble(type_of_model = type_of_model, 
                             mode = mode, 
                             regression_formula = str_c(as.character(regression_formula),collapse = ""),
                             grid_size = grid_size,
                             notes = notes)

if(!submission_mode){
  reg <- register_model(current_model_data = current_model_data)
}
    
set.seed(123)
splits <- initial_split(data, strata = Season)

stats_other <- training(splits)
stats_test  <- testing(splits)

set.seed(234)
val_set <- validation_split(stats_other, 
                            strata = Season, 
                            prop = 0.80)

cores <- parallel::detectCores()

message(str_c("you are using ", cores, " cores"))

if(type_of_model == "random_forest") {
  mod <-
    rand_forest(mtry = tune(),min_n = tune(),trees = tune()) %>%
    set_engine("ranger", num.threads = cores, keep.inbag = TRUE)
} else if(type_of_model == "log_reg"){
  mod <-
    logistic_reg(penalty = tune(),mixture = tune()) %>%
    set_engine("glmnet", num.threads = cores, keep.inbag = TRUE)
} else if(type_of_model == "boost_tree"){
  mod <-
    boost_tree(mtry = tune(),trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = 10) %>%
    set_engine("xgboost", num.threads = cores, keep.inbag = TRUE)
}

if (mode == "regression") {
  mod <-
    mod %>%
    set_mode("regression")
} else if (mode == "classification") {
  mod <-
    mod %>%
    set_mode("classification")
} 
  
  recipe <- 
    recipe(regression_formula, data = stats_other) %>%
    step_interact(terms = ~ conf_wins_against_B /(conf_wins_against_B + conf_loss_against_B)) %>%
    step_interact(terms = ~ conf_wins_against_A / (conf_wins_against_A + conf_loss_against_A)) %>%
    step_nzv(all_predictors(), - all_outcomes()) %>% 
    step_knnimpute(all_predictors()) %>%
    # step_meanimpute(all_numeric(), - all_outcomes()) %>% 
    step_center(all_numeric(), - all_outcomes()) %>% 
    step_scale(all_numeric(), - all_outcomes())  
  
  workflow <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(recipe)
  
  if(mode == "regression"){
    metrics <- c("rmse", "rsq", "ccc")
    metrics_id <- metric_set(rmse, rsq, ccc)
  } else if(mode == "classification"){
    metrics <- c("accuracy", "kap")
    metrics_id <- metric_set(accuracy, kap)
  }
  
  message(str_c("Begin CV to tune parameters with grid size of ", grid_size, " with ", metrics[1]), " on a ", mode, " model.")
  
  set.seed(345)
  res <- 
    workflow %>% 
    tune_grid(val_set,
              grid = grid_size,
              control = control_grid(save_pred = TRUE, verbose = T),
              metrics = metrics_id)
  
  message(str_c("Complete CV to tune parameters with grid size of ", grid_size))
  
  best <- 
    res %>% 
    select_best(metric = metrics[1])
  
  if(type_of_model == "random_forest"){
    last_mod <- 
      rand_forest(mtry = best$mtry, min_n = best$min_n, trees = best$trees) %>% 
      set_engine("ranger", num.threads = cores, keep.inbag=TRUE, importance = "impurity") 
  } else if(type_of_model == "log_reg"){
    last_mod <- 
      logistic_reg(penalty = best$penalty, mixture = best$mixture) %>% 
      set_engine("glmnet", num.threads = cores, keep.inbag=TRUE) 
  } else if(type_of_model == "boost_tree"){
    last_mod <- 
      boost_tree(mtry = best$min_n,trees = best$trees, min_n = best$min_n, tree_depth = best$tree_depth, learn_rate = best$learn_rate, loss_reduction = best$loss_reduction, sample_size = best$sample_size, stop_iter = 10) %>%
      set_engine("xgboost", num.threads = cores, keep.inbag=TRUE) 
  }
  
  if(mode == "regression"){
    
    last_mod <-  
      last_mod %>% 
      set_mode("regression")
  }else if(mode == "classification"){
    
    last_mod <-  
      last_mod %>% 
      set_mode("classification")
  }
  
  last_workflow <- 
    workflow %>% 
    update_model(last_mod)
  
  set.seed(345)
  last_fit <- 
    last_workflow %>% 
    last_fit(splits)
  
  message(str_c("Begin model on entire data"))
  
  final_model <- fit(last_workflow, data)
  
  if(mode == "regression"){
    
    message(str_c("Begin make predictions"))
    
    data_with_predictions <-
      prediction_data %>% 
      bind_cols(predict(final_model, new_data = prediction_data)) 
    
    if(!submission_mode){
    
      message(str_c("Score Model"))
  
      model_performance <-
        data_with_predictions %>% 
        group_by(Season) %>% 
        rmse(truth = win_by, estimate = .pred) %>% 
        summarise(mean_RMSE = mean(.estimate))
      
      message(str_c("This model has a ", metrics[1], " of ", round(model_performance$mean_RMSE,2)))
      
    
      new_registry <-
        reg %>% 
        bind_rows(
          current_model_data %>% 
            mutate(model_performance = as.double(model_performance), performance_measure = metrics[1])
        ) 
      
    }
   
  }else if(mode == "classification"){
    
    message(str_c("Begin make predictions"))
    
  
    data_with_predictions <-
      prediction_data %>% 
      bind_cols(predict(final_model, new_data = prediction_data, type = "prob")) %>% 
      bind_cols(predict(final_model, new_data = prediction_data)) 
    
    message("post set up data")
    
    if(!submission_mode){
      
    message(str_c("Score Model"))
    
    model_performance <-
      data_with_predictions %>% 
        mutate(across(.cols = c(.pred_lose,.pred_win),.fns = ~if_else(.>.999,.999,.) )) %>% 
        mutate(across(.cols = c(.pred_lose,.pred_win),.fns = ~if_else(.<.001,.001,.) )) %>% 
        mutate(kaggle = if_else(win == "win", -log(.pred_win), -log(.pred_lose))) %>% 
      group_by(Season) %>% 
        summarise(kaggle = mean(kaggle), accuracy = sum(.pred_class==win)/n()) %>% 
      summarise(mean_accuracy = mean(accuracy), mean_kaggle = mean(kaggle))
  
    message(str_c("This model has a ", metrics[1], " of ", round(model_performance$mean_accuracy,2), " and kaggle score of ", round(model_performance$mean_kaggle,6)))
  
    new_registry <-
      reg %>% 
      bind_rows(
        current_model_data %>% 
          mutate(model_performance = as.double(model_performance$mean_kaggle), performance_measure = "kaggle")
      ) 
    }
      
  }

  if(!submission_mode){
    write_csv(new_registry, "model_registry.csv")
  } else {
    if(mode=="regression"){
      data_with_predictions <-
        data_with_predictions %>%
        unite(col = ID, sep = "_", Season, WTeamID, LTeamID) %>%
        select(ID, Pred = contains("pred"))
    } else if(mode == "classification"){
      data_with_predictions <-
        data_with_predictions %>%
        unite(col = ID, sep = "_", Season, WTeamID, LTeamID) %>%
        select(ID, Pred = .pred_win)
    }
    id <-
    tibble(files = list.files("03_submissions")) %>% 
      mutate(files = str_remove(files, ".csv")) %>% 
      separate(files, sep = "_", c("trash","trash2","trash3","date","id")) %>% 
      select(date, id) %>% 
      mutate(date = lubridate::ymd(date), id = as.integer(id)) %>% 
      filter(date >= lubridate::today()) %>% 
      summarise(id = max(id)) %>% 
      mutate(id = ifelse(id==-Inf,1,id + 1)) %>% pull(id)
    
    write_csv(data_with_predictions, str_c("03_submissions/prediction_data_", mode,"_", lubridate::today(),"_",id, ".csv"))
  } 

 return(data_with_predictions)
}
