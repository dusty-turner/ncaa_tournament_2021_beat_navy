library(tidyverse)
source("02_scripts/support_functions.R")
source("02_scripts/data_prep.R")

cores <- parallel::detectCores()


stage_2_submission_data <-
  read_csv("01_data/MSampleSubmissionStage2.csv") %>% 
  select(-Pred) %>% 
  separate(col = ID, into = c("Season", "WTeamID", "LTeamID"), sep = "_", convert = T) %>% 
  left_join(staging_data, by = c("WTeamID" = "TeamID", "Season")) %>% 
                left_join(staging_data, by = c("LTeamID" = "TeamID", "Season"), suffix = c("_A", "_B")) %>%
  mutate(conf_record_one = str_c(conf_A, "_", conf_B)) %>%
  mutate(conf_record_two = str_c(conf_B, "_", conf_A)) %>%
  left_join(conf_rank, by = c("Season", "conf_record_one" = "conf_record")) %>% 
  left_join(conf_rank, by = c("Season", "conf_record_two" = "conf_record"), suffix = c("_against_B", "_against_A")) %>%
  select(-c(conf_record_one,conf_record_two,conf_A,conf_B)) %>% 
  mutate(across(.cols = contains("conf_"),.fns = ~replace_na(.,0))) %>% 
  left_join(rankings, by = c("WTeamID" = "TeamID", "Season")) %>% 
  left_join(rankings, by = c("LTeamID" = "TeamID", "Season"), suffix = c("_A", "_B")) %>% 
  left_join(seeds, by = c("WTeamID" = "TeamID", "Season")) %>% 
  left_join(seeds, by = c("LTeamID" = "TeamID", "Season"), suffix = c("_A", "_B")) %>% 
  left_join(quad_win_tracker, by = c("WTeamID" = "TeamID", "Season")) %>% 
  left_join(quad_win_tracker, by = c("LTeamID" = "TeamID", "Season"), suffix = c("_A", "_B"))
  

model <- register_make_execute_evaluate(data = model_data$Train, # model_data
# model <- register_make_execute_evaluate(data = model_data,
             # type_of_model = "log_reg",
             type_of_model = "random_forest",
             # type_of_model = "boost_tree",
             mode = "classification",
             # mode = "regression",
             # regression_formula = formula(win_by ~ rank_avg_A + rank_avg_B + last3offensive_efficiency_A + last3offensive_efficiency_B +
             #                                last3possessions_per_game_A + last3possessions_per_game_B + last3defensive_efficiency_A + last3defensive_efficiency_B + 
             #                                clust_A + clust_B + avg_win_A + avg_win_B + overallfree_throw_rate_A + overallfree_throw_rate_B +  
             #                                col_A + col_B + conf_wins_against_A + conf_wins_against_B + conf_loss_against_A + conf_loss_against_B +
             #                                quad_wins_A + quad_wins_B + quad_loss_A + quad_loss_B+ Seed_A + Seed_B +
             #                                ratingschedule_strength_by_other_A + ratingschedule_strength_by_other_B + t3_week_rank_avg_A + t3_week_rank_avg_B),
             regression_formula = formula(win ~ last3defensive_efficiency_B + clust_A + clust_B + avg_win_A + avg_win_B + 
                                            overallfree_throw_rate_A + overallfree_throw_rate_B + col_A + col_B + conf_wins_against_A + conf_wins_against_B + 
                                            conf_loss_against_A + conf_loss_against_B + quad_wins_A + quad_wins_B + quad_loss_A + quad_loss_B + 
                                            Seed_A + Seed_B + ratingschedule_strength_by_other_A + ratingschedule_strength_by_other_B + 
                                            t3_week_rank_avg_A + t3_week_rank_avg_B),
             grid_size = 100,
             notes = "building final model",
             prediction_data = model_data$Test,
             # prediction_data = stage_2_submission_data,
             submission_mode = F
             # submission_mode = T
)


