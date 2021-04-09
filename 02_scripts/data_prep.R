library(tidyverse)
source("02_scripts/support_functions.R")

seeds <-
read_csv("01_data/MNCAATourneySeeds.csv") %>% 
  mutate(Seed = as.numeric(str_sub(Seed,2,3)))

all_team_ids <- read_csv(here::here("01_data/MMasseyOrdinals.csv")) %>% select(TeamID) %>% distinct() %>% pull() %>% sort()

team_df <-
  read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
  select(Season, WTeamID, LTeamID) %>% 
  pivot_longer(cols = contains("Team"), values_to = "TeamID") %>% 
  distinct(Season,TeamID) %>% 
  arrange(Season,TeamID)

r_of_interest <- read_csv(here::here("01_data/MMasseyOrdinals.csv")) %>% filter(SystemName %in% c("SAG","POM","MOR","WLK", "RPI"))  

rankings <-
expand_grid(Season = 2003:2021, RankingDayNum = 1:154, TeamID = all_team_ids, SystemName = unique(r_of_interest$SystemName)) %>% 
  left_join(
    r_of_interest 
  ) %>% 
  arrange(Season,RankingDayNum) %>% 
  group_by(Season,TeamID, SystemName) %>% 
  fill(SystemName, OrdinalRank, .direction = "down") %>% 
  fill(SystemName, OrdinalRank, .direction = "up") %>% 
  ungroup() %>% 
  filter(!is.na(OrdinalRank)) %>% 
  pivot_wider(names_from = SystemName, values_from = OrdinalRank, values_fn = mean)  %>% 
  group_by(Season, TeamID) %>% 
  slice_max(RankingDayNum, n = 1) %>% 
  ungroup()

summary_stats <-
read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
  mutate(win = if_else(AScore > BScore, 1, 0), win_by = AScore - BScore) %>% 
  select(Season, DayNum, TeamID = ATeamID, AScore, Loc = ALoc, AFGM:APF, win, win_by) %>% 
  pivot_longer(cols = -c(Season,DayNum,TeamID,Loc)) %>% 
  bind_rows(

read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
  mutate(win = 0, win_by = BScore - AScore) %>% 
  select(Season, DayNum, TeamID = BTeamID, BScore, Loc = ALoc, BFGM:BPF, win, win_by) %>%
  mutate(Loc = case_when(Loc == "A" ~ "H",
                         Loc == "H" ~ "A",
                         TRUE ~ Loc)) %>% 
  pivot_longer(cols = -c(Season,DayNum,TeamID, Loc)) 

) %>% 
  mutate(name = str_sub(name, 2,-1)) %>% 
  group_by(Season, TeamID, name) %>% 
  summarise(summary_val = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = name, values_from = summary_val) %>% 
  ungroup() %>% 
  rename(avg_win = `in`, avg_win_by = `in_by`) %>% 
  relocate(c(avg_win,avg_win_by), .after = last_col())

summary_stats_final <-
  read_csv(here::here("01_data/MNCAATourneyDetailedResults.csv")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
  mutate(win = if_else(AScore > BScore, 1, 0), win_by = AScore - BScore) %>% 
  select(Season, DayNum, TeamID = ATeamID, AScore, AFGM:APF, win, win_by) %>% 
  pivot_longer(cols = -c(Season,DayNum,TeamID)) %>% 
  bind_rows(
    read_csv(here::here("01_data/MNCAATourneyDetailedResults.csv")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
      mutate(win = 0, win_by = BScore - AScore) %>% 
      select(Season, DayNum, TeamID = BTeamID, BScore, BFGM:BPF, win, win_by) %>% 
      pivot_longer(cols = -c(Season,DayNum,TeamID))
    ) %>%
  mutate(name = str_sub(name, 2,-1)) %>% 
  group_by(Season, TeamID, name) %>%
  summarise(summary_val = mean(value, na.rm = T)) %>%  
  pivot_wider(names_from = name, values_from = summary_val) %>% 
  ungroup() %>% 
  rename(avg_win = `in`, avg_win_by = `in_by`) %>% 
  relocate(c(avg_win,avg_win_by), .after = last_col())

ranking_data <- 
  read_csv("01_data/ranking_data_2.csv", guess_max = 10000) %>% 
  relocate(TeamID) %>% 
  janitor::clean_names() %>%
  select(TeamID = team_id, Season = season, rank_avg, OE = overalloffensive_efficiency, DE = overalldefensive_efficiency,clust, last3offensive_efficiency, last3possessions_per_game,
       last3defensive_efficiency, overallfree_throw_rate, col, ratingschedule_strength_by_other,overallpercent_of_points_from_3_pointers,last3_change,t3_week_rank_avg) 

quality_win_tracker <-
read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
  mutate(win = if_else(AScore > BScore, 1, 0), win_by = AScore - BScore) %>% 
  select(Season, DayNum, ATeamID, AScore, BTeamID, BScore, win, win_by) %>% 
  bind_rows(
    read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "B"), .cols = starts_with("W")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "A"), .cols = starts_with("L")) %>%
      mutate(win = 0, win_by = BScore - AScore) %>% 
      select(Season, DayNum, ATeamID, AScore, BTeamID, BScore, win, win_by)
    ) %>%
  arrange(Season, DayNum) %>% 
  group_by(Season, ATeamID) %>% 
  mutate(AGames = row_number(), AWins = cumsum(win), ALosses = AGames - AWins) %>% 
  group_by(Season, BTeamID) %>% 
  mutate(BGames = row_number(), BLosses = cumsum(win), BWins = BGames - BLosses) %>% 
  ungroup() %>% 
  mutate(win_perc_A = AWins / AGames, win_perc_B = BWins / BGames) %>% 
  filter(AGames >= 10 & BGames >= 10) %>% 
  group_by(Season, ATeamID) %>% 
  summarise(good_wins = sum(win == 1 & win_perc_B > .6), bad_loss = sum(win == 0 & win_perc_B < .3), wins = max(AWins), losses = max(BLosses)) %>% 
  rename(TeamID = ATeamID) %>% 
  ungroup()

quad_win_helper <-
read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "A"), .cols = starts_with("W")) %>% 
  rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "B"), .cols = starts_with("L")) %>%
  mutate(win = if_else(AScore > BScore, 1, 0), win_by = AScore - BScore) %>% 
  select(Season, DayNum, ATeamID, AScore, BTeamID, BScore, win, win_by) %>% 
  bind_rows(
    read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "W",replacement = "B"), .cols = starts_with("W")) %>% 
      rename_with(.fn = ~str_replace(string = .,pattern = "L",replacement = "A"), .cols = starts_with("L")) %>%
      mutate(win = 0, win_by = BScore - AScore) %>% 
      select(Season, DayNum, ATeamID, AScore, BTeamID, BScore, win, win_by) 
    )  %>% 
  left_join(ranking_data %>% select(TeamID, Season,rank_avg_B = rank_avg), by = c("Season", "BTeamID" = "TeamID")) %>% 
  mutate(game_of_interest_A = rank_avg_B < 30, game_of_interest_A_bad = rank_avg_B > 75) 


quad_win_tracker <-
quad_win_helper %>% 
  count(Season, ATeamID, win, game_of_interest_A) %>% 
  filter(!is.na(game_of_interest_A)) %>% 
  filter(game_of_interest_A) %>% 
  filter(win == 1) %>% 
  select(Season, TeamID = ATeamID, quad_wins = n) %>% 
  full_join(
quad_win_helper %>% 
  count(Season, BTeamID, win, game_of_interest_A_bad) %>% 
  filter(!is.na(game_of_interest_A_bad)) %>% 
  filter(game_of_interest_A_bad) %>% 
  filter(win == 0) %>% 
  select(Season, TeamID = BTeamID, quad_loss = n)
) %>% 
  mutate(across(.cols = c(quad_wins, quad_loss), .fns = ~replace_na(.,0)))

conf_start <-
  read_csv(here::here("01_data/MRegularSeasonDetailedResults.csv")) %>% 
    select(Season, WTeamID, LTeamID, WScore, LScore) %>% 
    left_join(
      read_csv(here::here("01_data/MTeamConferences.csv")), by = c("Season", "WTeamID" = "TeamID")
    ) %>% 
    rename(WConf = ConfAbbrev) %>% 
    left_join(
      read_csv(here::here("01_data/MTeamConferences.csv")), by = c("Season", "LTeamID" = "TeamID")
    ) %>% 
    rename(LConf = ConfAbbrev) %>% 
    count(Season, WConf, LConf) %>% 
  mutate(n = ifelse(WConf == LConf, 0, 1))

conf_rank <-
  conf_start %>% 
  left_join(conf_start %>% select(WConf, LConf, Season, n), by = c("Season", "LConf" = "WConf", "WConf" = "LConf")) %>% 
  rename(conf_wins = n.x, conf_loss = n.y) %>% 
  mutate(conf_loss = replace_na(conf_loss, 0)) %>% 
  mutate(conf_record = str_c(WConf, "_", LConf)) %>% 
    select(Season, conf_record, contains("conf_"))


  
base_builder <-
  read_csv(here::here("01_data/MNCAATourneyDetailedResults.csv")) %>%
    select(Season,contains("Team"), contains("Score"), DayNum) %>%
    mutate(win_by = WScore - LScore) %>% 
    relocate(-win_by) %>% 
    mutate(win = if_else(win_by > 0, "win", "lose")) %>% 
    select(Season, WTeamID, LTeamID, DayNum, win_by, win) 


base_builder <- 
base_builder %>% 
  bind_rows(
    base_builder %>% rename(LTeamID = WTeamID, WTeamID = LTeamID) %>% 
      mutate(win_by = -win_by, win = "lose")
  )


staging_data <-
  ranking_data %>% 
  left_join(summary_stats, by = c("Season","TeamID")) %>% 
  distinct() %>% 
  left_join(quality_win_tracker) %>% 
  left_join(
    read_csv(here::here("01_data/MTeamConferences.csv")), by = c("Season", "TeamID")
  ) %>% 
  relocate(ConfAbbrev, .after = Season) %>% 
  rename(conf = ConfAbbrev) 


model_data <-
base_builder %>% 
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
  left_join(quad_win_tracker, by = c("LTeamID" = "TeamID", "Season"), suffix = c("_A", "_B")) %>% 
  group_split(Season < 2015) %>%
  set_names(c("Test","Train")) %>%
  map(~select(.,-`Season < 2015`))
  
