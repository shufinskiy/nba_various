library(data.table)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(zoo)

lst <- list.files(pattern = ".csv", full.names = TRUE) 

data <- rbindlist(lapply(lst, fread))

id_player <- data %>% 
  filter(SHOT_TYPE == "3PT Field Goal") %>% 
  select(PLAYER_ID) %>% 
  group_by(PLAYER_ID) %>% 
  summarise(CNT_SHOT = n()) %>% 
  ungroup() %>% 
  arrange(desc(CNT_SHOT)) %>% 
  head(20)

df <- data %>% 
  filter(SHOT_TYPE == "3PT Field Goal") %>% 
  select(PLAYER_ID, PLAYER_NAME, GAME_ID, GAME_EVENT_ID, SHOT_MADE_FLAG, SHOT_ATTEMPTED_FLAG) %>% 
  inner_join(id_player, by="PLAYER_ID") %>% 
  arrange(PLAYER_ID, GAME_ID, GAME_EVENT_ID) %>% 
  group_by(PLAYER_ID) %>% 
  mutate(ROLL_PREC50 = rollmean(SHOT_MADE_FLAG, 50, na.pad = TRUE, fill = NA),
         ROLL_PREC300 = rollmean(SHOT_MADE_FLAG, 300, na.pad = TRUE, fill = NA)) %>% 
  ungroup() %>% 
  group_by(PLAYER_ID, PLAYER_NAME) %>% 
  summarise(CNT_SHOT = max(CNT_SHOT),
            MIN_ROLL50 = min(ROLL_PREC50, na.rm = TRUE),
            MAX_ROLL50 = max(ROLL_PREC50, na.rm = TRUE),
            MIN_ROLL300 = min(ROLL_PREC300, na.rm = TRUE),
            MAX_ROLL300 = max(ROLL_PREC300, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    DIFF_ROLL50 = MAX_ROLL50 - MIN_ROLL50,
    DIFF_ROLL300 = MAX_ROLL300 - MIN_ROLL300,
    IMG = paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/', PLAYER_ID, '.png')
  ) %>% 
  arrange(desc(CNT_SHOT)) %>% 
  gt() %>% 
  gtExtras::gt_theme_espn() %>% 
  cols_hide(columns = PLAYER_ID) %>% 
  cols_move(columns = IMG, after = PLAYER_NAME) %>% 
  cols_move(columns = DIFF_ROLL50, after = MAX_ROLL50) %>% 
  fmt_percent(columns = c(MIN_ROLL50, MAX_ROLL50, DIFF_ROLL50), decimals = 0) %>% 
  fmt_percent(columns = c(MIN_ROLL300, MAX_ROLL300, DIFF_ROLL300), decimals = 2) %>% 
  tab_spanner(label = md("**WINDOW 50 SHOTS**"), c(MIN_ROLL50, MAX_ROLL50, DIFF_ROLL50)) %>% 
  tab_spanner(label = md("**WINDOW 300 SHOTS**"), c(MIN_ROLL300, MAX_ROLL300, DIFF_ROLL300)) %>% 
  gt_hulk_col_numeric(MIN_ROLL50:DIFF_ROLL300, reverse = FALSE) %>% 
  cols_label(
    CNT_SHOT = "SHOTS",
    MIN_ROLL50 = "MIN%",
    MAX_ROLL50 = "MAX%",
    DIFF_ROLL50 = "DIFF%",
    MIN_ROLL300 = "MIN%",
    MAX_ROLL300 = "MAX%",
    DIFF_ROLL300 = "DIFF%"
  ) %>% 
  gt_img_rows(columns = IMG, img_source = "web", height = 30) %>% 
  cols_label(
    PLAYER_NAME = "",
    IMG = ""
  ) %>% 
  tab_header(title = "Best and worst series of 3P shots for a window of 50 and 300 shots",
             subtitle = "Season 2011/12 - precent days") %>% 
  tab_source_note("DATA:stats.nba.com; twitter: @vshufinskiy, Telegram: @nbaatlantic") %>% 
  tab_options(heading.align = "center") %>% 
  gtsave("3pa.png", vwidth = 1700, zoom=1, expand=0)
  
