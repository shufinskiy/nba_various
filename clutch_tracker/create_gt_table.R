library(dplyr)
library(gt)
library(gtExtras)

data <- read.csv("data/final_table.csv")

data %>% 
  mutate(CNT = CNT / 100) %>% 
  gt() %>% 
  gt_theme_espn() %>% 
  gt::tab_header(title = "The Jerry West Clutch Player of the Year Tracker",
                 subtitle = "Value in columns SH:BLK is average value of the weight of indicator at which the player won") %>% 
  gt::tab_source_note("DATA:inpredictable.com; twitter: @vshufinskiy, Telegram: @nbaatlantic") %>% 
  fmt_percent(
    columns = "CNT",
    decimals = 2
  ) %>% 
  cols_hide(columns = "IDX") %>% 
  cols_label("CNT" = "WIN%") %>% 
  gt_hulk_col_numeric(SH:BLK, reverse = FALSE) %>% 
  gtsave("clutch_tracker.png", vwidth = 1700)