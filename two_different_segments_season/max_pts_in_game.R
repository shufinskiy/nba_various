library(dplyr, warn.conflicts = FALSE)

season <- 2022

logs <- read.csv(paste0('./data/playergamelog_', season, '.csv'))

ntop <- 15

tbl <- logs %>% 
  group_by(GAME_ID, TEAM_ID) %>% 
  summarise(PTS = max(PTS)) %>% 
  ungroup() %>% 
  inner_join(., logs, by=c("GAME_ID", "TEAM_ID", "PTS")) %>% 
  select(GAME_ID, TEAM_ID, PLAYER_ID) %>% 
  group_by(TEAM_ID, PLAYER_ID) %>% 
  summarise(COUNT = n()) %>% 
  ungroup() %>% 
  arrange(desc(COUNT)) %>% 
  inner_join(., unique(select(logs, TEAM_ID, TEAM_NAME, PLAYER_ID, PLAYER_NAME)), by = c("TEAM_ID", "PLAYER_ID")) %>% 
  slice_head(n=ntop) %>% 
  select(PLAYER_ID, PLAYER_NAME, TEAM_ID, COUNT) %>% 
  inner_join(., logs %>% group_by(PLAYER_ID) %>% summarise(NGAME = n()), by="PLAYER_ID") %>% 
  mutate(SHARE = round(COUNT/NGAME*100, 2)) %>% 
  mutate(
    TEAM_ID = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID,"/global/L/logo.svg"),
    PLAYER_ID = paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/', PLAYER_ID, '.png')
  )

library(gt)
library(gtExtras)

tbl %>% 
  gt() %>% 
  gt_theme_espn() %>% 
  tab_spanner(
    "GAMES",
    columns = c(COUNT, NGAME)
  ) %>% 
  cols_align(align = "center") %>% 
  gt_hulk_col_numeric(SHARE, reverse = FALSE) %>% 
  gt_img_rows(columns = TEAM_ID, img_source = "web", height = 60) %>%
  gt_img_rows(columns = PLAYER_ID, img_source = "web", height = 60) %>% 
  cols_label(
    PLAYER_ID = "",
    PLAYER_NAME = "NAME",
    TEAM_ID = "",
    COUNT = html("TOP<br>SCORER"),
    NGAME = "GAMES",
    SHARE = "%"
  ) %>% 
  tab_header(title = "Top-15 players by count games with max points in team") %>%
  tab_source_note(md("DATA:stats.nba.com; twitter: **@vshufinskiy**, Telegram: **@nbaatlantic**")) %>%
  tab_options(
    heading.align = "center"
  ) %>% 
  gtsave("./charts/cnt_pts_leader.png", vheight = 2500)
