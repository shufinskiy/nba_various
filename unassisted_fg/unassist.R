library(dplyr)
library(tidyr)
library(data.table)
library(gt)
library(gtExtras)

untar("pbp_2022.tar.xz")
df <- fread("pbp_2022.csv")
unlink("pbp_2022.csv")

team_df <- df %>% 
  select(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME, EVENTMSGTYPE, PLAYER2_ID) %>% 
  filter(EVENTMSGTYPE == 1) %>% 
  mutate(PLAYER2_ID = if_else(PLAYER2_ID == 0, 1, 0)) %>% 
  group_by(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME) %>% 
  summarise(CNT_SHT = sum(EVENTMSGTYPE),
            UNASSISTED = sum(PLAYER2_ID)) %>% 
  ungroup() %>% 
  mutate(PCT_UNAST = UNASSISTED / CNT_SHT) %>% 
  select(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME, PCT_UNAST) %>% 
  rename(TEAM_ID = PLAYER1_TEAM_ID, TEAM_NICKNAME = PLAYER1_TEAM_NICKNAME)

player_df <- df %>% 
  select(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME, PLAYER1_ID, PLAYER1_NAME, PLAYER2_ID, EVENTMSGTYPE) %>% 
  filter(EVENTMSGTYPE == 1) %>% 
  mutate(PLAYER2_ID = if_else(PLAYER2_ID == 0, 1, 0)) %>% 
  group_by(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME, PLAYER1_ID, PLAYER1_NAME) %>% 
  summarise(CNT_SHT = sum(EVENTMSGTYPE),
            UNASSISTED = sum(PLAYER2_ID)) %>% 
  ungroup() %>% 
  filter(CNT_SHT >= 100) %>% 
  mutate(PCT_UNAST = UNASSISTED / CNT_SHT) %>% 
  group_by(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME) %>% 
  mutate(RANK = min_rank(desc(PCT_UNAST))) %>% 
  ungroup() %>% 
  filter(RANK <= 2) %>% 
  pivot_wider(id_cols = c(PLAYER1_TEAM_ID, PLAYER1_TEAM_NICKNAME), names_from = RANK, values_from = c(PLAYER1_NAME, PLAYER1_ID, PCT_UNAST)) %>% 
  rename(TEAM_ID = PLAYER1_TEAM_ID, TEAM_NICKNAME = PLAYER1_TEAM_NICKNAME)

gttable <- team_df %>% 
  inner_join(player_df, by=c("TEAM_ID", "TEAM_NICKNAME")) %>% 
  arrange(desc(PCT_UNAST)) %>% 
  select(TEAM_ID, TEAM_NICKNAME, PCT_UNAST, PLAYER1_NAME_1, PLAYER1_ID_1, PCT_UNAST_1, PLAYER1_NAME_2, PLAYER1_ID_2, PCT_UNAST_2) %>% 
  mutate(
    DIFF = PCT_UNAST_1 - PCT_UNAST_2,
    TEAM_ID = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID,"/global/L/logo.svg"),
    PLAYER1_ID_1 = paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/', PLAYER1_ID_1, '.png'),
    PLAYER1_ID_2 = paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/', PLAYER1_ID_2, '.png')) %>% 
    gt() %>% 
    gt_hulk_col_numeric(c(PCT_UNAST, PCT_UNAST_1, PCT_UNAST_2, DIFF), reverse = FALSE)

gttable1 <- gttable

gttable1[["_data"]] <- gttable1[["_data"]][seq(1, 15),]
gttable1[["_stub_df"]] <- gttable1[["_stub_df"]][seq(1, 15),]

row_styles <- seq(1, 240)[sapply(seq(1, 240), function(x){
  ((x-1) %/% 15) %% 2 == 0
})]

gttable1[["_styles"]] <- gttable1[["_styles"]][row_styles,]


gttable2 <- gttable

gttable2[["_data"]] <- gttable2[["_data"]][seq(16, 30),]
gttable2[["_stub_df"]] <- gttable2[["_stub_df"]][seq(1, 15),]

row_styles <- seq(1, 240)[sapply(seq(1, 240), function(x){
  ((x-1) %/% 15) %% 2 != 0
})]

gttable2[["_styles"]] <- gttable2[["_styles"]][row_styles,]
gttable2[["_styles"]]$rownum <- rep(seq(1, 15), 8)

build_gt_table <- function(gtobject, suffix_title){
  gtobject %>% 
    gt_theme_espn() %>%
    gt_img_rows(columns = TEAM_ID, img_source = "web", height = 60) %>%
    gt_img_rows(columns = PLAYER1_ID_1, img_source = "web", height = 60) %>%
    gt_img_rows(columns = PLAYER1_ID_2, img_source = "web", height = 60) %>%
    fmt_percent(c(PCT_UNAST, PCT_UNAST_1, PCT_UNAST_2, DIFF), decimals = 2) %>%
    tab_spanner("PLAYER1", columns = c(PLAYER1_NAME_1, PLAYER1_ID_1, PCT_UNAST_1)) %>%
    tab_spanner("PLAYER2", columns = c(PLAYER1_NAME_2, PLAYER1_ID_2, PCT_UNAST_2)) %>%
    cols_label(
      TEAM_ID = "",
      TEAM_NICKNAME = "",
      PCT_UNAST = "TEAM_UNAST%",
      PLAYER1_NAME_1 = "",
      PLAYER1_ID_1 = "",
      PCT_UNAST_1 = "UNAST%",
      PLAYER1_NAME_2 = "",
      PLAYER1_ID_2 = "",
      PCT_UNAST_2 = "UNAST%"
    ) %>%
    cols_align(align = "center") %>%
    tab_header(title = paste0("Percent unassisted FG in 2022/23. ", suffix_title)) %>%
    tab_source_note("DATA:stats.nba.com; twitter: @vshufinskiy, Telegram: @nbaatlantic") %>%
    tab_options(
      heading.align = "center"
    )
}

build_gt_table(gttable1, "Tiers 1-15") %>% 
  gtsave("unassist_1.png", vwidth = 1700, expand=0)

build_gt_table(gttable2, "Tiers 16-30") %>% 
  gtsave("unassist_2.png", vwidth = 1700, expand=0)
