`%>%` <- magrittr::`%>%`

df <- read.csv("gameLogs.csv")

full_data <- df %>% 
  dplyr::mutate(WL = dplyr::if_else(WL == "W", 1, 0)) %>% 
  dplyr::select(GAME_ID, GAME_DATE, TEAM_ID, WL)

series_data <- full_data %>% 
  dplyr::filter(WL == 1) %>% 
  dplyr::inner_join(full_data %>% dplyr::filter(WL == 0), by = c("GAME_ID", "GAME_DATE")) %>% 
  dplyr::mutate(
    TEAM1 = dplyr::if_else(TEAM_ID.x < TEAM_ID.y, TEAM_ID.x, TEAM_ID.y),
    TEAM2 = dplyr::if_else(TEAM_ID.x > TEAM_ID.y, TEAM_ID.x, TEAM_ID.y),
    WL = dplyr::if_else(TEAM_ID.x < TEAM_ID.y, WL.x, WL.y)
  ) %>% 
  dplyr::select(-c(TEAM_ID.x, TEAM_ID.y, WL.x, WL.y)) %>% 
  dplyr::arrange(dplyr::desc(GAME_DATE),dplyr:: desc(GAME_ID)) %>% 
  dplyr::group_by(TEAM1, TEAM2) %>% 
  dplyr::mutate(
    NGAME = dplyr::row_number(),
    CUM_WL = cumsum(WL)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(WP = CUM_WL / NGAME) %>% 
  dplyr::filter(NGAME >= 10 & (WP <= 0.2 | WP >= 0.8))

series_data %>% 
  dplyr::group_by(TEAM1, TEAM2) %>% 
  dplyr::top_n(1, NGAME) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::desc(NGAME)) %>% 
  dplyr::select(-c(GAME_ID, WL, CUM_WL)) %>% 
  head(15) %>% 
  dplyr::mutate(
    WIN_TEAM = dplyr::if_else(WP > 0.2, TEAM1, TEAM2),
    LOSE_TEAM = dplyr::if_else(WP > 0.2, TEAM2, TEAM1),
  ) %>% 
  dplyr::mutate(
    WP = dplyr::if_else(WP > 0.2, WP, 1-WP),
    WIN_TEAM = paste0("https://cdn.nba.com/logos/nba/", WIN_TEAM,"/global/L/logo.svg"),
    LOSE_TEAM = paste0("https://cdn.nba.com/logos/nba/", LOSE_TEAM,"/global/L/logo.svg")
  ) %>% 
  dplyr::select(GAME_DATE, WIN_TEAM, LOSE_TEAM, NGAME, WP) %>% 
  gt::gt() %>% 
  gtExtras::gt_theme_espn() %>% 
  gt::fmt_percent(WP) %>% 
  gt::fmt_date(GAME_DATE, date_style='m_day_year') %>% 
  gt::tab_header(title = "Top-15 active series with win precentage above 80%",
                 subtitle = "Sorted by DESC count games in series (min. 10 games)") %>%
  gt::tab_source_note(gt::md("DATA:stats.nba.com; twitter: **@vshufinskiy**, Telegram: **@nbaatlantic**")) %>%
  gtExtras::gt_img_rows(columns = WIN_TEAM, img_source = "web", height = 60) %>% 
  gtExtras::gt_img_rows(columns = LOSE_TEAM, img_source = "web", height = 60) %>% 
  gt::cols_label(
    GAME_DATE = "DATE START",
    WIN_TEAM = "WIN TEAM",
    LOSE_TEAM = "LOSE TEAM",
    WP = 'WIN%',
  ) %>% 
  gt::tab_footnote(
    footnote = "Including playoff. Data on 2023-04-25",
    locations = gt::cells_column_labels(
      columns = NGAME
    )
  ) %>% 
  gt::tab_options(
    heading.align = "center"
  ) %>% 
  gt::gtsave("longest_series.png", vheight = 2500)

series_data %>% 
  dplyr::filter(WP == 0 | WP == 1) %>% 
  dplyr::group_by(TEAM1, TEAM2) %>% 
  dplyr::top_n(1, NGAME) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::desc(NGAME)) %>% 
  dplyr::select(-c(GAME_ID, WL, CUM_WL)) %>% 
  head(15) %>% 
  dplyr::mutate(
    WIN_TEAM = dplyr::if_else(WP == 1, TEAM1, TEAM2),
    LOSE_TEAM = dplyr::if_else(WP == 1, TEAM2, TEAM1),
  ) %>% 
  dplyr::mutate(
    WIN_TEAM = paste0("https://cdn.nba.com/logos/nba/", WIN_TEAM,"/global/L/logo.svg"),
    LOSE_TEAM = paste0("https://cdn.nba.com/logos/nba/", LOSE_TEAM,"/global/L/logo.svg")
  ) %>% 
  dplyr::select(GAME_DATE, WIN_TEAM, LOSE_TEAM, NGAME) %>% 
  gt::gt() %>% 
  gtExtras::gt_theme_espn() %>% 
  gt::fmt_date(GAME_DATE, date_style='m_day_year') %>% 
  gt::tab_header(title = "Longest active unbeaten series",
                 subtitle = "Sorted by DESC count games in series (min. 10 games)") %>%
  gt::tab_source_note(gt::md("DATA:stats.nba.com; twitter: **@vshufinskiy**, Telegram: **@nbaatlantic**")) %>%
  gtExtras::gt_img_rows(columns = WIN_TEAM, img_source = "web", height = 60) %>% 
  gtExtras::gt_img_rows(columns = LOSE_TEAM, img_source = "web", height = 60) %>% 
  gt::cols_label(
    GAME_DATE = "DATE START",
    WIN_TEAM = "WIN TEAM",
    LOSE_TEAM = "LOSE TEAM",
  ) %>% 
  gt::tab_footnote(
    footnote = "Including playoff. Data on 2023-04-25",
    locations = gt::cells_column_labels(
      columns = NGAME
    )
  ) %>% 
  gt::tab_options(
    heading.align = "center"
  ) %>% 
  gt::gtsave("unbeaten_series.png", vheight = 2500)