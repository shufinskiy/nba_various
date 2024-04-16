library(hoopR)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(zoo)

team_dict <- list(
  'ATL' = c(1610612737, "Atlanta Hawks"),
  'BOS' = c(1610612738, "Boston Celtics"),
  'BKN' = c(1610612751, "Brooklyn Nets"),
  'CHA' = c(1610612766, "Charlotte Hornets"),
  'CHI' = c(1610612741, "Chicago Bulls"),
  'CLE' = c(1610612739, "Cleveland Cavaliers"),
  'DAL' = c(1610612742, "Dallas Mavericks"),
  'DEN' = c(1610612743, "Denver Nuggets"),
  'DET' = c(1610612765, "Detroit Pistons"),
  'GSW' = c(1610612744, "Golden State Warriors"),
  'HOU' = c(1610612745, "Houston Rockets"),
  'IND' = c(1610612754, "Indiana Pacers"),
  'LAC' = c(1610612746, "Los Angeles Clippers"),
  'LAL' = c(1610612747, "Los Angeles Lakers"),
  'MEM' = c(1610612763, "Memphis Grizzlies"),
  'MIA' = c(1610612748, "Miami Heat"),
  'MIL' = c(1610612749, "Milwaukee Bucks"),
  'MIN' = c(1610612750, "Minnesota Timberwolves"),
  'NOP' = c(1610612740, "New Orleans Pelicans"),
  'NYK' = c(1610612752, "New York Knicks"),
  'OKC' = c(1610612760, "Oklahoma City Thunder"),
  'ORL' = c(1610612753, "Orlando Magic"),
  'PHI' = c(1610612755, "Philadelphia 76ers"),
  'PHX' = c(1610612756, "Phoenix Suns"),
  'POR' = c(1610612757, "Portland Trail Blazers"),
  'SAC' = c(1610612758, "Sacramento Kings"),
  'SAS' = c(1610612759, "San Antonio Spurs"),
  'TOR' = c(1610612761, "Toronto Raptors"),
  'UTH' = c(1610612762, "Utah Jazz"),
  'WAS' = c(1610612764, "Washington Wizards")
)

recursive_df <- function(data, cnt=0, window_size = 20, wins_cnt = 18, less_or_more = 'more'){
  tmp_df <- data %>% 
    mutate(
      ROLL_W = rollsum(WL, window_size, fill = NA, align = 'right')
    )
  
  wins <- if (less_or_more == 'more') dim(filter(tmp_df, ROLL_W >= wins_cnt))[1] else dim(filter(tmp_df, ROLL_W <= wins_cnt))[1]
  
  if(wins > 0){
    cnt <- cnt + 1
    tmp_df <- if (less_or_more == 'more') slice(tmp_df, sort(which(tmp_df$ROLL_W >= wins_cnt))[1]+1:dim(tmp_df)[1]) else slice(tmp_df, sort(which(tmp_df$ROLL_W <= wins_cnt))[1]+1:dim(tmp_df)[1])
    
    return(recursive_df(tmp_df, cnt))
  } else{
    return(cnt)
  }
}

calculate_season_series <- function(seasons = seq(2004, 2023), cnt=0, window_size = 20, wins_cnt = 18, less_or_more = 'more'){
  df <- data.frame(SEASON_ID = integer(), TEAM_ID = character(), CNT = integer())
  season_wins <- data.frame(SEASON_ID = integer(), TEAM_ID = character(), WINS_SHARE = double())
  for(s in seasons){
    print(s)
    game_list <- nba_leaguegamelog(season = s)$LeagueGameLog
    
    data <- game_list %>% 
      select(SEASON_ID, TEAM_ID, GAME_DATE, WL) %>% 
      mutate(
        GAME_DATE = base::as.Date(GAME_DATE),
        WL = if_else(WL == 'W', 1, 0)
      ) %>% 
      arrange(TEAM_ID, GAME_DATE)
    
    wins_share <- data %>% 
      group_by(TEAM_ID) %>% 
      summarise(
        GAME = n(),
        WINS = sum(WL)
      ) %>% 
      ungroup() %>% 
      mutate(
        WINS_SHARE = round(WINS/GAME, 4)
      ) %>% 
      select(TEAM_ID, WINS_SHARE)
    wins_share$SEASON_ID <- s
    season_wins <- rbind(season_wins, wins_share)
    
    l <- sapply(sort(unique(data$TEAM_ID)), function(team){
      team_df <- filter(data, TEAM_ID == team)
      cnt <- recursive_df(team_df, cnt, window_size, wins_cnt, less_or_more)
      return(cnt)
    })
    season_df <- data.frame(SEASON_ID = s, TEAM_ID = names(l), CNT = as.vector(l))
    df <- rbind(df, season_df)
  }
  return(list('df' = df, 'season_wins' = season_wins))
}

wins_tbl_list <- calculate_season_series()
lose_tbl_list <- calculate_season_series(wins_cnt = 2, less_or_more = 'less')

wins_df <- wins_tbl_list[['df']]
season_wins <- wins_tbl_list[['season_wins']]

lose_df <- lose_tbl_list[['df']]
season_loses <- lose_tbl_list[['season_wins']]

check_position <- function(list, team_id){
  sapply(seq(1, length(team_dict)), function(x){team_dict[[x]][1] == team_id})
}

wins_df$TEAM <- sapply(wins_df$TEAM_ID, function(n){names(team_dict)[check_position(team_dict, team_id = n)]}, USE.NAMES = FALSE)
lose_df$TEAM <- sapply(lose_df$TEAM_ID, function(n){names(team_dict)[check_position(team_dict, team_id = n)]}, USE.NAMES = FALSE)

wins_text <- list(
  count_title = "Количество серий из 20 игр с процентом побед 90 или больше",
  precent_title = "5 команд с наименьшим процентом побед в сезоне из тех, кто имеет серию из 20 игр с процентом побед 90 или больше"
)

lose_text <- list(
  count_title = "Количество серий из 20 игр с процентом побед 10 или меньше",
  precent_title = "5 команд с наибольшим процентом побед в сезоне из тех, кто имеет серию из 20 игр с процентом побед 10 или меньше"
)

count_series_by_team <- function(data, texts){
  
  ggplot(data, aes(SEASON_ID, TEAM, fill=as.factor(CNT))) +
    geom_tile(colour='grey50') +
    scale_fill_viridis_d() +
    scale_x_continuous(breaks = seq(min(data$SEASON_ID), max(data$SEASON_ID)), expand = c(0, 0)) +
    guides(fill = guide_legend(position = 'bottom', title = "Количество серий",
                               theme = theme(
                                 legend.title.position = 'top',
                                 legend.text.position = 'bottom',
                                 legend.text = element_text(face = 'bold', size = 10),
                                 legend.title = element_text(face = 'bold', size = 10, hjust=0.5)
                               ))) +
    labs(title = texts[['count_title']],
         caption = "DATA: nba.com | https://t.me/nbaatlantic") +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(face = 'bold', size=11),
      axis.text.y = element_text(face = 'bold', size=11),
      plot.title = element_text(face = 'bold', size=14, hjust = 0.5),
      plot.caption = element_text(face = 'bold', size = 10)
    )
}

count_series_by_team(wins_df, wins_text)
count_series_by_team(lose_df, lose_text)

small_wins_precent <- wins_df %>% 
  inner_join(season_wins, by=c('SEASON_ID', 'TEAM_ID')) %>% 
  filter(CNT != 0) %>% 
  arrange(WINS_SHARE) %>%
  slice_head(n=5) %>% 
  mutate(
    TEAM_ID = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID,"/global/L/logo.svg")
  ) %>% 
  select(SEASON_ID, TEAM_ID, WINS_SHARE)

big_lose_precet <- lose_df %>% 
  inner_join(season_loses, by=c('SEASON_ID', 'TEAM_ID')) %>% 
  filter(CNT != 0) %>% 
  arrange(desc(WINS_SHARE)) %>%
  slice_head(n=5) %>% 
  mutate(
    TEAM_ID = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID,"/global/L/logo.svg")
  ) %>% 
  select(SEASON_ID, TEAM_ID, WINS_SHARE)

five_team_by_precent <- function(data, texts){
  data %>% 
    gt() %>% 
    gt_theme_espn() %>% 
    gt_img_rows(columns = TEAM_ID, img_source = "web", height = 60) %>% 
    tab_header(title = texts[['precent_title']],
               subtitle = "Сезоны 2004/2005 - 2023/24") %>% 
    cols_width(everything() ~ px(120)) %>%
    cols_align(align = "center") %>% 
    fmt_percent(
      columns = WINS_SHARE
    ) %>% 
    cols_label(
      TEAM_ID = "",
      SEASON_ID = "СЕЗОН",
      WINS_SHARE = "ПРОЦЕНТ<br>ПОБЕД",
      .fn = md
    ) %>% 
    tab_source_note(md("**DATA**: stats.nba.com | **Telegram: https://t.me/nbaatlantic**")) %>% 
    tab_options(
      heading.align = "center"
    )
}

five_team_by_precent(small_wins_precent, wins_text)
five_team_by_precent(big_lose_precet, lose_text)

tbl_count_series <- function(data){
  tbl <- data %>% 
    group_by(TEAM, TEAM_ID) %>% 
    summarise(
      CNT = sum(CNT)
    ) %>% 
    left_join(
      data %>% 
        filter(CNT != 0) %>% 
        group_by(TEAM) %>% 
        summarise(
          CNT_SEASON = n()
        )
      ,
      by = 'TEAM'
    ) %>% 
    ungroup() %>% 
    mutate(
      CNT_SEASON = if_else(is.na(CNT_SEASON), 0, CNT_SEASON),
      TEAM_ID = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID,"/global/L/logo.svg")
    ) %>% 
    select(TEAM_ID, TEAM, CNT, CNT_SEASON) %>% 
    arrange(desc(CNT))
}

tbl_wins <- tbl_count_series(wins_df)
tbl_lose <- tbl_count_series(lose_df)

gt_tbl_count <- function(data, texts, reverse = FALSE){
  data %>% 
    gt() %>% 
    gt_theme_espn() %>% 
    gt_img_rows(columns = TEAM_ID, img_source = "web", height = 60) %>% 
    tab_header(title = texts[["count_title"]],
               subtitle = "Сезоны 2004/2005 - 2023/24") %>% 
    gt_hulk_col_numeric(CNT:CNT_SEASON, reverse = reverse) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = CNT:CNT_SEASON
      )
    ) %>% 
    cols_label(
      TEAM_ID = "",
      TEAM = "",
      CNT = "Кол-во серий",
      CNT_SEASON = "Кол-во сезонов<br>с сериями",
      .fn = md
    ) %>% 
    cols_width(everything() ~ px(120)) %>%
    cols_align(align = "center") %>% 
    tab_source_note(md("**DATA**: stats.nba.com | **Telegram: https://t.me/nbaatlantic**")) %>% 
    tab_options(
      heading.align = "center"
    )
}

gt_tbl_count(tbl_wins, wins_text)
gt_tbl_count(tbl_lose, lose_text, reverse = TRUE)