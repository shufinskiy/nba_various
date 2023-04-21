library(data.table, warn.conflicts = FALSE)

season <- 2022

set.seed(42)
logs <- fread(paste0('./data/gamelog_', season, '.csv'))
logs <- logs[order(GAME_DATE)]

team_id <- unique(logs[, TEAM_ID])

team_dict <- list(
  'ATL' = 1610612737,
  'BOS' = 1610612738,
  'BKN' = 1610612751,
  'CHA' = 1610612766,
  'CHI' = 1610612741,
  'CLE' = 1610612739,
  'DAL' = 1610612742,
  'DEN' = 1610612743,
  'DET' = 1610612765,
  'GSW' = 1610612744,
  'HOU' = 1610612745,
  'IND' = 1610612754,
  'LAC' = 1610612746,
  'LAL' = 1610612747,
  'MEM' = 1610612763,
  'MIA' = 1610612748,
  'MIL' = 1610612749,
  'MIN' = 1610612750,
  'NOP' = 1610612740,
  'NYK' = 1610612752,
  'OKC' = 1610612760,
  'ORL' = 1610612753,
  'PHI' = 1610612755,
  'PHX' = 1610612756,
  'POR' = 1610612757,
  'SAC' = 1610612758,
  'SAS' = 1610612759,
  'TOR' = 1610612761,
  'UTH' = 1610612762,
  'WAS' = 1610612764
)

m <- list()
for(team_ids in team_id){
  team_logs <- logs[TEAM_ID == team_ids]
  ngame <- nrow(team_logs)
  f <- list()
  max_diff <- 0
  for(i in seq(25, ngame-25)){
    first_part <- team_logs[seq(1, i)]
    second_part <- team_logs[seq(i+1, ngame)]
    first_w <- as.integer(first_part[WL == 'W', .(.N), by=c("WL")][1, 2]) / nrow(first_part)
    second_w <- as.integer(second_part[WL == 'W', .(.N), by=c("WL")][1, 2]) / nrow(second_part)
    diff <- abs(first_w - second_w)
    if(max_diff < diff){
      max_diff <- diff
      f <- list()
      f[[paste0(i, ngame)]] <- c(team_ids, 1, i, i+1, ngame, team_logs[1, "GAME_DATE"][[1]], team_logs[i, "GAME_DATE"][[1]], 
                                 team_logs[i+1, "GAME_DATE"][[1]], team_logs[ngame, "GAME_DATE"][[1]], round(first_w*100, 2), round(second_w*100, 2), 
                                 round(diff*100, 2))
    } else if (max_diff == diff){
      f[[paste0(i, ngame)]] <- c(team_ids, 1, i, i+1, ngame, team_logs[1, "GAME_DATE"][[1]], team_logs[i, "GAME_DATE"][[1]], 
                                 team_logs[i+1, "GAME_DATE"][[1]], team_logs[ngame, "GAME_DATE"][[1]], round(first_w*100, 2), round(second_w*100, 2), 
                                 round(diff*100, 2))   
    } else {
      next
    }
  }
  m[[as.character(team_ids)]] <- f
}


d <- data.table(matrix(data = unlist(m, use.names = FALSE), nrow=30, ncol=12, byrow = TRUE))
colnames(d) <- c("TEAM_NAME", "FIRST_PART", "FIRST_PART_FINISH", "SECOND_PART", "SECOND_PART_FINISH", 
                 "DATE_FP", "DATE2", "DATE_SP", "DATE4", "FIRST_PART_W%", "SECOND_PART_W%", "DIFF")

date_cols <- c("DATE_FP", "DATE2", "DATE_SP", "DATE4")
d[, (date_cols) := lapply(.SD, lubridate::as_date), .SDcols = date_cols]
# d[, "TEAM_NAME" := sapply(TEAM_NAME, function(x) names(team_dict[team_dict == x]))]

d[, `:=`(FIRST_PART = paste0(FIRST_PART, '-', FIRST_PART_FINISH),
         SECOND_PART = paste0(SECOND_PART, '-', SECOND_PART_FINISH),
         DATE_FP = paste0(DATE_FP, " ", DATE2),
         DATE_SP = paste0(DATE_SP, " ", DATE4))][
           , c("FIRST_PART_FINISH", "SECOND_PART_FINISH", "DATE2", "DATE4") := NULL
         ]

d <- d[order(DIFF, decreasing = TRUE)]
d$TEAM_NAME <- paste0("https://cdn.nba.com/logos/nba/", d$TEAM_NAME,"/global/L/logo.svg")

library(gt)
library(gtExtras)

build_team_gt_table <- function(data){
  data %>% 
    gt() %>% 
    gt_theme_espn() %>% 
    text_transform(
      fn = function(x){
        gsub(" ","<br>", x)
      },
      locations = cells_body(columns=c(DATE_FP, DATE_SP))
    ) %>% 
    cols_align(align = "center") %>% 
    gt_img_rows(columns = TEAM_NAME, img_source = "web", height = 60) %>%
    tab_spanner(
      "№ Games",
      columns = c(FIRST_PART, SECOND_PART)
    ) %>% 
    tab_spanner(
      "DATE",
      columns = c(DATE_FP, DATE_SP)
    ) %>% 
    tab_spanner(
      "Win Percentage",
      columns = c(`FIRST_PART_W%`, `SECOND_PART_W%`, DIFF)
    ) %>% 
    tab_style(
      style = cell_fill(color='#64A5CC'),
      locations = cells_body(
        columns = DIFF,
        rows = `FIRST_PART_W%` - `SECOND_PART_W%` > 0
      )
    ) %>% 
    tab_style(
      style = cell_fill(color='#E17A60'),
      locations = cells_body(
        columns = DIFF,
        rows = `FIRST_PART_W%` - `SECOND_PART_W%` < 0
      )
    ) %>% 
    cols_label(
      TEAM_NAME = "TEAM",
      FIRST_PART = "FIRST PART",
      SECOND_PART = "SECOND_PART",
      DATE_FP = "FIRST PART",
      DATE_SP = "SECOND PART",
      `FIRST_PART_W%` = "FIRST PART",
      `SECOND_PART_W%` = "SECOND PART"
    ) %>% 
    tab_header(title = "Difference between Win precentage for two parts season 2022/23",
               subtitle = "Each part min 25 games") %>%
    tab_source_note(md("DATA:stats.nba.com; twitter: **@vshufinskiy**, Telegram: **@nbaatlantic**")) %>%
    tab_options(
      heading.align = "center"
    )
}

build_team_gt_table(d[seq(1, 15),]) %>% 
  gtsave("./charts/team_win_top.png", vheight = 2500)
build_team_gt_table(d[seq(16, 30),]) %>% 
  gtsave("./charts/team_win_bottom.png", vheight = 2500)