library(data.table, warn.conflicts = FALSE)

season <- 2022

set.seed(42)
logs <- fread(paste0('./data/playergamelog_', season, '.csv'))
logs <- logs[order(GAME_DATE)]

val <- "REB"
ntop <- 15

playercalculation <- function(value){
  player_id <- unique(logs[, PLAYER_ID])
  
  cols <- value
  
  m <- list()
  for(player in player_id){
    players_logs <- logs[PLAYER_ID == player]
    ngame <- nrow(players_logs)
    f <- list()
    max_diff <- 0
    if(ngame >= 50){
      for(i in seq(25, ngame-25)){
        first_pts <- round(mean(unlist(players_logs[seq(1, i), ..cols], use.names = FALSE)), 1)
        second_pts <- round(mean(unlist(players_logs[seq(i+1, ngame), ..cols], use.names = FALSE)), 1)      
        
        diff_pts <- abs(first_pts - second_pts)
        if(max_diff < diff_pts){
          max_diff <- diff_pts
          f <- list()
          f[[paste0(i, ngame)]] <- c(player, 1, i, i+1, ngame, players_logs[1, "GAME_DATE"][[1]], players_logs[i, "GAME_DATE"][[1]],
                                     players_logs[i+1, "GAME_DATE"][[1]], players_logs[ngame, "GAME_DATE"][[1]], first_pts, second_pts, diff_pts)
        } else if (max_diff == diff_pts){
          f[[paste0(i, ngame)]] <- c(player, 1, i, i+1, ngame, players_logs[1, "GAME_DATE"][[1]], players_logs[i, "GAME_DATE"][[1]],
                                     players_logs[i+1, "GAME_DATE"][[1]], players_logs[ngame, "GAME_DATE"][[1]], first_pts, second_pts, diff_pts)
        } else {
          next
        }
      }
      m[[as.character(player)]] <- f
    }
  }
  
  d <- data.table(matrix(data = unlist(m, use.names = FALSE), ncol=12, byrow = TRUE))
  
  colnames(d) <- c("PLAYER_NAME", "FIRST_PART", "FIRST_PART_FINISH", "SECOND_PART", "SECOND_PART_FINISH",
                   "DATE_FP", "DATE2", "DATE_SP", "DATE4", "FIRST_PART_VAL", "SECOND_PART_VAL", "DIFF")

  date_cols <- c("DATE_FP", "DATE2", "DATE_SP", "DATE4")
  d[, (date_cols) := lapply(.SD, lubridate::as_date), .SDcols = date_cols]
  pl_un <- unique(logs[, c("PLAYER_ID", "PLAYER_NAME")])
  d$PLAYER_ID <- d$PLAYER_NAME
  d[, "PLAYER_NAME" := sapply(PLAYER_NAME, function(x) pl_un[PLAYER_ID == x, PLAYER_NAME])]

  d[, `:=`(FIRST_PART = paste0(FIRST_PART, '-', FIRST_PART_FINISH),
           SECOND_PART = paste0(SECOND_PART, '-', SECOND_PART_FINISH),
           DATE_FP = paste0(DATE_FP, " ", DATE2),
           DATE_SP = paste0(DATE_SP, " ", DATE4))][
             , c("FIRST_PART_FINISH", "SECOND_PART_FINISH", "DATE2", "DATE4") := NULL
           ]
  return(d)
}

d <- playercalculation(val)

d <- d[order(DIFF, decreasing = TRUE)]
d <- d[!which(duplicated(d[, PLAYER_NAME]))]
d <- d[seq(1, ntop)]

library(gt)
library(gtExtras)

d %>% 
  gt() %>% 
  gt_theme_espn() %>% 
  cols_move_to_start(columns = PLAYER_ID) %>% 
  text_transform(
    locations = cells_body(columns = PLAYER_ID),
    fn = function(x){
      paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/', x, '.png')
    }
  ) %>% 
  text_transform(
    fn = function(x){
      gsub(" ","<br>", x)
    },
    locations = cells_body(columns=c(DATE_FP, DATE_SP))
  ) %>% 
  cols_align(align = "center") %>%
  gt_img_rows(columns = PLAYER_ID, img_source = "web", height = 60) %>%
  tab_spanner(
    "№ Games",
    columns = c(FIRST_PART, SECOND_PART),
    id = 'for_footnote'
  ) %>% 
  tab_spanner(
    "DATE",
    columns = c(DATE_FP, DATE_SP)
  ) %>% 
  tab_spanner(
    val,
    columns = c(FIRST_PART_VAL, SECOND_PART_VAL, DIFF)
  ) %>% 
  tab_footnote(
    footnote = "Sequence numbers of games in the season for the player. They may not equal 82.",
    locations = cells_column_spanners(spanners='for_footnote')
  ) %>% 
  tab_style(
    style = cell_fill(color='#64A5CC'),
    locations = cells_body(
      columns = DIFF,
      rows = FIRST_PART_VAL - SECOND_PART_VAL > 0
    )
  ) %>% 
  tab_style(
    style = cell_fill(color='#E17A60'),
    locations = cells_body(
      columns = DIFF,
      rows = FIRST_PART_VAL - SECOND_PART_VAL < 0
    )
  ) %>% 
  cols_label(
    .list = list(
      "PLAYER_ID" = "",
      "DATE_FP" = "FIRST PART",
      "DATE_SP" = "SECOND PART",
      "FIRST_PART_VAL" = "FIRST PART",
      "SECOND_PART_VAL" = "SECOND PART"
    )
  ) %>% 
  tab_header(title = paste0("Top-15 players by ", val, " difference between two parts season 2022/23"),
             subtitle = "Min. 50 games in season. Each part min. 25 games") %>%
  tab_source_note(md("DATA:stats.nba.com; twitter: **@vshufinskiy**, Telegram: **@nbaatlantic**")) %>%
  tab_options(
    heading.align = "center"
  ) %>% 
  gtsave(paste0("./charts/", tolower(val),".png"), vheight = 2500)
