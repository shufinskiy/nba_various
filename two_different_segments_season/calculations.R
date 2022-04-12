library(data.table, warn.conflicts = FALSE)

set.seed(42)
logs <- fread("./data/gamelog.csv")
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
d[, "TEAM_NAME" := sapply(TEAM_NAME, function(x) names(team_dict[team_dict == x]))]

d[, `:=`(FIRST_PART = paste0(FIRST_PART, '-', FIRST_PART_FINISH),
         SECOND_PART = paste0(SECOND_PART, '-', SECOND_PART_FINISH),
         DATE_FP = paste0(DATE_FP, " ", DATE2),
         DATE_SP = paste0(DATE_SP, " ", DATE4))][
           , c("FIRST_PART_FINISH", "SECOND_PART_FINISH", "DATE2", "DATE4") := NULL
         ]

d <- d[order(DIFF, decreasing = TRUE)]

library(RColorBrewer)
library(inlmisc)
library(grid)
library(gridExtra)
library(gtable)

cols <- sapply(seq(1, 30), function(x) if (d[, `FIRST_PART_W%`][x] > d[, `SECOND_PART_W%`][x]) "#64A5CC" else "#E17A60")

theme_tbl <- ttheme_minimal(core = list(bg_params = list(fill=cols, col=NA)))
t1 <- tableGrob(d[, 1:7], theme = ttheme_minimal(), rows = NULL)
t2 <- tableGrob(d[, 8], theme = theme_tbl, rows = NULL)
g <- replicate(31, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
t1 <- gtable_add_grob(t1, grobs = g, t=2, b=seq_len(nrow(t1)), r=1, l=ncol(t1))
t2 <- gtable_add_grob(t2, grobs = g, t=2, b=seq_len(nrow(t2)), r=1, l=ncol(t2))
t <- gtable_combine(t1,t2, along=1)
grid.arrange(t, top=paste0("Difference W% for two parts season 2021/22",
                           "\nEach part min. 25 games."), bottom=" Telegram: @nbaatlantic | Data: stats.nba.com")