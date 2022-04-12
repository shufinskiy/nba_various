library(data.table, warn.conflicts = FALSE)

set.seed(42)
logs <- fread("./data/playergamelog.csv")
logs <- logs[order(GAME_DATE)]

val <- "PTS"
ntop <- 30

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
                   "DATE_FP", "DATE2", "DATE_SP", "DATE4", paste0("FIRST_PART_", cols), paste0("SECOND_PART_", cols), "DIFF")
  
  date_cols <- c("DATE_FP", "DATE2", "DATE_SP", "DATE4")
  d[, (date_cols) := lapply(.SD, lubridate::as_date), .SDcols = date_cols]
  pl_un <- unique(logs[, c("PLAYER_ID", "PLAYER_NAME")])
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

library(RColorBrewer)
library(inlmisc)
library(grid)
library(gridExtra)
library(gtable)

cols <- sapply(seq(1, ntop), function(x) if (d[[6]][x] > d[[7]][x]) "#64A5CC" else "#E17A60")

theme_tbl <- ttheme_minimal(core = list(bg_params = list(fill=cols, col=NA)))
t1 <- tableGrob(d[, 1:7], theme = ttheme_minimal(), rows = NULL)
t2 <- tableGrob(d[, 8], theme = theme_tbl, rows = NULL)
g <- replicate(ntop+1, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
t1 <- gtable_add_grob(t1, grobs = g, t=2, b=seq_len(nrow(t1)), r=1, l=ncol(t1))
t2 <- gtable_add_grob(t2, grobs = g, t=2, b=seq_len(nrow(t2)), r=1, l=ncol(t2))
t <- gtable_combine(t1,t2, along=1)
grid.arrange(t, top=paste0("Top-", ntop," difference in ", val," value for two parts of 2021/22 season.",
                           "\nFor players with min. 50 games, each part min. 25 games."), bottom=" Telegram: @nbaatlantic | Data: stats.nba.com")