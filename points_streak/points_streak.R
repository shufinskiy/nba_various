library(dplyr)
library(tidyr)
library(ggplot2)
library(gtable)
library(ggthemes)
library(grid)
library(gridExtra)

source("utils.R")

pbp <- read.csv("gsw_dal.csv")

streak_report <- function(data){
  `%notin%` <- Negate(`%in%`)
  
  teams <- sort(unique(pbp$PLAYER1_TEAM_ABBREVIATION)[unique(pbp$PLAYER1_TEAM_ABBREVIATION) != ""])

  home <- data %>% 
    filter(PERSON1TYPE == 4) %>% 
    select(PLAYER1_TEAM_ABBREVIATION) %>% 
    distinct() %>% 
    pull()
  away <- data %>% 
    filter(PERSON1TYPE == 5) %>% 
    select(PLAYER1_TEAM_ABBREVIATION) %>% 
    distinct() %>% 
    pull()
  
  teams_color <- table_color %>% 
    filter(PLAYER1_TEAM_ABBREVIATION %in% teams) %>% 
    select(TEAM_COLOR) %>% 
    pull()
  
  cnt_period <- max(data$PERIOD)
  
  period_line <- function(cnt_period){
    if(cnt_period==4){
      return(c(720, 1440, 2160))
    } else{
      quater <- c(720, 1440, 2160, 2880)
      ot <- sapply(seq(5, cnt_period), function(x){return(2880+360*(x-5))})
      period_lines <- c(quater, ot)
      period_lines <- period_lines[!duplicated(period_lines)]
      return(period_lines)
    }
  }
  
  streak_df <- data %>% 
    filter((SCORE != "" & EVENTMSGTYPE %notin% c(12, 13, 18)) | (EVENTMSGTYPE == 13 & PERIOD == cnt_period)) %>% 
    select(PERIOD, PCTIMESTRING, SCORE, PLAYER1_TEAM_ABBREVIATION) %>% 
    mutate(PCTIMESTRING = convert_time_to_second(., PCTIMESTRING)) %>% 
    mutate(PLAYER1_TEAM_ABBREVIATION = if_else(PLAYER1_TEAM_ABBREVIATION == "", lag(PLAYER1_TEAM_ABBREVIATION), PLAYER1_TEAM_ABBREVIATION)) %>% 
    separate(., SCORE, c("AWAY", "HOME"), sep = " - ", convert = TRUE) %>% 
    mutate(MARGIN = AWAY - HOME,
           LAG_TEAM = replace_na(PLAYER1_TEAM_ABBREVIATION == lag(PLAYER1_TEAM_ABBREVIATION, default = NA), FALSE),
           LEAD_TEAM = replace_na(PLAYER1_TEAM_ABBREVIATION == lead(PLAYER1_TEAM_ABBREVIATION, default = NA), FALSE)) %>% 
    mutate(LAG_AWAY = lag(AWAY, default = 0),
           LAG_HOME = lag(HOME, default = 0),
           SCORE_AWAY = AWAY == lag(AWAY, default = 0),
           SCORE_HOME = HOME == lag(HOME, default = 0)) %>% 
    filter(LAG_TEAM == FALSE | LEAD_TEAM == FALSE) %>% 
    mutate(START_TIME = if_else(LAG_TEAM == FALSE, PCTIMESTRING, lag(PCTIMESTRING, default = 0)),
           END_TIME = lead(PCTIMESTRING, default = 2880)) %>% 
    mutate(STREAK_AWAY = if_else((LEAD_TEAM == FALSE & LAG_TEAM == FALSE), AWAY - lag(AWAY, default = 0), if_else(LEAD_TEAM == FALSE, AWAY - lag(LAG_AWAY), -100)),
           STREAK_HOME = if_else((LEAD_TEAM == FALSE & LAG_TEAM == FALSE), HOME - lag(HOME, default = 0), if_else(LEAD_TEAM == FALSE, HOME - lag(LAG_HOME), -100))) %>% 
    filter(STREAK_AWAY != -100) %>% 
    select(PLAYER1_TEAM_ABBREVIATION, START_TIME, END_TIME, STREAK_AWAY, STREAK_HOME, MARGIN) %>% 
    mutate(STREAK_HOME = -STREAK_HOME) %>% 
    mutate(STREAK = if_else(STREAK_AWAY == 0, STREAK_HOME, STREAK_AWAY)) %>% 
    select(PLAYER1_TEAM_ABBREVIATION, START_TIME, END_TIME, STREAK, MARGIN)
  
  cnt_streaks <- streak_df %>% 
    mutate(STREAK = abs(STREAK)) %>% 
    group_by(PLAYER1_TEAM_ABBREVIATION, STREAK) %>% 
    summarise(CNT = n()) %>% 
    ungroup()
  
  share_pts <- streak_df %>% 
    mutate(STREAK = abs(STREAK)) %>% 
    mutate(TYPE = cut(abs(streak_df$STREAK), breaks=c(-Inf, 3, 6, 10, Inf), labels = c("1-3 PTS", "4-6 PTS", "7-10 PTS", "10+ PTS"))) %>% 
    group_by(PLAYER1_TEAM_ABBREVIATION, TYPE) %>% 
    summarise(CNT = n(),
              PTS = sum(STREAK)) %>% 
    group_by(PLAYER1_TEAM_ABBREVIATION) %>% 
    mutate(PTS_PREC = round(PTS/sum(PTS)*100, 2),
           TEAM = PLAYER1_TEAM_ABBREVIATION) %>% 
    ungroup() %>% 
    select(TEAM, TYPE, CNT, PTS, PTS_PREC)
  
  gg <- ggplot(streak_df, aes(xmin=START_TIME, xmax=END_TIME, ymin=0, ymax=STREAK, fill=PLAYER1_TEAM_ABBREVIATION)) +
    geom_rect(colour="black") +
    geom_vline(xintercept = period_line(cnt_period)) +
    geom_text(aes(x = END_TIME - (END_TIME - START_TIME)/2, y = STREAK, label = abs(STREAK)), nudge_y = if_else(streak_df$STREAK > 0, 0.5, -0.5), size = 3) +
    scale_fill_manual(values = teams_color) +
    theme_tufte() +
    labs(fill = "Team") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "top",
          panel.border = element_rect(color="black",
                                      fill = NA,
                                      size=1))
  
  gg1 <- ggplot(streak_df, aes(x = END_TIME, y = MARGIN)) +
    geom_line(col = "gray80") +
    geom_point(col = if_else(streak_df$MARGIN > 0, teams_color[2], teams_color[1]), size = 3) +
    geom_vline(xintercept = period_line(cnt_period)) +
    geom_text(aes(label = MARGIN), nudge_y = if_else(streak_df$STREAK > 0, 1, -1), size = 2.5, angle=45) +
    theme_tufte() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_rect(color="black",
                                      fill = NA,
                                      size=1))
  
  
  gg2 <- ggplot(cnt_streaks, aes(x = as.factor(STREAK), y = CNT, 
                                 fill = PLAYER1_TEAM_ABBREVIATION)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    geom_text(aes(label = CNT, y = CNT + 0.25), position = position_dodge2(1), size = 3) +
    scale_fill_manual(values = teams_color) +
    theme_tufte() +
    labs(fill = "Team", x = "Streaks (PTS)") +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          panel.border = element_rect(color="black",
                                      fill = NA,
                                      size=1))
  
  table_fill <- c(rep(teams_color[1], times=dim(filter(share_pts, TEAM == teams[1]))[1]), 
                  rep(teams_color[2], times=dim(filter(share_pts, TEAM == teams[2]))[2]))

  gtable <- tableGrob(share_pts, theme = ttheme_minimal(core=list(bg_params = list(fill = table_fill, col=NA, alpha =0.25))), rows = NULL)
  gtable$widths <- unit(rep(1/ncol(gtable), ncol(gtable)), "npc")
  gtable$heights <- unit(rep(1/nrow(gtable), nrow(gtable)), "npc")
  g <- replicate(dim(share_pts)[1] + 1, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
  gtable <- gtable_add_grob(gtable, grobs = g, t=2, b=seq_len(nrow(gtable)), r=1, l=ncol(gtable))
  
  title <- paste("Streak and Margin in Game", away, "vs.", home, sep = " ")
  caption <- "Data sourse: stats.nba.com, Telegram: @NBAatlantic, Twitter: @vshufinskiy"
  
  plot<- arrangeGrob(gg, gg2, gg1, gtable, ncol = 2, top = title, bottom = caption)
  return(plot)
}

grid.arrange(streak_report(pbp))