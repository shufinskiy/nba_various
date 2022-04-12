library(dplyr, warn.conflicts = FALSE)

logs <- read.csv("./data/playergamelog.csv")

ntop <- 30

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
  select(PLAYER_NAME, TEAM_NAME, COUNT)

library(RColorBrewer)
library(inlmisc)
library(grid)
library(gridExtra)
library(gtable)

t1 <- tableGrob(tbl[, 1:3], theme = ttheme_minimal(), rows = NULL)
g <- replicate(ntop+1, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
t1 <- gtable_add_grob(t1, grobs = g, t=2, b=seq_len(nrow(t1)), r=1, l=ncol(t1))
grid.arrange(t1, top="Count of games with a lead by points", bottom=" Telegram: @nbaatlantic | Data: stats.nba.com")