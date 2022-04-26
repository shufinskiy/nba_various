library(httr, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)

source("./utils.R")

playoff_games <- lapply(seq(1996, 2021), function(x){
  t <- get_leaguegamelog(x, SeasonType="Playoffs")
  print(paste("Done:", x))
  return(t)
})

playoff_data <- rbindlist(playoff_games)

light_data <- playoff_data[, .(SEASON_ID, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME, MATCHUP, WL)]

light_data <- light_data %>% 
  mutate(MATCHUP = str_replace(str_remove(MATCHUP, "(vs.)|(@)"), "\\s+", " ")) %>% 
  separate(MATCHUP, c("TEAM", "OPP"), sep = " ") %>% 
  group_by(SEASON_ID, TEAM_ID, OPP, WL) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = WL, values_from = count, values_fill = 0) %>% 
  filter(L == 4 & W == 0) %>% 
  select(SEASON_ID, TEAM_ID, OPP)

playoff_data <- separate(mutate(playoff_data, MATCHUP = str_replace(str_remove(MATCHUP, "(vs.)|(@)"), "\\s+", " ")), MATCHUP, c("TEAM", "OPP"), sep = " ")
  
main_tbl <- playoff_data %>% 
  inner_join(light_data, by=c("SEASON_ID", "TEAM_ID", "OPP")) %>% 
  mutate(PLUS_MINUS = as.integer(PLUS_MINUS)) %>% 
  group_by(SEASON_ID, TEAM_ID, TEAM_NAME, OPP) %>% 
  summarise(PLUS_MINUS = sum(PLUS_MINUS)) %>% 
  ungroup() %>% 
  arrange(desc(PLUS_MINUS)) %>% 
  mutate(SEASON_ID = paste(as.integer(str_sub(SEASON_ID, 2)), 
                           str_pad(as.integer(str_sub(as.integer(str_sub(SEASON_ID, 2)), 3, 4))+1, 2, side = "left", pad="0"), 
                           sep = "-")) %>% 
  mutate(SEASON_ID = ifelse(SEASON_ID == "1999-100", "1999-00", SEASON_ID)) %>% 
  select(-TEAM_ID)

main_tbl <- main_tbl[seq(1, 10),]

# library(RColorBrewer)
# library(inlmisc)
library(grid)
library(gridExtra)
library(gtable)

t1 <- tableGrob(main_tbl, theme = ttheme_minimal(), rows = NULL)
g <- replicate(11, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
t1 <- gtable_add_grob(t1, grobs = g, t=2, b=seq_len(nrow(t1)), r=1, l=ncol(t1))
grid.arrange(t1)