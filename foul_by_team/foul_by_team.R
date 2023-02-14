library(data.table)
library(ggplot2)

source("helpers.R")

get_nba_data(seasons = seq(2011, 2022), data = "nbastats", untar = TRUE)

list_files <- list.files(pattern = ".csv")

df <- rbindlist(lapply(list_files, fread, select=c("GAME_ID", "EVENTMSGTYPE", "PLAYER1_TEAM_ID")))

unlink(list_files)

df[,`:=`(SEASON = fifelse((GAME_ID - 20000000) %/% 100000 > 50, (GAME_ID - 20000000) %/% 100000 + 1900, (GAME_ID - 20000000) %/% 100000 + 2000))]

foul_df <- df[EVENTMSGTYPE == 6]

gr_tbl <- na.omit(foul_df[, .(.N, uniqueN(GAME_ID)), by = .(SEASON, PLAYER1_TEAM_ID)])

f <- function(x){
  return(unlist(lapply(gr_tbl$PLAYER1_TEAM_ID, function(x) names(team_dict[team_dict == x]))))
}

gr_tbl[,`:=`(FOUL_PER_GAME = round(N/V2, 2),
             TEAM_NAME = f(PLAYER1_TEAM_ID))]

ggplot(gr_tbl, aes(as.factor(SEASON), as.factor(TEAM_NAME))) +
  geom_tile(colour="black", size=0.25, aes(fill=FOUL_PER_GAME)) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(face="bold", vjust=0.5),
        legend.title = element_text(vjust=0.8, face="bold"),
        plot.background = element_rect(fill = "#f9f9f9"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(face="italic")) +
  labs(fill = "Fouls per game",
       title = "Fouls per game by teams in seasons 2011/12 - 2022/23",
       subtitle = "without coach fouls",
       caption = "DATA: nba.stats.com; twitter: @vshufinskiy, Telegram: @nbaatlantic")