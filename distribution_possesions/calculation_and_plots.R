library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(inlmisc)
library(grid)
library(gridExtra)
library(gtable)

past_season <- read.csv('./data/2020.csv', stringsAsFactors = FALSE)
current_season <- read.csv('./data/2021.csv', stringsAsFactors = FALSE)

past_s <- past_season %>% 
  select(HomeTeamAbbreviation, AwayTeamAbbreviation, HomePossessions, AwayPossessions) %>% 
  pivot_longer(c(HomePossessions, AwayPossessions), names_to = "possessions", values_to = "cnt_poss") %>% 
  pivot_longer(c(HomeTeamAbbreviation, AwayTeamAbbreviation), names_to = "tmp", values_to = "teams") %>% 
  filter(str_sub(possessions, 1, 4) == str_sub(tmp, 1, 4)) %>% 
  select(teams, cnt_poss)
  
current_s <- current_season %>% 
  select(HomeTeamAbbreviation, AwayTeamAbbreviation, HomePossessions, AwayPossessions) %>% 
  pivot_longer(c(HomePossessions, AwayPossessions), names_to = "possessions", values_to = "cnt_poss") %>% 
  pivot_longer(c(HomeTeamAbbreviation, AwayTeamAbbreviation), names_to = "tmp", values_to = "teams") %>% 
  filter(str_sub(possessions, 1, 4) == str_sub(tmp, 1, 4)) %>% 
  select(teams, cnt_poss) 

tbl <- inner_join(past_s%>% 
                    group_by(teams) %>% 
                    summarise(poss_2020 = median(cnt_poss), .groups = 'drop'), 
                  current_s%>% 
                    group_by(teams) %>% 
                    summarise(poss_2021 = median(cnt_poss), .groups = 'drop'), by="teams") %>% 
  mutate(diff = poss_2021 - poss_2020) %>% 
  arrange(desc(diff))

ggplot(current_s, aes(teams, cnt_poss)) +
  geom_jitter(width=0.25) +
  stat_summary(fun = median, fun.min = median, fun.max = median,
               geom = "crossbar", width = 0.5, color='red') +
  geom_vline(xintercept = seq(0.5, 30.5, 1)) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'График количества владений за игру в сезоне 2021/22',
       subtitle = 'Красная линия - медианное значение владений за игру',
       caption = 'Telegram: @nbaatlantic | Data: pbpstats.com') +
  theme(plot.title = element_text(hjust=0.5))

diff <- tbl$diff
cols <- GetColors(30, scheme = "BuRd")[seq(30, 1)]

for(i in seq_len(length(cols))){
  if(i > 1){
    if(diff[i] == diff[i-1]){
      cols[i] <- cols[i-1]
    } else {
      next
    }
  } else {
    next
  }
}


theme_tbl <- ttheme_minimal(core = list(bg_params = list(fill=cols, col=NA)))
t1 <- tableGrob(tbl[, 1:3], theme = ttheme_minimal(), rows = NULL)
t2 <- tableGrob(tbl[, 4], theme = theme_tbl, rows = NULL)
g <- replicate(31, segmentsGrob(x0 = unit(0,"npc"), x1 = unit(1,"npc"), y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp=gpar(fill = NA, lwd = 2)), simplify = FALSE)
t1 <- gtable_add_grob(t1, grobs = g, t=2, b=seq_len(nrow(t1)), r=1, l=ncol(t1))
t2 <- gtable_add_grob(t2, grobs = g, t=2, b=seq_len(nrow(t2)), r=1, l=ncol(t2))
t <- gtable_combine(t1,t2, along=1)
grid.arrange(t, top="Разница числа владений между сезонами 2020/21 и 2021/22", bottom=" Telegram: @nbaatlantic | Data: pbpstats.com")


box_data <- bind_rows(mutate(current_s, season = "2021"), mutate(past_s, season = "2020"))

ggplot(box_data, aes(season, cnt_poss, group = season)) + 
  geom_boxplot() +
  facet_wrap(vars(teams), scales = "free_y") +
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'Сравнение количества владений в сезонах 2020/21 и 2021/22',
       caption = 'Telegram: @nbaatlantic | Data: pbpstats.com') +
  theme(plot.title = element_text(hjust=0.5))