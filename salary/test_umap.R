library(dplyr)
library(tidyr)
library(ggplot2)
library(umap)
# `%>%` <- magrittr::`%>%`

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
season <- '2324'
df <- read.csv(paste0("./data/raw_salary_nba", season, ".csv"))
colnames(df) <- c('PLAYER', 'SEASON', 'SEASON1', 'SEASON2', 'SEASON3', 'SEASON4', 'TEAM')

df1 <- df %>% 
  dplyr::tibble() %>% 
  dplyr::select(PLAYER, SEASON, TEAM) %>% 
  dplyr::mutate(SEASON = as.integer(stringr::str_remove_all(stringr::str_remove_all(SEASON, ","), "\\$"))) %>% 
  dplyr::rename(SALARY = SEASON) %>% 
  dplyr::arrange(TEAM, desc(SALARY)) %>% 
  dplyr::group_by(TEAM) %>% 
  dplyr::mutate(NSALARY = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(NSALARY <= 10) %>% 
  dplyr::group_by(NSALARY) %>% 
  dplyr::mutate(SCALE_SALARY = scale(SALARY) %>% as.vector(),
                SALARY_RANK = rank(-SALARY, ties.method = "min")) %>% 
  dplyr::ungroup()

pivot_df <- df1 %>% 
  pivot_wider(id_cols = TEAM,
              names_from = NSALARY,
              names_prefix = "T_",
              values_from = SALARY_RANK)

pivot_data <- dplyr::select(pivot_df, -TEAM)
# pivot_data <- as.data.frame(scale(dplyr::select(pivot_df, -TEAM)))
pivot_label <- dplyr::select(pivot_df, TEAM)

custom_config <- umap.defaults
custom_config$random_state <- 42
custom_config$n_neighbors <- 6
custom_config$min_dist <- 0.2

umap_list <- umap::umap(pivot_data, config = custom_config)
umap_data <- as.data.frame(umap_list$layout)
colnames(umap_data) <- c("V_1", "V_2")

umap_data <- dplyr::mutate(umap_data, TEAM = pivot_label$TEAM)

ggthemr::ggthemr('solarized')

ggplot2::ggplot(umap_data, aes(V_1, V_2, label=TEAM)) +
  # ggplot2::geom_point() +
  ggplot2::geom_text() +
  ggplot2::labs(title = paste0("Clustering of NBA teams by payroll in season ", substr(season, 1, 2), "/", substr(season, 3, 4)),
                caption = "DATA: hoopshype; twitter: @vshufinskiy, Telegram: @nbaatlantic") +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5))
  

# # cl <- dbscan::hdbscan(pivot_data, minPts = 3)
# cl <- dbscan::hdbscan(umap_data[, c(1, 2)], minPts = 3)
# 
# pivot_df$CL <- cl$cluster