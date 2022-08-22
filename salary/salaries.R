`%>%` <- magrittr::`%>%`

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

df <- read.csv("./data/raw_salary_nba.csv")

df1 <- df %>% 
  dplyr::tibble() %>% 
  dplyr::select(PLAYER, X2022.23, TEAM) %>% 
  dplyr::mutate(X2022.23 = as.integer(stringr::str_remove_all(stringr::str_remove_all(X2022.23, ","), "\\$"))) %>% 
  dplyr::rename(SALARY = X2022.23) %>% 
  dplyr::arrange(TEAM, desc(SALARY)) %>% 
  dplyr::group_by(TEAM) %>% 
  dplyr::mutate(NSALARY = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(NSALARY <= 10) %>% 
  dplyr::group_by(NSALARY) %>% 
  dplyr::mutate(SCALE_SALARY = scale(SALARY) %>% as.vector(),
                SALARY_RANK = rank(-SALARY, ties.method = "min")) %>% 
  dplyr::ungroup()

ggthemr::ggthemr('solarized')
### Boxplot всех позиций
ggplot2::ggplot(df1, ggplot2::aes(as.factor(NSALARY), SALARY))+
  ggplot2::geom_boxplot() +
  ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  ggplot2::xlab("position on team salary cap") +
  ggplot2::ylab("salary in million dollars") +
  ggplot2::labs(title = "Distribution salary by position in NBA team's salary",
                caption = "DATA: nba.com; twitter: @vshufinskiy, Telegram: @nbaatlantic") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5))

summary_rank <- df1 %>% 
  dplyr::group_by(TEAM) %>% 
  dplyr::summarise(SUM_RANK = sum(SALARY_RANK)) %>% 
  dplyr::ungroup()

### Summary rank
ggplot2::ggplot(summary_rank, ggplot2::aes(forcats::fct_reorder(TEAM, SUM_RANK), SUM_RANK)) +
  ggplot2::geom_col() +
  ggplot2::ylab("Total rank") +
  ggplot2::labs(title = "Total rank top-10 team's salary",
                caption = "DATA: nba.com; twitter: @vshufinskiy, Telegram: @nbaatlantic") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust=0.5))

salary_info_team <- function(abr_team, nba_dict=team_dict){
  
  one_team <- dplyr::filter(df1, TEAM == abr_team)
  ### Три графика для команды: 1. салари, 2. Ранг в НБА
  gg1 <- ggplot2::ggplot(one_team, aes(NSALARY, SALARY)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_x_continuous(breaks = seq(1, 10)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::ylab("salary in million dollars")
  
  gg3 <- ggplot2::ggplot(one_team, aes(NSALARY, SALARY_RANK)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(breaks = c(1, seq(5, 30, 5)), trans = "reverse") +
    ggplot2::scale_x_continuous(breaks = seq(1, 10)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::ylab("Rank salary")
  
  patchgg <- gg1 /gg3
  patchgg <- patchgg +
    patchwork::plot_annotation(
      title = paste0("Top 10 ", nba_dict[[abr_team]][2]," salaries"),
      caption = "DATA: nba.com; twitter: @vshufinskiy, Telegram: @nbaatlantic"
    ) &
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5)) 
  ggplot2::ggsave("./charts/tmp_chart.png", plot = patchgg)
  
  ### Таблица топ-10 игроков команды по зарплатам
  one_team %>% 
    dplyr::mutate(RANK = dplyr::row_number()) %>% 
    dplyr::select(RANK, SALARY_RANK, PLAYER, SALARY) %>% 
    gt::gt() %>% 
    gt::fmt_number(columns = SALARY,
                   suffixing = TRUE) %>% 
    gt::cols_label(SALARY_RANK = "NBA") %>% 
    gt::tab_header(gt::html(paste0(gt::html(
      paste0(gsub(">", "", gt::html(gt::local_image(paste0("./logo/", nba_dict[[abr_team]][1],".png"), height = 60))),
             ";width:60px;\" align=\"left\">")), 
      paste0(" Top 10 players on the <b>", nba_dict[[abr_team]][2],"</b> salary cap ")))) %>% 
    gt::tab_options(table.background.color = '#f9f9f9',
                    table.border.top.color = "#36454f",
                    table.border.bottom.color = "#36454f",
                    heading.title.font.size = 15,
                    heading.align = "center") %>% 
    gt::gtsave("./charts/tmp_salary.png", expand=0)
  
  pic1 <- magick::image_scale(magick::image_read("./charts/tmp_chart.png"), "x919")
  pic2 <- magick::image_read("./charts/tmp_salary.png")
  
  magick::image_write(magick::image_append(c(pic2, pic1)), path = paste0("./charts/", tolower(abr_team),".png"), format = "png")
}

for(name in names(team_dict)){
  salary_info_team(name)
}
