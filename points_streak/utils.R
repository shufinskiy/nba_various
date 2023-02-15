convert_time_to_second <- function(data, column){
  column <- enquo(column)
  data %>%
    separate(!!column, c('MIN', 'SEC'), sep = ':') %>%
    mutate_at(c('MIN', 'SEC'), as.numeric) %>%
    mutate(!!column := ifelse(PERIOD < 5, abs((MIN * 60 + SEC) - 720 * PERIOD), abs((MIN * 60 + SEC) - (2880 + 300 * (PERIOD - 4))))) %>%
    select(!!column) %>%
    pull()
}

table_color <- data.frame(PLAYER1_TEAM_ABBREVIATION = c("ATL", "BOS", "BKN", "CHA", "CHI", 
                                                        "CLE", "DAL", "DEN", "DET", "GSW", 
                                                        "HOU", "IND", "LAC", "LAL", "MEM", 
                                                        "MIA", "MIL", "MIN", "NOP", "NYK", 
                                                        "OKC", "ORL", "PHI", "PHX", "POR", 
                                                        "SAC", "SAS", "TOR", "UTA", "WAS"),
                          TEAM_COLOR = c("#E03A3E", "#007A33", "#000000", "#1D1160", "#CE1141", "#6F263D",
                                         "#00538C", "#0E2240", "#C8102E", "#FFC72C", "#CE1141", "#002D62",
                                         "#C8102E", "#552583", "#5D76A9", "#98002E", "#00471B", "#0C2340",
                                         "#0C2340", "#006BB6", "#007AC1", "#0077C0", "#006BB6", "#1D1160",
                                         "#E03A3E", "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C"), 
                          stringsAsFactors = FALSE)

