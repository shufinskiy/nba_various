source('download_utils.R')

season <- 2022

get_leaguegamelog <- function(Season=2020, ...){
  Season <- paste(Season, as.numeric(substr(Season, 3, 4)) + 1, sep = '-')
  url <- 'https://stats.nba.com/stats/leaguegamelog?'
  
  count <- 1
  response <- requests_nba(url, count, 5, Season = Season, ...)
  json <- jsonlite::fromJSON(httr::content(response, as = "text"))
  raw_data <- json$resultSets$rowSet[[1]]
  col_names <- json$resultSets$headers[[1]]
  nba_data <- data.frame(matrix(unlist(raw_data), ncol = length(col_names), byrow = FALSE))
  names(nba_data) <- col_names
  return(nba_data)
}

team_log <- get_leaguegamelog(season)

if (dir.exists('./data')){
  write.csv(team_log, paste0('./data/gamelog_', season, '.csv'), row.names = FALSE)
} else {
  dir.create('./data')
  write.csv(team_log, paste0('./data/gamelog_', season, '.csv'), row.names = FALSE)
}

player_log <- get_leaguegamelog(season, PlayerOrTeam='P')

if (dir.exists('./data')){
  write.csv(player_log, paste0('./data/playergamelog_', season, '.csv'), row.names = FALSE)
} else {
  dir.create('./data')
  write.csv(player_log, paste0('./data/playergamelog_', season, '.csv'), row.names = FALSE)
}