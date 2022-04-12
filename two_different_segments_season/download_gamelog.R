source('download_utils.R')

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

t <- get_leaguegamelog(2021)

if (dir.exists('./data')){
  write.csv(t, './data/gamelog.csv', row.names = FALSE)
} else {
  dir.create('./data')
  write.csv(t, './data/gamelog.csv', row.names = FALSE)
}

t <- get_leaguegamelog(2021, PlayerOrTeam='P')

if (dir.exists('./data')){
  write.csv(t, './data/playergamelog.csv', row.names = FALSE)
} else {
  dir.create('./data')
  write.csv(t, './data/playergamelog.csv', row.names = FALSE)
}