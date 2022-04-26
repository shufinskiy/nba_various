nba_request_headers = c(
  "Connection"= 'keep-alive',
  "Accept"= 'application/json, text/plain, */*',
  "x-nba-stats-token"= 'true',
  "X-NewRelic-ID"= 'VQECWF5UChAHUlNTBwgBVw==',
  "User-Agent"= 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.116 Safari/537.36',
  "x-nba-stats-origin"= 'stats',
  "Sec-Fetch-Site"= 'same-origin',
  "Sec-Fetch-Mode"= 'cors',
  "Referer"= 'https=//stats.nba.com',
  "Accept-Encoding"= 'gzip, deflate, br',
  "Accept-Language"= 'ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7'
)

team_dict <- list(
  'ATL' = 1610612737,
  'BOS' = 1610612738,
  'BKN' = 1610612751,
  'CHA' = 1610612766,
  'CHI' = 1610612741,
  'CLE' = 1610612739,
  'DAL' = 1610612742,
  'DEN' = 1610612743,
  'DET' = 1610612765,
  'GSW' = 1610612744,
  'HOU' = 1610612745,
  'IND' = 1610612754,
  'LAC' = 1610612746,
  'LAL' = 1610612747,
  'MEM' = 1610612763,
  'MIA' = 1610612748,
  'MIL' = 1610612749,
  'MIN' = 1610612750,
  'NOP' = 1610612740,
  'NYK' = 1610612752,
  'OKC' = 1610612760,
  'ORL' = 1610612753,
  'PHI' = 1610612755,
  'PHX' = 1610612756,
  'POR' = 1610612757,
  'SAC' = 1610612758,
  'SAS' = 1610612759,
  'TOR' = 1610612761,
  'UTH' = 1610612762,
  'WAS' = 1610612764
)

nba_params <- list(
  leaguegamelog = list(
    Counter = '1000',
    Direction = 'DESC',
    LeagueID = '00',
    PlayerOrTeam = 'T',
    Season = '2020-21',
    SeasonType = I('Regular+Season'),
    Sorter = 'DATE',
    DateTo = '',
    DateFrom = ''
  )
)

### Function for download nbadata
re_type_stats <- function(url){
  return(str_extract(url, '(?<=\\/)[:alnum:]+(?=\\?)'))
}

get_endpoints <- function(url){
  return(nba_params[[re_type_stats(url)]])
}

requests_nba <- function(url, count, n_rep, ...){
  arg <- c(as.list(environment()), list(...))
  param_nba <- get_endpoints(url)
  param_nba[intersect(names(arg), names(param_nba))] <- arg[intersect(names(arg), names(param_nba))]
  res <- trycatch_nbastats(url, 10, nba_request_headers, param_nba, count, n_rep, ...)
  return(res)
}

trycatch_nbastats <- function(url, t, nba_request_headers, param_nba, count, n_rep, ...){
  
  tryCatch({res <- GET(url = url, timeout(t), add_headers(nba_request_headers), query = param_nba)
  if(res$status_code != 200 | res$url == 'https://www.nba.com/stats/error/') {stop()}; return(res)},
  error = function(e){
    if (exists('res')){
      mes <- ' Response status is not equal to 200. Number of remaining attempts: '
    } else {
      mes <- ' No response was received from nbastats, a repeat request. Number of remaining attempts: '
    }
    if (count < n_rep){
      message(paste0(Sys.time(), mes, n_rep - count))
      Sys.sleep(2)
      return(requests_nba(url, count + 1, n_rep, ...))
    } else{
      stop(Sys.time(), ' No response was received from nbastats for ', n_rep, ' request attempts')
    }
  })
}

get_leaguegamelog <- function(Season=2020, ...){
  Season <- paste(Season, str_pad(as.numeric(substr(Season, 3, 4)) + 1, 2, side = "left", pad = "0"), sep = '-')
  if(Season == "1999-100"){
    Season <- "1999-00"
  }
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

