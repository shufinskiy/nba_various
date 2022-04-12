`%>%` <- magrittr::`%>%`

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

east_conf <- c(1610612737, 1610612738, 1610612751, 1610612766, 1610612741, 1610612739, 1610612765, 1610612754,
               1610612748, 1610612749, 1610612752, 1610612753, 1610612755, 1610612761, 1610612764)

west_conf <- c(1610612742, 1610612743, 1610612744, 1610612745, 1610612746, 1610612747, 1610612763, 1610612750,
               1610612740, 1610612760, 1610612756, 1610612757, 1610612758, 1610612759, 1610612762)

table_color <- data.frame(TEAM_ID = c(1610612737, 1610612738, 1610612751, 1610612766, 1610612741, 1610612739, 1610612742,
                                                1610612743, 1610612765, 1610612744, 1610612745, 1610612754, 1610612746, 1610612747,
                                                1610612763, 1610612748, 1610612749, 1610612750, 1610612740, 1610612752, 1610612760,
                                                1610612753, 1610612755, 1610612756, 1610612757, 1610612758, 1610612759, 1610612761,
                                                1610612762, 1610612764),
                                    TEAM_NAME = c("Atlanta Hawks",          "Boston Celtics",         "Brooklyn Nets",         
                                                  "Charlotte Hornets",      "Chicago Bulls",          "Cleveland Cavaliers",   
                                                  "Dallas Mavericks",       "Denver Nuggets",         "Detroit Pistons",       
                                                  "Golden State Warriors",  "Houston Rockets",        "Indiana Pacers",        
                                                  "LA Clippers",            "Los Angeles Lakers",     "Memphis Grizzlies",     
                                                  "Miami Heat",             "Milwaukee Bucks",        "Minnesota Timberwolves",
                                                  "New Orleans Pelicans",   "New York Knicks",        "Oklahoma City Thunder", 
                                                  "Orlando Magic",          "Philadelphia 76ers",     "Phoenix Suns",          
                                                  "Portland Trail Blazers", "Sacramento Kings",       "San Antonio Spurs",     
                                                  "Toronto Raptors",        "Utah Jazz",              "Washington Wizards"),
                                    TEAM_ABBREVIATION = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL",
                                                          "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR",
                                                          "UTA", "WAS"),
                                    col1 = c("#E03A3E", "#007A33", "#000000", "#1D1160", "#CE1141", "#6F263D", "#00538C", "#0E2240",
                                             "#C8102E", "#006BB6", "#CE1141", "#002D62", "#C8102E", "#552583", "#5D76A9", "#98002E",
                                             "#00471B", "#0C2340", "#0C2340", "#006BB6", "#007AC1", "#0077C0", "#006BB6", "#1D1160",
                                             "#E03A3E", "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C"),
                                    name_col1 = c("HAWKS RED",           "CELTICS GREEN",       "BLACK",              
                                                  "HORNETS PURPLE",      "BULLS RED",           "CAVALIERS WINE",     
                                                  "ROYAL BLUE",          "MIDNIGHT BLUE",       "RED",                
                                                  "WARRIORS ROYAL BLUE", "RED",                 "PACERS BLUE",        
                                                  "RED",                 "LAKERS PURPLE",       "BLUE",               
                                                  "RED",                 "GOOD LAND GREEN",     "MIDNIGHT BLUE",      
                                                  "PELICANS NAVY",       "KNICKS BLUE",         "THUNDER BLUE",       
                                                  "MAGIC BLUE",          "BLUE",                "PURPLE",             
                                                  "RED",                 "PURPLE",              "SILVER",             
                                                  "RED",                 "NAVY",                "NAVY BLUE"),
                                    col2 = c("#C1D32F", "#BA9653", "#FFFFFF", "#00788C", "#000000", "#041E42", "#002B5E", "#FEC524",
                                             "#006BB6", "#FDB927", "#000000", "#FDBB30", "#1D428A", "#FDB927", "#12173F", "#F9A01B",
                                             "#EEE1C6", "#236192", "#C8102E", "#F58426", "#EF3B24", "#C4CED4", "#ED174C", "#E56020",
                                             "#000000", "#63727A", "#000000", "#000000", "#00471B", "#E31837"),
                                    name_col2 = c("VOLT GREEN",       "CELTICS GOLD",     "WHITE",            "TEAL",            
                                                  "BLACK",            "CAVALIERS NAVY",   "NAVY BLUE",        "SUNSHINE YELLOW", 
                                                  "ROYAL",            "GOLDEN YELLOW",    "BLACK",            "YELLOW",          
                                                  "BLUE",             "GOLD",             "NAVY",             "YELLOW",          
                                                  "CREAM CITY CREAM", "LAKE BLUE",        "PELICANS RED",     "KNICKS ORANGE",   
                                                  "SUNSET",           "SILVER",           "RED",              "ORANGE",          
                                                  "BLACK",            "GRAY",             "BLACK",            "BLACK",           
                                                  "GREEN",            "RED"),
                                    col3 = c("#26282A", "#963821", "#CD1041",        "#A1A1A4", NA,        "#FFB81C", "#B8C4CA", "#8B2131",
                                             "#BEC0C2", "#26282A", "#C4CED4", "#BEC0C2", "#BEC0C2", "#000000", "#F5B112", "#000000",
                                             "#0077C0", "#9EA2A2", "#85714D", "#BEC0C2", "#002D62", "#000000", "#002B5C", "#000000",
                                             NA,        "#000000", NA,        "#A1A1A4", "#F9A01B", "#C4CED4"),
                                    name_col3 = c("HAWKS CHARCOAL",   "CELTICS BROWN",    "RED",                 "GRAY",            
                                                  NA,                 "CAVALIERS NAVY",  "SILVER",           "FLATIRONS RED",   
                                                  "GRAY",             "SLATE",            "SILVER",           "SILVER",          
                                                  "SILVER",           "BLACK",            "YELLOW",           "BLACK",           
                                                  "GREAT LAKES BLUE", "MOONLIGHT GREY",   "PELICANS GOLD",    "KNICKS SILVER",   
                                                  "BLUE",             "BLACK",            "NAVY",             "BLACK",           
                                                  NA,                "BLACK",            NA,                 "SILVER",          
                                                  "YELLOW",           "SILVER" ),
                                    col4 = c( NA,        "#E59E6D", NA,        NA,        NA,        "#000000", "#000000", "#1D428A",
                                              "#002D62", NA,        NA,        NA,        "#000000", NA,        "#707271", NA,       
                                              "#000000", "#78BE20", NA,        "#000000", "#FDBB30", NA,        "#C4CED4", "#63727A",
                                              NA,        NA,        NA,        "#B4975A", NA,        NA   ),
                                    name_col4 = c( NA,                "CELTICS BEIGE",   NA,                NA,               
                                                   NA,                "CAVALIERS BLACK", "BLACK",           "SKYLINE BLUE",   
                                                   "NAVY",            NA,                NA,                NA,               
                                                   "BLACK",           NA,                "GRAY",            NA,              
                                                   "BLACK",           "AURORA GREEN",    NA,                "KNICKS BLACK",   
                                                   "YELLOW",          NA,                "SILVER",          "GRAY",           
                                                   NA,                NA,                NA,                "GOLD",           
                                                   NA,                NA ),
                                    col5 = c(NA,        "#000000", NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#F9AD1B",
                                             NA,        NA,        NA,        NA,        NA,        NA   ),
                                    name_col5 = c(NA,              "CELTICS BLACK", NA,              NA,              NA,             
                                                  NA,              NA,              NA,              NA,              NA,             
                                                  NA,              NA,              NA,              NA,              NA,             
                                                  NA,              NA,              NA,              NA,              NA,             
                                                  NA,              NA,              NA,              "YELLOW",        NA,             
                                                  NA,             NA,              NA,              NA,              NA  ),
                                    col6 = c(NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,       NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#B95915",
                                             NA,        NA,       NA,        NA,        NA,        NA    ),
                                    name_col6 = c(NA,            NA,            NA,            NA,            NA,            NA,           
                                                  NA,            NA,            NA,            NA,            NA,            NA,           
                                                  NA,            NA,            NA,            NA,            NA,            NA,           
                                                  NA,            NA,            NA,            NA,            NA,            "DARK ORANGE",
                                                  NA,            NA,            NA,            NA,            NA,            NA ),
                                    col7 = c(NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                             NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#BEC0C2",
                                             NA,        NA,        NA,        NA,        NA,        NA ),
                                    name_col7 = c(NA,           NA,           NA,           NA,           NA,           NA,          
                                                  NA,           NA,           NA,           NA,           NA,           NA,          
                                                  NA,           NA,           NA,           NA,           NA,           NA,          
                                                  NA,           NA,           NA,           NA,           NA,           "LIGHT GRAY",
                                                  NA,           NA,           NA,           NA,           NA,           NA))

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

requests_nba <- function(url, count, n_rep, ...){
  argg <- c(as.list(environment()), list(...))
  param_nba <- get_endpoints(url)
  param_nba[intersect(names(argg), names(param_nba))] <- argg[intersect(names(argg), names(param_nba))]
  
  res <- trycatch_nbastats(url, 10, nba_request_headers, param_nba, count, n_rep, ...)
  return(res)
}

trycatch_nbastats <- function(url, t, nba_request_headers, param_nba, count, n_rep, ...){
  tryCatch({httr::GET(url = url, httr::timeout(t), httr::add_headers(nba_request_headers), query = param_nba)},
           error = function(e){
             if (count <= n_rep){
               message(paste0('Ответ от nbastats не получен, повторный запрос. Количество оставшихся попыток: ', n_rep - count))
               Sys.sleep(2)
               return(requests_nba(url, count + 1, n_rep, ...))
             } else{
               stop('Ответ от nbastats не получен за ', n_rep, ' попыток запроса')
             }
           })
}

get_endpoints <- function(url){
  return(nba_params[[re_type_stats(url)]])
}

re_type_stats <- function(url){
  return(stringr::str_extract(url, '(?<=\\/)[:alnum:]+(?=\\?)'))
}
