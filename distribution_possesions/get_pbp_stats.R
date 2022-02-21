library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(httr)

pbpstats_request_headers = c(
  "user-agent" = "Mozilla/5.0"
)

param_poss <- list(
  Season = '2021-22',
  SeasonType = I('Regular%2BSeason')
)

url <- 'https://api.pbpstats.com/get-games/nba'

response <- GET(url, add_headers(pbpstats_request_headers), query=param_poss, config = config(ssl_verifypeer = FALSE))
json <- fromJSON(content(response, as = "text", encoding = 'UTF-8'))

pbp_data <- json[['results']]

write.csv(pbp_data, './data/2021.csv', row.names = FALSE)
