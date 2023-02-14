get_nba_data <- function(seasons = seq(1996, 2021), data = c("datanba", "nbastats", "pbpstats", "shotdetail"), untar = FALSE){
  df <- expand.grid(data, seasons)
  
  need_data <- paste(df$Var1, df$Var2, sep = "_")
  
  temp <- tempfile()
  download.file("https://raw.githubusercontent.com/shufinskiy/nba_data/main/list_data.txt", temp)
  f <- readLines(temp)
  unlink(temp)
  
  v <- unlist(strsplit(f, "="))
  
  name_v <- v[seq(1, length(v), 2)]
  element_v <- v[seq(2, length(v), 2)]
  
  need_name <- name_v[which(name_v %in% need_data)]
  need_element <- element_v[which(name_v %in% need_data)]
  
  for(i in seq_along(need_element)){
    destfile <- paste0(need_name[i], ".tar.xz")
    download.file(need_element[i], destfile = destfile)
    if(untar){
      untar(destfile, paste0(need_name[i], ".csv"))
      unlink(destfile)
    }
  }  
}

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