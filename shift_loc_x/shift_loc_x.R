library(data.table, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggthemes)
library(gt)
library(gtExtras)

### Загрузка данных по броскам с сезона 2016/17 по сезон 2021/22
list_files <- list.files("../../experimet_dff_shot/datasets", pattern = "shotdetail", full.names = TRUE)

df <- data.table::rbindlist(lapply(list_files, data.table::fread))

### УДаление столбцов, которые не будут использоваться в исследовании
shots <- df[, .(GAME_ID, PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_NAME, LOC_X)]
rm(df)
### Преобразование столбца GAME_ID в столбец SEASON, LOC_X преобразуем в футы
data.table::setnames(shots, "GAME_ID", "SEASON")
shots$SEASON <- paste0("20", substr(shots$SEASON, 2, 3), "-", as.character(as.integer(substr(shots$SEASON, 2, 3)) + 1))
shots$LOC_X <- shots$LOC_X / 10

### Расчёт среднего LOC_X по командам и сезонам
agg_loc_x_mean <- shots[, .(LOC_X = mean(LOC_X)), by=c("SEASON", "TEAM_ID", "TEAM_NAME")]

### Добавляем фактор SIDE: если LOC_X < 0, то R, иначе L
agg_loc_x_side <- agg_loc_x_mean[, SIDE := data.table::fifelse(LOC_X < 0, 'R', 'L')][, .(.N), by=c("SEASON", "SIDE")]

### Построение barplot графика
ggplot2::ggplot(agg_loc_x_side, aes(SEASON, N, fill=SIDE)) +
  ggplot2::geom_col(position = "dodge2") +
  ggthemes::theme_fivethirtyeight()

### Количество side сезонов у каждой из команд
cnt_side_season <- data.table::dcast(agg_loc_x_mean[, .(.N), by=c("TEAM_ID", "TEAM_NAME", "SIDE")], TEAM_ID + TEAM_NAME ~ SIDE, fill = 0, value.var = "N")
cnt_side_season %>% 
  gt::gt() %>% 
  gt::text_transform(
    locations = gt::cells_body(TEAM_ID),
    fn = function(team_id){
      gt::local_image(
        filename = paste0("/home/shuf91/R/projects/nbalogoandcolor/png/", team_id, ".png"),
        height = 30
      )
    }
  )

### Таблица сезона 2021/2022
agg_loc_x_mean[SEASON == "2021-22", .(TEAM_ID, TEAM_NAME, LOC_X)][order(LOC_X)] %>% 
  gt::gt() %>% 
  gt::text_transform(
    locations = gt::cells_body(TEAM_ID),
    fn = function(team_id){
      gt::local_image(
        filename = paste0("/home/shuf91/R/projects/nbalogoandcolor/png/", team_id, ".png"),
        height = 30
      )
    }
  )

### Построение графика влияния на LOC_X для одной команды по убыванию количества бросков
coord_x_by_players <- function(data, team_id, clip=4.5){
  dt <- data[SEASON == "2021-22" & TEAM_ID == team_id][, .(CNT_SHOTS = .N, LOC_X_MEAN = mean(LOC_X)), by=c("PLAYER_ID", "PLAYER_NAME")][
    order(CNT_SHOTS, decreasing = TRUE)]
  
  # for(i in seq(1, dim(okc)[1])){
  #   okc$ROLL_LOC_X[i] <- weighted.mean(okc$LOC_X_MEAN[1:i], okc$CNT_SHOTS[1:i])
  # }
  # 
  # ggplot2::ggplot(okc, aes(forcats::fct_reorder(factor(PLAYER_NAME), CNT_SHOTS), ROLL_LOC_X, group=1)) +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_line() +
  #   ggplot2::coord_flip()
  
  dt$LOC_X_CLIP <- pmax(pmin(dt$LOC_X_MEAN, 4.5), -4.5)
  
  gg <- ggplot2::ggplot(dt, aes(forcats::fct_reorder(factor(PLAYER_NAME), CNT_SHOTS), LOC_X_CLIP, fill=CNT_SHOTS)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip(ylim = c(-5, 5)) +
    ggplot2::geom_text(aes(label = round(LOC_X_MEAN, 2)), hjust=ifelse(dt$LOC_X_MEAN > 0, -0.15, 1.1))
  return(gg)
}

okc <- coord_x_by_players(shots, 1610612760)
min <- coord_x_by_players(shots, 1610612750)
phx <- coord_x_by_players(shots, 1610612756)
bkn <- coord_x_by_players(shots, 1610612751)
uth <- coord_x_by_players(shots, 1610612762)