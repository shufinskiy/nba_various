library(data.table, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

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
agg_loc_x_mean <- agg_loc_x_mean[, SIDE := data.table::fifelse(LOC_X < 0, 'R', 'L')][, .(.N), by=c("SEASON", "SIDE")]

### Построение barplot графика
ggplot2::ggplot(agg_loc_x_mean, aes(SEASON, N, fill=SIDE)) +
  ggplot2::geom_col(position = "dodge2")