library(data.table)
library(ggplot2)
library(ggimage)
library(ggtext)

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

get_nba_data(seasons = 2022, data = "shotdetail", untar = TRUE)

df <- fread("shotdetail_2022.csv")

df[, `:=`(SHOT_VALUE = fifelse(SHOT_TYPE == "3PT Field Goal", "PT3", "PT2"))]

df1 <- dcast(df[, (mean(SHOT_DISTANCE)), by=.(TEAM_ID, SHOT_VALUE)], TEAM_ID ~ SHOT_VALUE, value.var = "V1")
df1$IMAGE <- paste0("../salary/logo/", df1$TEAM_ID, ".png")

ggplot(df1, aes(PT2, PT3)) +
  geom_point() +
  geom_image(aes(PT2, PT3, image=IMAGE)) +
  scale_x_continuous(breaks = seq(5.2, 7.4, 0.2)) +
  scale_y_continuous(breaks = seq(24.6, 25.8, 0.2)) +
  labs(title = '**Average shot distance for 2PT and 3PT attempts by teams in 2022/23**', 
       caption = "Data: **stats.nba.com**; Twitter: **@vshufinskiy**",
       x = "Average distance (ft.) for 2PT attempts",
       y = "Average distance (ft.) for 3PT attempts") + 
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(color="#f9f9f9", fill="#f9f9f9"),
    axis.ticks = element_blank(),
    plot.title = element_textbox_simple(
      size = 14, lineheight = 1,
      linetype = 1,
      halign=0.5,
      box.color = "#748696",
      fill = "#F0F7FF",
      r = grid::unit(3, "pt"),
      padding = margin(5, 5, 5, 5),
      margin = margin(0, 0, 10, 0)
    ),
    plot.caption = element_markdown(),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )