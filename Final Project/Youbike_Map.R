library(ggmap)
library(ggplot2)
library(gganimate)
library(viridis)
library(RColorBrewer)
library(raster)
library(tidyr)
library(tweenr)
library(magick)
library(stringr)
library(lubridate)
library(rgdal)
library(ggfortify)

res <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res.csv')
sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi.csv')
taipei <- readOGR(dsn = 'D:/GitHub/NTU-CS-X/Final Project/Youbike/data/taipei_town', layer = 'Taipei_town')
taipei <- fortify(taipei)
# Map
map <- get_map(location = c(min(taipei$long), min(taipei$lat), max(taipei$long), max(taipei$lat)), maptype = "toner")

# Map + Data
res.stat.map <- ggmap(map, darken = c(0.8, "white")) +
  stat_summary_2d(data = res, aes(x = lng, y = lat, z = X308), fun = median, alpha = 0.6) +
  geom_polygon(data = taipei,
               aes(x = long, y = lat, group = group),
               fill = NA, col = "black") +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(x = "Longitude", y = "Latitude") +
  coord_map() +
  ggtitle('Remaining Amount of Youbike in Taipei')

print(res.stat.map)


ggmap(map, darken = c(0.7, "white")) +
  geom_polygon(data = taipei,
               aes(x = long, y = lat, group = group),
               fill = NA, col = "#000000", width = 0.1)

# res
res3 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res2.csv')
res3_g <- gather(res3, time, per, X7:X24)
res3_g$time <- str_replace(res3_g$time, 'X', '')

# Animation
res.stat.map.ani <- ggmap(map, darken = c(0.6, "white")) %+% res3_g + aes(x = lng, y = lat, z = per) +
  stat_summary_2d(fun = median, alpha = 0.8) + 
  geom_polygon(data = taipei,
               aes(x = long, y = lat, group = group),
               fill = NA, col = "#666666",
               inherit.aes = FALSE) +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(title = 'Sunday, {closest_state}', x = "Longitude", y = "Latitude") +
  coord_map() +
  transition_states(time, transition_length = 1, state_length = 1) 

print(res.stat.map.ani)

# Location
ggmap(map, darken = c(0.5, "white")) %+% res + aes(x = lng, y = lat) +
  geom_jitter()