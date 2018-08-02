library(ggmap)
library(ggplot2)
library(gganimate)
library(viridis)
library(RColorBrewer)
library(raster)
library(tidyr)
library(magick)

res <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res.csv')
sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi.csv')

# Map
map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = "toner-lite")

# Map + Data
res.stat.map <- ggmap(map) %+% res + aes(x = lng, y = lat, z = X307) +
  stat_summary_2d(fun = median, alpha = 0.6) +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(x = "Longitude", y = "Latitude") +
  coord_map() +
  ggtitle('Remaining Amount of Youbike in Taipei')

print(res.stat.map)

res_g <- gather(res, time, per, X307:X318)

# Animation
res.stat.map.ani <- ggmap(map) %+% res + aes(x = lng, y = lat, z = X307, frame = time) +
  stat_summary_2d(fun = median, alpha = 0.6) +
  scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
  labs(x = "Longitude", y = "Latitude") +
  coord_map()

gg_animate(res.stat.map.ani, interval = 1.0, filename = 'vocab.gif')
