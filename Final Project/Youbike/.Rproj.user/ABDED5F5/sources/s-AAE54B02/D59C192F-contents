library(shiny)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(lubridate)

function(input, output) {
  # Data
  res <- read.csv('data/Youbike_res.csv')
  res3 <- read.csv('data/Youbike_res3.csv')
  res3_g <- gather(res3, time, per, X7:X24)
  res3_g$time <- str_replace(res3_g$time, 'X', '')
  
  # Location
  output$jitter <- renderPlot({
    # Map
    map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = 'toner-lite')
    locat.map <- ggmap(map, darken = c(0.5, "white")) %+% res + aes_string(x = "lng", y = "lat") +
      geom_jitter(col = 'red', shape = 16, alpha = 0.8) +
      labs(x = "Longitude", y = "Latitude") +
      coord_map() +
      ggtitle('Youbike Stations in Taipei')
    
    print(locat.map)
    
  }, height = 700)
  
  # Heat Map
  output$heat <- renderPlot({
    # Map
    map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = "toner-lite")
    
    res.stat.map <- ggmap(map) %+% res + aes_string(x = "lng", y = "lat", z = input$time) +
      stat_summary_2d(fun = median, alpha = 0.6) +
      scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      labs(x = "Longitude", y = "Latitude") +
      coord_map() +
      ggtitle('Remaining Percentage of Youbike in Taipei')
    
    print(res.stat.map)
    
  }, height = 800)
  
  # animation
  output$animation <- renderImage({
    filename <- normalizePath(file.path('./data/ani3.gif'))
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
  
}