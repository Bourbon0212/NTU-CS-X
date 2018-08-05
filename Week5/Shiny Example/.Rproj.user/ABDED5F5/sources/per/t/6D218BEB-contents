library(shiny)
library(ggmap)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(raster)

function(input, output) {

  # Data
  res <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res.csv')
  res_g <- gather(res, time, per, 6:ncol(res))
  sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi.csv')
  sbi_g <- gather(sbi, time, quan, 6:ncol(sbi))
  
  res1 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res3.csv') # CSV
  colnames(res1)[6:23] <- c(7:24) 
  res1_g <- gather(res1, time, per, '7':'24')
  
  res2 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res3.csv') # CSV
  colnames(res2)[6:23] <- c(7:24)
  res2_g <- gather(res2, time, per, '7':'24')
  
  res3 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res3.csv')
  colnames(res3)[6:23] <- c(7:24)
  res3_g <- gather(res3, time, per, '7':'24')
  
  res4 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res4.csv')
  colnames(res4)[6:23] <- c(7:24)
  res4_g <- gather(res4, time, per, '7':'24')
  
  res5 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res5.csv')
  colnames(res5)[6:23] <- c(7:24)
  res5_g <- gather(res5, time, per, '7':'24')
  
  res6 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res3.csv') # CSV
  colnames(res6)[6:23] <- c(7:24)
  res6_g <- gather(res6, time, per, '7':'24')
  
  res7 <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_res3.csv') # CSV
  colnames(res7)[6:23] <- c(7:24)
  res7_g <- gather(res7, time, per, '7':'24')
  
  dataset <- list(res1, res2, res3, res4, res5, res6, res7)
  dataset_g <- list(res1_g, res2_g, res3_g, res4_g, res5_g, res6_g, res7_g)
  rawdata <- list(res_g, sbi_g)
  
  # Reactive data inout
  data_day1 <- reactive({
    req(input$day)
    temp <- data.frame(dataset[[as.numeric(input$day1)]])
  })
  
  
  output$plot <- renderPlot({
    
    # Map
    map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = "toner-lite")
    
    res.stat.map <- ggmap(map, darken = c(0.5, "white")) %+% res + aes_string(x = "lng", y = "lat", z = input$time) +
      stat_summary_2d(fun = median, alpha = 0.6) +
      scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), space = 'Lab') +
      labs(x = "Longitude", y = "Latitude") +
      coord_map() +
      ggtitle('Remaining Amount of Youbike in Taipei')
    
    res.stat.map
    
  }, height=700)
  
  output$x <- renderPrint({
    print(input$time)
  })
  
}