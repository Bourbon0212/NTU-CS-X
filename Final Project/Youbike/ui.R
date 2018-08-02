library(shiny)
library(ggmap)
library(ggplot2)
library(gganimate)
library(tweenr)
library(magick)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(lubridate)

# Data
res <- read.csv('data/Youbike_res.csv')

navbarPage(
  'Youbike Analysis',
  tabPanel(
    'Introduction',
    tags$h1("Welcome to Youbike Analysis!"),
    tags$p("It takes a few seconds loading the map."),
    br(),
    mainPanel(
      plotOutput('animation')
    )
  ),
  tabPanel(
    '2D Heatmap',
    tags$p("It takes a few seconds loading the map."),
    sidebarPanel(  
      selectInput('time', 'Time', c(names(res[6:ncol(res)])))
    ),

    mainPanel(

      plotOutput('heat')
      
    )
  ))
