library(shiny)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(tools)

# Data
res <- read.csv('data/Youbike_res.csv')

navbarPage(
  # Theme
  theme = shinytheme('flatly'),
  'Youbike Analysis',
  
  tabPanel(
    'Raw Data',
   
    sidebarPanel(
      h2('Data'),
      br(),
      selectInput(inputId = 'rawdata',
                  label = 'Select Data',
                  choices = c('Percentages' = '1',
                              'Quantities' = '2')),
      br(),
      downloadButton(outputId = "download_maindata", label = "Download .CSV")
      
      
    ),
    
    mainPanel(
      # Data Table Output
      DT::dataTableOutput(outputId = "maindata")
    )
  ),
  tabPanel(
    '2D Heatmap',
    
    sidebarPanel(
      h2('Heat Map'),
      hr(),
      # Day Input
      selectInput(inputId = 'day1',
                  label = 'Day',
                  choices = c('Monday' = '1',
                              'Tuesday' = '2',
                              'Wednesday' = '3',
                              'Thursday' = '4',
                              'Friday' = '5',
                              'Saturday' = '6',
                              'Sunday' = '7'),
                  selected = 'Wednesday'),
      
      hr(),
      # Time Input
      selectInput(inputId = 'time1',
                  label = 'Time',
                  choices = c('07:00' = 'X7',
                              '08:00' = 'X8',
                              '09:00' = 'X9',
                              '10:00' = 'X10',
                              '11:00' = 'X11',
                              '12:00' = 'X12',
                              '13:00' = 'X13',
                              '14:00' = 'X14',
                              '15:00' = 'X15',
                              '16:00' = 'X16',
                              '17:00' = 'X17',
                              '18:00' = 'X18',
                              '19:00' = 'X19',
                              '20:00' = 'X20',
                              '21:00' = 'X21',
                              '22:00' = 'X22',
                              '23:00' = 'X23',
                              '24:00' = 'X24')
                   )
    ),

    mainPanel(
      tabsetPanel(type = 'tabs',
                  # Tab 1: Plot
                  tabPanel(title = 'Map',
                           br(),
                           tags$p('Please wait, it takes a few seconds to load the map.'),
                           tags$p('You can point at specific Youbike station for more information.'),
                           plotOutput('heat', hover = "plot_hover"),
                           br(),
                           br(),
                           br(),
                           hr(),
                           dataTableOutput(outputId = "locat")
                           ),
                  # Tab 2 : Animation
                  tabPanel(title = 'Animation',
                           br(),
                           tags$p('Please wait, it takes a few seconds to load the map.'),
                           tags$p('The speed depends on your network connection.'),
                           plotOutput('animation'),
                           br(),
                           br(),
                           br(),
                           br(),
                           h5("Built with",
                              img(src = "https://raw.githubusercontent.com/thomasp85/gganimate/master/man/figures/logo.png", height = "30px"),
                              ".")),
                  # Tab 3 : Bar Chart
                  tabPanel(title = 'Bar Chart'),
                  # Tab 4 : Data
                  tabPanel(title = 'Data',
                           br(),
                           br(),
                           DT::dataTableOutput(outputId = "data"),
                           br(),
                           downloadButton(outputId = "download_data", label = "Download .CSV")))
      
    )
  ))
