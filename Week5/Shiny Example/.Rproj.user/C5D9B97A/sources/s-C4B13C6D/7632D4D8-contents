library(shiny)
library(ggplot2)

# Data
res <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike_res.csv')


fluidPage(
  
  titlePanel("Youbike Test"),
  
  sidebarPanel(  
    selectInput('time', 'Time', c('None', names(res[6:ncol(res)])))
  ),
  
  mainPanel(
    verbatimTextOutput("x"),
    plotOutput('plot')
  )
)