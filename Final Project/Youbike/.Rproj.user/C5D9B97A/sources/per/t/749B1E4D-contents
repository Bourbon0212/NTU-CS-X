library(shiny)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(tools)
library(vcd)
library(rgdal)
library(ggfortify)

# Data
res <- read.csv('data/Youbike_res.csv')

# Line Plot Data
sbi <- read.csv('data/Youbike_sbi(1).csv')
sbi_line <- sbi %>% gather( Time, value, X2018.8.1.7:X2018.8.7.24, na.rm = TRUE)
sbi_line <- select(sbi_line,sarea,Time,value)
sbi_line <- sbi_line %>% group_by(sarea, Time) %>% summarise(mean = mean(value), sum = sum(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line <- separate(sbi_line, Time, c("day", "hour"),sep = " ",remove = FALSE)

all_regions <- sort(unique(sbi_line$sarea))

navbarPage(
  # Theme
  theme = shinytheme('flatly'),
  'Youbike Analysis',
  tabPanel(
    'Introduction',
    
    sidebarPanel(
      h2('Introduction'),
      hr(),
      wellPanel(
        tags$article('在台北的街頭風景之中，穿梭於街道之上，亮黃色的Ubike已成為台北印象不可或缺的一部分。以gogoro的熱度地圖為發想點，利用政府公開資料平台、Python與SQlite，我們取得了8/1~8/7台北市7點到24點內每個小時Ubike站點狀況的資料，使用r語言做分析，其中包括了使用日期與地區作為分類的依據，進而將資料整理成折線圖、馬賽克式圖表並畫成熱度地圖，依照時間順序製作動圖來呈現Ubike車數在時間軸上的變化。'),
        hr(),
        tags$article('在分析資料的過程中，我們不只觀察到各區站點車輛數的變化狀況與差異，也藉由Ubike這樣的通勤工具來一窺台北短時間內的人潮脈動與活動熱區。這份資料科學的研究，不僅鍛鍊了我們coding與資料處理的能力，更使我們體會了如何運用自己所能將理想藍圖化為真實的心路歷程與成就感。'))
    ),
    
    mainPanel(
      HTML('<iframe width="960" height="540" src="https://www.youtube.com/embed/wMW9yLWHTnk" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
    )
  ),
  
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
                              'Sunday' = '7')),
      
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
                           tags$p('You can point at specific Youbike station for more information in the below.'),
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
                  # Tab 3 : Summary
                  tabPanel(title = 'Summary',
                           br(),
                           plotOutput('barchart')
                           ),
                  # Tab 4 : Data
                  tabPanel(title = 'Data',
                           br(),
                           br(),
                           DT::dataTableOutput(outputId = "data"),
                           br(),
                           downloadButton(outputId = "download_data", label = "Download .CSV")))
      
    )
  ),
  tabPanel(
    'Mosaic Plots',
    
    sidebarPanel(
      h2('Chi-Squared Test'),
      hr(),
      selectInput(inputId = 'day2',
                  label = 'Day',
                  choices = c('Monday' = '1',
                              'Tuesday' = '2',
                              'Wednesday' = '3',
                              'Thursday' = '4',
                              'Friday' = '5',
                              'Saturday' = '6',
                              'Sunday' = '7'))
    ),
    
    mainPanel(
      tabsetPanel(type = 'tabs',
                  # Tab 1: Mosaic
                  tabPanel(title = 'Mosaic',
                           br(),
                           tags$p('The higher the residual, the more overrepresented a segment is.'),
                           tags$p('The lower the residual, the more underrepresented a segment is.'),
                           tags$p('The size of each rectangle represents the quantity of bikes then.'),
                           plotOutput('mosaic')),
                  
                  # Tab 2: Residuals
                  tabPanel(title = 'Residual',
                           br(),
                           DT::dataTableOutput(outputId = "residual")),
                  
                  # Tab 3: Expected
                  tabPanel(title = 'Expected',
                           br(),
                           DT::dataTableOutput(outputId = "expected")),
                  
                  # Tab 4: Observation
                  tabPanel(title = 'Observation',
                           br(),
                           DT::dataTableOutput(outputId = "observation")))
    )
  ),
  tabPanel(
    'Line Plots',
           sidebarPanel(
             h2('Line Plots'),
             hr(),
             h5('Pick dates between 2018-08-01 and 2018-08-07.'),
             dateRangeInput(inputId = "Time",
                            label = "Time:",
                            start = "2018-08-01",
                            end = "2018-08-04",
                            min = "2018-08-01", max = "2018-08-07",
                            startview="date"),
             
             selectInput(inputId = "sarea",
                         label = "Region:",
                         choices = all_regions,
                         multiple = T,
                         selected = '大安區')
           ),
           
           mainPanel(
             tabsetPanel(type = 'tabs',
                         
                         # Tab 1: Mean
                         tabPanel(title = 'Mean',
                                  br(),
                                  plotOutput(outputId = "line_mean")),
                         # Tab 2: Sum
                         tabPanel(title = 'Sum',
                                  br(),
                                  plotOutput(outputId = 'line_sum'))
                         )
           )
           ),
  
  tabPanel('Conclusion',
           sidebarPanel(h2('Conclusion'),
                        width = 4,
                        
                        wellPanel(
                          h3('大安區'),
                          tags$article('平常日每天大致上有三次的波動。第一次波動在早上上班時間，而到了下午三點左右，車輛數有一波增加趨勢，隨後又減少，接著在下班時間，約七點左右，又有一波明顯的增加，並在九點之後又減少。到了假日，雖然在早上仍有車輛減少的情況，但也許是因為假日休息的緣故，落差不如平常日巨大。')
                        ),
                        wellPanel(
                          h3('士林區、中山區、信義區、松山區'),
                          tags$article('平常日從早上開始呈現下降的趨勢，然而到了大約七點左右的下班時間，車輛數目會急遽下降，顯示此區的人們使用ubike的活動增加，或者是人潮開始向外移動。到了假日，早上呈現的不再是緩降的趨勢，反而是一直保持在相對高點，並有些起伏。')
                        ),
                        wellPanel(
                          h3('大同區、中正區'),
                          tags$article('假日跟平常日的趨勢比較相像。在早上接近中午的時候會有一小段時間腳踏車數特別短暫快速下降，之後又回升。從早上開始站點大致上呈現車輛數下降的趨勢，尤其到了下班時間，數目急遽下降。')
                        ),
                        wellPanel(
                          h3('內湖區、南港區'),
                          tags$article('假日與工作日ubike平均數落差明顯。顯示此區是屬於上班族的領域，平常通勤頻繁，使用率高，到了假日使用熱度便會冷卻。')
                        ),
                        wellPanel(
                          h3('文山區'),
                          tags$article('可以看出工作日的使用熱度有明顯別於假日。在工作日中，每天接近中午的車輛數有下降再回升的情形。在假日中，白天使用ubike的熱度較低，因此車輛數維持在高點，導致與晚上的落差非常巨大。')
                        ),
                        wellPanel(
                          h3('北投區、萬華區'),
                          tags$article('相較其他區變化比較規律，推測使用率不高造成。直到中午才出現第一次回彈，並在下午時段車輛數也有一次小小下降後回升的狀況。')
                        ),
                        wellPanel(
                          h3('Overall'),
                          tags$article('總體來說，台北市的ubike使用狀況大致上是從早是開始趨於頻繁，有些地區在中午時也會出現一波ubike使用潮，並且在晚上時出現車輛數大量下滑的現象，並且很快的在午夜之前回升。推測可能是下班時間使用率快速增加，或是官方單位進行調度或維修造成。')
                        )
           ),
           mainPanel(width = 8,
                     tabsetPanel(type = 'tabs',
                                 
                                 
                                 #Tab 2: Weekdays
                                 tabPanel(title = 'Weekdays',
                                          br(),
                                          plotOutput('con_weekdays')
                                          ),
                                 #Tab 1: Weekends
                                 tabPanel(title = 'Weekends',
                                          br(),
                                          plotOutput('con_weekends')
                                          )
                     ))
  ),
  
  tabPanel(
    'Reference',
    wellPanel(
      h2('Reference'),
      hr(),
      h3('Members'),
      tags$p('廖昱嘉、甘佳昀、龔泓愷'),
      hr(),
      h3('Data'),
      tags$p('Youbike: http://data.taipei/youbike'),
      tags$p('Gogoro Heatmap: https://mowd.tw/gostation/map/battery/'),
      hr(),
      h3('Tutorial'),
      tags$p('Building Web Applications in R with Shiny'),
      tags$p('https://www.datacamp.com/courses/building-web-applications-in-r-with-shiny'),
      br(),
      tags$p('Data Visualization with ggplot2'),
      tags$p('https://www.datacamp.com/courses/data-visualization-with-ggplot2-1'),
      tags$p('https://www.datacamp.com/courses/data-visualization-with-ggplot2-2'),
      tags$p('https://www.datacamp.com/courses/data-visualization-with-ggplot2-3'),
      hr(),
      h3('Tools'),
      tags$p("Built with",
             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
             "by",
             img(src = 'https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png', height = '30px'),
             ".")
    )
  )
  
  )
