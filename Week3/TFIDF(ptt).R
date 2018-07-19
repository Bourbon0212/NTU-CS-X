library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)

# PTT 網路爬蟲抓出所有文章內文所對應的網址
from <- 2938 
to   <- 2943 
prefix = "https://www.ptt.cc/bbs/PC_Shopping/index"

data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)

head(data)

# 利用所有文章的網址去抓所有文章內文, 並解析出文章的內容並依照 hour 合併儲存。
library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  name <- paste0('./Data/', hour, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)
