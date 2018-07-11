library(ggplot2)
#Sepal萼片，Petal花瓣，Species
head(iris)

library(dplyr)
ggplot(iris, aes(x = Species)) +
  geom_bar()

ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Species)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Species)

ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Species)

ggplot(iris, aes(x = Petal.Width)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Species)

  geom_point()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point()

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot()

iris1 = iris %>%
  mutate(Sepal.Size = Sepal.Length * Sepal.Width, Petal.Size = Petal.Length * Petal.Width)
head(iris1)

ggplot(iris1, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Size)) +
  geom_point()

ggplot(iris1, aes(x = Petal.Length, y = Petal.Width, color = Species, size = Sepal.Size)) +
  geom_point()

if(!require(ggmap))install.packages("ggmap",repos = "http://cran.us.r-project.org")
if(!require(readr))install.packages("readr",repos = "http://cran.us.r-project.org")
library(ggmap)
library(readr)
map <- get_map(location = c(lon = 121.5521599, lat = 25.0592136),
               zoom = 12, language = "zh-TW", maptype = "toner-lite")

WGS <- read_csv('C:/Users/dx788/Documents/WGS.csv')
ggmap(map) + geom_point(aes(x = X_WGS, y = Y_WGS, color = 'red'), data = WGS)