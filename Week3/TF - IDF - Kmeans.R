library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(factoextra)
library(Matrix)
library(wordcloud2)

# 詞頻矩陣
docs.corpus <- Corpus(DirSource("./Conan"))
docs.seg <- tm_map(docs.corpus, segmentCN)
docs.tdm <- TermDocumentMatrix(docs.seg)

# TF-IDF
docs.tf <- apply(as.matrix(docs.tdm), 2, function(word) { word/sum(word) })
idf <- function(doc) {
  return ( log2( length(doc)+1 / nnzero(doc)) )
}
docs.idf <- apply(as.matrix(docs.tdm), 1, idf)
docs.tfidf <- docs.tf * docs.idf

# 轉置
docs.tfidf_t<- t(docs.tfidf)

# 文字雲
f <- sort(rowSums(docs.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
wordcloud2(docs.df, minSize=10, gridSize=5, fontFamily = "微軟正黑體",
           backgroundColor='white', color='random-light')

# PCA & 繪圖 (R內建PCA, 可直接套用)
docs.pca <- prcomp(docs.tfidf_t, scale = T)

# Plot1
fviz_eig(docs.pca)
# Plot2
fviz_pca_ind(docs.pca, geom.ind = c("point"), col.ind = "cos2")
# Plot3
fviz_pca_var(docs.pca, col.var = "contrib",repel = TRUE)
# Plot 4
fviz_pca_biplot(docs.pca, geom.ind = "point")

# K means
# Plot1
ind.coord2 <- docs.ind$coord[, 1:2]
wss <- c()
for (i in 1:10) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")

# Plot2
km <- kmeans(ind.coord2, 3)
plot(ind.coord2, col = km$cluster)
points(km$centers, col = 1:3, pch = 8, cex = 2)