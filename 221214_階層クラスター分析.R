
library(dendextend)
library(ggplot2)
library(tidyverse)

# 階層クラスタリング --------------------------------------------------------------

iris

data <- iris[, 1:4]
head(data)

str(data)
nrow(data)
summary(iris$Species)

# 非類似度(距離)の計算
# dist(data, method = "euclidean", diag = FALSE, upper = FALSE)
distance <- dist(data)

# 樹形図作成
hc <- hclust(distance, "ward.D2")

# デンドログラムのプロット
plot(hc)
plot(as.dendrogram(hc), horiz = TRUE)

# as.dendrogram(hc) %>%
#   set('branches_k_color', k=4) %>% 
#   plot()


result <- cutree(hc, k=3)

# 精度確認
answer <- iris[, 5]
table <- table(answer, result)
table



# 非階層クラスタリング --------------------------------------------------------------

km <- kmeans(data, 3)
result2 <- km$cluster
result2


library(cluster)
clusplot(data, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# clusplot(data, result, color=TRUE, shade=TRUE, labels=2, lines=0)
# clusplot(data, result2, color=TRUE, shade=TRUE, labels=2, lines=0)

table2 <- table(answer, result2)
table2



































































