
# 参考資料
# 大隅昇 「質的データのマイニングと対応分析法［スライド資料統合］.pdf」

library(tidyverse)

# コレスポンデンス分析の実行(スクラッチ) -----------------------------------------------------------

レストラン <- c(
  "いりふね", 
  "かりや", 
  "きくみ", 
  "さとみ", 
  "クラーク", 
  "コルシカ", 
  "バッハ", 
  "ムガール", 
  "ラ・マレ", 
  "ロゴスキー"
  )

評価基準 <- c(
  "工夫・サービス", 
  "味", 
  "量"
  )

data <- c(98, 25, 32, 
          105, 35, 38, 
          35, 8, 67,
          42, 46, 7,
          34, 14, 54,
          32, 77, 13,
          48, 76, 18,
          49, 44, 16,
          49, 82, 15,
          48, 35, 42) %>% 
  matrix(
    nrow = 10,
    byrow = TRUE,
    dimnames = list(レストラン, 評価基準)
    )
data

# 総度数
N <-  sum(data)

# 同時確率分布(10×3)
P <-  data / N

# 行質量の分布(10×10)
pi <-  rowSums(P)
PI <-  diag(pi)

# 列質量の分布(3×3)
pj <-  colSums(P)
PJ <-  diag(pj)

# 行プロファイル(10×3)
NI <-  solve(PI) %*% P
rownames(NI) <- レストラン
NI
rowSums(NI)

# 列プロファイル(3×10)
NJ <-  solve(PJ) %*% t(P)
rownames(NJ) <- 評価基準
NJ
rowSums(NJ)

# (行プロファイル間の)ユークリッド距離
NI
t(NI[4,] - NI[3,]) %*% (NI[4,] - NI[3,]) # さとみーきくみ
t(NI[4,] - NI[8,]) %*% (NI[4,] - NI[8,]) # さとみームガール
t(NI[4,] - NI[1,]) %*% (NI[4,] - NI[1,]) # さとみーいりふね

# d <- dist(NI, method = "euclidean", diag = TRUE, upper = TRUE) # dist関数
# d^2

dist <- c()
for (s in 1:10){
  for (t in 1:10){
  dist <- c(dist, t(NI[s,] - NI[t,]) %*% (NI[s,] - NI[t,]))
  }
}
dist_matrix_euclidean <- matrix(dist, nrow=10, byrow = TRUE)
dist_matrix_euclidean

# (行プロファイル間の)カイ二乗距離 <- ストレッチ・プロファイル間のユークリッド距離
NI
t(NI[4,] - NI[3,]) %*% solve(PJ) %*% (NI[4,] - NI[3,]) # さとみーきくみ
t(NI[4,] - NI[8,]) %*% solve(PJ) %*% (NI[4,] - NI[8,]) # さとみームガール
t(NI[4,] - NI[1,]) %*% solve(PJ) %*% (NI[4,] - NI[1,]) # さとみーいりふね

dist <- c()
for (s in 1:10){
  for (t in 1:10){
    dist <- c(dist, t(NI[s,] - NI[t,]) %*% solve(PJ) %*% (NI[s,] - NI[t,]))
  }
}
dist_matrix_chisquare <- matrix(dist, nrow=10, byrow = TRUE)
dist_matrix_chisquare

# ((行プロファイル)重心からのカイ二乗距離
NI
t(NI[4,] - pj) %*% solve(PJ) %*% (NI[4,] - pj) # さとみー重心

dist <- c()
for (s in 1:10){
    dist <- c(dist, t(NI[s,] - pj) %*% solve(PJ) %*% (NI[s,] - pj))
    }
dist_from_G_chisquare <- dist
dist_from_G_chisquare

dist_from_G_chisquare * pi
sum(dist_from_G_chisquare * pi) # 全慣性（総変動）



# ストレッチ・プロファイル
NI
pj

XIJ <- NI %*% solve(sqrt(PJ))
colnames (XIJ) <- 評価基準
XIJ

XJ_bar <- apply(XIJ, 2, mean)
XJ_bar # 列質量の平方根と同値

XIJ_centerd <- sweep(XIJ, 2, XJ_bar, "-")
XIJ_centerd

# ストレッチ・プロファイルの分散共分散行列
V <- t(XIJ_centerd) %*% PI %*% XIJ_centerd

sum(eigen(V)$values)










































