
# 参考サイト
# https://bob3.hatenablog.com/entry/2022/01/22/180059

library(tidyverse)

# コレスポンデンス分析の実行(スクラッチ) -----------------------------------------------------------

data <- c(395, 2456, 1758, 147, 153, 916, 694, 327, 1347) %>% 
  matrix(
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      地域 = c("オスロ", "中部", "北部"),
      犯罪 = c("強盗", "詐欺", "破壊")
      )
    )
data

n <- sum(data)
P <-  data / n
row.masses <- rowSums(P)
col.masses <- colSums(P)
E <- row.masses %o% col.masses
R <- P -E
I <- R / E

list(
  `総度数(n)` = n,
  `観測割合(P)` = P,
  `行質量(row.masses)` = row.masses,
  `列質量(col.masses)` = col.masses,
  `期待値(E)` = E,
  `残差(R)` = R,
  `指標化残差(I)` = I
  ) %>%  
  print()

# 標準化残差
Z <- I * sqrt(E)
Z

# 特異値分解
res.SVD <- svd(Z)
res.SVD
res.SVD$d

U <- res.SVD$u[, 1:2]
rownames(U) <- rownames(P)
U

V <- res.SVD$v[, 1:2]
rownames(V) <- colnames(P)
V

# 確認１(特異値分解の逆)
res.SVD$u %*% diag(res.SVD$d) %*% t(res.SVD$v)

# 固有値
eigenvalues <- res.SVD$d^2
round(eigenvalues, 4)

# 寄与率
eigen_prop <- prop.table(eigenvalues) 
round(eigen_prop, 4)

# 累積寄与率
cumsum(eigen_prop) 

# 確認２(固有値の平方根)
sqrt(eigen(t(Z) %*% Z)$values)


# 座標 ----------------------------------------------------------------------

# 標準座標（行のベクトルを行の質量で割り、列のベクトルを列の質量で割る）
standard.coordinates.rows <- sweep(U, 1, sqrt(row.masses), "/")
standard.coordinates.rows
standard.coordinates.columns <- sweep(V, 1, sqrt(col.masses), "/")
standard.coordinates.columns

# 主座標（標準座標に特異値を乗じる）
principal.coordinates.rows <- sweep(standard.coordinates.rows, 2, res.SVD$d[1:2], "*")
principal.coordinates.rows
principal.coordinates.columns <- sweep(standard.coordinates.columns, 2, res.SVD$d[1:2], "*")
principal.coordinates.columns

# 対称バイプロット（標準座標に特異値の平方根を乗じる）
sympc.coordinates.rows <- sweep(standard.coordinates.rows, 2, sqrt(res.SVD$d[1:2]), "*")
sympc.coordinates.rows
sympc.coordinates.columns <- sweep(standard.coordinates.columns, 2, sqrt(res.SVD$d[1:2]), "*")
sympc.coordinates.columns





