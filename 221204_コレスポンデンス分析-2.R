
# 参考サイト
# https://bob3.hatenablog.com/entry/2022/01/22/180059

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
res.SVD$d

U <- res.SVD$u[, 1:2]
rownames(U) <- rownames(P)
U

V <- res.SVD$v[, 1:2]
rownames(V) <- colnames(P)
V

# 固有値
eigenvalues <- res.SVD$d^2
round(eigenvalues, 4)

# 寄与率
eigen_prop <- prop.table(eigenvalues) 
round(eigen_prop, 4)

# 累積寄与率
cumsum(eigen_prop) 


















