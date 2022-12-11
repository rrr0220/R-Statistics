
# 参考サイト
# https://bob3.hatenablog.com/entry/2022/01/15/123412

library(tidyverse)
library(ca)
library(factoextra)

# データ準備
data <- c(5, 18, 19, 12, 3, 7, 46, 29, 40, 7, 2, 20, 39, 49, 16) %>% 
  matrix(
    nrow = 5,
    dimnames = list(
      Brands = c("Butterbeer", "Squishee", "Slurm", "Fizzy Lifting Drink", "Brawndo"),
      Attributes = c("Tasty", "Aesthetic", "Economic")
      )
    )
data

# コレポン実行
res.ca_ca <- ca(data)
summary(res.ca_ca)

# プロット
res.biplot_symbiplot <- plot(res.ca_ca,
                             map = "symbiplot",
                             arrows = c(TRUE, TRUE),
                             xlim = c(-1.1, 0.9),
                             ylim = c(-1.1, 0.9),
                             main = "ca - Biplot - symbiplot")
res.biplot_symbiplot

# プロット２
fviz_ca(res.ca_ca,
        map = "symbiplot",
        arrows = c(TRUE, TRUE),
        title = "CA - Biplot - symbiplot") +
  scale_x_continuous(limits = c(-1.1, 0.9)) +
  scale_y_continuous(limits = c(-1.1, 0.9))

# 指標化残差
n <- sum(data)
n

# 同時確率分布
P <- data/n
P

# 行周辺確率分布（行質量）と列周辺確率分布（列）
row_masses_pi <- rowSums(P)
row_masses_pi

col_masses_pi <- colSums(P)
col_masses_pi

# 期待割合
E <- row_masses_pi %o% col_masses_pi
E

# 残差
R <- P - E
R

# 指標化残差
I <- R / E
I

# 指標化残差 = (座標の)内積
res.biplot_symbiplot$rows[1, 1] *
  res.biplot_symbiplot$cols[1, 1] +
  res.biplot_symbiplot$rows[1, 2] *
  res.biplot_symbiplot$cols[1, 2]

# モザイクプロット
library(vcd)
data %>% 
  mosaic(shade=TRUE, legend=TRUE, main="Beer?", direction="v")


