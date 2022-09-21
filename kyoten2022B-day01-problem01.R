####################################################
# 公開臨海データ解析実習　B日程
# 2022 September 21 PM
# Greg Nishihara
####################################################
# パッケージの読み込み
library(tidyverse) # データ処理
library(lubridate) # 時系列データ
library(emmeans)   # 多重比較
library(car)       # Type-II ANOVA
library(statmod)   # qresiduals (ダンスミス残渣・ランダム化残渣)
library(ggpubr)　　# theme_pubr()
library(patchwork) # 複数図の結合
library(magick)    # pdf を png に変換する
library(showtext)  # フォント埋め込み用
################################################################
# font_files() |> as_tibble() # システムフォントの閲覧
# font_files() |> as_tibble() |>
# # filter(str_detect(ps_name, "NotoSansCJK")) |> 
# select(file, face, ps_name) 
# 埋め込みフォントの指定
font_add(family = "notosansjp",
         regular = "NotoSansCJKjp-Regular.otf")
# フォントを有効にする
theme_gray(base_family = "notosansjp") |> theme_set()
# Windows の場合
# font_add(family = "meiryo", regular = "meiryo.ttc")
# theme_gray(base_family = "meiryo") |> theme_set()
showtext_auto()

################################################################
# 一元配置分散分析
# グループごとの分散は等しい
# グループごとのデータ数は等しい
# グループごとのデータは正規分布に従う
# データは独立同一分布である
iris = iris |> as_tibble()

iris |> 
  group_by(Species) |> 
  mutate(Sepal.Length = cut(Sepal.Length, breaks = 3,
                            labels = c("S", "M", "L"))) |> 
  group_by(Species, Sepal.Length) |> 
  summarise(n = length(Petal.Length))


iris = iris |> 
  group_by(Species) |> 
  mutate(Sepal.Length = cut(Sepal.Length, breaks = 3,
                            labels = c("S", "M", "L"))) |> 
  select(Species, Sepal.Length, Petal.Length)


ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Petal.Length,
                   fill = Sepal.Length))

# （１）二元配置分散分析
# （２）標準化残渣のQQプロットと
# （３）Speciesごと標準化残渣の散布図
# （４）Sepal.Lengthごと標準化残渣の散布図

iris |> 
  group_by(Species, Sepal.Length) |> 
  summarise(n = length(Petal.Length))

m0 = lm(Petal.Length ~ 1, data = iris)
m1 = lm(Petal.Length ~ Species, data = iris)
m2 = lm(Petal.Length ~ Species + Sepal.Length, data = iris)
m3 = lm(Petal.Length ~ Species * Sepal.Length, data = iris)



# （１）二元配置分散分析
anova(m0, m1, m2, m3, test = "F")

# （２）標準化残渣のQQプロットと
fortify(m3) |> 
  as_tibble() |> 
  ggplot() +
  geom_qq(aes(sample = .stdresid)) +
  geom_qq_line(aes(sample = .stdresid))

# （３）Speciesごと標準化残渣の散布図
 
fortify(m3) |> 
  as_tibble() |> 
  ggplot() +
  geom_point(aes(x = Species, y = .stdresid),
             position = position_jitter(0.1))

# （４）Sepal.Lengthごと標準化残渣の散布図 
fortify(m3) |> 
  as_tibble() |> 
  ggplot() +
  geom_point(aes(x = Sepal.Length, y = .stdresid),
             position = position_jitter(0.1))














