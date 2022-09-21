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

iris = iris |> as_tibble()

ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Petal.Length))










##















































































####################################################
# 二元配置分散分析
URL = "https://raw.githubusercontent.com/dzchilds/stats-for-bio/master/data_csv/FESTUCA.CSV"
festuca = read_csv(URL)
