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
####################################################

# 一元配置分散分析

iris = iris |> as_tibble()

ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Petal.Length))


























































































####################################################
# 二元配置分散分析
URL = "https://raw.githubusercontent.com/dzchilds/stats-for-bio/master/data_csv/FESTUCA.CSV"
festuca = read_csv(URL)
