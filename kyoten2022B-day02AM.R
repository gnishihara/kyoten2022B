####################################################
# 公開臨海データ解析実習　B日程
# 2022 September 22 AM
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

# faraway パッケージが必要です。
# install.packages("faraway")
data(gala, package = "faraway")
class(gala)
names(gala)
island = rownames(gala)

# Species: 植物の種数
# Endemics: 島の固有種の種数
# Area: 島の面積
# Adjacent: 一番近い島の面積
# Elevation: 高度
# Nearest: 一番近い島からの距離
# Scruz: Scruz島からの距離

gala = gala |> as_tibble()
gala = gala |> mutate(Island = island, .before = Species)

gala |> 
  pivot_longer(cols = !Island) |> 
  ggplot() + 
  geom_col(aes(y = Island,
               x = value)) +
  facet_wrap(facets = vars(name), scales = "free")



gala = gala |> select(-Endemics)
gala


# 多重共線性の問題 

# 変数ごとの総関係数
gala |> select(-Island) |> cor()


# Species と 各変数の関係


gala |> 
  pivot_longer(cols = c(Area, Elevation,
                        Nearest, Scruz,
                        Adjacent)) |> 
  ggplot() + 
  geom_point(aes(x = value, y = Species)) + 
  facet_wrap(vars(name), scales = "free")


# 単純な線形モデル
# 正規分布に従うと仮定する

# H0: ヌルモデル (m0)
# HF: フルモデル (mf)
m0 = lm(Species ~ 1, data = gala)
mf = lm(Species ~ Area + Adjacent + Elevation + Nearest + Scruz, data = gala)

# ここでやっている検定は F検定
anova(m0, mf, test = "F")                # Type-I 平方和
Anova(m0, mf, test = "F", type = "III")  # Type-III 平方和

# 情報理論　AIC
# AIC の低い値のモデルを選ぶ
AIC(m0, mf)


# 選んだモデルの診断図

## 標準化残渣の正規性を確認する
fortify(mf) |> 
  ggplot() +
  geom_histogram(aes(x = .stdresid, y = ..density..)) + 
  stat_function(fun = dnorm, color = "red")

fortify(mf) |> 
  ggplot() + 
  geom_qq(aes(sample = .stdresid)) + 
  geom_qq_line(aes(sample = .stdresid))














