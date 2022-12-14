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


## 標準化残渣と他の変数との関係



fortify(mf) |> 
  pivot_longer(cols = c(Area, Elevation,
                        Nearest, Scruz,
                        Adjacent, Species)) |> 
  ggplot() + 
  geom_point(aes(x = log(value), y = .stdresid)) + 
  facet_wrap(vars(name), scales = "free")

# 分散拡大係数 (Variance Inflation Factor)
# VIF = 1 / (1 - R^2)
# VIF > 10 多重共線性の問題をおこしている
vif(mf)

# 応答変数のログ変換
gala = gala |> 
  mutate(logSpecies = log(Species))



gala |> 
  pivot_longer(cols = c(Area, Elevation,
                        Nearest, Scruz,
                        Adjacent)) |> 
  ggplot() + 
  geom_point(aes(x = value, y = logSpecies)) + 
  facet_wrap(vars(name), scales = "free")



m0 = lm(logSpecies ~ 1, data = gala)
mf = lm(logSpecies ~ Area + Adjacent + Elevation + Nearest + Scruz, data = gala)

AIC(m0, mf)
vif(mf)

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


## 標準化残渣と他の変数との関係

fortify(mf) |> 
  pivot_longer(cols = c(Area, Elevation,
                        Nearest, Scruz,
                        Adjacent, Species)) |> 
  ggplot() + 
  geom_point(aes(x = (value), y = .stdresid)) + 
  facet_wrap(vars(name), scales = "free")

## 飛び地・異常値の確認
## クックの距離

dof = summary(mf) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])

fortify(mf) |> as_tibble() |> 
  mutate(n = 1:n(), .before = Species) |> 
  ggplot() + 
  geom_point(aes(x = n,
                 y  =.cooksd)) +
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed")




# 説明変数ごとの総関係数
gala |> select(-Island,
               -Species,
               -logSpecies) |> cor()

# 相関関係をみながら、重複していそうな変数をはずす

ma = lm(logSpecies ~ Area + Nearest + Adjacent,
        data = gala)

AIC(m0, ma, mf)

# 選んだモデルの診断図

## 標準化残渣の正規性を確認する
fortify(ma) |> 
  ggplot() +
  geom_histogram(aes(x = .stdresid, y = ..density..)) + 
  stat_function(fun = dnorm, color = "red")

fortify(ma) |> 
  ggplot() + 
  geom_qq(aes(sample = .stdresid)) + 
  geom_qq_line(aes(sample = .stdresid))


## 標準化残渣と他の変数との関係

fortify(ma) |> 
  pivot_longer(cols = c(Area, 
                        Nearest, 
                        Adjacent, logSpecies)) |> 
  ggplot() + 
  geom_point(aes(x = (value), y = .stdresid)) + 
  facet_wrap(vars(name), scales = "free")

## 飛び地・異常値の確認
## クックの距離

dof = summary(ma) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])

fortify(ma) |> as_tibble() |> 
  mutate(n = 1:n(), .before = logSpecies) |> 
  ggplot() + 
  geom_point(aes(x = n,
                 y  =.cooksd)) +
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed")



# もう一度データを可視化して確認
gala |> 
  pivot_longer(cols = c(Area, Nearest, Adjacent)) |> 
  ggplot() + 
  geom_point(aes(x = value, y = logSpecies)) + 
  facet_wrap(vars(name), scales = "free")


# もう一度データを可視化して確認
gala |> 
  mutate(logArea = log(Area),
         logAdjacent = log(Adjacent)) |> 
  pivot_longer(cols = c(logArea, Nearest, logAdjacent)) |> 
  ggplot() + 
  geom_point(aes(x = value, y = logSpecies)) + 
  facet_wrap(vars(name), scales = "free")




gala = gala |> 
  mutate(logArea = log(Area),
         logAdjacent = log(Adjacent)) 



# ４かいめのモデル

mb = lm(logSpecies ~ logArea + Nearest + logAdjacent,
        data = gala)

AIC(m0, ma, mb, mf)

# 選んだモデルの診断図

## 標準化残渣の正規性を確認する
fortify(mb) |> 
  ggplot() +
  geom_histogram(aes(x = .stdresid, y = ..density..)) + 
  stat_function(fun = dnorm, color = "red")

fortify(mb) |> 
  ggplot() + 
  geom_qq(aes(sample = .stdresid)) + 
  geom_qq_line(aes(sample = .stdresid))


## 標準化残渣と他の変数との関係

fortify(mb) |> 
  pivot_longer(cols = c(logArea, 
                        Nearest, 
                        logAdjacent, logSpecies)) |> 
  ggplot() + 
  geom_point(aes(x = (value), y = .stdresid)) + 
  geom_smooth(aes(x = (value), y = .stdresid)) + 
  facet_wrap(vars(name), scales = "free")

## 飛び地・異常値の確認
## クックの距離

dof = summary(mb) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])

fortify(mb) |> as_tibble() |> 
  mutate(n = 1:n(), .before = logSpecies) |> 
  ggplot() + 
  geom_point(aes(x = n,
                 y  =.cooksd)) +
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed")

# model mb の AIC がもっとも低い：選択
# 診断図の結果を確認すると、mb を採択してもいい
# が、標準化残渣と logSpecies との問題は残っている

m0 = lm(logSpecies ~ 1, data = gala)
mf = lm(logSpecies ~ Area + Adjacent + Elevation + Nearest + Scruz, data = gala)
ma = lm(logSpecies ~ Area + Adjacent + Nearest, data = gala)
mb = lm(logSpecies ~ logArea + logAdjacent + Nearest, data = gala)
mbx = lm(logSpecies ~ logArea * logAdjacent * Nearest, data = gala)
summary(mbx)


AIC(m0, ma, mb, mf, mbx) |> 
  as_tibble(rownames = "Model") |> 
  arrange(AIC)

