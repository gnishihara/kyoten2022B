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


## GLM (一般化線形モデル)　を用いた解析
# 線形予測子： eta = b0 + b1 x1 + b2 x2 + ... + bn xn
# リンク関数：g(mu) = eta or mu = g^-1(eta)
# 分布：離散型(Poisson, binomial, ...)と連続型(Gaussian, Gamma, ...)

gala = gala |> 
  mutate(logArea = log(Area),
         logAdjacent = log(Adjacent),
         logSpecies = log(Species))

# Poisson-GLM
# Species ~ Pois(mu)
# E(Species) = mu
# mu = exp(eta)
# eta = b0 + b1 logArea + b2 logAdjacent + b3 Nearest
# 
# log(Species) = log(mu) = eta :=: b0 + b1 logArea + b2 logAdjacent + b3 Nearest

# ヌルモデル
g0 = glm(Species ~ 1, data = gala, family = poisson(link = "log"))
# フルモデル
gf = glm(Species ~ logAdjacent + logArea + Elevation + Nearest + Scruz, 
         data = gala, family = poisson(link = "log"))
# a モデル
ga = glm(Species ~ logAdjacent + logArea + Nearest, 
         data = gala, family = poisson(link = "log"))


AIC(g0, gf, ga)


# 診断図の使う残渣は ダンスミス残渣（ランダム化残渣）でおこなう

fgala = fortify(gf) |> as_tibble()
fgala = fgala |> mutate(qresid = statmod::qresiduals(gf))


## 標準化残渣の正規性を確認する
ggplot(fgala) +
  geom_histogram(aes(x = qresid, y = ..density..)) + 
  stat_function(fun = dnorm, color = "red")

ggplot(fgala) + 
  geom_qq(aes(sample = qresid)) + 
  geom_qq_line(aes(sample = qresid))


## 標準化残渣と他の変数との関係

fgala |> 
  pivot_longer(cols = c(logArea, 
                        Nearest, 
                        Elevation, Nearest, 
                        Scruz,
                        logAdjacent, Species)) |> 
  ggplot() + 
  geom_point(aes(x = (value),  y = qresid)) + 
  geom_smooth(aes(x = (value), y = qresid)) + 
  facet_wrap(vars(name), scales = "free")

## 飛び地・異常値の確認
## クックの距離

dof = summary(gf) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])

fgala |> 
  mutate(n = 1:n(), .before = Species) |> 
  ggplot() + 
  geom_point(aes(x = n,
                 y  =.cooksd)) +
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed")



# ga モデルの診断図

# 診断図の使う残渣は ダンスミス残渣（ランダム化残渣）でおこなう

fgala = fortify(gf) |> as_tibble()
fgala = fgala |> mutate(qresid = statmod::qresiduals(gf))


## 標準化残渣の正規性を確認する
ggplot(fgala) +
  geom_histogram(aes(x = qresid, y = ..density..)) + 
  stat_function(fun = dnorm, color = "red")

ggplot(fgala) + 
  geom_qq(aes(sample = qresid)) + 
  geom_qq_line(aes(sample = qresid))


## 標準化残渣と他の変数との関係

fgala |> 
  pivot_longer(cols = c(logArea, 
                        Nearest, 
                        Elevation, Nearest, 
                        Scruz,
                        logAdjacent, Species)) |> 
  ggplot() + 
  geom_point(aes(x = (value),  y = qresid)) + 
  geom_smooth(aes(x = (value), y = qresid)) + 
  facet_wrap(vars(name), scales = "free")

## 飛び地・異常値の確認
## クックの距離

dof = summary(gf) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])

fgala |> 
  mutate(n = 1:n(), .before = Species) |> 
  ggplot() + 
  geom_point(aes(x = n,
                 y  =.cooksd)) +
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed")








