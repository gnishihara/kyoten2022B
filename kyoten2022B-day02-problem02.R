####################################################
# 公開臨海データ解析実習　B日程
# 2022 September 21 PM 問題
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

data(mammalsleep, package = "faraway")

# 説明変数：brain, lifespan, gestation, sleep, dream
# 応答変数：body


# 一般化線形モデルを当てはめ、診断図と解析結果を示す。

mammalsleep = mammalsleep |>
  as_tibble() |>
  select(body, brain, lifespan, gestation, sleep, dream)

mammalsleep |>
  pivot_longer(cols = c(-body)) |>
  ggplot() +
  geom_point(aes(x = log(value),
                 y = log(body))) +
  facet_wrap(vars(name), scales = "free")

mammalsleep = mammalsleep |>
  mutate(across(everything(),
                list(log = log)))

mammalsleep = mammalsleep |> drop_na()

mammalsleep = mammalsleep |>
  filter(if_all(everything(),
                is.finite))

m0 = glm(body ~ 1, data = mammalsleep, family = gaussian())
m1 = glm(
  body ~ brain_log +
    dream_log +
    gestation_log +
    lifespan_log +
    sleep_log,
  data = mammalsleep,
  family = gaussian()
)

AIC(m0, m1)

dset = fortify(m1) |> mutate(qresid = statmod::qresiduals(m1))


ggplot(dset)  +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))


dset |>
  pivot_longer(cols = matches("body|brain|lifespan|gestation|sleep|dream")) |>
  ggplot() +
  geom_point(aes(x = log(value), y = qresid)) +
  facet_wrap(vars(name), scales = "free")


dof = summary(m1) |> pluck("df") # モデル自由度

threshold = qf(0.5, dof[1], dof[2])

dset |>
  mutate(n = 1:n(), .before = body) |>
  ggplot() +
  geom_point(aes(x = n,
                 y  = .cooksd)) +
  geom_hline(yintercept = threshold,
             color = "red",
             linetype = "dashed")

dset |> head()



summary(m1)
m2 = glm(
  body ~ dream_log +
    gestation_log +
    lifespan_log +
    sleep_log,
  data = mammalsleep,
  family = gaussian()
)

m3 = glm(
  body ~ dream_log +
    gestation_log +
    sleep_log,
  data = mammalsleep,
  family = gaussian()
)


AIC(m0,m1,m2, m3)

dset = fortify(m3) |> mutate(qresid = statmod::qresiduals(m3))
ggplot(dset)  +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))
dset |>
  pivot_longer(cols = matches("body|gestation|sleep|dream")) |>
  ggplot() +
  geom_point(aes(x = log(value), y = qresid)) +
  facet_wrap(vars(name), scales = "free")
dof = summary(m3) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])
dset |>
  mutate(n = 1:n(), .before = body) |>
  ggplot() +
  geom_point(aes(x = n,
                 y  = .cooksd)) +
  geom_hline(yintercept = threshold,
             color = "red",
             linetype = "dashed")

# Gamma -- GLM

g0 = glm(body ~ 1, data = mammalsleep, family = Gamma("log"))
g1 = glm(
  body ~ 
    brain_log +
    dream_log +
    gestation_log +
    lifespan_log +
    sleep_log,
  data = mammalsleep,
  family = Gamma("log")
)
g2 = glm(
  body ~ 
    brain_log +
    dream_log +
    lifespan_log +
    sleep_log,
  data = mammalsleep,
  family = Gamma("log")
)
g3 = glm(
  body ~ 
    brain_log + 
    dream_log +
    sleep_log,
  data = mammalsleep,
  family = Gamma("log")
)
AIC(g0, g1, g2, g3) |> 
  as_tibble(rownames = "model") |> arrange()

dset = fortify(g3) |> mutate(qresid = statmod::qresiduals(g3))
ggplot(dset)  +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))
dset |>
  pivot_longer(cols = matches("body|brain|sleep|dream")) |>
  ggplot() +
  geom_point(aes(x = log(value), y = qresid)) +
  facet_wrap(vars(name), scales = "free")

dof = summary(g3) |> pluck("df") # モデル自由度
threshold = qf(0.5, dof[1], dof[2])
dset |>
  mutate(n = 1:n(), .before = body) |>
  ggplot() +
  geom_point(aes(x = n,
                 y  = .cooksd)) +
  geom_hline(yintercept = threshold,
             color = "red",
             linetype = "dashed")

# eta ~ brain_log + dream_log + sleep_log
# E(body) = exp(eta)
# body ~ Gamma(mu, theta)

summary(g3)  

# リンクスケールにおける結果は良さそう
dset |> 
  select(brain_log, dream_log, sleep_log) |> 
  modelr::add_predictions(g3, type = "response") |> 
  ggplot() + 
  geom_point(aes(x =  brain_log, y = pred),
             size = 3, 
             color = "red") +
  geom_point(aes(x = brain_log, y = body), 
             data = dset |> select(-sleep_log))+
  scale_y_log10()


# 自然スケールにおける結果の場合、極端の値の説明はできていない。
dset |> 
  select(brain_log, dream_log, sleep_log) |> 
  modelr::add_predictions(g3, type = "response") |> 
  ggplot() + 
  geom_point(aes(x =  brain_log, y = pred),
             size = 3, 
             color = "red") +
  geom_point(aes(x = brain_log, y = body), 
             data = dset |> select(-sleep_log))

dset |> 
  select(body, brain_log, dream_log, sleep_log) |> cor()

