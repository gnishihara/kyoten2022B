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

###################################################a#############
# 一元配置分散分析
# グループごとの分散は等しい
# グループごとのデータ数は等しい
# グループごとのデータは正規分布に従う
# データは独立同一分布である
iris = iris |> as_tibble()

iris |> 
  pivot_longer(cols = matches("Length|Width")) |> 
  group_by(Species, name) |> 
  summarise(mean = mean(value),
            sd = sd(value),
            var = var(value)) |> 
  arrange(name)

ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Petal.Length))


# 共分散分散分析
iris
ggplot() + 
  geom_point(aes(x = Sepal.Length,
                 y = Petal.Length,
                 color = Species),
             data = iris)


m0 = lm(Petal.Length ~ 1, data = iris)
m1 = lm(Petal.Length ~ Sepal.Length, data = iris)
m2 = lm(Petal.Length ~ Sepal.Length + Species, data = iris)
m3 = lm(Petal.Length ~ Sepal.Length * Species, data = iris)

# H0: 説明変数の影響はない
# H1: Sepal.length の影響がない
# H2: Sepal.Length と Species の影響はない
# H3:相互作用の影響はない（Speciesごとの傾きがおなじ）

anova(m0, m1, m2, m3, test = "F")

fortify(m3) |> as_tibble() |> 
  ggplot() + 
  geom_qq(aes(sample = .stdresid)) +
  geom_qq_line(aes(sample = .stdresid))


# キムモデルの場合、残渣は正規分布に従わない
fortify(m0) |> as_tibble() |> 
  ggplot() + 
  geom_qq(aes(sample = .stdresid)) +
  geom_qq_line(aes(sample = .stdresid))

fortify(m3) |> as_tibble() |> 
  ggplot() + 
  geom_point(aes(x = Sepal.Length,
                 y = .stdresid, 
                 color = Species))

fortify(m3) |> as_tibble() |> 
  ggplot() + 
  geom_point(aes(x = Sepal.Length,
                 y = sqrt(abs(.stdresid)), 
                 color = Species)) +
  geom_smooth(aes(x = Sepal.Length,
                  y = sqrt(abs(.stdresid))))


emtrends(m3, pairwise ~ Species, var = "Sepal.Length")



# 作図

piris = iris |> group_by(Species) |> 
  summarise(min = min(Sepal.Length),
            max = max(Sepal.Length)) |> 
  mutate(Sepal.Length = map2(min, max, \(x, y) {
    seq(x, y, length = 11)
  })) |> 
  unnest(Sepal.Length)

# piris = piris |> mutate(fit = predict(m3, newdata = piris))

tmp = predict(m3, newdata = piris, se = TRUE) |> as_tibble()

piris = bind_cols(piris, tmp)

piris = piris |> 
  mutate(lse = fit - se.fit,
         use = fit + se.fit,
         l95 = fit - 1.96*se.fit,
         u95 = fit + 1.96*se.fit)

z = summary(m3)

z$adj.r.squared # 調整済み決定係数

text1 = sprintf("R[adj]^2 == %0.4f", z$adj.r.squared)
text2 = sprintf("F['%0.0f, %0.f'] == %0.1f~'(P < 0.0001)'", 
        z$fstatistic[2], z$fstatistic[3], z$fstatistic[1])
ggplot() + 
  geom_ribbon(aes(x = Sepal.Length,
                  ymin = lse,
                  ymax = use,
                  fill = Species), 
              data = piris,
              alpha = 0.25) +
  geom_ribbon(aes(x = Sepal.Length,
                  ymin = l95,
                  ymax = u95,
                  fill = Species), 
              data = piris,
              alpha = 0.25) +
  geom_point(aes(x = Sepal.Length,
                 y = Petal.Length,
                 color = Species),
             data = iris) +
  geom_line(aes(x = Sepal.Length,
                y = fit,
                color = Species),
            data = piris) +
  annotate("text",
           x = 4,
           y = 8,
           label = text1,
           parse = TRUE,
           hjust = 0,
           vjust = 1) +
  annotate("text",
           x = 4,
           y = 7,
           label = text2,
           parse = TRUE,
           hjust = 0,
           vjust = 1) +
  scale_x_continuous("Sepal Length (cm)",
                     limits = c(4, 8)) +
  scale_y_continuous("Petal Length (cm)",
                     limits = c(0, 8))  +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.title = element_blank())
 

# MANOVA
# 多変量解析

iris

manova(cbind(Petal.Length, Petal.Width) ~ Species, 
   data = iris) |> 
  summary()


ggplot() + 
  geom_point(aes(x = Petal.Width, 
                 y = Petal.Length,
                 color = Species),
             data = iris) +
  geom_smooth(aes(x = Petal.Width, 
                  y = Petal.Length),
              data = iris,
              method = "lm",
              formula = y~x, se = )
 












