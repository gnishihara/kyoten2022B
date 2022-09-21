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
  pivot_longer(cols = matches("Length|Width")) |> 
  group_by(Species, name) |> 
  summarise(mean = mean(value),
            sd = sd(value),
            var = var(value)) |> 
  arrange(name)

ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Petal.Length))




# 分散は等しい？
# H0: 分散は等しい
leveneTest(Petal.Length ~ Species, data = iris)

apply_lt = function(x) {
  # H0: 分散は等しい
  leveneTest(value ~ Species, data = x)
}

apply_sw = function(x) {
  # H0: 正規分布の母集団から抽出した
  shapiro.test(x$value)
}

iris |> 
  pivot_longer(cols = matches("Length|Width")) |> 
  group_nest(name) |> 
  mutate(levene = map(data, function(x) {
    print(x)
    leveneTest(value ~ Species, data = x)
  }))

iris |> 
  pivot_longer(cols = matches("Length|Width")) |> 
  group_nest(name) |> 
  mutate(levene = map(data, \(x) {
    print(x)
    leveneTest(value ~ Species, data = x)
  }))

iris |> 
  pivot_longer(cols = matches("Length|Width")) |> 
  group_nest(name) |> 
  mutate(levene = map(data, apply_lt)) |> 
  pull(levene)

iris |> 
  pivot_longer(cols = matches("Length|Width")) |> 
  group_nest(name) |> 
  mutate(levene = map(data, apply_lt)) |> 
  mutate(shapiro = map(data, apply_sw)) |> 
  pull(shapiro)


## 分散分析はrobust。


lm(Sepal.Length ~ Species, data = iris) |> 
  summary.aov()


m1 = lm(Sepal.Length ~ Species, data = iris) 

iris2 = iris |> 
  mutate(fit = fitted(m1),
         residuals = residuals(m1),
         rstandard = rstandard(m1),
         rstudent = rstudent(m1))


# 残渣タイ因子の図
# 
ggplot(iris2) +
  geom_point(aes(x = Species,
                 y = residuals),
             position = position_jitter(0.2))
  
# 標準化残渣のヒストグラム
fortify(m1) |> as_tibble() |> 
  ggplot() +
  geom_histogram(aes(x = .stdresid))

# 標準化残渣のQQプロット

fortify(m1) |> as_tibble() |> 
  ggplot() +
  geom_qq(aes(sample = .stdresid)) +
  geom_qq_line(aes(sample = .stdresid))

# 標準化残渣と説明変数の関係

fortify(m1) |> as_tibble() |> 
  ggplot() +
  geom_point(aes(x = Species,
                 y = .stdresid),
             position = position_jitter(0.2))

# scaled-absolute standardized residuals
fortify(m1) |> as_tibble() |> 
  ggplot() +
  geom_point(aes(x = Species,
                 y = sqrt(abs(.stdresid))),
             position = position_jitter(0.2))

m1 |> summary.aov()

# 多重比較
c1 = emmeans(m1, specs = pairwise ~ Species)
c1
c1$emmeans |> confint() # 群ごとの平均値
c1$contrasts |> confint() # 比較ごとの差の平均値


####################################################
# 二元配置分散分析
URL = "https://raw.githubusercontent.com/dzchilds/stats-for-bio/master/data_csv/FESTUCA.CSV"
festuca = read_csv(URL)


ggplot(festuca) + 
  geom_point(aes(x = pH, 
                 y = Weight,
                 color = Calluna),
             position = position_jitterdodge(0.1, 0, 0.2))

# H0: pH の影響はない
# H1: Callunaの影響はない
# H2: pH ✕ Calluna の相互作用の影響はない

m0  = lm(Weight ~ 1, data = festuca) # 帰無モデル（説明変数なし）
m1a = lm(Weight ~ pH, data = festuca) 
m1b = lm(Weight ~ Calluna, data = festuca) 
m2a  = lm(Weight ~ pH + Calluna, data = festuca) 
m2b  = lm(Weight ~ Calluna + pH, data = festuca) 
m3a  = lm(Weight ~ pH + Calluna + pH:Calluna, data = festuca) 
m3b  = lm(Weight ~ Calluna + pH + Calluna:pH, data = festuca) 


anova(m0, m1a, m2a, m3a, test = "F")
anova(m0, m1b, m2b, m3b, test = "F")

festuca

# データのバランス・釣り合いがいいとき、上のどの解析でもいい

festuca2 = festuca |> slice(c(1:4, 5:8, 10:18))

m0  = lm(Weight ~ 1, data = festuca2) # 帰無モデル（説明変数なし）
m1a = lm(Weight ~ pH, data = festuca2) 
m1b = lm(Weight ~ Calluna, data = festuca2) 
m2a  = lm(Weight ~ pH + Calluna, data = festuca2) 
m2b  = lm(Weight ~ Calluna + pH, data = festuca2) 
m3a  = lm(Weight ~ pH + Calluna + pH:Calluna, data = festuca2) 
m3b  = lm(Weight ~ Calluna + pH + Calluna:pH, data = festuca2) 

# W = C + P と W = P + H の結果は違いが、データはおなじ。
# 問題です。この分散分析は Type-I 平方和
anova(m0, m1a, m2a, m3a, test = "F")
anova(m0, m1b, m2b, m3b, test = "F")

# 釣り合いのない・データ数が異なるデータの分散分析は
# Type-III 平方をつかう
# (Intercept): 総平均値(全データの平均値)
Anova(m3a, test = "F", type = "III")
Anova(m3b, test = "F", type = "III")


emmeans(m3a, pairwise ~ pH|Calluna)
emmeans(m3a, pairwise ~ Calluna|pH)



















