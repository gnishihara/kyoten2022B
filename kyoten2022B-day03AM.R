####################################################
# 公開臨海データ解析実習　B日程
# 2022 September 22 PM
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
library(mgcv)      # GAM 解析用
library(googlesheets4) # google のスプレッドシートの読み込み用

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


gs4_deauth()
maaji = "https://docs.google.com/spreadsheets/d/1zVX7exLAAzWgHPQOSpQ2eeTIltXYhEYHmUT12REid0M/edit?usp=sharing"
seaweed = "https://docs.google.com/spreadsheets/d/1RBGytrJa5z7UYOkraqeul7Axhi2LlzjkZsaWyiOMjG0/edit?usp=sharing"
shirogisu = "https://docs.google.com/spreadsheets/d/1rb6WA3wkweOUfcpIziGJm9qdcnfVwOYURY1aUkwmdMM/edit?usp=sharing"

maaji = read_sheet(maaji, na = "NA")
seaweed = read_sheet(seaweed, na = "NA")
shirogisu = read_sheet(shirogisu, na = "NA")

names(maaji)

maaji |> select(temperature) # temperature に chr と dbl が混ざっている

ggplot(maaji) +
  geom_point(aes(x = month, y = size)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_continuous(limits = c(0, 13),
                     breaks = 1:12)

ggplot(maaji) +
  geom_point(aes(x = depth_range, y = size)) +
  scale_y_continuous(limits = c(0, 40))


ggplot(maaji) +
  geom_point(aes(x = dist_from_river, y = size)) +
  scale_y_continuous(limits = c(0, 40))

ggplot(maaji) +
  geom_point(aes(x = fetch, y = size)) +
  scale_y_continuous(limits = c(0, 40))

ggplot(maaji) +
  geom_point(aes(x = temperature, y = size)) +
  scale_y_continuous(limits = c(0, 40)) +
  geom_smooth(
    aes(x = temperature, y = size, color = "TP"),
    method = "gam",
    formula = y ~ s(x, k = 6)
  ) +
  geom_smooth(
    aes(x = temperature, y = size, color = "CC"),
    method = "gam",
    formula = y ~ s(x, k = 6, bs = "cc"),
    method.args = list(knots = list(month = c(0.5, 12.5)))
  )

ggplot(maaji) +
  geom_point(aes(x = date, y = size)) +
  scale_y_continuous(limits = c(0, 40))



# GAM を ggplot であてはめる
# 仮かいせき
ggplot(maaji) +
  geom_point(aes(x = month, y = size)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_continuous(limits = c(1, 12),
                     breaks = 1:12) +
  geom_smooth(aes(x = month, y = size, color = "GLM"),
              method = "glm",
              formula = y ~ x) +
  geom_smooth(aes(x = month, y = size, color = "TP"),
              method = "gam",
              formula = y ~ s(x, k = 6)) +
  geom_smooth(
    aes(x = month, y = size, color = "CC"),
    method = "gam",
    formula = y ~ s(x, k = 6, bs = "cc"),
    method.args = list(knots = list(month = c(0.5, 12.5)))
  )

# mgcv::gam()
# Generalized Additive Model
# 一般化加法モデル
g0 = glm(size ~ month, data = maaji, family = gaussian())
g1 = gam(size ~ s(month, k = 3), data = maaji, family = gaussian())
g2 = gam(
  size ~ s(month, k = 3, bs = "cc"),
  knots = list(month = c(0.5, 12.5)),
  data = maaji,
  family = gaussian()
)

summary(g0)
summary(g1)
summary(g2)

AIC(g0, g1, g2)


gt0 = glm(size ~ temperature, data = maaji, family = gaussian())
gt1 = gam(size ~ s(temperature, k = 3),
          data = maaji,
          family = gaussian())
gt2 = gam(
  size ~ s(temperature, k = 3, bs = "cc"),
  knots = list(month = c(0.5, 12.5)),
  data = maaji,
  family = gaussian()
)

summary(gt0)
summary(gt1)
summary(gt2)
AIC(gt0, gt1, gt2)
################################################################################
seaweed = seaweed |>
  mutate(month = month(date),
         station = factor(station))

p1 = seaweed |>
  pivot_longer(cols = c(wave, temperature)) |>
  ggplot() +
  geom_point(aes(x = value, y = seaweed_sp_richness)) +
  geom_smooth(aes(x = value, y = seaweed_sp_richness),
              method = "gam",
              formula = y ~ s(x)) +
  facet_wrap(vars(name))

p2 = seaweed |>
  pivot_longer(cols = c(wave, temperature)) |>
  ggplot() +
  geom_point(aes( x = month, y = value, color = station)) +
  geom_smooth(aes(x = month, y = value),
              method = "gam",
              formula = y ~ s(x)) +
  facet_wrap(vars(name))

p3 = ggplot(seaweed) +
  geom_point(aes(
    x = month,
    y = seaweed_sp_richness,
    color = factor(station)
  )) +
  geom_smooth(
    aes(
      x = month,
      y = seaweed_sp_richness,
      group = factor(station)
    ),
    method = "gam",
    formula = y ~ s(x, bs = "cc", k = 10),
    method.args = list(family = poisson("log"),
                       knots = list(month = c(0.5, 12.5))),
    se = F
  )
p1 + p2 + p3 + plot_layout(ncol = 1)


# QQプロットにより、モデルを却下
g1 = gam(
  seaweed_sp_richness ~ s(month,
                          bs = "cc",
                          k = 10),
  data = seaweed,
  family = poisson("log")
)

seaweed |>
  mutate(
    resid = residuals(g1),
    qresid = statmod::qresiduals(g1),
    fit   = fitted((g1))
  ) |>
  ggplot() +
  geom_point(aes(x = fit,
                 y = qresid))

seaweed |>
  mutate(
    resid = residuals(g1),
    qresid = statmod::qresiduals(g1),
    fit   = fitted((g1))
  ) |>
  ggplot() +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))


# QQプロットにより、モデルを却下
g2 = gam(
  seaweed_sp_richness ~ s(month,
                          bs = "cc",
                          k = 10,
                          by = station) + station,
  data = seaweed,
  family = poisson("log")
)

seaweed |>
  mutate(
    resid = residuals(g2),
    qresid = statmod::qresiduals(g2),
    fit   = fitted((g2))
  ) |>
  ggplot() +
  geom_point(aes(x = fit,
                 y = qresid))


seaweed |>
  mutate(
    resid = residuals(g2),
    qresid = statmod::qresiduals(g2),
    fit   = fitted((g2))
  ) |>
  ggplot() +
  geom_point(aes(x = fit,
                 y = sqrt(abs(qresid)))) +
  geom_smooth(aes(x = fit,
                  y = sqrt(abs(qresid))))

seaweed |>
  mutate(
    resid = residuals(g2),
    qresid = statmod::qresiduals(g2),
    fit   = fitted((g2))
  ) |>
  ggplot() +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))


summary(g2)
# モデルに temperature と wave を追加する

gl3s = glm(seaweed_sp_richness ~ temperature * wave * station,
          data = seaweed,
          family = poisson("log"))
gl3p = glm(seaweed_sp_richness ~ temperature * wave,
          data = seaweed,
          family = poisson("log"))
AIC(gl3p, gl3s)

seaweed |> drop_na() |> 
  mutate(
    resid = residuals(gl3s),
    qresid = statmod::qresiduals(gl3s),
    fit   = fitted(gl3s)
  ) |>
  ggplot() +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))
  

seaweed |> drop_na() |> 
  mutate(
    resid = residuals(gl3s),
    qresid = statmod::qresiduals(gl3s),
    fit   = fitted(gl3s)
  ) |>
  ggplot() +
  geom_point(aes(x = temperature, y = qresid)) +
  geom_smooth(aes(x = temperature, y = qresid)) 
  
seaweed |> drop_na() |> 
  mutate(
    resid = residuals(gl3s),
    qresid = statmod::qresiduals(gl3s),
    fit   = fitted(gl3s)
  ) |>
  ggplot() +
  geom_point(aes(x  = wave, y = qresid)) +
  geom_smooth(aes(x = wave, y = qresid))



# s3 を gl3s と比較する
s3 = gam(
  seaweed_sp_richness ~
    s(temperature) + wave + station,
  data = seaweed,
  family = poisson("log")
)

seaweed |> drop_na() |> 
  mutate(
    resid = residuals(s3),
    qresid = statmod::qresiduals(s3),
    fit   = fitted(s3)
  ) |>
  ggplot() +
  geom_qq(aes(sample = qresid)) +
  geom_qq_line(aes(sample = qresid))


seaweed |> drop_na() |> 
  mutate(
    resid = residuals(s3),
    qresid = statmod::qresiduals(s3),
    fit   = fitted(s3)
  ) |>
  ggplot() +
  geom_point(aes(x = temperature, y = qresid)) +
  geom_smooth(aes(x = temperature, y = qresid)) 

seaweed |> drop_na() |> 
  mutate(
    resid = residuals(s3),
    qresid = statmod::qresiduals(s3),
    fit   = fitted(s3)
  ) |>
  ggplot() +
  geom_point(aes(x  = wave, y = qresid)) +
  geom_smooth(aes(x = wave, y = qresid))

AIC(s3, gl3s)







####################################################################
dset = read_csv("data/fukue_jma.csv")

dset = dset |>
  group_by(ymd) |>
  summarise(hpa = mean(hpa),
            temperature_air = mean(temperature_air)) |>
  mutate(month = month(ymd),
         year = year(ymd))

dset = dset |>
  mutate(tau = as.numeric(ymd) / 1000)

dset |>
  ggplot() +
  geom_line(aes(x = ymd, y = hpa)) +
  geom_point(aes(x = ymd, y = hpa))

dset |>
  ggplot() +
  geom_point(aes(x = month, y = hpa))





m1 = gamm(temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) , data = dset)

layout(matrix(c(1, 2), ncol = 2))
plot(m1$gam, scale = 0)

acf(resid(m1$lme))
pacf(resid(m1$lme))


m2 = gamm(
  temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) ,
  data = dset,
  correlation = corARMA(form = ~ 1 | year, p = 1)
)
m3 = gamm(
  temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) ,
  data = dset,
  correlation = corARMA(form = ~ 1 | year, p = 2)
)
m4 = gamm(
  temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) ,
  data = dset,
  correlation = corARMA(form = ~ 1 | year, p = 3)
)

AIC(m1$lme,
    m2$lme,
    m3$lme,
    m4$lme) |> as_tibble(rownames = "model") |> arrange(AIC)


layout(matrix(c(1, 2), ncol = 2))
plot(m2$gam, scale = 0)

acf(resid(m2$lme, type = "normalized"))
pacf(resid(m2$lme, type = "normalized"))
