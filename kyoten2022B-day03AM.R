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
library(mgcv)
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

dset = read_csv("data/fukue_jma.csv")

dset = dset |> 
  group_by(ymd) |> 
  summarise(hpa = mean(hpa),
            temperature_air = mean(temperature_air)) |> 
  mutate(month = month(ymd),
         year = year(ymd))

dset = dset |> 
  mutate(tau = as.numeric(ymd)/1000)

dset |> 
  ggplot() + 
  geom_line(aes(x = ymd, y = hpa)) +
  geom_point(aes(x = ymd, y = hpa))

dset |> 
  ggplot() + 
  geom_point(aes(x = month, y = hpa))





m1 = gamm(temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) , data = dset)

layout(matrix(c(1,2), ncol = 2))
plot(m1$gam, scale = 0)

acf(resid(m1$lme))  
pacf(resid(m1$lme))


m2 = gamm(temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) , data = dset, correlation = corARMA(form = ~ 1|year, p = 1))
m3 = gamm(temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) , data = dset, correlation = corARMA(form = ~ 1|year, p = 2))
m4 = gamm(temperature_air ~ s(month, bs = "cc", k = 12) + s(tau) , data = dset, correlation = corARMA(form = ~ 1|year, p = 3))

AIC(
  m1$lme,
  m2$lme,
  m3$lme,
  m4$lme
    ) |> as_tibble(rownames = "model") |> arrange(AIC)


layout(matrix(c(1,2), ncol = 2))
plot(m2$gam, scale = 0)

acf(resid(m2$lme, type = "normalized"))  
pacf(resid(m2$lme, type = "normalized"))







