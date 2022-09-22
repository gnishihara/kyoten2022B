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
library(nlstools)  # 非線形モデル用
library(minpack.lm)  # 非線形モデル用
library(googlesheets4)

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

gs4_deauth()
spreadsheet1 = "https://docs.google.com/spreadsheets/d/1CsY7ILKZRFlwQEIzSgu1veMQ964IPVegIJOo04lIGVE/edit#gid=1846404397"
spreadsheet2 = "https://docs.google.com/spreadsheets/d/1yeC-rJdxdiVa_icoNHZ1xrt4HWHyGCeQMnUt1r2_hnk/edit#gid=540001236" 
spreadsheet3 = "https://docs.google.com/spreadsheets/d/1Im8Qg-ukk8uh_3z4H6IwirTc4nhxPqKrDWrjhK4gZ0o/edit#gid=2099964525"

mgldata = tibble(fnames = c(spreadsheet1, spreadsheet2, spreadsheet3), day = 1:3) |> 
  mutate(data = map(fnames, read_sheet, sheet = "光合成データ")) |> 
  select(-fnames)

seaweed = tibble(fnames = c(spreadsheet1, spreadsheet2, spreadsheet3), day = 1:3) |> 
  mutate(data = map(fnames, read_sheet, sheet = "海藻資料データ"))|> 
  select(-fnames)

lightdata = tibble(fnames = c(spreadsheet1, spreadsheet2, spreadsheet3), day = 1:3) |> 
  mutate(data = map(fnames, read_sheet, sheet = "光環境データ"))|> 
  select(-fnames)


# 光環境データ

lightdata = lightdata |> 
  unnest(data) |> 
  select(day,
         light = "光環境",
         sample = matches("サンプル"),
         ppfd = matches("photons")) |> 
  group_by(day, light) |> 
  summarise(ppfd = mean(ppfd))

tmp = tibble(light = rep("アルミホイル",3), 
             ppfd =  rep(0, 3),
             day = 1:3)

lightdata = 
  bind_rows(lightdata, tmp) |> 
  mutate(light = factor(light), 
         day = factor(day))


# 溶存酸素濃度のデータ

mgldata = mgldata |> unnest(data) |> 
  select(day,
         han = matches("班"), 
         sample = matches("サンプル"),
         min = matches("時間"),
         mgl = matches("mg"),
         temperature = matches("温"),
         light = matches("光"),)


seaweed = seaweed |> unnest(data) |> 
  select(day, 
         han = matches("班"),
         sample = matches("サンプル"),
         vol = matches("ml"),
         gww = matches("湿"),
         seaweed = "海藻")


dset = full_join(mgldata, seaweed,
          by = c("day", "han", "sample"))

dset |> 
  ggplot() +
  geom_point(aes(x = min, y = mgl)) + 
  facet_grid(rows = vars(light),
             cols = vars(seaweed))

# 光合成速度を求める関数

calculate_rate = function(data) {
  z = lm(mgl ~ min, data = data)
  vol = data$vol[1]
  gww = data$gww[1]
  
  coefficients(z)[2]  * vol / gww
}


dset = dset |> 
  group_nest(day, han, sample, seaweed, light) |> 
  mutate(rate = map(data, calculate_rate)) |> 
  unnest(rate)

dset = dset |> 
  select(light, seaweed, han, sample, rate)

dset = dset |> 
  mutate(light = factor(light),
         seaweed = factor(seaweed))

alldata = full_join(dset, lightdata,
          by = c("light")) |> 
  select(seaweed, ppfd, rate)

ggplot(alldata) + 
  geom_point(aes(x = ppfd, y = rate, color = seaweed))


# 光合成光モデル
pimodel = function(ppfd, pmax, rd, alpha) {
  pmax * (1 - exp(-alpha / pmax * ppfd)) - rd
}


preview(rate ~ pimodel(ppfd, pmax, rd, alpha),
        data = alldata,
        variable = 2,
        start = list(pmax = 10, alpha = 0.1, rd = 0.5))


m1 = nls(rate ~ pimodel(ppfd, pmax, rd, alpha),
    data = alldata,
    start = list(pmax = 10, alpha = 0.1, rd = 0.5))

pdata = alldata |> 
  expand(ppfd = modelr::seq_range(ppfd, n = 21)) |> 
  modelr::add_predictions(m1)

ggplot(alldata) + 
  geom_point(aes(x = ppfd, y = rate, color = seaweed)) +
  geom_line(aes(x = ppfd, y = pred), data = pdata)




# 残渣たい期待値
alldata |> 
  modelr::add_residuals(m1) |> 
  modelr::add_predictions(m1) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = resid))

alldata |> 
  modelr::add_residuals(m1) |> 
  modelr::add_predictions(m1) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = resid)) +
  facet_wrap(vars(seaweed))

# 残渣たい期待値
alldata |> 
  modelr::add_residuals(m1) |> 
  modelr::add_predictions(m1) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = sqrt(abs(resid))))

alldata |> 
  modelr::add_residuals(m1) |> 
  modelr::add_predictions(m1) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = sqrt(abs(resid)))) +
  facet_wrap(vars(seaweed))



################
# 海藻ごとにモデルをあてはめる


m1 = nls(rate ~ pimodel(ppfd, pmax, rd, alpha),
         data = alldata,
         start = list(pmax = 10, alpha = 0.1, rd = 0.5))

START = lapply(coefficients(m1), rep, 3)

m2 = nls(rate ~ pimodel(ppfd, 
                        pmax[seaweed], 
                        rd[seaweed], 
                        alpha[seaweed]),
         data = alldata,
         start = START)
summary(m2)

# 残渣たい期待値
alldata |> 
  modelr::add_residuals(m2) |> 
  modelr::add_predictions(m2) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = resid)) +
  facet_wrap(vars(seaweed))

alldata |> 
  modelr::add_residuals(m2) |> 
  modelr::add_predictions(m2) |> 
  ggplot() +
  geom_point(aes(x = pred,
                 y = sqrt(abs(resid)))) +
  geom_smooth(aes(x = pred,
                  y =  sqrt(abs(resid)))) +
 facet_wrap(vars(seaweed), scales = "free_x")


AIC(m1, m2)


fit_model = function(data) {
  nls(rate ~ pimodel(ppfd, pmax, rd, alpha),
      data = data,
      start = list(pmax = 10, alpha = 0.1, rd = 0.5))
}


pdata = alldata |> 
  expand(ppfd = modelr::seq_range(ppfd, n = 21),
         seaweed) |> 
  modelr::add_predictions(m2)



xlabel = "PPFD~(mu*mol~photons~m^{-2}~s^{-1})"
ylabel = "'Net photosynthesis rate'~~(mg~O[2]~g[ww]^{-1}~min^{-1})"

panellabel = alldata |> 
  select(seaweed) |> 
  distinct() |> 
  mutate(label = c("(C)", "(B)", "(A)"))

library(lemon)

theme_pubr(base_family = "notosansjp") |> theme_set()

ggplot(alldata) + 
  geom_point(aes(x = ppfd, y = rate, color = seaweed),
             alpha = 0.5,
             show.legend = F) +
  geom_line(aes(x = ppfd, y = pred), data = pdata,
            size = 1.5,
            color = "white",
            show.legend = F) +
  geom_line(aes(x = ppfd, y = pred, color = seaweed), data = pdata,
            size = 1,
            show.legend = F) +
  geom_text(aes(x = 400, y = 20, label = label),
            data = panellabel) +
  scale_x_continuous(name = parse(text = xlabel),
                     limits = c(0, 400)) + 
  scale_y_continuous(name = parse(text = ylabel),
                     limits = c(0, 20)) + 
  scale_color_viridis_d(end = 0.8) +
  facet_rep_wrap(vars(seaweed), ncol = 1) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )

pdfname = "plot2.pdf"

ggsave(pdfname,
       width = 80, 
       height = 80*1.5, 
       units = "mm")

summary(m2)
# 非線形モデルには決定係数は存在しない！！ R^2


