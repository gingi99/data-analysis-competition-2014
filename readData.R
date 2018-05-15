# ========================================
# ライブラリ読み込み
# ========================================
library(ggplot2)
library(dplyr)
library(knitr)

# ========================================
# データ読み込み
# ========================================
fsp <- readRDS("/data/fsp2.RDS")
receipt <- readRDS("/data/receipt2.RDS")
store <- read.csv("/data/store.csv")

# データ基本概要
## 期間：2013.07〜2014.06
## お店：10店舗（詳細はstore.csv参照）

# ひとまず100万行くらい抽出してみる
fsp.sample <- fsp[1:1000000,]
receipt.sample <- receipt[1:1000000,]

# ========================================
# データ前処理
# ========================================

## Policy
## 1. 汎用的なものはclensing.Rの時点で実施していく（型変換，列名変更，列追加）
## 2. 個別に必要なもの（これは作業やっていけばなんとなく出てくる）


# ========================================
# 軽く分析
# ========================================

# 1.receiptデータ
## 日別の販売売上の推移　結果：7月上旬・10月中旬・2月上旬に売上が立ってる
datebreaks <- seq(as.Date("2013-7-1"), as.Date("2014-6-30"), by="1 month")
receipt.sample %.%
  select(システム年月日,販売売上_税抜) %.%
  group_by(システム年月日) %.%
  summarise(sum_uri=sum(販売売上_税抜)) %.%
  ggplot(aes(x=システム年月日, y=sum_uri)) + 
   geom_line() + scale_x_date(breaks=datebreaks) + 
   theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
   labs(title="日別の売上推移")

## 日別の販売売上の推移（POS大分類別）結果：わかりづらいな…
datebreaks <- seq(as.Date("2013-7-1"), as.Date("2014-6-30"), by="1 month")
receipt.sample %.%
  select(システム年月日,POS大分類名, 販売売上_税抜) %.%
  group_by(システム年月日, POS大分類名) %.%
  summarise(sum_uri=sum(販売売上_税抜)) %.%
  ggplot(aes(x=システム年月日, y=sum_uri, group=POS大分類名, color=POS大分類名)) + 
  geom_line() + scale_x_date(breaks=datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  labs(title="日別の売上推移（POS大分類別）")

## 月別の販売売上の棒グラフ（POS大分類別）結果：なんかよくわからんな
receipt.sample %.%
  select(month,POS大分類名, 販売売上_税抜) %.%
  group_by(month, POS大分類名) %.%
  summarise(sum_uri=sum(販売売上_税抜)) %.%
  ggplot(aes(x=POS大分類名, y=sum_uri, fill=POS大分類名)) + 
  geom_histogram(stat="identity") + facet_grid(month~.)

## 月別の販売売上の推移（POS大分類別）結果：大きく３グループくらいに分けれそう。
receipt.sample %.%
  select(month, POS大分類名, 販売売上_税抜) %.%
  group_by(month, POS大分類名) %.%
  summarise(sum_uri=sum(販売売上_税抜)) %.%
  ggplot(aes(x=month, y=sum_uri, group=POS大分類名, color=POS大分類名)) + 
  geom_line()

## 月別の販売売上の推移（POS中分類別）結果：中分類までいくと結構差がある商品がわかる
receipt.sample %.%
  select(month, POS大分類名, POS中分類名, 販売売上_税抜) %.%
  group_by(month, POS大分類名, POS中分類名) %.%
  summarise(sum_uri=sum(販売売上_税抜)) %.%
  ggplot(aes(x=month, y=sum_uri, group=POS中分類名, color=POS中分類名)) + 
  geom_line() + facet_grid(.~POS大分類名)

# 月別のPOS中分類の販売売上の推移　結果：わかりづらいので不要
#receipt.sample %.%
#  select(month,POS中分類名,販売売上_税抜) %.%
#  group_by(month,POS中分類名) %.%
#  summarise(sum_uri=sum(販売売上_税抜)) %.%
#  ggplot(aes(x=month, y=sum_uri, fill=POS中分類名)) + 
#    geom_histogram(stat="identity")

# POS中分類の販売売上ランキング
receipt.sample %.%
  select(month,POS中分類名,販売売上) %.%
  group_by(POS中分類名) %.%
  summarise(sum_uri=sum(販売売上)) %.%
  ggplot(aes(x=reorder(POS中分類名, sum_uri), y=sum_uri)) + 
   geom_histogram(stat="identity") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 顧客別の売上
# receipt.sample %.%
#  select(顧客CD, 販売売上) %.%
#  group_by(顧客CD) %.%
#  summarise(sum_uri=sum(販売売上)) %.%
#  arrange(desc(sum_uri)) %.%
#  filter(sum_uri>=0) -> aaa
#ggplot(aaa[-1,], aes(x=reorder(顧客CD,sum_uri), y=sum_uri)) + 
#  geom_histogram(stat="identity") + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))




