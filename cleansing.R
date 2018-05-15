## ライブラリの読み込み
library(plyr)

## 元データ読み込み
fsp <- readRDS("/data/origin/fsp.RDS")
receipt <- readRDS("/data/origin/receipt.RDS")
store <- read.csv("/data/store.csv")

## receiptデータの前処理
# receipt型変換
receipt$システム年月日 <- as.Date(as.character(receipt$システム年月日), "%Y%m%d")
receipt$販売売上 <- as.numeric(receipt$販売売上)

# month 列を追加とそのレベル順の変更
receipt$month <- as.character(as.POSIXlt(receipt$システム年月日)$mon+1)
receipt$month <- factor(receipt$month, levels = c("7", "8", "9", "10", "11", "12","1", "2", "3", "4", "5", "6"))

# 列名を変更
receipt <- rename(receipt, replace=c("販売売上"="販売売上_税抜"))

# ユニークなレシートを特定する列を追加
receipt %.%
  select(システム年月日,店舗, レジ番号, レシート番号) %.%
  unique() -> receipt.id
receipt.id <- mutate(receipt.id, reID=as.factor(seq(1, nrow(receipt.id))))
receipt <- merge(receipt, receipt.id, by=c("システム年月日","店舗","レジ番号", "レシート番号")) # inner joinに書き換える

# 顧客CDの入ってない箇所（非会員）をnon-memberとする
receipt$顧客CD <- as.character(receipt$顧客CD)
receipt$顧客CD <- ifelse(receipt$顧客CD=="", "non-member", receipt$顧客CD)

# FSP利用フラグのNAを0に置換する
receipt$FSP利用フラグ <- ifelse(is.na(receipt$FSP利用フラグ), 0, 1)

## fspデータの前処理
# fsp型変換
fsp$顧客CD <- as.character(fsp$顧客CD)

# 本部推奨売価 → 税抜き価格と税込み価格へ
fsp$本部推奨売価_税抜 <- ifelse(fsp$年月 > 201403, fsp$本部推奨売価, round(fsp$本部推奨売価/1.05,0))
fsp$本部推奨売価_税込 <- ifelse(fsp$年月 > 201403, round(fsp$本部推奨売価*1.08,0), fsp$本部推奨売価)
fsp <- subset(fsp, select=-本部推奨売価)

# 特典値 → FSP価格_税込
fsp$FSP価格_税込 <- ifelse(fsp$年月 > 201312, fsp$本部推奨売価_税込-fsp$特典値, fsp$特典値)
fsp <- subset(fsp, select=-特典値)

# 月間数量のNAを0に置換する
fsp$月間数量 <- ifelse(is.na(fsp$月間数量), 0, fsp$月間数量)

# 表記のゆれを無くす
store <- plyr::rename(store, replace=c("storeID"="店舗"))

## Save
write.csv(fsp, "/data/fsp2.csv")
saveRDS(fsp, "/data/fsp2.RDS")
write.csv(receipt, "/data/receipt2.csv")
saveRDS(receipt, "/data/receipt2.RDS")
write.csv(store, "/data/store.csv")