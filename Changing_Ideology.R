library(readxl)
library(tidyverse)

congress <- read_csv("congress.csv") #csvファイルの読み込み

#第80議会のデータを抽出して記述統計
cong_80 <- congress %>% filter(congress == 80)
cong_80_rep <- cong_80 %>% filter(party == "Republican")
summary(cong_80_rep$dwnom1)


#第112議会のデータを抽出して記述統計
cong_112 <- congress %>% filter(congress == 112)
cong_112_rep <- cong_112 %>% filter(party == "Republican")
summary(cong_112_rep$dwnom1)

#ヴァイオリンプロットを作成
cong_80_112_rep <- congress %>% filter(congress == 80 & party == "Republican" | congress == 112 & party == "Republican")
ggplot(cong_80_112_rep, aes(x=factor(congress), dwnom1)) +
  geom_violin() + geom_boxplot(fill = "grey", width=0.1) +
  labs(title = "共和党議員の経済イデオロギーの変遷", x="congress", y="eco_ideology")

# 1と同様にまず該当するケースだけ取り出してデータフレームを作成する。大統領を除外すること。

#第100議会の共和党議員を抽出して相関係数を求める
cong_100_rep <- congress %>% filter(congress == 100 & party == "Republican" & district != 0)
with(cong_100_rep, cor(dwnom1,dwnom2))

#第100議会の民主党議員を抽出して相関係数を求める
cong_100_dem <- congress %>% filter(congress == 100 & party == "Democrat" & district != 0)
with(cong_100_dem, cor(dwnom1,dwnom2))

# 大統領だけを取り出して階層クラスター分析をおこなう
# dist関数でケrースの類似度(距離)を測定する
# hclust(ケースの類似度データ, "方法")
# 今回はウォード法を使うのでward.D2と指定する

#大統領を取り出してイデオロギー変数のみ抽出
cong_president <- congress %>% filter(district == 0) %>% select(dwnom1,dwnom2)

#各ケースの類似度を測る
dis_president <- dist(cong_president)

#ウォード法を用いて2次元平面に階層クラスタリング、デンドログラム作成
hc_president <- hclust(dis_president, "ward.D2")
plot(hc_president)

