# JIREI10.TXTのサンプルを作る
# syunsuke.fukuda@gmail.com
# 2019/10/17

library(tidyverse)

make_sample_jirei10 <- function(n, path = NULL){
  
  #############################
  # 主要な列の内容を作成
  #############################
  
  # 調査年月日

  inv_date <-  paste0("2019",
                 sprintf("%02d",sample(1:10,n, replace = T)), 
                 sprintf("%02d",sample(1:25,n, replace = T))) %>% ymd()

  # 登記年月日
  touki_date <-  inv_date - 
    months(sample(1:3,n,replace = T)) - 
    days(sample(1:20,n,replace = T))

  
  # 建築年等
  building_date <-  touki_date - 
    years(sample(1:10,n,replace = T)) - 
    months(sample(1:3,n,replace = T)) - 
    days(sample(1:20,n,replace = T))

  building_year <- year(building_date) - 1988
  building_month <- month(building_date)
  
  howold_building <- year(touki_date) - year(building_date)
  howold_building <- ifelse(howold_building < 0, 0, howold_building)
  
  # 類型
  ruikei <- sample(c(1,2,4,5,0),n,replace = T)
  
  # 用途地域
  youto <- sample(c(11:17,4:8),n,replace = T)
  
  kenpei_main <- ifelse(youto %in% c(4,5), 80,
                   ifelse(youto %in% c(11,12), 50, 60))
  
  youseki_main<- ifelse(youto %in% c(4,5), sample(c(300,400,500),n,replace = T),
                        ifelse(youto %in% c(11,12), 100, 200))
  
  
  

  # ダミーの事例番号
  jirei_num1 <- sample(1:400,n,replace = T) +1190000 
  jirei_num2 <- sample(1:100000,n,replace = F) 
  
  # 所在名称
  address <- 
    paste0(sample(c("山手町", "下手町", "北町", "東町", "西町", "南町"),n,replace = T), 
           sample(1:5,n,replace = T)) 
  
  # 土地の面積
  land_size <- sample(3000:200000,n,replace = T)/100
  
  # 間口奥行
  maguchi <- land_size / 1.5 %>% round(2)
  okuyuki <- land_size / maguchi %>% round(2)

  # 土地単価と総額
  land_tanka   <- sample(80000:300000,n,replace = T) 
  land_sougaku <- land_tanka * land_size %>% round(0)
  
  
  
  # 建物の階層
  building_floor <- sample(1:4,n,replace = T)
  
  
  # 建物の建築面積と延べ床面積
  one_floor_size <- land_size * 60
  total_floor_size <- one_floor_size * building_floor %>% as.numeric()
  
  # 建物の単価と総額
  building_tanka <- sample(150000:250000,n,replace = T)
  building_tanka <- building_tanka * (40 - howold_building)/40
  building_sougaku <- building_tanka * total_floor_size %>% round(0)
  total_price <- land_sougaku + building_sougaku
  
  # 建物のありなしな判定
  building_floor <- ifelse(ruikei %in% c(1,2,0), "", building_floor)
  one_floor_size <- ifelse(ruikei %in% c(1,2,0), "", one_floor_size)
  total_floor_size <- ifelse(ruikei %in% c(1,2,0), "", total_floor_size)
  building_tanka <- ifelse(ruikei %in% c(1,2,0), "", building_tanka)
  building_sougaku <- ifelse(ruikei %in% c(1,2,0), "", building_sougaku)
  total_price <- ifelse(ruikei %in% c(1,2,0), land_sougaku, total_price)
  building_date <- ifelse(ruikei %in% c(1,2,0), "", building_date)
  building_year <- ifelse(ruikei %in% c(1,2,0), "", building_year)
  building_month <- ifelse(ruikei %in% c(1,2,0), "", building_month)
  
  
  # 駅距離(道路、直線)
  st_length <- sample(c(20:1500,rep(200:500,20)),n,replace = T)
  st_s_length <- st_length * 0.8 %>% round(0)
    

  #############################
  # データフレームの作成
  #############################


    ans <- data.frame(
    X1 = rep("",n),

    # 事例番号関連
    X2 = jirei_num1,
    X3 = jirei_num2,
    X4 = jirei_num1,
    X5 = jirei_num2,
    
    # 都道府県(大阪府)
    X6 = rep(27,n),

    # 市町村（四条畷市）
    X7 = rep(229,n),

    # 分科会
    X8 = rep("09",n),

    # 作成者
    X9 = rep("XXXXXX",n),

    # 公示年度
    X10 = rep(2019,n),

    # 調査年月日
    X11 = format(inv_date,"%Y%m%d"),
    X12 = rep("",n),

    # 所在、地番
    X13 = paste0(address,"丁目"),
    X14 = paste0(sample(1:100,n,replace = T),"番",sample(1:100,n,replace = T)),
    
    # 住居表示
    X15 = paste0(address,"-",sample(1:10,n,replace = T),"-",sample(1:20,n,replace = T)),
    X16 = rep("",n),
    
    # 売主買主
    X17 = sample(c("個人","法人"),n,replace = T),
    X18 = sample(c("個人","法人"),n,replace = T),
    
    # 土地種別
    X19 = sample(c(61:66,71,72,74:77,81,84,85,41,0),n,replace = T),
    
    # 類型
    X20 = ruikei,
    X21 = rep("",n),

    # 現況地目
    X22 = rep("宅地",n),


    # 取引総額
    X23 = total_price,

    # 土地総額、単価
    X24 = land_sougaku,
    X25 = land_tanka,
    X26 = rep(0,n),

    # 建物総額、単価
    X27 = building_sougaku,
    X28 = building_tanka,


    X29 = rep("",n),
    X30 = rep("",n),
    X31 = rep("",n),
    X32 = rep("",n),
    X33 = rep("",n),
    X34 = rep("",n),
    X35 = rep("",n),
    
    # 相続路線価年度、路線額
    X36 = rep(2019,n),
    X37 = round(land_tanka * 0.8,-3),

    # 建物建築時点元号（2:平成）、年、月
    X38 = rep(2,n),
    X39 = building_year,
    X40 = building_month,

    # 建物構造
    X41 = sample(1:4,n,replace = T),
    X42 = rep("",n),

    # 地上階数、地下階数
    X43 = building_floor,
    X44 = rep(0,n),

    # 建築面積、延べ床面積
    X45 = one_floor_size,
    X46 = total_floor_size,

    # 建物種類
    X47 = sample(c(rep("居宅",30),"共同住宅","店舗", "事務所"),n,replace = T),
    
    # 登記原因日、契約日
    X48 = paste0("2019",
                 sprintf("%02d",sample(1:12,n,replace = T)), 
                 sprintf("%02d",sample(1:25,n, replace = T))),
    X49 = paste0("2019",
                 sprintf("%02d",sample(1:12,n,replace = T)), 
                 sprintf("%02d",sample(1:25,n, replace = T))),

    # 事情
    X50 = sample(c(0,1),n,replace = T),

    # 不明
    X51 = rep("",n),
    X52 = rep("",n),
    X53 = rep("",n),
    X54 = rep(100,n),
    X55 = rep(100,n),
    X56 = rep(0,n),
    X57 = rep(1,n),
    X58 = rep(100,n),
    X59 = rep(100,n),
    X60 = rep(0,n),
    X61 = rep(0,n),
    X62 = rep("",n),
    X63 = rep("",n),
    X64 = rep("",n),
    X65 = rep("",n),
    X66 = rep("",n),
    X67 = rep("",n),
    X68 = rep("",n),
    X69 = rep("",n),
    X70 = rep("",n),
    X71 = rep("",n),
    X72 = rep("",n),
    X73 = rep("",n),
    X74 = rep("",n),
    X75 = rep("",n),
    X76 = rep("",n),
    X77 = rep("",n),
    X78 = rep("",n),
    X79 = rep("",n),
    X80 = rep("",n),
    X81 = rep("",n),
    X82 = rep("",n),
    X83 = rep("",n),
    X84 = rep("",n),
    X85 = rep("",n),
    X86 = rep("",n),
    X87 = rep("",n),
    X88 = rep("",n),
    X89 = rep("",n),
    X90 = rep("",n),
    X91 = rep("",n),
    X92 = rep("",n),
    X93 = rep("",n),
    X94 = rep("",n),
    X95 = rep("",n),
    X96 = rep("",n),
    X97 = rep("",n),
    X98 = rep("",n),
    X99 = rep("",n),
    X100 = rep("",n),
    X101 = rep("",n),
    X102 = rep("",n),
    X103 = rep("",n),
    X104 = rep("",n),
    X105 = rep("",n),
    X106 = rep("",n),
    X107 = rep("",n),
    
    #/////////////
    # 街路条件
    #/////////////
    
    # 方位、幅員
    X108 = sample(0:8,n,replace = T),
    X109 = sample(c(rep(4:15,10),16:25),n,replace = T),
    X110= rep("",n),

    # 歩道、舗装、種類、名称、系統連続性
    X111 = sample(0:1, n, replace = T),
    X112 = sample(c(rep(1,20),2,3,4),n,replace = T),
    X113 = sample(c(rep(31,20),10,23),n,replace = T),
    X114 = paste0("第",sample(1:10,n,replace = T),"号線"),
    X115 = rep(4,n),
    X116 = rep("",n),


    #/////////////
    # 交通条件
    #/////////////
    
    # 路線、駅
    X117 = sample(c("猫鉄道","カピバラ鉄道","犬鉄道"),n,replace = T),
    X118 = sample(c("亜駅","伊駅","宇駅","尾駅"),n,replace = T),
    
    # 方位、道路距離、直線距離
    X119 = sample(1:8,n,replace = T),
    X120 = st_length,
    X121 = st_s_length,


    X122 = rep("",n),
    X123 = rep("",n),
    X124 = rep("",n),
    X125 = rep("",n),
    X126 = rep("",n),
    X127 = rep("",n),
    X128 = rep("",n),
    X129 = rep("",n),
    X130 = rep("",n),
    X131 = rep("",n),
    X132 = rep("",n),
    X133 = rep("",n),
    X134 = rep("",n),
    X135 = rep("",n),
    X136 = rep("",n),
    X137 = rep("",n),
    X138 = rep("",n),
    X139 = rep("",n),
    X140 = rep("",n),

    
    #/////////////
    # 画地条件
    #/////////////
    
    # 公簿面積、実測面積
    X141 = land_size,
    X142 = rep(0,n),
    X143 = rep(0,n),

    # 間口、奥行
    X144 = maguchi,
    X145 = okuyuki,

    X146 = rep("",n),
    X147 = rep("",n),
    X148 = rep("",n),
    X149 = rep("",n),
    X150 = rep("",n),
    X151 = rep("",n),

    # 形状、接面状況
    X152 = sample(c(rep(c(1:4,8),10),7,9,0),n,replace=T),
    X153 = sample(c(rep(c(6),20),1,3,4,5),n,replace=T),
    
    # その他道路その１
    X154 = rep("",n),
    X155 = rep("",n),
    X156 = rep("",n),
    X157 = rep("",n),
    
    # その他道路その１
    X158 = rep("",n),
    X159 = rep("",n),
    X160 = rep("",n),
    X161 = rep("",n),

    # その他道路その１
    X162 = rep("",n),
    X163 = rep("",n),
    X164 = rep("",n),
    X165 = rep("",n),
    
    X166 = rep("",n),
    X167 = rep("",n),
    X168 = rep("",n),
    
    #/////////////
    # 画地条件
    #/////////////
    
    # 区域区分
    X169 = rep(1,n),

    # 用途地域、その他規制
    X170 = youto,
    X171 = rep("",n),

    X172 = rep("",n),
    X173 = rep("",n),
    X174 = rep("",n),
    X175 = rep("",n),
    X176 = rep("",n),
    X177 = rep("",n),
    X178 = rep("",n),
    X179 = rep("",n),
    X180 = rep("",n),
    X181 = rep("",n),
    X182 = rep("",n),
    X183 = rep("",n),
    X184 = rep("",n),
    X185 = rep("",n),

    # 建蔽率（主、基準）
    X186 = kenpei_main,
    X187 = kenpei_main,
    
    # 容積率（主、基準）
    X188 = youseki_main,
    X189 = youseki_main,
    
    # 防火指定
    X190 = sample(0:2,n,replace = T),

    # 特記事項
    X191 = rep("",n),
    X192 = rep("",n),
    X193 = rep("",n),
    X194 = rep("",n),


    X195 = rep("",n),
    X196 = rep("",n),
    X197 = rep("",n),
    X198 = rep("",n),
    X199 = rep("",n),
    X200 = rep("",n),
    X201 = rep("",n),
    X202 = rep("",n),
    X203 = rep("",n),
    X204 = rep("",n),
    X205 = rep("",n),
    X206 = rep("",n),
    X207 = rep("",n),
    X208 = rep("",n),

    # 東経、北緯
    X209 = (sample(3854:41687,n,replace = T) + 135600000) * 10 ,
    X210 = (sample(30094:47115,n,replace = T) + 34700000) * 10

  )
  
  if(!is.null(path)){
    write.table(ans, file = "SAMPLE_JIREI10.TXT", append = FALSE, quote = TRUE, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = c("escape", "double"),
                fileEncoding = "cp932")
    }
  
  return(ans)
}
