# JIREI10.TXTを読み込む
# syunsuke.fukuda@gmail.com
# 2019/10/18

library(tidyverse)

# 通常のRデータ化
read_jirei10 <- function(path, encoding = "cp932"){
  
  org_jirei <- read_jirei10_original(path, encoding = "cp932")

  org_jirei$`東経` = as.numeric(org_jirei$`東経`)/10000000
  org_jirei$`北緯` = as.numeric(org_jirei$`北緯`)/10000000
  
  # デコード
  org_jirei$`類型` <- decode_func(ruikei_vector)(org_jirei$`類型`)
  
  org_jirei$`方位` <- decode_func(direction_vector)(org_jirei$`方位`)
  org_jirei$`別街路１方位` <- decode_func(direction_vector)(org_jirei$`別街路１方位`)
  org_jirei$`別街路２方位` <- decode_func(direction_vector)(org_jirei$`別街路２方位`)
  org_jirei$`別街路３方位` <- decode_func(direction_vector)(org_jirei$`別街路３方位`)
  org_jirei$`駅方位` <- decode_func(direction_vector)(org_jirei$`駅方位`)
  
  org_jirei$`土地形状` <- decode_func(keijyou_vector)(org_jirei$`土地形状`)
  org_jirei$`街路接面状況` <- decode_func(setumen_vector)(org_jirei$`街路接面状況`)
  org_jirei$`用途地域` <- decode_func(youtochiiki_vector)(org_jirei$`用途地域`)

  
  
  # 役に立つ行のみのフィルタリング  
  ans_jirei <- org_jirei %>% select(-matches("X\\d+"))
  
  return(ans_jirei)

}


# 取り込みオリジナル
read_jirei10_original <- function(path, encoding = "cp932"){
  
  jire_data <- read_csv(file = path,
                        col_names = F,
                        locale = locale(encoding = encoding)) %>% 
    rename(`事例番号１` = X2,
           `事例番号２` = X3,
           `整理番号１` = X4,
           `整理番号２` = X5,
           `都道府県コード` = X6,
           `市町村コード` = X7,
           `分科会番号` = X8,
           `評価員番号` = X9,
           `公示年度` = X10,
           `調査年月日` = X11,
           `所在` = X13,
           `地番` = X14,
           `住居表示` = X15,
           `売主` = X17,
           `買主` = X18,
           `種別` = X19,
           `類型` = X20,
           `現況地目` = X22,
           `取引価格総額` = X23,
           `土地総額` = X24,
           `土地単価` = X25,
           `更地価格等補正` = X26,
           `建物総額` = X27,
           `建物単価` = X28,
           `相続路線価年度` = X36,
           `相続路線価価格` = X37,
           `建築時点元号` = X38,
           `建築時点年` = X39,
           `建築時点月` = X40,
           `建物構造` = X41,
           `地上階数` = X43,
           `地下階数` = X44,
           `建築面積` = X45,
           `延床面積` = X46,
           `建物用途` = X47,
           `契約日` = X48,
           `登記原因日` = X49,
           `事情` = X50,
           `方位` = X108,
           `幅員` = X109,
           `歩道` = X111,
           `舗装` = X112,
           `街路種類` = X113,
           `街路名称` = X114,
           `系統連続性` = X115,
           `鉄道路線` = X117,
           `駅名` = X118,
           `駅方位` = X119,
           `駅道路距離` = X120,
           `駅直線距離` = X121,
           `土地面積公簿` = X141,
           `土地面積実測` = X142,
           `私道面積` = X143,
           `間口` = X144,
           `奥行` = X145,
           `土地形状` = X152,
           `街路接面状況` = X153,
           `別街路１方位` = X154,
           `別街路１幅員` = X155,
           `別街路１舗装` = X156,
           `別街路１種類` = X157,
           `別街路２方位` = X158,
           `別街路２幅員` = X159,
           `別街路２舗装` = X160,
           `別街路２種類` = X161,
           `別街路３方位` = X162,
           `別街路３幅員` = X163,
           `別街路３舗装` = X164,
           `別街路３種類` = X165,
           `区域区分` = X169,
           `用途地域` = X170,
           `その他行政条件` = X171,
           `建ぺい率主` = X186,
           `建ぺい率基` = X187,
           `容積率主` = X188,
           `容積率基` = X189,
           `防火指定` = X190,
           `左特記事項１` = X191,
           `左特記事項２` = X192,
           `左特記事項３` = X193,
           `左特記事項４` = X194,
           `東経` = X209,
           `北緯` = X210
           )
  

    return(jire_data)  
}




#####################################################################
# デコード用のベクトルを定義
# ゼロから始まるようにベクトルを作る
#####################################################################

#  方位、駅方位
direction_vector <- 
  c(
    "その他",
    "東",
    "南",
    "西",
    "北",
    "南東",
    "南西",
    "北西",
    "北東"
    )

# 用途地域
youtochiiki_vector <- 
  c(
    "記載なし",
  rep("",3),  
  "近商",
  "商業",
  "工専",
  "工業",
  "準工",
  "","",
  "１低専","２低専",
  "１中専","２中専",
  "１住居","２住居",
  "準住居","田園"
  )

# 類型
ruikei_vector <- 
  c(
    "その他", 
    "更地",
    "底地",
    "", 
    "建付地",
    "貸家建付地" 
    )

# 形状
keijyou_vector <- 
  c(
    "記載なし",
    "正方形", 
    "ほぼ正方形",
    "長方形",
    "ほぼ長方形", 
    "台形",
    "ほぼ台形",
    "不整形", 
    "ほぼ整形",
    "袋地等" 
    )

# 街路接面状況
setumen_vector <-
  c(
    "",
    "角地", 
    "準角地",
    "二方路",
    "三方路", 
    "四方路",
    "中間画地",
    "無道路地"
    )


#####################################################################
# 汎用のデコード関数
# 0からはじまる整数の範囲と
# それに対応する文字列をマッピングして
# デコードする関数を戻す
#####################################################################
decode_func <- function(str_vec){

  tmp_func <- function(i){
    tmp_vector <- seq_along(str_vec)
    names(tmp_vector) <- str_vec
    
    if(is.na(i) | i == "" | is.null(i)){
      ans <-  ""
    }else{
      ans <- names(tmp_vector[i+1])
    }
    
    return(ans)
  }
  
  ans_func <- function(x){
    purrr::map_chr(x, tmp_func)
  }
  
  return(ans_func)
}










