# JIREI10のRデータからマップオブジェクトを作る際に
# 便利になるルーチン
# syunsuke.fukuda@gmail.com
# 2019/10/18

library(tidyverse)


##########################################################
# 桁区切り文字を返す
##########################################################
keta_str <- function(v, s = "㎡", nsmall = 0){
  paste0(format(as.numeric(v), big.mark = ",",nsmall = nsmall, scientific = F), s)
}


##########################################################
# 表示用面積作成ルーチン
##########################################################
prt_land_size <- function(jirei10_df){
  
  # 個別の関数を定義
  sub_f <- function(koubo, jissoku, sidou){
    
    if(jissoku != 0){
      ans <- paste0("実測",keta_str(jissoku,nsmall = 2))
      if(sidou != 0){
        ans <- paste0("有効面積",keta_str(jissoku - sidou, nsmall = 2),
                      "(実測",keta_str(jissoku, nsmall = 2),
                      "うち私道",keta_str(sidou,nsmall = 2),
                      ")")
      }
      
    }else{
      ans <- paste0("公簿",keta_str(koubo, nsmall = 2))
      if(sidou != 0){
        ans <- paste0("有効面積",keta_str(koubo - sidou, nsmall = 2),
                      "(公簿",keta_str(koubo, nsmall = 2),
                      "うち私道",keta_str(sidou,nsmall = 2),
                      ")")
      }
    }
    return(ans)
  }
  
  ans <- pmap(list(jirei10_df$`土地面積公簿`,
                   jirei10_df$`土地面積実測`,
                   jirei10_df$`私道面積`),
              sub_f) %>% 
    unlist()
  
  return(ans)
  
}  

tentf <- function(o){
  return(o$x[2])
}

##########################################################
# add_my_marker
##########################################################
add_my_marker <- function(map_obj, .data = NULL){
  
  # 事例データを抜き出し
  if(!is.null(.data)){
    jirei10_df <- .data
  }else{
    jirei10_df <- attr(map_obj$x, "leafletData")
  }

  # addMarkerレイヤーを適用
  ans_obj <- 
    map_obj %>% addMarkers(lng = ~`東経`,
                           lat = ~`北緯`,
                           popup = my_popup_def(jirei10_df),
                           clusterOptions = markerClusterOptions(),
                           label = ~keta_str(`土地単価`,"円／㎡"),
                           labelOptions =  labelOptions(noHide = T)
                           )
  
  return(ans_obj)
}




##########################################################
# ポップアップの中身を作る例
##########################################################
my_popup_def <- function(jirei10_df){
  
  popup_str <- paste(sep = "<br>",
                     paste0(jirei10_df$`整理番号１`,"-",jirei10_df$`整理番号２`),
                     
                     paste("単価:", keta_str(jirei10_df$`土地単価`,"円／㎡"), "(",jirei10_df$`類型`,")"),
                     paste("所在:", jirei10_df$`所在`),
                     paste("面積:", prt_land_size(jirei10_df)),
                     paste("道路:", jirei10_df$`方位`, keta_str(jirei10_df$`幅員`,"m")),
                     paste("駅　:", jirei10_df$`駅名`, keta_str(jirei10_df$`駅道路距離`,"m")),
                     paste("用途:", 
                           jirei10_df$`用途地域`, 
                           paste0("(主", 
                                  jirei10_df$`建ぺい率主`, "/", jirei10_df$`容積率主`,
                                  "、基",
                                  jirei10_df$`建ぺい率基`, "/", jirei10_df$`容積率基`,
                                  ")")),
                     paste("時点:", jirei10_df$`登記原因日`))
  
  return(popup_str)
  
}




