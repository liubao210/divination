library(dplyr)
library(purrr)


# 运用先天五行八卦，启示吉凶之法
bagua = data.frame(
  index = c(1:8),
  name = c("乾", "兑", "离" ,"震", "巽", "坎", "艮", "坤"),
  pic = c("III", "IIO", "IOI", "IOO", "OII", "OIO", "OOI", "OOO"),
  wuxing_index = c(1, 1, 4, 3, 3, 2, 5, 5),
  wuxing_name = c("金", "金" , "火", "木", "木", "水", "土", "土")
)

# 看谁的时候就把谁当作12345的起始位置，然后计算index的差代表吉凶
wuxing = data.frame(
  wuxing = c("金" ,"水" ,"木" ,"火", "土"),
  `金` = c(1, 2, 3, 4, 5),
  `水` = c(5, 1, 2, 3, 4),
  `木` = c(4, 5, 1, 2, 3),
  `火` = c(3, 4, 5, 1, 2),
  `土` = c(2, 3, 4, 5, 1)
)

# 吉凶
orders = data.frame(
  value = c(1:5),
  means = c("小吉", "小凶", "小吉", "大吉", "大凶")
)


get_lucky_num = function(){
  sample(1:8, 2, replace = T)
}


good_luck <- function(index_1 = NULL, index_2 = NULL) {
  if(is.null(index_1)){
    index_1 = sample(1:8, 1, replace = T)
  }

  if(is.null(index_2)){
    index_2 = sample(1:8, 1, replace = T)
  }

  i1 = min(
    if_else(index_1 %% 8 == 0, 8, index_1 %% 8),
    if_else(index_2 %% 8 == 0, 8, index_2 %% 8)
    )
  i2 = max(
    if_else(index_1 %% 8 == 0, 8, index_1 %% 8),
    if_else(index_2 %% 8 == 0, 8, index_2 %% 8)
    )

  # 变爻
  i_change = if_else((i1 + i2) %% 6 == 0, 6, (i1 + i2) %% 6)
  # 体、用
  i_t = if_else(i_change <= 3, i1, i2)
  i_y = if_else(i_change <= 3, i2, i1)
  # 下互、上互
  pic_ty = (paste0(
    bagua$pic[bagua$index == i_t],
    bagua$pic[bagua$index == i_y]
  ) |>
    strsplit(NULL))[[1]] |>
    rev()
  i_h1 = bagua$index[bagua$pic == paste(pic_ty[2:4] |> rev(), collapse = "")]
  i_h2 = bagua$index[bagua$pic == paste(pic_ty[3:5] |> rev(), collapse = "")]
  # 变
  pic_ty[i_change] = if_else(pic_ty[i_change] == "O", "I", "O")
  i_b = bagua$index[bagua$pic == if_else(
    i_change <= 3,
    paste(pic_ty[1:3] |> rev(), collapse = ""),
    paste(pic_ty[4:6] |> rev(), collapse = ""),
    )]

  # 所有的卦
  gua = c(i_t, i_y, i_h1, i_h2, i_b)
  # 所有卦对应的五行
  wuxing_gua = gua |>
    map(\(.){bagua$wuxing_name[bagua$index == .]}) |>
    unlist()
  # 体与用互互变的关系
  wuxing_gua_tmp = wuxing_gua |>
    map(\(.){wuxing[[wuxing_gua[1]]][wuxing$wuxing == .]}) |>
    unlist()

  results = orders$means[(wuxing_gua_tmp[-1])]

  print(paste0("报数: ", index_1 |> as.character(), "|", index_2 |> as.character(), ""))
  print(paste0("卦象: ", paste(bagua$name[gua], collapse = ",")))
  get_summary(results)
}


get_summary = function(results){
  explain = list(
    # 小凶
    `小凶` = c("进展不顺利", "有小的波折或阻碍", "稍有难度", "难以大成, 需另辟蹊径"),
    # 大凶
    `大凶` = c("极其不利于取得好结果", "十分艰难困苦", "艰辛挫折非常不幸", "可能遭受较大损失", "需要小心奸臣小人"),
    # 小吉
    `小吉` = c("比较顺利，可以继续尝试", "比较有利于取得成就", "得到应有的回报", "值得坚持"),
    # 大吉
    `大吉` = c("非常顺利", "有贵人出现", "能获得超出努力的回报", "获得成就水到渠成")
  )

  e1 = explain[[results[1]]] |> sample(1)
  explain[[results[1]]] = setdiff(explain[[results[1]]], e1)

  e2 = explain[[results[2]]] |> sample(1)
  explain[[results[2]]] = setdiff(explain[[results[2]]], e2)

  e3 = explain[[results[3]]] |> sample(1)
  explain[[results[3]]] = setdiff(explain[[results[3]]], e3)

  e4 = explain[[results[4]]] |> sample(1)
  explain[[results[4]]] = setdiff(explain[[results[4]]], e4)

  message = paste0(
    sample(c("开始时", "一开始", "起初"), 1), e1,
    if_else(gsub("大|小", "", results[2]) == gsub("大|小", "", results[1]), ", 继续发展会依旧", ", 随着时间的推移, 会"),
    e2,
    if_else(gsub("大|小", "", results[3]) == gsub("大|小", "", results[2]), "。中期还会", "。但进入中期会有变化, 此时"),
    e3,
    if_else(gsub("大|小", "", results[4]) == gsub("大|小", "", results[3]), "。最终", "。然坚持到后期,"),
    e4
  )

  message
}






