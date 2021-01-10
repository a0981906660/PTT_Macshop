library(rvest)
library(httr)
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(sjmisc) #dummies
#robust s.e.
#https://www.brodrigues.co/blog/2018-07-08-rob_stderr/
library(robustbase)
library(tidyverse)
library(sandwich)
library(lmtest)
library(modelr)
library(broom)

# environment
getwd()
wd_path <- "/Users/Andy 1/Google 雲端硬碟 (r08323004@g.ntu.edu.tw)/0 Semesters/109-1/一234 資料科學/0_Final_Project/PTT_Macshop"
setwd(wd_path)
load("2_preprocessing/processed/tidy_df.rda")
load("2_preprocessing/processed/iphone6s_df.rda")


##################################
# 以下開始實作嘗試抓取面交地點，以進行區位分析以及視覺化
# 首先先使用地區的常用關鍵字嘗試抓取所有文章的地址
loc <- regex("台北|臺北|雙北|新北|北市|基隆|宜蘭|林口|中彰|新竹|桃園|楊梅|苗栗|桃竹|中壢|竹苗|彰化|台中|臺中|高雄|臺南|台南|屏東|高屏|雲林|嘉義|樹林|北北基|竹北|南投|北部|竹中苗|竹南|花蓮|台東|臺東|板橋|金門|中部|南部|員林|蘆洲|內湖|台大|興大|木柵|大安|中山|公館|士林|土城|雲嘉|南港|汐止|北桃|永和|中和|忠孝復興|南京敦化|三重|基北|東湖|潭子|七張|忠孝新生|北車|新莊|古亭|大里|忠孝敦化|國父紀念館|西門|迴龍|平鎮|松江南京|萬隆|善導寺|實踐大學|板南|亞東醫院|頂埔|六張犁|三峽|南崁|斗六|捷運|鳳山|南高|八德路|新蘆線|永安市場|大寮|埔里|行天宮|石牌")
loc_all <- regex("全國|全省|台灣|全台|全臺")

# 抓取位置
iphone6s_df %>%
    mutate(loc_content = str_extract(pcontent, loc)) %>%
    mutate(loc_title = str_extract(ptitle, loc)) %>%
    mutate(loc_all = ifelse(str_detect(ptitle, loc_all)|str_detect(pcontent, loc_all), 1, 0)) %>%
    # filter(is.na(loc_content)&is.na(loc_title)&loc_all != 1) %>%
    filter(is.na(loc_content)&is.na(loc_title)&loc_all == 0) %>%
    View
# 只有145個貼文是沒有寫位置的

loc_df <- iphone6s_df %>%
    mutate(loc_content = str_extract(pcontent, loc)) %>%
    mutate(loc_title = str_extract(ptitle, loc)) %>%
    mutate(loc_Taiwan = ifelse(str_detect(ptitle, loc_all)|str_detect(pcontent, loc_all), 1, 0))

# 現在把22個縣市的關鍵字分門別類，從loc_df裡面做出dummy variable

### 直轄市
Taipei <- regex("台北|臺北|雙北|北市|北北基|北部|台大|木柵|大安|中山|公館|士林|南港|北桃|忠孝復興|南京敦化|基北|東湖|忠孝新生|北車|古亭|忠孝敦化|國父紀念館|西門|平鎮|松江南京|萬隆|善導寺|實踐大學|板南|六張犁|八德路|新蘆線|行天宮|石牌")
New_Taipei <- regex("雙北|新北|林口|樹林|北北基|板橋|蘆洲|土城|汐止|北桃|永和|中和|三重|基北|七張|新莊|古亭|松江南京|萬隆|善導寺|實踐大學|板南|亞東醫院|頂埔|三峽|新蘆線|永安市場|行天宮")
Taoyuan <- regex("桃園|楊梅|桃竹|中壢|北部|北桃|平鎮|南崁")
Taichung <- regex("中彰|台中|臺中|竹中苗|竹南|中部|興大|潭子|大里")
Tainan <- regex("臺南|台南|南高")
Kaohsiung <- regex("高雄|高屏|南部|南高|大寮|鳳山")

### 非直轄
Yilan <- regex("宜蘭")
Hsinchu <- regex("新竹|桃竹|竹苗|竹北|竹中苗")
Miaoli <- regex("苗栗|竹苗|竹中苗|竹南")
Changhua <- regex("中彰|彰化|員林")
Nantou <- regex("南投|埔里")
Yunlin <- regex("雲林|雲嘉|斗六|斗南|虎尾")
Chiayi <- regex("嘉義|雲嘉")
Pingtung <- regex("屏東|高屏|南部")
Taitung <- regex("台東|臺東|蘭嶼|綠島")
Hualien <- regex("花蓮")
Penghu <- regex("澎湖")
Keelung <- regex("基隆|北北基|基北")
Kinmen <- regex("金門")
# 基隆市併在基隆，嘉義市併在嘉義，新竹市併在新竹，連江縣沒文章




# 建立22個dummy variables
loc_tidy <- loc_df %>%
    mutate(loc_Taipei = ifelse(str_detect(ptitle, Taipei)|str_detect(pcontent, Taipei)|(!str_detect(loc_title, "高雄")&(str_detect(loc_content, "捷運"))), 1, 0)) %>%
    # 檢查是不是如果捷運同時有高雄，loc_Taipei就等於0 filter(loc_content == "捷運") %>%
    mutate(loc_New_Taipei = ifelse(str_detect(ptitle, New_Taipei)|str_detect(pcontent, New_Taipei), 1, 0)) %>%
    mutate(loc_Taoyuan = ifelse(str_detect(ptitle, Taoyuan)|str_detect(pcontent, Taoyuan), 1, 0)) %>%
    mutate(loc_Taichung = ifelse(str_detect(ptitle, Taichung)|str_detect(pcontent, Taichung), 1, 0)) %>%
    mutate(loc_Tainan = ifelse(str_detect(ptitle, Tainan)|str_detect(pcontent, Tainan), 1, 0)) %>%
    mutate(loc_Kaohsiung = ifelse(str_detect(ptitle, Kaohsiung)|str_detect(pcontent, Kaohsiung), 1, 0)) %>%
    # 以上為直轄市
    mutate(loc_Yilan = ifelse(str_detect(ptitle, Yilan)|str_detect(pcontent, Yilan), 1, 0)) %>%
    mutate(loc_Hsinchu = ifelse(str_detect(ptitle, Hsinchu)|str_detect(pcontent, Hsinchu), 1, 0)) %>%
    mutate(loc_Miaoli = ifelse(str_detect(ptitle, Miaoli)|str_detect(pcontent, Miaoli), 1, 0)) %>%
    mutate(loc_Changhua = ifelse(str_detect(ptitle, Changhua)|str_detect(pcontent, Changhua), 1, 0)) %>%
    mutate(loc_Nantou = ifelse(str_detect(ptitle, Nantou)|str_detect(pcontent, Nantou), 1, 0)) %>%
    mutate(loc_Yunlin = ifelse(str_detect(ptitle, Yunlin)|str_detect(pcontent, Yunlin), 1, 0)) %>%
    mutate(loc_Chiayi = ifelse(str_detect(ptitle, Chiayi)|str_detect(pcontent, Chiayi), 1, 0)) %>%
    mutate(loc_Pingtung = ifelse(str_detect(ptitle, Pingtung)|str_detect(pcontent, Pingtung), 1, 0)) %>%
    mutate(loc_Taitung = ifelse(str_detect(ptitle, Taitung)|str_detect(pcontent, Taitung), 1, 0)) %>%
    mutate(loc_Hualien = ifelse(str_detect(ptitle, Hualien)|str_detect(pcontent, Hualien), 1, 0)) %>%
    mutate(loc_Penghu = ifelse(str_detect(ptitle, Penghu)|str_detect(pcontent, Penghu), 1, 0)) %>%
    mutate(loc_Keelung = ifelse(str_detect(ptitle, Keelung)|str_detect(pcontent, Keelung), 1, 0)) %>%
    mutate(loc_Kinmen = ifelse(str_detect(ptitle, Kinmen)|str_detect(pcontent, Kinmen), 1, 0)) %>%
    select(-loc_content, -loc_title)

loc_tidy[is.na(loc_tidy)] <- 0

# 得到台灣各地的po文密度
apply(loc_tidy[-22:-1], 2, sum)  # 2表對行加總



# 參考另一篇文章
# install.packages('sf')
library(sf)
# install.packages('raster')
library(raster)
library(dplyr)
library(stringr) # for working with strings (pattern matching)
library(tidyr)   # for unite() and separate()

library(spData)

taiwan.map <- st_read("gadm36_TWN_shp/gadm36_TWN_2.shp")
taiwan.map
print(taiwan.map, n = 22)

plot(st_geometry(taiwan.map))

# 將統計資料與地圖結合
my.taiwan.map <- taiwan.map[c("NL_NAME_2", "geometry")]
my.taiwan.map$NL_NAME_2 <- as.character(my.taiwan.map$NL_NAME_2)
head(my.taiwan.map)

# 將統計資料重整，建立一個新的column
apply(loc_tidy[-22:-1], 2, sum)  # 2表對行加總
my.taiwan.map$NL_NAME_2
num_post <- c(4, 0, 1700, 3205, 2581, 1457, 7019,
              369, 278, 278, 1511, 1511, 26, 241,
              153, 146, 3, 240, 5, 1507, 91,
              110)
my.taiwan.map.data <- my.taiwan.map %>%
    mutate(num_post = num_post)
ggplot(data = my.taiwan.map.data) +
    geom_sf(aes(fill = num_post))

# 最終版：可互動視覺化
# install.packages("mapview")
library(mapview)
mapview(my.taiwan.map.data["num_post"],alpha = 0.9,burst = TRUE,legend = TRUE)
library(fields)
mapview(my.taiwan.map.data["num_post"], col.regions = tim.colors(16))


# 做平均價格
# 先根據年份做dummy
loc_tidy <- loc_tidy %>%
    mutate(time_2020 = ifelse(str_detect(ptime, "2020"), 1, 0)) %>%
    mutate(time_2019 = ifelse(str_detect(ptime, "2019"), 1, 0)) %>%
    mutate(time_2018 = ifelse(str_detect(ptime, "2018"), 1, 0)) %>%
    mutate(time_2017 = ifelse(str_detect(ptime, "2017"), 1, 0)) %>%
    mutate(time_2016 = ifelse(str_detect(ptime, "2016"), 1, 0)) %>%
    mutate(time_2015 = ifelse(str_detect(ptime, "2015"), 1, 0))

my.taiwan.map$NL_NAME_2
# 得到各地2020年的均價
average_price_2020 <- c(mean(loc_tidy$avg_price[loc_tidy$loc_Kinmen==1&loc_tidy$time_2020==1]),
                   0, mean(loc_tidy$avg_price[loc_tidy$loc_Kaohsiung==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_New_Taipei==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taichung==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Tainan==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taipei==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Changhua==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hualien==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Keelung==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Miaoli==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Nantou==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Penghu==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Pingtung==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taitung==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taoyuan==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Yilan==1&loc_tidy$time_2020==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Yunlin==1&loc_tidy$time_2020==1])
                   )

average_price_2015 <- c(mean(loc_tidy$avg_price[loc_tidy$loc_Kinmen==1&loc_tidy$time_2015==1]),
                        0, mean(loc_tidy$avg_price[loc_tidy$loc_Kaohsiung==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_New_Taipei==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Taichung==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Tainan==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Taipei==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Changhua==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Hualien==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Keelung==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Miaoli==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Nantou==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Penghu==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Pingtung==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Taitung==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Taoyuan==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Yilan==1&loc_tidy$time_2015==1]),
                        mean(loc_tidy$avg_price[loc_tidy$loc_Yunlin==1&loc_tidy$time_2015==1])
)

average_price <- c(mean(loc_tidy$avg_price[loc_tidy$loc_Kinmen == 1]),
                   0,
                   mean(loc_tidy$avg_price[loc_tidy$loc_Kaohsiung == 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_New_Taipei== 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taichung == 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Tainan == 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taipei == 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Changhua ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Chiayi ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hsinchu ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Hualien ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Keelung == 1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Miaoli ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Nantou ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Penghu ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Pingtung ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taitung ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Taoyuan ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Yilan ==1]),
                   mean(loc_tidy$avg_price[loc_tidy$loc_Yunlin ==1])
                   )
my.taiwan.map.data <- my.taiwan.map.data%>%
    mutate(average_price_2020 = average_price_2020) %>%
    mutate(average_price_2015 = average_price_2015) %>%
    mutate(average_price = average_price)
    
mapview(my.taiwan.map.data["average_price_2020"], col.regions =tim.colors(16))
mapview(my.taiwan.map.data["average_price_2015"], col.regions = tim.colors(16))
mapview(my.taiwan.map.data["average_price"], col.regions = tim.colors(16))

### 以台北市為1
### 以橫跨n縣市作為neighbor，賦予距離n+1
# 北部
dis_Taipei = 1
dis_New_Taipei = 2
dis_Keelung = 2
# 先走東部
dis_Yilan = 3
dis_Hualien = 4
dis_Taitung = 5
# 再走西部
dis_Taoyuan = 3
dis_Hsinchu = 3
dis_Miaoli = 3
dis_Taichung = 4
dis_Nantou = 4
dis_Changhua = 4
dis_Yunlin = 5
dis_Chiayi = 5
dis_Tainan = 6
dis_Kaohsiung = 6
dis_Pingtung = 7

dis_Penghu = 8
dis_Kinmen = 9
# Goal:得到一個新的column裡面存放距離
# Now有台北到各地的距離
# Problem：同一個貼文有許多地點
# 解決方法:1.平均所有有的地點，2.擇一作為我們的地點
# 試試看1.
# 建立一個存放距離的vector
avg_distance <- c()
# 建立個按照順序的距離的vector：順序跟colnames(loc_tidy[24:42])相同
taiwan_city_dis <- c(1, 2, 3, 4,
                     6, 6, 3, 3,
                     3, 4, 4, 5,
                     5, 7, 5, 4,
                     8, 2, 9)

# 先用一個for loop走遍所有的row
# 再用一個for loop走遍所有column從loc_Taipei到loc_Kinmen
for(i in 1:nrow(loc_tidy)){
    # 這個vector用來裝放所有距離
    dis <- c()
    # 這個回圈是用來計算該行有哪一些是有要記錄距離的
    for(j in 24:42){
        # 做一個迴圈計數器，等等拿來抓taiwan_city_dis資料
        if(j == 24){
            count = 1
        }else{
            count = count + 1
        }
        # 多一個條件判斷
        if(loc_tidy[i, j] == 1){
            dis <- c(dis, taiwan_city_dis[count])
        }
    }
    # 跳脫了這個迴圈後，首先檢查dis vector是不是空的
    if(is.null(dis)){
        # 如果是空的，表該列(row)沒有面交地點
        avg_distance <- c(avg_distance, 0)
    }else{
        # 如果不是空vector，做平均
        avg_distance <- c(avg_distance, mean(dis))
    }
}
length(avg_distance)
colnames(loc_tidy[24:42])

loc_tidy <- loc_tidy %>%
    mutate(avg_distance = avg_distance)
# 完成！

# check 0 的row
count <- 0
for(i in 1:length(avg_distance)){
    if(avg_distance[i] < 1){
        count <- count + 1
    }
}
print(count)
# 共有297個entry是沒有面交地點的貼文，等等要做spatial model時要把他篩掉
loc_tidy %>%
    filter(avg_distance < 1) %>%
    View

df_spatial <- loc_tidy %>%
    filter(avg_distance >= 1)
# 剩下14265個observation，我們就來跑spatial吧！

### 失敗的發現過程


# we do the spatial analysis based on distanse -> neighbor
# install.packages("spdep")
library(spdep)
nb <- dnearneigh()
summary(df_spatial$avg_distance)
?nb2listw

moran.test(df_spatial$avg_price, df_spatial$avg_distance)

# 至此，發現前面那些工作全都徒勞無功
# 真正要給的是，經度跟緯度
loc_tidy %>%
    filter(time_2020 == 1&ROM == 64&Is_6s_plus == 0) %>%
    View
# 先把格式調整校正到跟WTP相同格式
i6s_WTP_df <- loc_tidy %>% 
    filter(IsBuy == 1) %>% 
    mutate(week = as.Date(cut(ptime, "week"))) %>% 
    group_by(week, ROM) %>% 
    mutate(quantity = n()) %>%
    ungroup() %>% 
    group_by(week) %>% 
    summarise(price = mean(avg_price),
              quantity = quantity,
              ROM = ROM,
              TimeUsed = mean(TimeUsed),
              Is_6s_plus = Is_6s_plus) %>% 
    filter(!duplicated(week, ROM))

# 根據spatial regression ppt，我們需要一個Spatial weight matrix based on distance
# 並且 y = rho*W*y + x*beta + e
# 若W要能跟y相乘，必須是一個跟y維度相同的方陣，而且該方陣是點到點距離或是是否為鄰近的方陣
# 因此我們要跑spatial model，必須先group by不同縣市，也就是讓row是每一個縣市
# 首先，我們必須先把有多個地點的全部降為1個地點
    
# 問題：時間維度不能保留，因為一旦保留，會有些row是同一個地點，但模型是每一個row都是不同地點
# 且weight matrix必須會有不在diagonal term上，是0，但不代表不相鄰，而是代表相同

# 算了，這樣解決不了問題
# 現在要做的是先給每一個row一個經緯度
# 每個row一個縣市

taiwan_city_list <- c(colnames(loc_tidy[24:42]))

only_loc <- c()

for(i in 1:nrow(loc_tidy)){
    # 這個vector用來裝放所有可能縣市
    place <- c()
    # 這個回圈是用來計算該行有哪一些縣市是應該被抓出來做隨機抽取的
    for(j in 24:42){
        # 做一個迴圈計數器，等等拿來抓taiwan_city_list資料
        if(j == 24){
            count = 1
        }else{
            count = count + 1
        }
        # 多一個條件判斷
        if(loc_tidy[i, j] == 1){
            place <- c(place, count)
        }
    }
    # 跳脫了這個迴圈後，首先檢查place vector是不是空的
    if(is.null(place)){
        # 如果是空的，表該列(row)沒有面交地點
        only_loc <- c(only_loc, 0)
    }else{
        # 如果不是空vector，就從這些中挑一個地點
        only_loc <- c(only_loc, place[sample(length(place),size=1)])
    }
}
length(only_loc)
# 把每筆資料坍縮到剩下一個地點了，那先來找各縣市的經緯度
# 各自用一個vector裝著，具有順序
longitude <- c(121.5598, 121.6739, 121.2168, 120.9417, 
               120.2513, 120.666, 121.7195, 120.9647,
               120.9417, 120.4818, 120.9876, 120.3897,
               120.4473, 120.62, 120.9876, 121.3542, 
               119.6151, 121.7081, 118.3186)
latitude <- c(25.09108, 24.91571, 24.93759, 24.23321,
              23.1417, 23.01087, 24.69295, 24.80395,
              24.48927, 23.99297, 23.83876, 23.75585,
              23.47545, 22.54951, 22.98461, 23.7569,
              23.56548, 25.10898, 24.43679)
# 好啦，那現在有了經緯度，我們就可以根據剛剛的那個only_loc，給予每個row經緯度了
# 我們把我們等等要mutate進去dataframe的經緯度分別叫做long & lat
# long <- c()
# lat <- c()
# for(i in 1:length(only_loc)){
#     if(only_loc[i] == 0){
#         long <- c(long, 0)
#         lat <- c(lat, 0)
#     }else{
#         long <- c(long, longitude[only_loc[i]])
#         lat <- c(lat, latitude[only_loc[i]])
#     }
# }

# jitter的概念，把每個點擾動一下
long <- c()
lat <- c()
for(i in 1:length(only_loc)){
    if(only_loc[i] == 0){
        long <- c(long, 0)
        lat <- c(lat, 0)
    }else{
        long <- c(long, longitude[only_loc[i]]*101+rnorm(1, mean=0, sd=3))
        lat <- c(lat, latitude[only_loc[i]]*111+rnorm(1, mean=0,sd=3))
    }
}

# 那現在bind回去，並且根據1~19賦予經緯度
Final_loc_tidy <- loc_tidy %>%
    mutate(long = long) %>%
    mutate(lat = lat)
Final_loc_tidy %>% View
# 搞定啦！！！

### Spatial Econometrics model(Spatial Lag Regression)

library(spdep)
library(spatialreg)

# 換個方式
i6s_WTP_df_with_long_lat <- Final_loc_tidy %>% 
    filter(IsBuy == 1) %>% 
    # 先把非面交者濾掉
    filter(long!=0|lat!=0) %>%
    mutate(week = as.Date(cut(ptime, "week"))) %>% 
    # 這邊要額外groupby long lat
    group_by(week, ROM, long, lat) %>% 
    mutate(quantity = n()) %>%
    ungroup() %>% 
    group_by(week, long, lat) %>% 
    summarise(price = mean(avg_price),
              quantity = quantity,
              ROM = ROM,
              TimeUsed = mean(TimeUsed),
              Is_6s_plus = Is_6s_plus,
              long = long,
              lat = lat) %>% 
    filter(!duplicated(week, ROM))

# 新方法
i6s_WTP_df_group_only_long_lat <- Final_loc_tidy %>% 
    filter(IsBuy == 1) %>% 
    # 先把非面交者濾掉
    filter(long!=0|lat!=0) %>%
    # 只看i6s
    filter(Is_6s_plus == 0) %>%
    mutate(week = as.Date(cut(ptime, "week"))) %>% 
    # 這邊要額外groupby long lat
    group_by(week, ROM, long, lat) %>% 
    mutate(quantity = n()) %>%
    ungroup() %>% 
    group_by(long, lat) %>% 
    summarise(price = mean(avg_price),
              quantity = quantity,
              ROM = ROM,
              TimeUsed = mean(TimeUsed),
              long = long,
              lat = lat) %>% 
    filter(!duplicated(long, lat))


mydata <- i6s_WTP_df_group_only_long_lat
mydata <- Final_loc_tidy
attach(mydata)
#detach(mydata)

Y <- cbind(mydata$avg_price)
X <- cbind(TimeUsed, ROM)
dim(Y)
dim(X)
xy <- cbind(mydata$long, mydata$lat)
dim(xy)
# Spatial weight matrix based on distance (with lower and upper bounds for distance, d1 and d2)
?dnearneigh

set.ZeroPolicyOption(TRUE) #https://r.789695.n4.nabble.com/Spatial-Ananlysis-zero-policy-TRUE-doesn-t-work-for-no-neighbour-regions-td4664367.html
nb <- dnearneigh(xy, d1=0, d2=30)
listw <- nb2listw(nb, style="W", zero.policy=TRUE)
?nb2listw
print(listw) 
class(listw)
summary(listw, zero.policy=TRUE)

# Moran's I test
moran.test(mydata$avg_price, listw, zero.policy=TRUE)
moran.plot(mydata$avg_price, listw, zero.policy=TRUE)
# p-value 約等於1，顯著拒絕存在spatial dependence.

# Spatial lag model
spatial.lag1 <- lagsarlm(mydata$avg_price ~ TimeUsed+ROM, data = mydata, listw)
summary(spatial.lag1)
