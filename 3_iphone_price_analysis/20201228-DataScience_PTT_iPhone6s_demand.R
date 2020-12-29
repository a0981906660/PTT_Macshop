library(rvest)
library(httr)
library(tidyverse)
library(stringr)
options(stringsAsFactors = F)
httr::set_config(httr::config(http_version = 0))

# MBP:113*20
# Macbook Pro:1148*20
# MBA:31
# Macbook Air:1148*20
# MAC:2375
# iphone:9625*20
# ipad:3418*20

# Mac Air:19

# environment
getwd()
wd_path <- "/Users/Andy 1/Google 雲端硬碟 (r08323004@g.ntu.edu.tw)/0 Semesters/109-1/一234 資料科學/0_Final_Project/PTT_Macshop"
setwd(wd_path)
load("1_data_scraping/collected_datasets/DS_Posts_iphone_2020-12-24_.rda")

# Sketch
# Posts %>% 
#     filter(str_detect(string = pcontent, pattern = "女用機")) %>% View
# Posts %>% 
#     filter(str_detect(string = pcontent, pattern = "男用機")) %>% View

# 女用機
female_all <- Posts %>% 
    mutate(IsSell = ifelse(str_detect(ptitle, "\\[販售\\]|\\[賣|出售|販售|售") |
                               str_detect(string = pcontent, pattern = "售價"), 1, 0),
           IsBuy = ifelse(str_detect(ptitle, "\\[收購\\]|\\[買") |
                              str_detect(string = pcontent, pattern = "我想要買"), 1, 0),
           IsChange = ifelse(str_detect(ptitle, "\\[交換\\]|\\[換"), 1, 0))%>%
    mutate(IsSold = ifelse(str_detect(string = ptitle, pattern = "售出|已售出|已售|已出售"), 1, 0),
           IsBought = ifelse(str_detect(string = ptitle, pattern = "徵到|徵得|已徵得|已徵到|收到|已購買|已購入"), 1, 0)) %>% 
    filter((IsSold==1 | IsBought==1)) %>% 
    filter(str_detect(string = pcontent, pattern = "女用機|女用|女生用"))
# 男用機
male_all <- Posts %>% 
    mutate(IsSell = ifelse(str_detect(ptitle, "\\[販售\\]|\\[賣|出售|販售|售") |
                               str_detect(string = pcontent, pattern = "售價"), 1, 0),
           IsBuy = ifelse(str_detect(ptitle, "\\[收購\\]|\\[買") |
                              str_detect(string = pcontent, pattern = "我想要買"), 1, 0),
           IsChange = ifelse(str_detect(ptitle, "\\[交換\\]|\\[換"), 1, 0))%>%
    mutate(IsSold = ifelse(str_detect(string = ptitle, pattern = "售出|已售出|已售|已出售"), 1, 0),
           IsBought = ifelse(str_detect(string = ptitle, pattern = "徵到|徵得|已徵得|已徵到|收到|已購買|已購入"), 1, 0)) %>% 
    filter((IsSold==1 | IsBought==1)) %>% 
    filter(str_detect(string = pcontent, pattern = "男用機|男用|男生用")) %>% 
    View



# iphone 6s
tidy_df <- Posts %>% 
    filter(str_detect(string = ptitle, pattern = "6s")) %>% 
    mutate(IsSell = ifelse(str_detect(ptitle, "\\[販售\\]|\\[賣|出售|販售|售") |
                           str_detect(string = pcontent, pattern = "售價"), 1, 0),
           IsBuy = ifelse(str_detect(ptitle, "\\[收購\\]|\\[買") |
                          str_detect(string = pcontent, pattern = "我想要買"), 1, 0),
           IsChange = ifelse(str_detect(ptitle, "\\[交換\\]|\\[換"), 1, 0))%>%
    mutate(IsSold = ifelse(str_detect(string = ptitle, pattern = "售出|已售出|已售|已出售"), 1, 0),
           IsBought = ifelse(str_detect(string = ptitle, pattern = "徵到|徵得|已徵得|已徵到|收到|已購買|已購入"), 1, 0)) %>% 
    filter(!(IsBuy==0 & IsSell ==0 & IsChange ==0 & IsSold==0 & IsBought==0))
    

# Seeking for Gender Rent
tidy_df %>% 
    filter(str_detect(string = pcontent, pattern = "女用機")) %>% View

"(價.*\\d{2,5})|(價格.*\\d{2,5})"
"價.{1,8}(\d{2,5}){1,2}"
# Prices
iphon6s_df <- tidy_df %>% 
    mutate(price_text = str_extract(pcontent, "價.{1,12}(\\d{2,5}){1,2}|價格.{1,8}(\\d{2,5}){1,2}")) %>% 
    mutate(price_text = gsub(",", "", price_text)) %>% 
    mutate(price = str_extract_all(price_text, "\\d{3,4}0")) %>%
    filter(lengths(price)<=2) %>% 
    drop_na() %>% 
    unnest(price) %>% 
    mutate(price = as.numeric(price)) %>% 
    group_by(url) %>% 
    mutate(avg_price = mean(price)) %>% 
    ungroup() %>% 
    filter(!duplicated(url)) %>% 
    select(-price,-price_text)
    
# Seeking for Gender Rent
iphon6s_df %>% 
    filter(str_detect(string = pcontent, pattern = "女用機")) %>% View

