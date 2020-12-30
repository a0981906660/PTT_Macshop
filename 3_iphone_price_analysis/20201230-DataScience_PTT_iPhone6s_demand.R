library(rvest)
library(httr)
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(sjmisc) #dummies
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


# all iphone prices
tidy_df <- Posts %>% 
    mutate(IsSell = ifelse(str_detect(ptitle, "\\[販售\\]|\\[賣|出售|販售|售") |
                               str_detect(string = pcontent, pattern = "售價"), 1, 0),
           IsBuy = ifelse(str_detect(ptitle, "\\[收購\\]|\\[買") |
                              str_detect(string = pcontent, pattern = "我想要買"), 1, 0),
           IsChange = ifelse(str_detect(ptitle, "\\[交換\\]|\\[換"), 1, 0))%>%
    mutate(IsSold = ifelse(str_detect(string = ptitle, pattern = "售出|已售出|已售|已出售"), 1, 0),
           IsBought = ifelse(str_detect(string = ptitle, pattern = "徵到|徵得|已徵得|已徵到|收到|已購買|已購入"), 1, 0)) %>% 
    filter(!(IsBuy==0 & IsSell ==0 & IsChange ==0 & IsSold==0 & IsBought==0))
#extract prices
tidy_df <- tidy_df %>% 
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
#hist(tidy_df$avg_price)

color_pattern <- regex("銀|金|灰|玫瑰|白|綠|紅|藍|石墨")

tidy_df <- tidy_df %>% 
    drop_na() %>% 
    # ROM
    mutate(ROM_text = str_extract(pcontent, "[1-2]\\d{2}G|[1-2]\\d{2}g|\\d{2}G|\\d{2}g")) %>% 
    mutate(ROM = str_extract(ROM_text, "\\d{2,3}")) %>% 
    mutate(ROM = as.numeric(ROM)) %>% 
    select(-ROM_text) %>% 
    # Color
    mutate(color_c = str_extract(pcontent, color_pattern)) %>% 
    mutate(color_t = str_extract(ptitle, color_pattern)) %>% 
    mutate(color = ifelse(!is.na(color_c), color_c, color_t)) %>% 
    select(-color_t, -color_c) %>% 
    mutate(color = ifelse(!is.na(color), color, "No_Color")) %>% 
    
    # Usage
    mutate(ptime = parse_datetime(ptime, format = "%a %b %d %H:%M:%S %Y")) %>% 
    #mutate(ptime = as.POSIXct(ptime)) %>% 
    #mutate(referenceTime = as.POSIXct(parse_datetime("2015-09-25", "%Y-%m-%d"))) %>% 
    #mutate(TimeUsed = as.numeric(difftime(ptime-referenceTime, units='days'))/365) %>% 
    #filter(TimeUsed>=0) %>% 
    mutate(TimeUsed = as.numeric(ptime)) %>% 
    
    # Gender
    mutate(IsFemale = ifelse(str_detect(string = pcontent, pattern = "女用機|女用|女生用"), 1, 0))%>% 
    mutate(IsMale = ifelse(str_detect(string = pcontent, pattern = "男用機|男用|男生用"), 1, 0))

# get dummies
tidy_df <- tidy_df %>% 
    to_dummy(color, suffix = "label") %>% 
    bind_cols(tidy_df) %>% 
    select(everything())

#################################

# Conditional on iPhone 6s
#型號、RAM/ROM容量、color、發文時間（作為使用幾年的依據）
type_pattern <- regex("plus|\\+", ignore_case=TRUE)

iphone6s_df <- tidy_df %>% 
    filter(str_detect(string = ptitle, pattern = "6s")) %>% 
    # Type
    mutate(Is_6s_plus = (ifelse(str_detect(ptitle, type_pattern), 1, 0))) %>% 
    # ROM
    # mutate(ROM_text = str_extract(pcontent, "[1-2]\\d{2}G|[1-2]\\d{2}g|\\d{2}G|\\d{2}g")) %>% 
    # mutate(ROM = str_extract(ROM_text, "\\d{2,3}")) %>% 
    # mutate(ROM = as.numeric(ROM)) %>% 
    # select(-ROM_text) %>% 
    # drop_na() %>% 
    # Color
    # mutate(color_c = str_extract(pcontent, color_pattern)) %>% 
    # mutate(color_t = str_extract(ptitle, color_pattern)) %>% 
    # mutate(color = ifelse(!is.na(color_c), color_c, color_t)) %>% 
    # select(-color_t, -color_c) %>% 
    # mutate(color = ifelse(!is.na(color), color, "No_Color")) %>% 
    
    # Usage
    # mutate(ptime = parse_datetime(ptime, format = "%a %b %d %H:%M:%S %Y")) %>% 
    mutate(TimeUsed = as.numeric(ptime-parse_datetime("2015-09-25", "%Y-%m-%d"))/24) %>% 
    filter(TimeUsed>=0)
    
    # Gender
    # mutate(IsFemale = ifelse(str_detect(string = pcontent, pattern = "女用機|女用|女生用"), 1, 0))%>% 
    # mutate(IsMale = ifelse(str_detect(string = pcontent, pattern = "男用機|男用|男生用"), 1, 0))

nrow(tidy_df)
nrow(tidy_df %>% drop_na())

nrow(iphone6s_df)
nrow(iphone6s_df %>% drop_na())

########## finish cleaning data ##########

# 1.Seeking for Gender Rent

# Sketch
tidy_df %>%
    filter(str_detect(string = pcontent, pattern = "女用機")) %>% View
tidy_df %>%
    filter(str_detect(string = pcontent, pattern = "男用機")) %>% View

# Gender Rent
gender_df <- tidy_df %>%
    mutate(IsFemale = ifelse(str_detect(string = pcontent, pattern = "女用機|女用|女生用"), 1, 0))%>% 
    mutate(IsMale = ifelse(str_detect(string = pcontent, pattern = "男用機|男用|男生用"), 1, 0))%>% 
    filter(IsMale==1 | IsFemale==1)
colnames(gender_df)
# not at equilibrium
reg1 <- lm(formula = avg_price ~ IsFemale + TimeUsed + ROM + 
               color_白 + color_紅 + color_灰 + color_金 + color_藍 + color_綠 + color_玫瑰 + color_石墨 + color_銀,
           data = gender_df)
summary(reg1)


# at equilibrium
gender_df <- tidy_df %>%
    mutate(IsFemale = ifelse(str_detect(string = pcontent, pattern = "女用機|女用|女生用"), 1, 0))%>% 
    mutate(IsMale = ifelse(str_detect(string = pcontent, pattern = "男用機|男用|男生用"), 1, 0))%>% 
    filter(IsMale==1 | IsFemale==1) %>% 
    filter(IsSold==1 | IsBought==1)
# We don't even have partial evidence that there exist profit for "女用機"
# The direction tends to say that 女用機 has a higher price
# but the result is constrainted to small and unbalanced sample size
reg2 <- lm(formula = avg_price ~ IsFemale + TimeUsed + ROM + 
               color_白 + color_紅 + color_灰 + color_金 + color_藍 + color_綠 + color_玫瑰 + color_石墨 + color_銀,
   data = gender_df)
summary(reg2)

# 2. Estimating the supply and demand for iphone6s

# iphone 6s : 15880 obs
iphone6s_df %>% 

