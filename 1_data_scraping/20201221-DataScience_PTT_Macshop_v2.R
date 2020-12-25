library(rvest)
library(httr)
library(tidyverse)
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
wd_path <- "/Users/Andy 1/Google 雲端硬碟 (r08323004@g.ntu.edu.tw)/0 Semesters/109-1/一234 資料科學/0_Final_Project/1_data_scraping"
setwd(wd_path)

#keywords <- c("ipad", "MAC", "MBR", "MBA", "Macbook")
keywords <- c("MAC", "MBR", "MPA", "Macbook")
pre <- "https://www.ptt.cc"


##### Defining Function #####

# Given a keyword, scrape for all article links
scrape_article_links <- function(query){
    
    # Creating an empty data frame by data_frame()
    post.df <- data_frame()
    
    page <- 1
    while(TRUE){
        url <- str_c("https://www.ptt.cc/bbs/Macshop/search?page=",
                     page,
                     "&q=",
                     query)
        print(url)
        res <- GET(url, config = set_cookies("over18" = "1"))
        page <- page+1
        if(res$status_code!=200){
            break
        }
        doc <- res %>%
            content("text") %>%
            read_html()
        nodes <- html_nodes(doc, ".r-ent")
        nrec <- html_node(nodes, ".nrec span") %>% html_text() %>% as.numeric()
        title <- html_node(nodes, ".title a") %>% html_text()
        link <- html_node(nodes, ".title a") %>% html_attr("href") %>%
            str_c(pre, .)
        author <- html_node(nodes, ".meta .author") %>% html_text()
        page.df <- data_frame(nrec, title, link, author)
        post.df <- bind_rows(post.df, page.df)
        message(nrow(post.df))
        Sys.sleep(0.1)
    }
    return(post.df)
}

##### Main #####
for(query in keywords){
    message("Scraping the keyword: ", query, " now")
    post.df <- scrape_article_links(query)
    save(post.df,
         file = paste0("DS_post.df_", query, "_", Sys.Date(), "_.rda"))
    
    # Create a dataframe containing all articles
    Posts <- tibble()
    for(url in post.df$link){
        message("Scraping the keyword: ", query, " now")
        try({
            doc <- GET(url, config = set_cookies("over18" = "1")) %>%
                content("text") %>%
                read_html()   
            pcontent <- html_nodes(doc, xpath = '//*[@id="main-content"]/text()') %>%
                html_text() %>%
                str_c(collapse = "") %>%
                str_replace_all("\n", "")
            
            metadata <- html_nodes(doc, "#main-content > div.article-metaline > .article-meta-value") %>% html_text()
            
            ptitle <- metadata[2]
            ptime <- metadata[3]
            pauthor <- metadata[1]
            
            
            post.temp <- tibble(url, pcontent, ptitle, pauthor, ptime)
            
            Posts <- bind_rows(Posts, post.temp)
            #Sys.sleep(0.05)
        }, silent = T)
    }
    save(Posts, 
         file = paste0("DS_Posts_", query, "_", Sys.Date(), "_.rda"))
}

##### Main Ends #####
