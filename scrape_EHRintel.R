#load packages
install.packages("rvest")

library(rvest)
library(tidyr)
library(dplyr)
library(magrittr)
library(xml2)

#sepcify website
news_page1<-"https://ehrintelligence.com/news"

#read html
news_page<-read_html(news_page1)

#navigate to news within the folder and retrieve
# - new's title
title_html<-html_nodes(news_page,"a")
title_text<-html_text(title_html)
# output:
# "EHR Use, High Administrative Burden Driving Healthcare Spending"

# - new's content
content_html<-html_nodes(news_page,"p")
content_text<-html_text(content_html)
content_text[3]
# output:
# [1] "August 01, 2018 - While CMS and other federal entities...


#TODO: automatically go to sub-link and collect title and contents
#collect children of a link
web<-"https://ehrintelligence.com/news"
web_page<-read_html(web)
xml_children(web)
