#### web scraping for AD+machine learning ####
rm(list=ls())
gc()

# trace(utils:::unpackPkgZip, edit=TRUE)
source("./util.R")
source("./pubmed_search.R")
require_libraries(c("XML",
                    "RCurl",
                    "stringr",
                    "dplyr",
                    "tidyr",
                    "magrittr",
                    "rentrez",
                    "rscopus"))

# "health recommender system"
search_rslt<-pubmed_scrape(keyword1="health",
                           keyword2="recommender system")

# AD + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("alzheimer","dementia"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))


