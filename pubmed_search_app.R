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
                    "rscopus",
                    "tm",
                    "openxlsx"))

# health recommender system
search_rslt<-pubmed_scrape(keyword1="health",
                           keyword2="recommender system")
write.xlsx(search_rslt,file="./output/AD_pubmed_search_result.xlsx")


# AD + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("alzheimer","dementia"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))
write.xlsx(search_rslt,file="./output/AD_pubmed_search_result.xlsx")


# ALS + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("ALS","Lou Gehrig"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))
write.xlsx(search_rslt,file="./output/ALS_pubmed_search_result.xlsx")



# CKD/DKD + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("chronic kidney","chronic renal",
                                      "diabetic kidney","diabetic renal","diabetic nephropathy",
                                      "end stage renal"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))
write.xlsx(search_rslt,file="./output/CKD_pubmed_search_result.xlsx")


# precision medicine + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("precision medicine","personalized medicine"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))
write.xlsx(search_rslt,file="./output/PrecMed_pubmed_search_result.xlsx")

# Sepsis + EMR/algorithm
search_rslt<-pubmed_scrape(keyword1=c("sepsis"),
                           keyword2=c("electronic medical record","EMR",
                                      "electronic health record","EHR",
                                      "predictive","algorithm","analytics",
                                      "machine learning","multivariate",
                                      "feature selection","biomarker","data mining",
                                      "big data","large data","high dimensional data"))
write.xlsx(search_rslt,file="./output/Sepsis_pubmed_search_result.xlsx")


