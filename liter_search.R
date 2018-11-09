#### web scraping for AD+machine learning ####
rm(list=ls())
gc()

source("./R/util.R")
require_libraries(c("dplyr",
                    "tidyr",
                    "magrittr",
                    "stringr",
                    "rentrez",
                    "rscopus",
                    "XML",
                    "RCurl",
                    "ggplot2",
                    "ggrepel"))

AD_syn<-c("alzheimer","AD")
EMR_syn<-c("electronic medical record","EMR",
           "electronic health record","EHR")
Alg_syn<-c("machine learning","knowledge discovery",
           "learning","algorithm","analytics",
           "big data","large data","high-dimensional data")

query_grid<-expand.grid(key1=AD_syn,
                        key2=c(EMR_syn,Alg_syn),
                        stringsAsFactors = F) %>%
  mutate(query_key=paste0(key1," AND ", key2))

liter_rel<-list()

for(i in seq_len(nrow(query_grid))){
  query<-query_grid$query_key[i]
  liter_evol<-c()
  
  start_i<-Sys.time()

  # Pubmed
  liter_pubmed<-get_pubmed_full(query,max_return=200)
  liter_evol<-c(liter_evol,nrow(liter_pubmed))
  
  # Cochrane Library--TODO
  # liter_cochrane<-get_cochrane_full(query,max_return=200)
  # liter_evol<-c(liter_evol,nrow(liter_pubmed))
  
  # scopus --TODO
  # api_key<-"311c8c86fe53aba1e2107584fc41e390"
  # liter_scopus<-get_scopus_full(query,max_return=200)
  # liter_evol<-c(liter_evol,nrow(liter_scopus))
  
  # 
  
  liter<-liter_pubmed
  liter_evol<-c(liter_evol,nrow(liter))
  
  if(nrow(liter) > 0){
    #clean-up: contain target word group I?
    liter %<>%
      filter(grepl(query_grid$key1[i],abstract,ignore.case = T))

    #clean-up: contain target word group II?
    liter %<>%
      filter(grepl(query_grid$key2[i],abstract,ignore.case = T))
  }
  liter_evol<-c(liter_evol,nrow(liter))
  
  liter_rel[[query_grid$query_key[i]]]<-list(drop_out=liter_evol,
                                             elig_liter=liter)

  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
  
  Sys.sleep(15)
}



