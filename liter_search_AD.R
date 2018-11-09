#### web scraping for AD+machine learning ####
rm(list=ls())
gc()

# trace(utils:::unpackPkgZip, edit=TRUE)
source("./util.R")
require_libraries(c("rentrez",
                    "rscopus",
                    "XML",
                    "RCurl",
                    "stringr",
                    "dplyr",
                    "tidyr",
                    "magrittr",
                    "ggplot2",
                    "ggrepel"))

AD_syn<-c("alzheimer","AD")
EMR_syn<-c("electronic medical record","EMR",
           "electronic health record","EHR")
Alg_syn<-c("machine learning","feature selection",
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

  # Cochrane Library--TODO
  # liter_cochrane<-get_cochrane_full(query,max_return=200)
  # liter_evol<-c(liter_evol,nrow(liter_pubmed))
  
  
  # Pubmed
  liter_pubmed<-get_pubmed_full(query,max_return=100)
  liter_evol<-c(liter_evol,nrow(liter_pubmed))
  
  
  # scopus --TODO
  # api_key<-"311c8c86fe53aba1e2107584fc41e390"
  # liter_scopus<-get_scopus_full(query,max_return=200)
  # liter_evol<-c(liter_evol,nrow(liter_scopus))
  
  
  # web of science -- TODO
  
  
  # WorldCat -- TODO
  
  
  # Google Scholar
  liter_google<-get_google_scholar_full(query,max_return=100)
  liter_evol<-c(liter_evol,nrow(liter_google))
  
  # WHO -- TODO
  
  
  # Grey Literature -- TODO
  
  
  # stack all
  liter_data<-liter_google$data %<>% mutate(engine="google scholar")
    bind_rows(liter_pubmed$data %>% mutate(engine="pubmed"))
  
  liter_meta<-liter_google$metadata %<>%
    bind_rows(liter_pubmed$metadata)
  
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



