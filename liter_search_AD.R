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

  start_i<-Sys.time()

  # Cochrane Library--TODO
  # liter_cochrane<-get_cochrane_full(query,max_return=200)

  # Pubmed
  liter_pubmed<-get_pubmed_full(query,max_return=100)

  # scopus
  api_key<-"311c8c86fe53aba1e2107584fc41e390"
  liter_scopus<-get_scopus_full(api_type="scopus",query,max_return=100,api_key)
  
  # science direct
  api_key<-"311c8c86fe53aba1e2107584fc41e390"
  liter_scopus<-get_scopus_full(api_type="sciencedirect",query,max_return=100,api_key)

  # web of science -- TODO
  
  
  # WorldCat -- TODO
  
  
  # WHO -- TODO
  
  
  # Grey Literature -- TODO
  
  
  # stack all
  liter_data<-liter_google$data %<>% mutate(engine="google scholar")
    bind_rows(liter_pubmed$data %>% mutate(engine="pubmed"))
  
  liter_meta<-liter_google$metadata %<>%
    bind_rows(liter_pubmed$metadata)
  
  if(nrow(liter) > 0){
    #clean-up: contain target word group I?
    liter_data %<>%
      mutate(title2=title,abstract2=abstract) %>%
      unite("title_abstr",c("title2","abstract2")) %>%
      filter(grepl(query_grid$key1[i],title_abstr,ignore.case = T))

    #clean-up: contain target word group II?
    liter_data %<>%
      filter(grepl(query_grid$key2[i],title_abstr,ignore.case = T)) %>%
      dplyr::select(-title_abstr)
  }

  liter_rel[[query_grid$query_key[i]]]<-list(liter_data=liter_data,
                                             liter_meta=liter_meta)

  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
  
  Sys.sleep(15)
}


# special treatment for google scholar search
i<-1 #...1
query<-query_grid$query_key[i]

data<-c()
# page 1
liter_google<-get_google_scholar_full(query,page=1)
data %<>% bind_rows(liter_google$data)
metadata<-bind_rows(liter_google$metadata)
# page 2
liter_google<-get_google_scholar_full(query,page=2)
data %<>% bind_rows(liter_google$data)
# page 3
liter_google<-get_google_scholar_full(query,page=3)
data %<>% bind_rows(liter_google$data)
# page 4
liter_google<-get_google_scholar_full(query,page=4)
data %<>% bind_rows(liter_google$data)
# page 5
liter_google<-get_google_scholar_full(query,page=5)
data %<>% bind_rows(liter_google$data)
# page 6
liter_google<-get_google_scholar_full(query,page=6)
data %<>% bind_rows(liter_google$data)
# page 7
liter_google<-get_google_scholar_full(query,page=7)
data %<>% bind_rows(liter_google$data)
# page 8
liter_google<-get_google_scholar_full(query,page=8)
data %<>% bind_rows(liter_google$data)
# page 9
liter_google<-get_google_scholar_full(query,page=9)
data %<>% bind_rows(liter_google$data)
# page 10
liter_google<-get_google_scholar_full(query,page=10)
data %<>% bind_rows(liter_google$data)


