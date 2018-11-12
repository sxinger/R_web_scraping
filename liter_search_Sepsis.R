#### web scraping for AD+machine learning ####
rm(list=ls())
gc()

# trace(utils:::unpackPkgZip, edit=TRUE)
source("./util.R")
require_libraries(c("XML",
                    "RCurl",
                    "stringr",
                    "dplyr",
                    "tidyr",
                    "magrittr",
                    "ggplot2",
                    "ggrepel"))

AD_syn<-c("sepsis")
EMR_syn<-c("electronic medical record","EMR",
           "electronic health record","EHR")
Alg_syn<-c("predictive","machine learning","feature selection",
           "algorithm","analytics","multivariate",
           "big data","large data","high-dimensional data")

query_grid<-expand.grid(key1=AD_syn,
                        key2=c(EMR_syn,Alg_syn),
                        stringsAsFactors = F) %>%
  mutate(query_key=paste0(key1," AND ", key2))


#pubmed
require_libraries("rentrez")
liter_data<-c()
liter_meta<-c()
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()

  liter<-get_pubmed_full(query,max_return=5000)

  #post-filter
  liter_data_i<-liter$data %>% mutate(keywd=query)
  
  if(nrow(liter_data_i) > 0){
    #clean-up: contain target word group I?
    liter_data_i %<>%
      mutate(title2=title,abstract2=abstract) %>%
      unite("title_abstr",c("title2","abstract2")) %>%
      filter(grepl(query_grid$key1[i],title_abstr,ignore.case = T))

    #clean-up: contain target word group II?
    liter_data_i %<>%
      filter(grepl(query_grid$key2[i],title_abstr,ignore.case = T)) %>%
      dplyr::select(-title_abstr)
    
    #collect filtered results
    filter_cnt<-nrow(liter_data_i)
    metadata<-liter$metadata %>%
      mutate(filter_cnt=filter_cnt)
    liter$metadata<-metadata
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$metadata)
  }

  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
liter_pubmed<-list(liter_data=liter_data,
                   liter_meta=liter_meta)
saveRDS(liter_pubmed,file="./data/pubmed_search_result.rda")


# scopus
require_libraries("rscopus")
for(i in seq_len(nrow(query_grid))){
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  api_key<-"311c8c86fe53aba1e2107584fc41e390"
  
  # scrape
  liter_scopus<-get_scopus_full(api_type="scopus",query,max_return=100,api_key)
  liter_sciencedirect<-get_scopus_full(api_type="sciencedirect",query,max_return=100,api_key)
  
  #post-filter
  liter_data_i<-liter$data %>% mutate(keywd=query)
  
  if(nrow(liter_data_i) > 0){
    #clean-up: contain target word group I?
    liter_data_i %<>%
      mutate(title2=title,abstract2=abstract) %>%
      unite("title_abstr",c("title2","abstract2")) %>%
      filter(grepl(query_grid$key1[i],title_abstr,ignore.case = T))
    
    #clean-up: contain target word group II?
    liter_data_i %<>%
      filter(grepl(query_grid$key2[i],title_abstr,ignore.case = T)) %>%
      dplyr::select(-title_abstr)
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
  
  Sys.sleep(15)
}



# cochrane
for(i in seq_len(nrow(query_grid))){
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # Cochrane Library--TODO
  # liter_cochrane<-get_cochrane_full(query,max_return=200)
  
  #post-filter
  liter_data_i<-liter$data %>% mutate(keywd=query)
  
  if(nrow(liter_data_i) > 0){
    #clean-up: contain target word group I?
    liter_data_i %<>%
      mutate(title2=title,abstract2=abstract) %>%
      unite("title_abstr",c("title2","abstract2")) %>%
      filter(grepl(query_grid$key1[i],title_abstr,ignore.case = T))
    
    #clean-up: contain target word group II?
    liter_data_i %<>%
      filter(grepl(query_grid$key2[i],title_abstr,ignore.case = T)) %>%
      dplyr::select(-title_abstr)
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
  
  Sys.sleep(15)
}

# web of science
for(i in seq_len(nrow(query_grid))){
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # web of science -- TODO
  
  
  #post-filter
  liter_data_i<-liter$data %>% mutate(keywd=query)
  
  if(nrow(liter_data_i) > 0){
    #clean-up: contain target word group I?
    liter_data_i %<>%
      mutate(title2=title,abstract2=abstract) %>%
      unite("title_abstr",c("title2","abstract2")) %>%
      filter(grepl(query_grid$key1[i],title_abstr,ignore.case = T))
    
    #clean-up: contain target word group II?
    liter_data_i %<>%
      filter(grepl(query_grid$key2[i],title_abstr,ignore.case = T)) %>%
      dplyr::select(-title_abstr)
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$meta)
  }
  
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


