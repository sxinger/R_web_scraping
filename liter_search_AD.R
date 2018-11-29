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

AD_syn<-c("alzheimer","dementia")
EMR_syn<-c("electronic medical record","EMR",
           "electronic health record","EHR")
Alg_syn<-c("predictive","algorithm","analytics",
           "machine learning","multivariate",
           "feature selection","biomarker","data mining",
           "big data","large data","high dimensional data")

query_grid<-expand.grid(key1=AD_syn,
                        key2=c(EMR_syn,Alg_syn),
                        stringsAsFactors = F) %>%
  mutate(query_key=paste0(key1," AND ", key2))

##==============pubmed===========
require_libraries("rentrez")

liter_data<-c()
liter_meta<-c()
for(i in seq_len(nrow(query_grid))){
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
      mutate(filter_cnt=filter_cnt,keywd=query)
  }
  
  #stack results
  liter_data %<>% bind_rows(liter_data_i)
  liter_meta %<>% bind_rows(metadata)

  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
liter_data %<>% filter(!duplicated(pubmed_id))
write.csv(liter_data,file="./output/AD_pubmed_search_data.csv",row.names = F)

liter_pubmed<-list(liter_data=liter_data,
                   liter_meta=liter_meta)
saveRDS(liter_pubmed,file="./data/AD_pubmed_search_result.rda")


#====collect citations from google scholar====
require_libraries(c("rentrez","tm"))
stopwords_regex<-paste0('\\b',
                        paste(stopwords('en'), collapse = '\\b|\\b'),
                        '\\b')
query_item<-c("&as_epq=",
              "&as_oq=",
              "&as_eq=",
              "&as_occt=any",
              "&as_sauthors=",
              "&as_publication=",
              "&as_yhi=",
              "&hl=en&as_sdt=0%2C5")

liter_pubmed<-readRDS("./data/AD_pubmed_search_result.rda")
liter_data<-liter_pubmed$liter_data
for(q in seq_len(nrow(liter_date))){
  start_q<-Sys.time()

  title<-liter_data$title[q]
  cite_q<-get_google_scholar_citation(title)
  cite<-c(cite,cite_q)

  lapse_q<-Sys.time()-start_q
  cat(query,"...finish get citation data for<",title,">in",lapse_q,units(lapse_q),".\n")
}

liter_data %<>%
  mutate(cited_by=cite) %>%
  separate("cited_by",c("cited_by_gs","cited_by_wos"),",") %>%
  dplyr::select(-cited_by)
liter_pubmed$liter_data<-liter_data
saveRDS(liter_pubmed,file="./data/AD_pubmed_search_result.rda")

#remove duplicates
liter_pubmed<-readRDS("./data/AD_pubmed_search_result.rda")
liter_data<-liter_pubmed$liter_data %>%
  
liter_pubmed$liter_data<-liter_data
save(liter_pubmed,file="./data/AD_pubmed_search_result.rda")
write.csv(liter_data,file="./output/AD_pubmed_search_data.csv",row.names = F)

##==============scopus=============
require_libraries("rscopus")
liter_data<-c()
liter_meta<-c()
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # scrape
  api_key<-"b2c86c0a2013c7a788198cd75369672d"
  liter<-get_scopus_full(api_type="scopus",
                         query=query,
                         max_return=2000,
                         count=25,
                         api_key=api_key)

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
    liter_meta %<>% bind_rows(liter$meta)
  }
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
liter_scopus<-list(liter_data=liter_data,
                   liter_meta=liter_meta)
saveRDS(liter_scopus,file="./data/scopus_search_result.rda")


##==============science direct===============
require_libraries("rscopus")
liter_data<-c()
liter_meta<-c()
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # scrape
  api_key<-"3ccb1529e4db2c490e19e0abe1e95da9"
  liter<-get_scopus_full(api_type="sciencedirect",
                         query=query,
                         max_return=2000,
                         count=25,
                         api_key=api_key)
  
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
    liter_meta %<>% bind_rows(liter$meta)
  }
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
liter_scidirect<-list(liter_data=liter_data,
                   liter_meta=liter_meta)
saveRDS(liter_scidirect,file="./data/scidirect_search_result.rda")


##==============cochrane=============
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
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
    
    #collect filtered results
    filter_cnt<-nrow(liter_data_i)
    metadata<-liter$metadata %>%
      mutate(filter_cnt=filter_cnt)
    liter$metadata<-metadata
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}


##==============web of science===================
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
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
    
    #collect filtered results
    filter_cnt<-nrow(liter_data_i)
    metadata<-liter$metadata %>%
      mutate(filter_cnt=filter_cnt)
    liter$metadata<-metadata
    
    #stack results
    liter_data %<>% bind_rows(liter_data_i)
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}


##==============worldcat===================
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # worldCat -- TODO
  
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
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
##==============world health organization===================
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # world health organization -- TODO
  
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
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
##==============grey literature===================
for(i in seq_len(nrow(query_grid))){
  brk_t<-sample(10:30,1)
  Sys.sleep(brk_t)
  
  query<-query_grid$query_key[i]
  start_i<-Sys.time()
  
  # grey literature -- TODO
  
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
    liter_meta %<>% bind_rows(liter$meta)
  }
  
  lapse_i<-Sys.time()-start_i
  cat("finish searching query:'",query,"'in",lapse_i,units(lapse_i),".\n")
}
##==============google scholar==============
data<-c()
metadata<-c()

for(q in seq_len(nrow(query_grid))){
  query<-query_grid$query_key[q]
  
  # page 1
  liter_google<-get_google_scholar_full(query,page=1)
  data %<>% bind_rows(liter_google$data)
  metadata %<>% bind_rows(liter_google$metadata)
  
  pg_n<-ceiling(metadata$search_result[nrow(metadata)]/10)
  chk_size<-5
  chk_seq<-c(seq(2,pg_n,by=chk_size),max(pg_n))
  pg_chk<-ceiling(pg_n/chk_size)
  
  for(b in 2:pg_chk){
    start_b<-Sys.time()
    Sys.sleep(20)
    
    for(p in chk_seq[b]:chk_seq[b+1]){
      liter_google<-get_google_scholar_full(query,page=p)
      data %<>% bind_rows(liter_google$data) 
    }
    
    lapse_b<-Sys.time()-start_b
    cat("finish batch",b,"(",chk_size,"pages per batch) in",lapse_b,units(lapse_b),".\n")
  }
}

