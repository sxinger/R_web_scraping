#### web scraping for ALS+machine learning ####
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

ALS_syn<-c("ALS","Lou Gehrig")
EMR_syn<-c("electronic medical record","EMR",
           "electronic health record","EHR")
Alg_syn<-c("predictive","algorithm","analytics",
           "machine learning","multivariate",
           "feature selection","biomarker","data mining",
           "big data","large data","high dimensional data")

query_grid<-expand.grid(key1=ALS_syn,
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
liter_pubmed<-list(liter_data=liter_data,
                   liter_meta=liter_meta)
saveRDS(liter_pubmed,file="./data/ALS_pubmed_search_result.rda")


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

