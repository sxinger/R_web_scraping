get_cochrane_full<-function(query,max_return=2000,count=10,api_key,verb=T){
 #TODO
}


cochrane_scrape<-function(keyword1,keyword2){
  #--build query grid
  query_grid<-expand.grid(key1=keyword1,
                          key2=keyword2,
                          stringsAsFactors = F) %>%
    mutate(query_key=paste0(key1," AND ", key2))
  
  #--search cochrane
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
  
  liter_cochrane<-list(liter_data=liter_data,
                       liter_meta=liter_meta)
  
  return(liter_cochrane)
}