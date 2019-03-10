get_scopus_full<-function(api_type=c("scopus","sciencedirect"),query,max_return=2000,count=10,api_key,verb=T){
  keywd<-query
  api_key<-get_api_key(api_key)
  api_key<-"311c8c86fe53aba1e2107584fc41e390"
  # search scopus
  start<-Sys.time()
  if(api_type=="scopus"){
    scopus_obj<-scopus_search(query=query,
                              api_key=api_key,
                              count=count,
                              view="STANDARD",
                              sort="relevancy",
                              verbose=T)
  }else{
    scopus_obj<-sciencedirect_search(query=query,
                                     api_key=api_key,
                                     count=count,
                                     view="STANDARD",
                                     sort="relevancy",
                                     verbose=F)
  }
  if(verb){
    lapse<-Sys.time()-start
    cat("finish search scopus/science_direct in",lapse,units(lapse),".\n")
  }
  
  
  start<-Sys.time()
  # retrieve search result
  article_cnt<-scopus_obj$total_results
  
  # retrieve scopus id
  id<-sapply(scopus_obj$entries, function(x) gsub("SCOPUS_ID:","",x$`dc:identifier`))
  
  # retrieve title
  title<-sapply(scopus_obj$entries, function(x) gsub("\\\"","\"",x$`dc:title`))
  
  # retrieve author
  author<-sapply(scopus_obj$entries, function(x) x$`dc:creator`)
  
  # retrieve date
  date<-sapply(scopus_obj$entries, function(x) x$`prism:coverDate`)
  
  #retrieve journal
  journal<-sapply(scopus_obj$entries, function(x) x$`prism:publicationName`)
  
  # retrieve link
  link<-sapply(scopus_obj$entries, function(x) paste0("//doi.org/",x$`prism:doi`))
  
  # get citations
  cite_by<-sapply(scopus_obj$entries, function(x) as.numeric(x$`citedby-count`))
  
  # retrieve domain
  domain<-sapply(scopus_obj$entries, function(x) x$`prism:aggregationType`)
  if(verb){
    lapse<-Sys.time()-start
    cat("finish retrieve info (except abstract) in",lapse,units(lapse),".\n")
  }
  
  start<-Sys.time()
  # retrieve abstract
  max_return<-min(max_return,article_cnt)
  abstract<-c()
  for(idi in id[seq_len(max_return)]){
    brk_t<-sample(10:30,1)
    Sys.sleep(brk_t)
    
    get_abstr<-abstract_retrieval(idi,"scopus_id",api_key=api_key)
    redirect_link<-get_abstr$content$`abstracts-retrieval-response`$coredata$link
    redirect_link<-sapply(redirect_link, function(x) ifelse(x$`@rel`=="scopus",x$`@href`,NA))
    redirect_link<-redirect_link[!is.na(redirect_link)]
    if(length(redirect_link)>1){
      get_link<-GET(redirect_link[1])
      doc<-htmlParse(get_link, encoding="UTF-8", useInternal = TRUE)
      abstract_idi<-xpathSApply(doc,"//html//body//section[@id='abstractSection']//p",xmlValue)
    }else{
      abstract_idi<-NA
    }
    abstract<-c(abstract,abstract_idi)
  }
  if(verb){
    lapse<-Sys.time()-start
    cat("finish retrieve abstract in",lapse,units(lapse),".\n")
  }
  
  data<-data.frame(retrieve_ord=seq_len(max_return),
                   title=unlist(title[seq_len(max_return)]),
                   author=unlist(author[seq_len(max_return)]),
                   journal=unlist(journal[seq_len(max_return)]),
                   date=unlist(date[seq_len(max_return)]),
                   abstract=abstract,
                   cite_by=unlist(cite_by[seq_len(max_return)]),
                   link=unlist(link[seq_len(max_return)]),
                   domain=unlist(domain[seq_len(max_return)]),
                   stringsAsFactors = F)
  
  metadata<-data.frame(query=keywd,
                       engine="scopus",
                       search_result=article_cnt,
                       sort_by="relevance")
  
  out<-list(data=data,metadata=metadata)
  return(out)
}


scope_scrape<-function(keyword1,keyword2){
  #--build query grid
  query_grid<-expand.grid(key1=keyword1,
                          key2=keyword2,
                          stringsAsFactors = F) %>%
    mutate(query_key=paste0(key1," AND ", key2))
  
  #--search scopus
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
  
  return(liter_scopus)
}

scope_scrape<-function(keyword1,keyword2){
  #--build query grid
  query_grid<-expand.grid(key1=keyword1,
                          key2=keyword2,
                          stringsAsFactors = F) %>%
    mutate(query_key=paste0(key1," AND ", key2))
  
  #--search science direct
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
  
  return(liter_scidirect)
}