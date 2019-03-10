get_pubmed_full<-function(query,max_return=20) {
  #sleep before start
  brk_t<-sample(15:30,1)
  Sys.sleep(brk_t)
  
  # formulate the query term
  query<-paste(query,"('2010/01/01'[PDat]:'3000/12/31'[PDat])") # date restriction
  query<-gsub("'", "%22", gsub(" ", "+", query))
  query<-paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=",
               max_return,"&sort=relevance&term=", 
               query, "&usehistory=y", sep = "")
  
  #add an error-prevention machanism
  counter<-1 #in case there 
  myopt<-curlOptions(connecttimeout = 200)
  get_url<-getURL(query,.opts=myopt)
  temp<-try(get_url)
  while(grepl("(Bad request)|(Error)+",temp) & counter <= 5) {
    brk_t<-sample(10:30,1)
    Sys.sleep(brk_t)
    
    get_url<-getURL(query)
    temp<-try(get_url)
    counter<-counter+1
  } 
  
  #parse xml
  query<-xmlParse(get_url, encoding="UTF-8", useInternal = TRUE)
  
  #retrieve number of related articles
  result<-xpathApply(query,"//Count",xmlValue)
  article_cnt<-as.numeric(result[1])
  
  #collect pubmed ids
  ids<-xpathApply(query,"//IdList/Id",xmlValue)
  ids<-unlist(ids)
  
  if(length(ids)==0){
    article_cnt<-0
    data<-data.frame(retrieve_ord=NA,
                     title=NA,
                     author=NA,
                     journal=NA,
                     date=NA,
                     abstract=NA,
                     link=NA,
                     domain=NA,
                     grands=NA,
                     stringsAsFactors = F)
  }else{
    chk_size<-20 # changable
    chk_seq<-c(seq(1,min(max_return,article_cnt),by=chk_size),
               min(max_return,article_cnt)+1)
    data<-c()
    for(k in seq_along(chk_seq[-1])){
      data_pubmed<-entrez_fetch(db = "pubmed",
                                id = ids[chk_seq[k]:(chk_seq[k+1]-1)],
                                rettype = "xml",
                                parsed = TRUE)
      
      #retrieve title
      title<-xpathSApply(data_pubmed,
                         "//PubmedArticle//Article|//PubmedBookArticle//BookDocument",function(x) {
                           val <- xpathSApply(x, "./ArticleTitle", xmlValue)
                           if (length(val)==0) val <- xpathSApply(x,"//PubmedBookArticle//BookDocument//Book//BookTitle",
                                                                  function(x) paste0("Book Name:",xmlValue(x)))
                           val
                         })
      
      #retrieve author
      author_ln<-xpathSApply(data_pubmed, "//PubmedArticle//Article|//PubmedBookArticle//Book", function(x) {
        val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/LastName", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      author_fn<-xpathSApply(data_pubmed, "//PubmedArticle//Article|//PubmedBookArticle//Book", function(x) {
        val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/ForeName", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      author<-mapply(function(x,y) paste(unlist(x),unlist(y),sep=","),author_ln,author_fn)
      if(!is.list(author)){
        author<-paste(author,collapse = ";")
      }else{
        author<-sapply(author, function(x) paste(unlist(x),collapse = ";"))
      }
      
      
      #retrieve publication date
      pub_yr<-xpathSApply(data_pubmed, "//PubmedData//History|//PubmedBookData//History", function(x) {
        val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Year", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      pub_mth<-xpathSApply(data_pubmed, "//PubmedData//History|//PubmedBookData//History", function(x) {
        val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Month", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      
      #retrieve journal
      journal<-xpathSApply(data_pubmed, "//PubmedArticle//Article//Journal//Title|
                           //PubmedBookArticle//BookDocument//Book//BookTitle",
                           xmlValue)
      
      # #retrieve mesh terms
      # mesh_term<-xpathSApply(data_pubmed, "//PubmedArticle//PubmedData//ArticleIdList//ArticleId[@IdType='pubmed']|
      #                                      //PubmedBookArticle//PubmedBookData//ArticleIdList//ArticleId[@IdType='pubmed']|
      #                                      //MeshHeadingList",xmlValue)
      # mesh_major<-xpathSApply(data_pubmed, "//MeshHeadingList", function(x) {
      #   val <- xpathSApply(x, "./MeshHeading/DescriptorName[@MajorTopicYN='Y']", xmlValue)
      #   if (length(val)==0) val <- NA_character_
      #   val
      # })
      # mesh_minor<-xpathSApply(data_pubmed, "//MeshHeadingList", function(x) {
      #   val <- xpathSApply(x, "./MeshHeading/DescriptorName[@MajorTopicYN='N']", xmlValue)
      #   if (length(val)==0) val <- NA_character_
      #   val
      # })
      # if(!is.list(mesh_major)|length(mesh_major)==0){
      #   mesh_major<-paste(mesh_major,collapse=";")
      # }else{
      #   mesh_major<-sapply(mesh_major,function(x) unique(ifelse(is.na(x),NA,paste(x,collapse=";"))))
      # }
      # if(!is.list(mesh_minor)|length(mesh_minor)==0){
      #   mesh_minor<-paste(mesh_minor,collapse=";")
      # }else{
      #   mesh_minor<-sapply(mesh_minor,function(x) unique(ifelse(is.na(x),NA,paste(x,collapse=";"))))
      # }
      # 
      # mesh_df<-data.frame(id_mesh=unlist(mesh_term),stringsAsFactors = F) %>%
      #   mutate(rn=1:n()) %>%
      #   mutate(id_or_mesh=ifelse(id_mesh %in% unlist(ids[chk_seq[k]:(chk_seq[k+1]-1)]),"id","mesh"),
      #          rn=ifelse(id_mesh %in% unlist(ids[chk_seq[k]:(chk_seq[k+1]-1)]),rn,NA)) %>%
      #   fill(rn,.direction="up") 
      # if(!any(mesh_df$id_or_mesh=="mesh")){
      #   mesh_df %<>% bind_rows(mesh_df %>% mutate(id_mesh=NA,id_or_mesh="mesh"))
      # }
      # mesh_df %<>% unique %>% spread(id_or_mesh,id_mesh)
      # mesh_df2<-mesh_df %>% filter(!is.na(mesh)) %>%
      #   mutate(mesh_major=mesh_major,
      #          mesh_minor=mesh_minor) %>%
      #   bind_rows(mesh_df %>% filter(is.na(mesh)) %>% 
      #               mutate(mesh_major=NA, mesh_minor=NA)) %>%
      #   mutate(rn=rank(rn)) %>% arrange(rn)
      
      #retrieve domain
      domain<-xpathSApply(data_pubmed, "//PubmedArticle|//PubmedBookArticle//Book", function(x) {
        val <- xpathSApply(x, "./MedlineCitation", xmlGetAttr,"Status")
        if (length(val)==0) val <- "Book"
        val
      })
      
      #retrieve grants
      grands<-xpathSApply(data_pubmed, "//PubmedArticle//Article|//PubmedBookArticle//BookDocument//Book", function(x) {
        val <- xpathSApply(x, "./GrantList[@CompleteYN='Y']/Grant/GrantID", xmlValue)
        if (length(val)==0) val <- xpathSApply(x,"./Publisher/PublisherName",
                                               function(x) paste0("Publisher:",xmlValue(x)))
        val
      })
      grands<-sapply(grands,function(x) paste(unlist(x),collapse=","))
      
      #retrieve abstracts for the searched IDs
      abstract<-xpathSApply(data_pubmed, "//PubmedArticle//Article|//PubmedBookArticle//BookDocument", function(x) {
        val <- xpathSApply(x, "./Abstract", xmlValue)
        if(length(val)==0) val <- xpathSApply(x, "./Abstract/AbstractText", xmlValue)
        if(length(val)==0) val <- NA_character_
        val
      })
      
      #retrieve links
      link<-xpathSApply(data_pubmed, "//PubmedData//ArticleIdList|//PubmedBookArticle//PubmedBookData", function(x) {
        doi <- xpathSApply(x, "./ArticleId[@IdType='doi']", xmlValue)
        pmc <- xpathSApply(x, "./ArticleId[@IdType='pmc']", xmlValue)
        pubmed <- xpathSApply(x, "./ArticleId[@IdType='pubmed']", xmlValue)
        ifelse(length(doi)!=0,paste0("//doi.org/",doi),
               ifelse(length(pmc)!=0,paste0("//pmc/articles/",pmc),
                      paste0("www.ncbi.nlm.nih.gov/pubmed/",pubmed)))
      })
      
      data %<>%
        bind_rows(data.frame(retrieve_ord=seq(chk_seq[k],(chk_seq[k+1]-1)),
                             pubmed_id=ids[chk_seq[k]:(chk_seq[k+1]-1)],
                             title=unlist(title),
                             author=unlist(author),
                             journal=unlist(journal),
                             date=unlist(paste(pub_yr,pub_mth,sep = "-")),
                             abstract=abstract,
                             # mesh_major=mesh_df2$mesh_major,
                             # mesh_minor=mesh_df2$mesh_minor,
                             # link=link,
                             domain=domain,
                             grands=unlist(grands),
                             stringsAsFactors = F))
    }
  }
  metadata<-data.frame(engine="pubmed",
                       sort_by="relevance",
                       search_result=article_cnt,
                       stringsAsFactors = F)
  out<-list(data=data,metadata=metadata)
  return(out)
}

pubmed_scrape<-function(keyword1,keyword2){
  #--build query grid
  query_grid<-expand.grid(key1=keyword1,
                          key2=keyword2,
                          stringsAsFactors = F) %>%
    mutate(query_key=paste0(key1," AND ", key2))
  
  #--search pubmed
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

  return(liter_pubmed)
}

