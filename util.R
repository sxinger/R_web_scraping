
get_pubmed_full<-function(query,max_return=20) {
  # change spaces to + and single-quotes to URL-friendly %22 in query
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
    get_url<-getURL(query)
    temp<-try(get_url)
    counter<-counter+1
  } 
  
  query<-xmlParse(get_url, encoding="UTF-8", useInternal = TRUE)
  
  #retrieve number of related articles
  result<-xpathApply(query,"//Count",xmlValue)
  article_cnt<-as.numeric(result[1])
  
  #collect pubmed ids
  ids<-xpathApply(query,"//IdList/Id",xmlValue)
  ids<-unlist(ids)
  
  if(length(ids)==0){
    abstracts<-list()
  }else{
    data_pubmed<-entrez_fetch(db = "pubmed", id = ids, rettype = "xml",
                              parsed = TRUE)
    #retrieve title
    title<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./ArticleTitle", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve author
    author_ln<-xpathSApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/LastName", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    author_fn<-xpathSApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/ForeName", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    author<-mapply(function(x,y) paste(unlist(x),unlist(y),sep=","),author_ln,author_fn)
    author<-sapply(author, function(x) paste(unlist(x),collapse = ";"))
    
    #retrieve publication date
    pub_yr<-xpathSApply(data_pubmed, "//PubmedData//History", function(x) {
      val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Year", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    pub_mth<-xpathSApply(data_pubmed, "//PubmedData//History", function(x) {
      val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Month", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
      
    #retrieve journal
    journal<-xpathSApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./Journal/Title", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve domain
    domain<-xpathSApply(data_pubmed, "//PubmedArticle", function(x) {
      val <- xpathSApply(x, "./MedlineCitation", xmlGetAttr,"Status")
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve grants
    grands<-xpathSApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./GrantList[@CompleteYN='Y']/Grant/GrantID", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    grands<-lapply(grands,function(x) paste(unlist(x),collapse=","))
    
    #retrieve abstracts for the searched IDs
    abstract<-xpathSApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./Abstract", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve links
    link<-xpathSApply(data_pubmed, "//PubmedData//ArticleIdList", function(x) {
      doi <- xpathSApply(x, "./ArticleId[@IdType='doi']", xmlValue)
      pmc <- xpathSApply(x, "./ArticleId[@IdType='pmc']", xmlValue)
      ifelse(length(doi)==0,
             paste0("/pmc/articles/",pmc),
             paste0("//doi.org/",doi))
    })
    
    #retrieve citation by searching google scholar
    title<-unlist(title)
    cite_by<-c()
    cite_wos<-c()
    for(i in seq_along(title)){
      query_url<-paste("https://scholar.google.com/scholar?as_q=",
                       gsub(" ","+",title[i]),
                       "&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=",
                       "&as_ylo=&as_yhi=&hl=en&as_sdt=0%2C5",
                       sep = "")
      
      get_url<-getURL(query_url,.opts=myopt)
      query<-htmlParse(get_url,encoding="UTF-8")
      
      cite_by_i<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Cited by')]//text()",xmlValue)
      cite_by<-c(cite_by,as.numeric(gsub("(Cited by )+","",cite_by_i)))

      cite_wos_i<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Web of Science')]//text()",xmlValue)
      cite_wos<-c(cite_wos,as.numeric(gsub("(Web of Science: )+","",cite_wos_i)))
      
      brk_t<-sample(10:30,1)
      Sys.sleep(brk_t)
    }
  }
  
  tot_retn<-length(title)
  return_df<-data.frame(retrieve_ord=seq_len(tot_retn),
                        title=unlist(title),
                        author=unlist(author),
                        date=unlist(paste(pub_yr,pub_mth,sep = "-")),
                        journal=unlist(journal),
                        abstract=unlist(abstract),
                        cite_gs=cite_by,
                        cite_wos=cite_wos,
                        link=link,
                        grands=unlist(grands),
                        domain=domain,
                        stringsAsFactors = F)
  return(return_df)
}


get_google_scolar_full<-function(query,max_return=20){
  query<-strsplit(query," AND ")[[1]]
  for(i in seq_along(query)){
    if(str_count(trimws(query[i],"both")," ") > 0){
      phr<-gsub(" ","+",trimws(query[i],"both"))
    }else{
      wd<-trimws(query[i])
    }
  }

  pg_n<-ceiling(max_return/10)
  return_df<-c()
  for(i in seq_len(pg_n)){
    query_url<-paste("https://scholar.google.com/scholar?start=",(i-1)*10,
                     "&as_q=",wd,"&as_epq=",phr,
                     "&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=",
                     "&as_ylo=2000&as_yhi=&hl=en&as_sdt=0%2C5",
                     sep = "")
    
    #add an error-prevention machanism
    counter<-1 #in case there
    myopt<-curlOptions(connecttimeout = 200)
    get_url<-getURL(query_url,.opts=myopt)
    temp<-try(get_url)
    while(grepl("(Bad request)|(Error)+",temp) & counter <= 5) {
      get_url<-getURL(query)
      temp<-try(get_url)
      counter<-counter+1
    } 
    
    query<-htmlParse(get_url,encoding="UTF-8")
    
    if(i==1){
      #retrieve number of related articles
      result<-xpathSApply(query,"//html//body//div[@class='gs_ab_mdw']",xmlValue)
      article_cnt<-as.numeric(gsub(",","",str_match(result[2],"(About )(.*?)( results)")[,3]))
    }
    
    #retrieve title
    title<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//h3//a",xmlValue)

    #retrieve author, date, journal
    author_dt_journal<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_a']",xmlValue)

    #retrieve link
    link<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//h3//a[@data-clk!='undefined']",xmlGetAttr,"href")
    
    #retrieve abstract
    abstract<-lapply(xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_rs']",xmlValue),
                     function(x) gsub("\\n"," ",x))
    
    #retrieve citation
    cite_by<-sapply(xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Cited by')]//text()",xmlValue),
                    function(x) as.numeric(gsub("(Cited by )+","",x)),USE.NAMES=F) 
    cite_wos<-sapply(xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Web of Science')]//text()",xmlValue),
                     function(x) as.numeric(gsub("(Web of Science: )+","",x)),USE.NAMES=F)
    
    return_df %<>% 
      bind_rows(data.frame(
        retrieve_ord=seq(10*(i-1)+1,10*i),
        title=unlist(title),
        author_dt_journal=unlist(author_dt_journal),
        cite_gs = cite_by,
        cite_wos = cite_wos,
        abstract=unlist(abstract),
        link=unlist(link),
        stringsAsFactors = FALSE) %>%
          mutate(author_dt_journal=gsub("<U+00A0>","",author_dt_journal)) %>%
          separate(author_dt_journal,c("author","journal_date","domain"),"- ",
                   extra = "merge",fill="left") %>%
          separate(journal_date,c("journal","date"),", ",
                   extra = "merge",fill="left"))
    
    brk_t<-sample(10:30,1)
    Sys.sleep(brk_t)
  }

  

  
  
}

get_scopus_full<-function(query,max_return=20,api_key){
  scopus_obj<-scopus_search(query,count=max_return,api_key=api_key)
}
