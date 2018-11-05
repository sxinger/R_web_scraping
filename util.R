#TODO!
keys_generator<-function(k1=list(),k2=list(),
                         logic=c("AND","OR","BUT")){
  
}


get_pubmed_abstr<-function(query,max_return=20) {
  # change spaces to + and single-quotes to URL-friendly %22 in query
  query<-gsub("'", "%22", gsub(" ", "+", query))
  query<-paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=",
               max_return,"&sort=relevance&term=", 
               query, "&usehistory=y", sep = "")
  
  #add an error-prevention machnism
  counter<-1 #in case there 
  myopt<-curlOptions(connecttimeout = 200)
  get_url<-getURL(query,.opts=myopt)
  temp<-try(get_url)
  while(grepl("(Bad request)|(Error)+",temp) & counter <= 5) {
    get_url<-getURL(query)
    temp<-try(get_url)
    counter<-counter+1
  } 
  
  query<-xmlParse(get_url, useInternal = TRUE)
  
  #collect pubmed ids
  ids<-xpathApply(query,"//IdList/Id",xmlValue)
  ids<-unlist(ids)
  
  if(length(ids)==0){
    abstracts<-list()
  }else{
    #retrieve abstracts for the searched IDs
    data_pubmed<-entrez_fetch(db = "pubmed", id = ids, rettype = "xml",
                              parsed = TRUE)
    
    abstracts<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./Abstract", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
  }
  
  # names(abstracts)<-ids
  return(abstracts)
}


get_pubmed_full<-function(query,max_return=20) {
  # change spaces to + and single-quotes to URL-friendly %22 in query
  query<-gsub("'", "%22", gsub(" ", "+", query))
  query<-paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=",
               max_return,"&sort=relevance&term=", 
               query, "&usehistory=y", sep = "")

  #add an error-prevention machnism
  counter<-1 #in case there 
  myopt<-curlOptions(connecttimeout = 200)
  get_url<-getURL(query,.opts=myopt)
  temp<-try(get_url)
  while(grepl("(Bad request)|(Error)+",temp) & counter <= 5) {
    get_url<-getURL(query)
    temp<-try(get_url)
    counter<-counter+1
  } 
  
  query<-xmlParse(get_url, useInternal = TRUE)
  
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
    author_ln<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/LastName", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    author_fn<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./AuthorList[@CompleteYN='Y']/Author[@ValidYN='Y']/ForeName", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    author<-mapply(function(x,y) paste(unlist(x),unlist(y),sep=","),author_ln,author_fn)
    author<-sapply(author, function(x) paste(unlist(x),collapse = ";"))
    
    #retrieve publication date
    pub_yr<-xpathApply(data_pubmed, "//PubmedData//History", function(x) {
      val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Year", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    pub_mth<-xpathApply(data_pubmed, "//PubmedData//History", function(x) {
      val <- xpathSApply(x, "./PubMedPubDate[@PubStatus='pubmed']/Month", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
      
    #retrieve journal
    journal<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./Journal/Title", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve keywords
    keywds<-xpathApply(data_pubmed, "//MedlineCitation//KeywordList", function(x) {
      val <- xpathSApply(x, "./Keyword", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    
    #retrieve grants
    grands<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./GrantList[@CompleteYN='Y']/Grant/GrantID", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
    grands<-lapply(grands,function(x) paste(unlist(x),collapse=","))
    
    #retrieve abstracts for the searched IDs
    abstract<-xpathApply(data_pubmed, "//PubmedArticle//Article", function(x) {
      val <- xpathSApply(x, "./Abstract", xmlValue)
      if (length(val)==0) val <- NA_character_
      val
    })
  }
  
  # names(abstracts)<-ids
  tot_retn<-length(title)
  return_df<-data.frame(retrieve_ord=seq_len(tot_retn),
                        id=paste0("pubmed:",ids),
                        title=unlist(title),
                        date=unlist(paste(pub_yr,pub_mth,sep = "-")),
                        author=unlist(author),
                        journal=unlist(journal),
                        grands=unlist(grands),
                        # keywds=keywds,
                        abstract=unlist(abstract),
                        stringsAsFactors = F)
  
  return(return_df)
}


get_scopus_full<-function(query,max_return=20,api_key){
  scopus_obj<-scopus_search(query,count=max_return,api_key=api_key)
}
