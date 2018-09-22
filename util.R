get_pubmed_abstr<-function(query) {
  # change spaces to + and single-quotes to URL-friendly %22 in query
  query<-gsub("'", "%22", gsub(" ", "+", query))
  query<-paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", 
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



