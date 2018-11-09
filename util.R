require_libraries<-function(package_list){
  #install missing packages
  new_packages<-package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)>0){
    install.packages(new_packages,repos = "http://cran.us.r-project.org")
  }
  
  for (lib in package_list) {
    library(lib, character.only=TRUE)
    cat("\n", lib, " loaded.", sep="")
  }
}

shuffle_agent<-function(){
  agent_lst<-c(
    #Chrome
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36',
    'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
    'Mozilla/5.0 (Windows NT 5.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
    'Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36',
    'Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
    'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36',
    'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36',
    #Firefox
    'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1)',
    'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)',
    'Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (Windows NT 6.2; WOW64; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/5.0)',
    'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)',
    'Mozilla/5.0 (Windows NT 6.1; Win64; x64; Trident/7.0; rv:11.0) like Gecko',
    'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)',
    'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)',
    'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)'
  )
  sample(agent_lst,1)
}

get_pubmed_full<-function(query,max_return=20) {
  keywd<-query
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
  
  #parse xml
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
    cite<-c()
    for(i in seq_along(title)){
      #rotate agent
      agent<-shuffle_agent()
      #sleep
      brk_t<-sample(20:30,1)
      Sys.sleep(brk_t)
      
      query_url<-paste("https://scholar.google.com/scholar?as_q=",
                       gsub(" ","+",title[i]),
                       "&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=",
                       "&as_ylo=&as_yhi=&hl=en&as_sdt=0%2C5",
                       sep = "")
      
      get_url<-getURL(query_url,.opts=list(useragent=agent,followlocation=TRUE))
      query<-htmlParse(get_url,encoding="UTF-8")
      
      cite_i<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a//text()",xmlValue)
      cite_i<-data.frame(cite_src=unlist(cite_i)[unlist(sapply(cite_i,function(x) grepl("((Cited by)|(Web of Science))+",x)))],
                         stringsAsFactors = F) %>%
        mutate(cite_type=str_extract(cite_src,"((Cited by)|(Web of Science))+"),
               cite_val=as.numeric(gsub("((Cited by )|(Web of Science: ))+","",cite_src)),
               rn=ifelse(cite_type=="Cited by",1:n(),NA)) %>%
        fill(rn,.direction="down") %>% ungroup %>%
        dplyr::select(rn,cite_type,cite_val) %>%
        spread(cite_type,cite_val)
      
      cite %<>% bind_rows(cite_i)
    }
  }
  
  tot_retn<-length(title)
  data<-data.frame(retrieve_ord=seq_len(tot_retn),
                   title=unlist(title),
                   author=unlist(author),
                   journal=unlist(journal),
                   date=unlist(paste(pub_yr,pub_mth,sep = "-")),
                   abstract=unlist(abstract),
                   cite_gs=cite$`Cited by`,
                   cite_wos=cite$`Web of Science`,
                   link=link,
                   domain=domain,
                   grands=unlist(grands),
                   stringsAsFactors = F)
  
  metadata<-data.frame(query=keywd,
                       engine="pubmed",
                       search_result=article_cnt,
                       sort_by="relevance")
  
  out<-list(data=data,metadata=metadata)
  return(out)
}


get_google_scholar_full<-function(query,max_return=20){
  keywd<-query
  #url-friendly version
  query<-strsplit(query," AND ")[[1]]
  for(i in seq_along(query)){
    if(str_count(trimws(query[i],"both")," ") > 0){
      phr<-gsub(" ","+",trimws(query[i],"both"))
    }else{
      wd<-trimws(query[i])
    }
  }

  pg_n<-ceiling(max_return/10)
  data<-c()
  for(i in seq_len(pg_n)){
    #rotate agent
    agent<-shuffle_agent()
    #sleep
    brk_t<-sample(20:30,1)
    Sys.sleep(brk_t)
    
    query_url<-paste("https://scholar.google.com/scholar?start=",(i-1)*10,
                     "&as_q=",wd,"&as_epq=",phr,
                     "&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=",
                     "&as_ylo=2000&as_yhi=&hl=en&as_sdt=0%2C5",
                     sep = "")
    
    #parse xml
    get_url<-getURL(query_url,.opts=list(useragent=agent,followlocation=TRUE))
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
    cite<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']//a//text()",xmlValue)
    cite<-data.frame(cite_src=unlist(cite)[unlist(sapply(cite,function(x) grepl("((Cited by)|(Web of Science))+",x)))],
                     stringsAsFactors = F) %>%
      mutate(cite_type=str_extract(cite_src,"((Cited by)|(Web of Science))+"),
             cite_val=as.numeric(gsub("((Cited by )|(Web of Science: ))+","",cite_src)),
             rn=ifelse(cite_type=="Cited by",1:n(),NA)) %>%
      fill(rn,.direction="down") %>% ungroup %>%
      dplyr::select(rn,cite_type,cite_val) %>%
      spread(cite_type,cite_val)

    data %<>% 
      bind_rows(data.frame(
        retrieve_ord=seq(10*(i-1)+1,10*i),
        title=unlist(title),
        author_dt_journal=unlist(author_dt_journal),
        cite_gs = cite$`Cited by`,
        cite_wos = cite$`Web of Science`,
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
  
  data %<>% 
    dplyr::select(retrieve_ord,
                  title,
                  author,
                  journal,
                  date,
                  abstract,
                  cite_gs,
                  cite_wos,
                  link,
                  domain)
  
  metadata<-data.frame(query=keywd,
                       engine="google scholar",
                       search_result=article_cnt,
                       sort_by="relevance")
  
  out<-list(data=data,metadata=metadata)
  return(out)
}

#TODO
get_scopus_full<-function(query,max_return=20,api_key){
  scopus_obj<-scopus_search(query,count=max_return,api_key=api_key)
}


get_wos_full<-function(query,max_return=20,api_key){
  
}
