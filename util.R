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

url_random<-function(title){
  # title<-str_replace_all(title,stopwords_regex,"")
  query_obj<-strsplit(title," ")[[1]]
  query_obj<-query_obj[query_obj!=""]
  len_r<-sample(seq(min(6,length(query_obj)),length(query_obj)),1)
  query<-paste(query_obj[1:len_r],collapse="+")
  
  yr_r<-sample(seq(1990,2009),1)
  item_r<-paste(sample(query_item,5),collapse = "")
  query_url<-paste("https://scholar.google.com/scholar?&as_q=",query,
                  "&as_ylo=",yr_r,item_r,
                   sep = "")
  
  return(query_url)
}

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
    chk_size<-50 # changable
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
      
      #retrieve mesh terms
      mesh_term<-xpathSApply(data_pubmed, "//PubmedArticle//PubmedData//ArticleIdList//ArticleId[@IdType='pubmed']|
                                           //PubmedBookArticle//PubmedBookData//ArticleIdList//ArticleId[@IdType='pubmed']|
                                           //MeshHeadingList",xmlValue)
      mesh_major<-xpathSApply(data_pubmed, "//MeshHeadingList", function(x) {
        val <- xpathSApply(x, "./MeshHeading/DescriptorName[@MajorTopicYN='Y']", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      mesh_minor<-xpathSApply(data_pubmed, "//MeshHeadingList", function(x) {
        val <- xpathSApply(x, "./MeshHeading/DescriptorName[@MajorTopicYN='N']", xmlValue)
        if (length(val)==0) val <- NA_character_
        val
      })
      if(!is.list(mesh_major)|length(mesh_major)==0){
        mesh_major<-paste(mesh_major,collapse=";")
      }else{
        mesh_major<-sapply(mesh_major,function(x) unique(ifelse(is.na(x),NA,paste(x,collapse=";"))))
      }
      if(!is.list(mesh_minor)|length(mesh_minor)==0){
        mesh_minor<-paste(mesh_minor,collapse=";")
      }else{
        mesh_minor<-sapply(mesh_minor,function(x) unique(ifelse(is.na(x),NA,paste(x,collapse=";"))))
      }
      
      mesh_df<-data.frame(id_mesh=unlist(mesh_term),stringsAsFactors = F) %>%
        mutate(rn=1:n()) %>%
        mutate(id_or_mesh=ifelse(id_mesh %in% unlist(ids[chk_seq[k]:(chk_seq[k+1]-1)]),"id","mesh"),
               rn=ifelse(id_mesh %in% unlist(ids[chk_seq[k]:(chk_seq[k+1]-1)]),rn,NA)) %>%
        fill(rn,.direction="up") 
      if(!any(mesh_df$id_or_mesh=="mesh")){
        mesh_df %<>% bind_rows(mesh_df %>% mutate(id_mesh=NA,id_or_mesh="mesh"))
      }
      mesh_df %<>% spread(id_or_mesh,id_mesh)
      mesh_df2<-mesh_df %>% filter(!is.na(mesh)) %>%
        mutate(mesh_major=mesh_major,
               mesh_minor=mesh_minor) %>%
        bind_rows(mesh_df %>% filter(is.na(mesh)) %>% 
                    mutate(mesh_major=NA, mesh_minor=NA)) %>%
        mutate(rn=rank(rn)) %>% arrange(rn)
      
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
                             mesh_major=mesh_df2$mesh_major,
                             mesh_minor=mesh_df2$mesh_minor,
                             link=link,
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


# get_web_of_science_full<-function(){
#   
# }

# get_worldcat_full<-function(){
#   
# }

# get_who_full<-function(){
#   
# }

# get_greyliter_full<-function(){
#   
# }

get_google_scholar_citation<-function(title){
  #sleep before start
  brk_t<-sample(25:40,1)
  Sys.sleep(brk_t)

  #randomly generate url with different structure
  query_url<-url_random(title)
  
  #parse xml
  # agent<-shuffle_agent()
  get_url<-getURL(query_url)
  query<-htmlParse(get_url,encoding="UTF-8")
  
  #retrieve citations
  result<-xpathSApply(query,"//html//body//div[@class='gs_ab_mdw']",xmlValue)
  cite<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']",xmlValue)
  cite_df<-data.frame(gs_fl=unlist(cite)) %>%
    mutate(cited_by=as.numeric(gsub("Cited by ","",str_extract(cite,"(Cited by [[1-9]]+)+"))),
           cited_wos=as.numeric(gsub("Web of Science: ","",str_extract(cite,"(Web of Science: [[1-9]]+)+")))) %>%
    mutate(cited_by2=cited_by,cited_wos2=cited_wos) %>%
    unite("cited_str",c("cited_by2","cited_wos2"),sep=",")

  return(cite_df$cited_str[1])
}

get_google_scholar_full<-function(query,page=1){
  #sleep before start
  brk_t<-sample(30:60,1)
  Sys.sleep(brk_t)
  keywd<-query
  
  #url-friendly version
  query<-strsplit(query," AND ")[[1]]
  phr<-c()
  wd<-c()
  for(i in seq_along(query)){
    if(str_count(trimws(query[i],"both")," ") > 0){
      phr<-gsub(" ","+",trimws(query[i],"both"))
    }else{
      wd<-c(wd,trimws(query[i]))
    }
  }
  if(length(wd)>1){
    wd<-paste0(wd,collapse = "+")
  }
  
  query_url<-paste("https://scholar.google.com/scholar?start=",(page-1)*10,
                   "&as_q=",wd,"&as_epq=",phr,
                   "&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=",
                   "&as_ylo=2010&as_yhi=&hl=en&as_sdt=0%2C5",
                   sep = "")
  
  #parse xml
  # agent<-shuffle_agent()
  get_url<-getURL(query_url)
  query<-htmlParse(get_url,encoding="UTF-8")
  
  #retrieve number of related articles
  result<-xpathSApply(query,"//html//body//div[@class='gs_ab_mdw']",xmlValue)
  article_cnt<-as.numeric(gsub(",","",str_match(result[2],"(About )(.*?)( results)")[,3]))
  
  #retrieve title
  title_full<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//h3",xmlValue)
  title<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//h3//a",xmlValue)
  active_link<-!grepl("(\\[CITATION\\])+",title_full)
  
  #retrieve author, date, journal
  author_dt_journal<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_a']",xmlValue)
  
  #retrieve link
  link<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//h3//a[@data-clk!='undefined']",xmlGetAttr,"href")
  
  #retrieve abstract
  author_abstr<-lapply(xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[contains(@class,'gs_a') or contains(@class,'gs_rs')]",xmlValue),
                   function(x) gsub("\\n"," ",x))
  abstr_align<-data.frame(author_abstr=unlist(author_abstr),stringsAsFactors = F) %>%
    mutate(rn=1:n()) %>%
    mutate(author_or_abstract=case_when((author_abstr %in% unlist(author_dt_journal)) ~ "author",
                                        !(author_abstr %in% unlist(author_dt_journal)) ~ "abstract")) %>%
    mutate(rn=ifelse(author_or_abstract=="author",rn,NA)) %>%
    fill(rn,.direction="down") %>%
    spread(author_or_abstract,author_abstr) %>%
    arrange(rn)
  
  #retrieve citation
  cite<-xpathSApply(query,"//html//body//div[@class='gs_r gs_or gs_scl']//div[@class='gs_ri']//div[@class='gs_fl']",xmlValue)
  cite_df<-data.frame(gs_fl=unlist(cite)) %>%
    mutate(cited_by=as.numeric(gsub("Cited by ","",str_extract(cite,"(Cited by [[1-9]]+)+"))),
           cited_wos=as.numeric(gsub("Web of Science: ","",str_extract(cite,"(Web of Science: [[1-9]]+)+")))) %>%
    mutate(cited_by2=cited_by,cited_wos2=cited_wos) %>%
    unite("cited_str",c("cited_by2","cited_wos2"),sep=",")
  
  actlink_cite<-active_link&(!is.na(cite_df$cited_by))
  data<-data.frame(
    retrieve_ord=seq(10*(page-1)+1,10*page)[actlink_cite],
    title=unlist(title)[actlink_cite],
    author_dt_journal=unlist(author_dt_journal)[actlink_cite],
    cite_by = cite_df$cited_str[actlink_cite],
    abstract=abstr_align$abstract[actlink_cite],
    link=unlist(link)[actlink_cite],
    stringsAsFactors = FALSE) %>%
    mutate(author_dt_journal=gsub("<U+00A0>","",author_dt_journal)) %>%
    separate(author_dt_journal,c("author","journal_date","domain"),"- ",
             extra = "merge",fill="left") %>%
    separate(journal_date,c("journal","date"),", ",
             extra = "merge",fill="left") %>% 
    dplyr::select(retrieve_ord,title,author,journal,date,
                  abstract,cite_by,link,domain)
  
  metadata<-data.frame(query=keywd,
                       engine="google scholar",
                       search_result=article_cnt,
                       sort_by="relevance",
                       stringsAsFactors = F)
  
  out<-list(data=data,metadata=metadata,url=query_url)
  return(out)
}

