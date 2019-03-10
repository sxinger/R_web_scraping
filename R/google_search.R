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

google_scrape<-function(keyword1,keyword2){
  #--build query grid
  query_grid<-expand.grid(key1=keyword1,
                          key2=keyword2,
                          stringsAsFactors = F) %>%
    mutate(query_key=paste0(key1," AND ", key2))
  
  #--search google scholar
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
  
  liter_google<-list(liter_data=data,
                        liter_meta=metadata)
  
  return(liter_google)
}