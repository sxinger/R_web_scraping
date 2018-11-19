rm(list=ls())

source("./util.R")
require_libraries(c("XML",
                    "RCurl",
                    "rvest",
                    "httr",
                    "stringr",
                    "dplyr",
                    "tidyr",
                    "magrittr",
                    "ggplot2",
                    "fuzzyjoin"))

#lab keys
lab_key<-readRDS("./data/var_lst.rda") %>%
  dplyr::filter(VARIABLE_CATEG %in% c("LABORATORY TESTS",
                                      "LABTESTS",
                                      "CARDIOLABTESTS")) %>%
  mutate(key=gsub("\\(.*\\)","",C_NAME)) %>%
  mutate(key=gsub("\\\\","",key)) %>%
  mutate(key=gsub("\\%","",key)) %>%
  mutate(key=ifelse(!is.na(MOD)&!grepl("(COMPONENT_ID)+",Feature),paste0(tolower(key),MOD),
                    tolower(key))) %>%
  mutate(key=str_replace_all(key,"@@","")) %>%
  mutate(key_for_search=trimws(gsub("@.*","",key),"both")) %>%
  mutate(key_for_search=recode(key_for_search,
                               `bld urea nitrogen`="bun",
                               `microalb/cr ratio-urine random`="microalbumin creatinine ratio",
                               `hyperlipidem`="hyperlipidemia",
                               `glucose,random`="glucose",
                               `microalbumin, ran`="microalbumin",
                               `poc glucose`="glucose",
                               `absolute lymph count`="lymph",
                               `alk phosphatase`="alp",
                               `absolute mono count`="mono",
                               `urine spec gravity`="urine specific gravity",
                               `absolute eos count`="eosinophil",
                               `tv rest pulmonary artery pressure`="pulmonary artery pressure at rest",
                               `squamous epi cells`="squamous epithelial cells",
                               `peak bp`="peak blood pressure",
                               `baseline bp`="baseline blood pressure")) %>%
  dplyr::select(Feature,rank,C_VISUAL_PATH,C_NAME,MOD,key_for_search)

####==============one-at-a-time google search===================
keys<-unique(lab_key$key_for_search)
normal_range<-c()
normal_range_ref<-c()
for(i in seq_along(keys)){
  start_i<-Sys.time()
  
  #sleep before start
  brk_t<-sample(25:40,1)
  Sys.sleep(brk_t)

  url<-paste0("https://google.com/search?q=",gsub(" ","+",keys[i]),"+normal+range")
  get_url<-GET(url)
  query<-htmlParse(get_url, encoding="UTF-8")
  range_parse<-xpathSApply(query,"//div[@class='KpMaL']",xmlValue)
  range_ref<-xpathSApply(query,"//div[@class='hJND5c']//cite",xmlValue)
  range_parse2<-xpathSApply(query,"//span[@class='st']",xmlValue)
  
  if(length(range_parse)==0){
    range_parse<-paste0(unlist(range_parse2),collapse = "||")
  }
  normal_range<-c(normal_range,range_parse)
  normal_range_ref<-c(normal_range_ref,range_ref[1])
  
  lapse_i<-Sys.time()-start_i
  cat("finish search normal range for",keys[i],"in",lapse_i,units(lapse_i),".\n")
}

#--parsing out the value range
#manually create a list of expressions suggesting ranges
num<-"[0-9\\.\\,\\%]{1,}"
rg_exp<-paste0("(",
               paste(paste("between",num,"(and|to)",num),
                     paste0(num," ?(to|-|–) ?",num),
                     paste0(num," ?± ?",num),
                     sep = ")|("),
               ")")

range_df<-data.frame(keys=keys,
                     raw_text=normal_range,
                     ref_link=normal_range_ref,
                     stringsAsFactors = F) %>%
  mutate(lab_range=trimws(str_extract(tolower(raw_text),rg_exp),"both")) %>%
  mutate(raw_text2=str_replace(tolower(raw_text),rg_exp,"@@@@")) %>%
  separate("raw_text2",c("text_bef","text_aft"),sep="@@@@",fill="right",extra="merge")

#auto-curation
range_df2<-range_df %>% filter(!is.na(lab_range)) %>%
  mutate(lab_range=gsub("\\,","",lab_range)) %>%
  mutate(lab_num=str_extract_all(lab_range,num)) %>%
  separate("lab_num",c("lab_low","lab_high"),sep=",",
           extra="merge",fill="right") %>%
  mutate(lab_low=gsub("(c\\(\")|(\")","",lab_low),
         lab_high=gsub("(\")|(\"\\))","",lab_high)) %>%
  mutate(percentage=ifelse(grepl("\\%",lab_low)|grepl("\\%",lab_high),T,F)) %>%
  mutate(lab_low=str_replace(lab_low,"\\%",""),
         lab_high=str_replace_all(lab_high,"\\%","")) %>%
  mutate(lab_high=gsub("\\.$","",lab_high)) %>%
  mutate(lab_low=as.numeric(lab_low),
         lab_high=as.numeric(lab_high)) %>%
  mutate(lab_low2=ifelse(lab_high<lab_low,lab_low-lab_high,lab_low),
         lab_high2=ifelse(lab_high<lab_low,lab_low+lab_high,lab_high))

#manual-curation
range_df3<-range_df %>% filter(is.na(lab_range)) %>%
  mutate(lab_range=recode(keys,
                          "vitamin d"="20,50",
                          "vitamin b12"="100,Inf",
                          "c-reactive protein"="1.0,3.0",
                          "left ventricular internal dimension in diastole"="-Inf,5.6",
                          "bsa"="-Inf,1.7",
                          "rbc, ua"="-Inf,4")) %>%
  separate("lab_range",c("lab_low","lab_high"),sep=",") %>%
  mutate(lab_low=as.numeric(lab_low),
         lab_high=as.numeric(lab_high))

#out
lab_rg_cur<-range_df2 %>% dplyr::select(keys,lab_low2,lab_high2,raw_text,ref_link) %>%
  dplyr::rename(lab_low=lab_low2,lab_high=lab_high2) %>%
  bind_rows(range_df3 %>% dplyr::select(keys,lab_low,lab_high,raw_text,ref_link))

lab_range<-lab_key %>%
  left_join(lab_rg_cur,by=c("key_for_search"="keys"))

saveRDS(lab_range,file="./data/lab_value_range.rda")
write.csv(lab_range,file="./data/lab_value_range.csv",row.names = F)


####================one-shot scraping: meditect==================####
##only able to match 60% of labs and exist some ambiguities
url<-"https://www.meditec.com/resourcestools/medical-reference-links/normal-lab-values"
doc<-GET(url)
parse_doc<-htmlParse(doc,encoding="UTF-8")

lab_grp<-xpathSApply(parse_doc,"//div[@id='ValuesWrapper']//h3",xmlValue)
lab_val<-xpathSApply(parse_doc,"//div[@id='ValuesWrapper']//ul",xmlValue)
lab_df<-data.frame(lab_grp=unlist(lab_grp),
                   lab_val=unlist(lab_val),
                   stringsAsFactors = F) %>%
  mutate(lab_n=str_count(lab_val,"\\n")) %>%
  mutate(lab_val=trimws(gsub("\\n",";",lab_val),"both"),
         lab_grp=trimws(gsub("\\.","",gsub("(Back to top)+","",lab_grp)),"both")) %>%
  separate("lab_val",paste0("lab",1:53),sep=";",fill="right",extra="drop") %>%
  gather(lab_idx,lab_val,-lab_grp,-lab_n) %>%
  filter(!is.na(lab_val)) %>%
  dplyr::select(-lab_idx) %>%
  mutate(lab_val2=str_replace(lab_val,"( [[0-9]\\.]{1,} – [[0-9]\\.]{1,} )|(( Less than | Greater than )[[0-9]\\.]{1,} )"," @@ "),
         lab_range=trimws(str_extract(lab_val,"( [[0-9]\\.]{1,} – [[0-9]\\.]{1,} )|(( Less than | Greater than )[[0-9]\\.]{1,} )"),"both")) %>%
  filter(!is.na(lab_range)) %>%
  separate("lab_val2",c("lab_nm","lab_units"),sep=" @@ ",fill="right",extra="merge") %>%
  separate("lab_nm",c("lab_key","lab_subtype"),sep="\\(",fill="right",extra="merge") %>%
  mutate(lab_subtype=gsub("\\)","",lab_subtype),
         lab_key=tolower(trimws(lab_key,"both")))



  
