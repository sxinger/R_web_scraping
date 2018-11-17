rm(list=ls())

source("./util.R")
require_libraries(c("XML",
                    "RCurl",
                    "rvest",
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
  mutate(key_for_search=gsub("@.*","",key)) %>%
  mutate(key_for_search=recode(key_for_search,
                               `bld urea nitrogen`="bun",
                               `microalb/cr ratio-urine random`="microalbumin creatinine ratio",
                               `hyperlipidem`="hyperlipidemia",
                               `glucose,random`="glucose",
                               `microalbumin, ran`="microalbumin",
                               `poc glucose`="glucose",
                               `absolute lymph count`="lymph",
                               `absolute mono count`="mono",
                               `urine spec gravity`="urine specific gravity",
                               `absolute eos count`="eosinophil")) %>%
  dplyr::select(Feature,rank,C_VISUAL_PATH,C_NAME,MOD,key_for_search)

####==============one-at-a-time google search===================
keys<-unique(lab_key$key_for_search)
normal_range<-c()
for(i in seq_along(keys)){
  start_i<-Sys.time()
  
  #sleep before start
  brk_t<-sample(30:60,1)
  Sys.sleep(brk_t)

  url<-paste0("https://google.com/search?q=",gsub(" ","+",keys[i]),"+lab+normal+range")
  get_url<-GET(url)
  query<-htmlParse(get_url, encoding="UTF-8")
  range_parse<-xpathSApply(query,"//div[@class='KpMaL']",xmlValue)
  
  if(length(range_parse)==0){
    range_parse<-NA
  }
  normal_range<-c(normal_range,range_parse)
  
  lapse_i<-Sys.time()-start_i
  cat("finish search normal range for",keys[i],"in",lapse_i,units(lapse_i),".\n")
}
#--parsing out the value range
#manually create a list of expressions suggesting ranges
num<-"[0-9\\.]{1,}"
rg_exp<-paste0("(",
               paste(paste("between",num,"and",num),
                     paste("(less|greater) than (or equal to)",num),
                     paste(num,"or (higher|less)"),
                     paste(num," (to|-) ",num),
                     collapse = ")|("),
               ")")

max_sentence<-max(sapply(normal_range,function(x) str_count("\\.",x)),na.rm=T)
range_df<-data.frame(keys=keys,raw_text=normal_range,stringsAsFactors = F) %>%
  separate(raw_text,paste0("s",1:(max_sentence+5)),fill="right",extra="merge") %>%
  gather(sentence,raw_text,-keys) %>%
  filter(!is.na(raw_text)&grepl("[0:9]",raw_text)) %>%
  mutate(lab_val2=str_replace(lab_val,rg_exp,"@@@@"),
         lab_range=trimws(str_extract(lab_val,rg_exp),"both")) %>%
  separate("lab_val2",c("rg_prefix","rg_suffix"),sep="@@@@",fill="right",extra="merge")


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



  
