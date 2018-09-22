rm(list=ls())

# trace(utils:::unpackPkgZip, edit=TRUE)
# install.packages("XML")
# install.packages("stringr")
# install.packages("ggrepel")

library(rentrez)
library(XML)
library(RCurl)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggrepel)
source("./util.R")

dkd_synonyms<-c("DKD",
                "diabetic kidney disease",
                "diabetic renal disease",
                "diabetic nephropathy")
ckd_synonyms<-c("CKD",
                "chronic kidney disease",
                "chronic renal disease",
                "chronic renal failure")
esrd_synonyms<-c("ESRD",
                 "end-stage renal disease",
                 "end-stage renal failure")
target_key<-paste(c(paste0("'",dkd_synonyms,"'"),
                    paste0("'",ckd_synonyms,"'"),
                    paste0("'",esrd_synonyms,"'")),
                  collapse = " OR ")
target_key2<-paste0("(",
                    paste(c(paste0("(",dkd_synonyms,")"),
                            paste0("(",ckd_synonyms,")"),
                            paste0("(",esrd_synonyms,")")),
                          collapse = "|"),
                    ")+")
# load("./data/feature_set_opt.Rdata")
# opt_set %<>%
#   dplyr::select(Feature,VARIABLE_CATEG, C_VISUAL_PATH, C_NAME, rank, sel_cnt, pat_cnt, dkd_rate) %>%
#   mutate(MOD=str_extract(Feature,"(@).*"))
# saveRDS(opt_set,"./data/var_lst.rda")

vari_key<-readRDS("./data/var_lst.rda") %>%
  mutate(key=ifelse(VARIABLE_CATEG=="REPORTS",
                    str_extract(gsub("\\\\Yes","",C_VISUAL_PATH),'(\\\\)[^\\\\]*$'),
                    C_NAME)) %>%
  mutate(key=gsub("\\(.*\\)","",key)) %>%
  mutate(key=gsub("\\\\","",key)) %>%
  mutate(key=gsub("\\%","",key)) %>%
  mutate(key=ifelse(!is.na(MOD)&!grepl("(COMPONENT_ID)+",Feature),paste0(tolower(key),MOD),
                    tolower(key))) %>%
  group_by(key) %>% top_n(n=1L,wt=-rank) %>% ungroup %>%
  mutate(key=str_replace_all(key,"@@","")) %>%
  mutate(key_for_search=gsub("@.*","",key))

#manual clean-up
vari_key %<>%
  mutate(key_for_search=recode(key_for_search,
                               `bld urea nitrogen`="blood urea nitrogen",
                               `microalb/cr ratio-urine random`="microalbumin creatinine ratio",
                               `gender_male`="sex",
                               `race_white`="race",
                               `race_other`="race",
                               `packs per day`="smoking",
                               `years of tabacco usage`="tabacco",
                               `jpc-kumw im`="internal medicine",
                               `hyperlipidem`="hyperlipidemia",
                               `glucose,random`="glucose",
                               `microalbumin, ran`="microalbumin",
                               `poc glucose`="glucose",
                               `absolute lymph count`="lymph",
                               `absolute mono count`="mono",
                               `urine spec gravity`="urine specific gravity",
                               `absolute eos count`="eosinophil")) %>%
  mutate(key_for_search=ifelse(VARIABLE_CATEG=="MEDICATIONS",gsub(" .*","",key_for_search),key_for_search))

batch_size<-50
batch_num<-round(nrow(vari_key)/batch_size)+1
abstr_key<-list()
article_cnt<-c()

start_b<-1 #allow continuous search, if break
for(b in start_b:batch_num){
  if(b==batch_num){
    seq_b<-((b-1)*batch_size+1):nrow(vari_key)
  }else{
    seq_b<-((b-1)*batch_size+1):(b*batch_size)
  }
  
  # seq_b<-seq(318,350) #manually set-up in case of break
  for(i in seq_b){
    start_i<-Sys.time()
    
    query<-paste0(target_key," AND ",paste0("'",vari_key$key_for_search[i],"'"))
    
    abstr<-get_pubmed_abstr(query)
    
    if(length(abstr) > 0){
      #clean-up: contain target words?
      abstr_chk1<-unlist(lapply(abstr,function(x) grepl(target_key2,x,ignore.case = T)))
      #clean-up: contain variable word?
      abstr_chk2<-unlist(lapply(abstr,function(x) grepl(vari_key$key_for_search[i],x,ignore.case = T)))
      
      abstr<-abstr[seq_along(abstr_chk1)[abstr_chk1 & abstr_chk2]]
    }
    
    abstr_key[[paste(i,vari_key$key[i],sep = ".")]]<-abstr
    article_cnt<-c(article_cnt,length(abstr))
    
    lapse_i<-Sys.time()-start_i
    cat("finish searching for",vari_key$key[i],"in",lapse_i,units(lapse_i),".\n")
  }
  
  Sys.sleep(30) #to allow continuous search
  
}


result<-vari_key %>%
  dplyr::select(VARIABLE_CATEG,key,rank,sel_cnt) %>%
  mutate(pubmed_cnt=article_cnt) %>%
  mutate(VARIABLE_CATEG=recode(VARIABLE_CATEG,
                               CARDIOLABTESTS="LABORATORY TESTS",
                               MICROBIOLOGY="LABORATORY TESTS",
                               LABTESTS="LABORATORY TESTS",
                               PROCORDERS="ORDERS,PROCEDURES",
                               PROCEDURES="ORDERS,PROCEDURES",
                               NCDR="REPORTS,EXTERNAL REGISTRIES",
                               NTDS="REPORTS,EXTERNAL REGISTRIES",
                               REPORTS="REPORTS,EXTERNAL REGISTRIES",
                               UHC="UHC,DIAGNOSES",
                               DIAGNOSES="UHC,DIAGNOSES",
                               DEMOGRAPHICS="DEMOGRAPHICS, HISTORY",
                               HISTORY="DEMOGRAPHICS, HISTORY",
                               ALERTS="ALERTS, ALLERGY",
                               ALLERGY="ALERTS, ALLERGY"))

result %<>%
  mutate(label=ifelse((pubmed_cnt<=8|pubmed_cnt>=48) & rank<=150,key,"")) %>%
  mutate(label=recode(label,
                      `gender_male`="sex_male",
                      `jpc-kumw im`="internal medicine visit")) %>%
  mutate(label=ifelse(str_count(label)>=60,
                      paste0(substr(label,1,57),"-\n",substr(label,58,str_count(label))),
                      label))

saveRDS(abstr_key,file="./data/abstr_key.rda")
saveRDS(result,file="./data/result.rda")

#plot
abstr_key<-readRDS("./data/abstr_key.rda")
result<-readRDS("./data/result.rda")

ggplot(result %>% filter(sel_cnt >= 10),
       aes(x=rank,y=pubmed_cnt,label=label))+
  geom_point()+geom_label_repel()+
  facet_wrap(~VARIABLE_CATEG,ncol=2) +
  labs(x="Importance Rank",y="Article Counts on PubMed (max:50)")

#print out some results
abstr_key$`2.anion gap`

result %>%
  group_by(pubmed_cnt) %>%
  dplyr::summarize(cd_cnt=n(),
                   min_rank=min(rank),
                   q1_rank=quantile(rank,probs=0.25),
                   med_rank=median(rank),
                   q3_rank=quantile(rank,probs=0.75),
                   max_rank=max(rank)) %>%
  View

result %>%
  filter(pubmed_cnt==0) %>%
  group_by(VARIABLE_CATEG) %>%
  dplyr::summarize(cnt=n())

