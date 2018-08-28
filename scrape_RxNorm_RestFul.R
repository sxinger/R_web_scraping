#### scrape API ####
trace(utils:::unpackPkgZip, edit=TRUE)

install.packages("doParallel")
# install.packages("httr")

library(tidyr)
library(dplyr)
library(magrittr)
library(httr)
library(doParallel)

# API parameters
get_rxcui_name <- function(rxcui) {
  rx_url<-paste0("http://rxnav.nlm.nih.gov/REST/rxcui/",rxcui,"/")
  rxcui_obj <- GET(url = rx_url)
  rxcui_content<-content(rxcui_obj)
  if (is.null(rxcui_content$idGroup$name)){
    rxcui_name<-NA
  }else{
    rxcui_name<-rxcui_content$idGroup$name
  }
  return(rxcui_name)
}


get_rxcui_name_batch<-function(rxcuis){
  rxcui_map<-data.frame(rxcui=rxcuis) %>%
    do(mutate(.,rxcui_name=lapply(rxcui,get_rxcui_name)))
  return(rxcui_map)
}


# collect rxcui names in batch
med<-data.frame(RXNORM_CUI=c(c(1191,687078,161,11124,7980),
                             sample(1000:10000,50,replace=F)))
rxcuis<-unique(med$RXNORM_CUI)

#test functions
get_rxcui_name(rxcuis[1])
get_rxcui_name_batch<-get_rxcui_name_batch(rxcuis)

