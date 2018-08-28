#### scrape API ####
install.packages(c("httr", "jsonlite"))

library(tidyr)
library(dplyr)
library(magrittr)
library(httr)

# API parameters
get_rxcui_name <- function(rxcui) {
  rxcui_obj <- GET(url = paste0("http://rxnav.nlm.nih.gov/REST/rxcui/",rxcui,"/"))
  rxcui_content<-content(rxcui_obj)
  if (is.null(rxcui_content$idGroup$name)){
    rxcui_name<-NA
  }else{
    rxcui_name<-rxcui_content$idGroup$name
  }
  return(rxcui_name)
}


# collect rxcui names in batch
med<-data.frame(RXNORM_CUI=c(1191,687078,161,11124,7980))
rxcuis<-unique(med$RXNORM_CUI)
rxcui_map<-data.frame(rxcui=rxcuis) %>%
  do(mutate(.,rxcui_name=lapply(rxcui,get_rxcui_name)))

