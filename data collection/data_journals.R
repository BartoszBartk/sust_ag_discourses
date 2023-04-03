#########################################
#script for restructuring data from scientific journal articles (extracted from Scopus and WoS by using the search string "TITLE: sustainab* AND (farm* OR agricultur*)") for use in a topic modelling analysis of sustainable agriculture discourses
#contact: Bartosz Bartkowski, bartosz.bartkowski@ufz.de 
#########################################
####
# Adapted from Nils Droste's code (https://github.com/NilsDroste/AERE)
####
##check installed packages
list.of.packages <- c("tidyverse", "revtools", "here", "xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##load packages
require(tidyverse)
require(revtools)
require(here)
require(xlsx)
require(dplyr)

#read in data
files_wos <-  paste0(here(),"/data_WOS/",list.files(paste0(here(),"/data_WOS/")))
files_scopus <- paste0(here(),"/data_Scopus/",list.files(paste0(here(),"/data_Scopus/")))

##two databases
wos <- files_wos[grep(".txt",files_wos)]
scopus <- files_scopus[grep("scopus_",files_scopus)]

##read in both
scopus_data <- read_bibliography(scopus) %>% rename("doi"="DOI","keywords"="author_keywords","citation_count"="cited_by") %>% mutate("journal"=toupper(source_title)) %>% unite("pages",c("page_start","page_end"),sep="-") %>% select(-source_title)

##run custom function to read wos data (source: https://github.com/alberto-martin/read.wos.R)
source("read_wos_custom.r",echo=T)
wos_data=read.wos(wos)
wos_data<-wos_data%>%
  rename("author"="AU","editors"="BE","title"="TI","journal"="SO","language_of_original_document"="LA","document_type"="DT","conference_name"="CT","conference_date"="CY","conference_location"="CL","sponsors"="SP","keywords"="DE","index_keywords"="ID","abstract"="AB","author_with_affiliations"="C1","correspondence_address"="EM","author_s_id"="RI","funding_details"="FU","funding_text_1"="FX","references"="CR",
         "citation_count"="Z9","publisher"="PU","ISSN"="SN","ISBN"="BN","abbreviated_source_title"="J9","year"="PY","volume"="VL","issue"="IS","art_no"="AR","doi"="D2","pubmed_id"="PM","source"="UT","open_access"="OA","page_count"="PG")%>%
  unite("pages",c("BP","EP"),sep="-")%>%mutate(year=as.numeric(year),"citation_count"=as.numeric(citation_count),"page_count"=as.numeric(page_count))

##combine
data_all <- bind_rows(wos_data,scopus_data)

##exclude duplicated entries
doi_match <- find_duplicates(data_all,
                             match_variable="doi",
                             group_variables=NULL,
                             match_function="exact"
)

data_unique <- extract_unique_references(data_all,doi_match)

##clean up
#check which variable is the unique identifier and keep that!!!
rows_to_keep <- c("author","year","title","journal","abstract","keywords","volume","issue","pages","doi","citation_count","document_type","label")

literature <- data_unique %>% select(rows_to_keep)

#remove all entries with empty abstracts (mainly books reviews, editorials and letters; 229 articles --> not worth bothering to manually include those)
literature <- subset(literature,abstract!="")
literature <- subset(literature,doi!="")

#write out
save(literature,file="data_for_analysis/literature.RData")
write.csv(literature,"journals_data.csv",row.names=F,fileEncoding="UTF-8")