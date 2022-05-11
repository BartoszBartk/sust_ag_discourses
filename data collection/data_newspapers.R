#########################################
#data extraction script for German newspaper articles from the wiso database mentioning "nachhaltige landwirtschaft" for use in a topic modelling analysis of sustainable agriculture discourses
#contact: Bartosz Bartkowski, bartosz.bartkowski@ufz.de 
#########################################
####
# Adapted from code provided by Mariana Madruga de Brito, originally used for de Brito, M.M., Kuhlicke, C., Marx, A., 2020. Near-real-time drought impact assessment: a text mining approach on the 2018/19 drought in Germany. Environ. Res. Lett. 15, 1040a9. https://doi.org/10.1088/1748-9326/aba4ca
####
list.of.packages <- c("readr","textreadr","pdfsearch","findR","pdftools","textreuse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load
require(here)
require(dplyr)
require(pdfsearch)
require(pdftools)
require(readr)
require(textreadr)
require(tidyverse)
require(findR)
require(textreuse)

#create list of saved PDF files from wiso-net.de database
files <- list.files(paste0(here(),"/wiso_PDFs/"),full.names=T)

#create an empty matrix for the data
newspapers <- matrix(c(""),0,4)
colnames(newspapers) <- c("id","text","date","source")

#run loop through all PDFs to extract relevant data
for (i in 1:length(files)){
  imported_text <- readtext::readtext(file=files[i])
  full_text <- imported_text$text
  ##extract relevant info
  #extract the basic data for each article (Aachener Zeitung & Aachener Nachrichten from wiso108.pdf don't have dates, same for STERN from wiso133.pdf, same for DIE ZEIT & Neue Westfälische from wiso147.pdf, Hamburger Morgenpost from wiso155.pdf, Franfurter Neue Presse from wiso160.pdf, UniSPIEGEL from wiso115.pdf; strange code for a Rheinische Post article in wiso155.pdf)
  base <- stringr::str_extract_all(string=full_text,pattern="Quelle:[ ]+[ äüöAÖÜßa-zA-Z-0123456789&/:;,.()_\n]{2,80}[ ]*( vom | VOM | v. |, v. |, |, den | )([:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:]|[:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|/dot/dc4/rp/archiv0009|[:digit:][:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]|[:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:])|(Quelle:[ ]+Aachener [äüöAÖÜßa-zA-Z]+)|(Quelle:[ ]+STERN)|(Quelle:[ ]+DIE ZEIT)|(Quelle:[ ]+Neue Westfälische)|(Quelle:[ ]+Hamburger Morgenpost)|(Quelle:[ ]+Frankfurter Neue Presse)|(Quelle:[ ]+UniSPIEGEL)|(Quelle:[ ]+Zeit Campus)|(Quelle:\n\n\n\n[ ]+Seite 2 von 70\n\n[ ]+Dokumente\n\n[ ]+LVZ/Leipziger-Volkszeitung, 01.12.2000)",simplify=T)
  #sources
  sources <- stringr::str_remove_all(string=base,pattern="Quelle:[ ]+|(Quelle:\n\n\n\n[ ]+Seite 2 von 70\n\n[ ]+Dokumente\n\n[ ]+)|( vom| VOM| v. |, |, den |([:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:]|[:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|/dot/dc4/rp/archiv0009|[:digit:][:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]|[:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]))")
  #dates
  dates <- stringr::str_extract_all(string=base,pattern="[:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:]|[:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|/dot/dc4/rp/archiv0009|[:digit:][:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]|[:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]|Aachener|STERN|ZEIT|Neue Westfälische|Hamburger Morgenpost|Frankfurter Neue Presse|UniSPIEGEL|Zeit Campus",simplify=T)[,1]
  #correct Aachener by including NA
  dates <- ifelse(dates=="Aachener"|dates=="STERN"|dates=="ZEIT"|dates=="Neue Westfälische"|dates=="Hamburger Morgenpost"|dates=="/dot/dc4/rp/archiv0009"|dates=="Frankfurter Neue Presse"|dates=="UniSPIEGEL"|dates=="Zeit Campus",NA,dates)
  #cut text at Quelle line, ignore last chunk at the end of final article
  texts <- stringr::str_split(string=full_text,pattern="Quelle:[ ]+[ äüöAÖÜßa-zA-Z-0123456789&/:;,.()_\n]{2,80}[ ]*( vom | VOM | v. |, v. |, |, den | )([:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:][:digit:].[:digit:][:digit:]|[:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:].[:digit:][:digit:].[:digit:][:digit:][:digit:][:digit:]|[:digit:][:digit:].[:digit:].[:digit:][:digit:][:digit:][:digit:]|/dot/dc4/rp/archiv0009|[:digit:][:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:]|[:digit:]. [äa-zA-Z]+ [:digit:][:digit:][:digit:][:digit:])|(Quelle:[ ]+Aachener [äüöAÖÜßa-zA-Z]+)|(Quelle:[ ]+STERN)|(Quelle:[ ]+DIE ZEIT)|(Quelle:[ ]+Neue Westfälische)|(Quelle:[ ]+Hamburger Morgenpost)|(Quelle:[ ]+Frankfurter Neue Presse)|(Quelle:[ ]+UniSPIEGEL)|(Quelle:[ ]+Zeit Campus)|(Quelle:\n\n\n\n[ ]+Seite 2 von 70\n\n[ ]+Dokumente\n\n[ ]+LVZ/Leipziger-Volkszeitung, 01.12.2000)",simplify=T)[,1:length(dates)]
  ##remove irrelevant parts
  #remove ToC heading
  texts <- stringr::str_remove(string=texts,pattern="Dokumente\n\n\n\n\nInhaltsverzeichnis\n\n")
  #remove ToC
  texts <- stringr::str_remove_all(string=texts,pattern="([:digit:]|[:digit:][:digit:]). [ äüöAÖÜßa-zA-Z-0123456789&/:;,.?()»«\"']+( ...\n)*[ ]+[. ]*([:digit:]|[:digit:][:digit:])\n")
  #remove page counts
  texts <- stringr::str_remove_all(string=texts,pattern="Seite ([:digit:]|[:digit:][:digit:]) von [:digit:][:digit:]")
  #remove page headers
  texts <- stringr::str_remove_all(string=texts,pattern="[ ]+Dokumente(\n)+")
  #remove Ressort
  texts <- stringr::str_remove_all(string=texts,pattern="Ressort:[ ]+[äüöAÖÜßa-zA-Z-]+")
  #remove Ausgabe
  texts <- stringr::str_remove_all(string=texts,pattern="Aus?gabe:[ ]+[ äüöAÖÜßa-zA-Z0123456789;/]{5,350}")
  #remove Dokumentnummer
  texts <- stringr::str_remove_all(string=texts,pattern="Dokumentnummer:[ ]+[ a-zA-Z0123456789._-]{5,50}")
  #remove Rubrik
  texts <- stringr::str_remove_all(string=texts,pattern="Rubrik:[ ]+[äüöAÖÜßa-zA-Z]+")
  #replace line breaks \n by spaces
  texts <- stringr::str_replace_all(string=texts,pattern="(\n)+",replacement=" ")
  #remove link, copyright statement, Genios logo
  texts <- stringr::str_remove_all(string=texts,pattern="Dauerhafte Adresse des Dokuments: https://www.wiso-net.de/document/[a-zA-Z0123456789_%-]+ Alle Rechte vorbehalten: [ -äüöAÖÜßa-zA-Z0123456789()&.]{10,80}( Alle Rechte vorbehalten: [ -äüöAÖÜßa-zA-Z0123456789()&.]{10,80})?[ ]+© GBI-Genios Deutsche Wirtschaftsdatenbank GmbH")
    #remove remainders from the Quelle line
  texts <- stringr::str_remove_all(string=texts,pattern="^[, ]{1,3}(S.|Seite|SEITE|Nr.) ([:digit:][:digit:]|[:digit:]|[A-Z][:digit:]|[A-Z][:digit:][:digit:])")
  #add id's
  file_id <- rep(imported_text$doc_id,length(dates))
  article_id <- seq(1:length(dates))
  id <- paste0(file_id,"_",article_id)
  #add extracted data to the results table
  newspapers <- rbind(newspapers,cbind(id,texts,dates,sources))
  #print status
  print(paste0("Successfully extracted ",length(dates)," articles from ",imported_text$doc_id))
}
rm(i,id,imported_text,full_text,base,sources,dates,texts)

#delete NA in text column
newspapers_ <- data.frame(newspapers)
subset <- newspapers_$text
inds <- complete.cases(subset) 
newspapers_clean <- newspapers_[inds,]

save(newspapers_clean,file="data_for_analysis/newspapers.RData")
write.csv(newspapers_clean,"newspapers_data.csv",row.names=F,fileEncoding="UTF-8")

##remove articles that appear multiple times
minhash <- minhash_generator(n=480,seed=2345)
corpus <- TextReuseCorpus(text=newspapers_clean[,"text"],tokenizer=tokenize_ngrams,n=5,
                          minhash_func=minhash,keep_tokens=T,
                          progress=T)
buckets <- lsh(corpus,bands=160,progress=T)

#calculate similarity score to identify doubles among the articles
candidates <- lsh_candidates(buckets)
results <- lsh_compare(candidates,corpus,jaccard_similarity,progress=F)

##clean the results
results$a <- stringr::str_remove(string=results$a,pattern="doc-")
results$b <- stringr::str_remove(string=results$b,pattern="doc-")

to_remove <- results[which(results[,3]>=0.9),2] #similarity score >=0.9 as threshold for removal              
to_remove <- unique(to_remove)
to_remove <- as.numeric(to_remove$b)
to_remove <- sort(to_remove)

remove <- as_data_frame(to_remove)
names(remove)[1] <- "id_2"

#merge remove and the full dataset
newspapers_clean$id_2 <- seq(1:8066)
newspapers_cleaner <- anti_join(newspapers_clean,remove,by="id_2",copy=FALSE)

#clean up environment
rm(buckets,candidates,corpus,remove,results,to_remove,minhash)

#add year based on date
newspapers_cleaner$year <- stringr::str_extract(newspapers_cleaner$date,"[:digit:][:digit:][:digit:][:digit:]")
newspapers_cleaner$year <- ifelse(is.na(newspapers_cleaner$year),
                          stringr::str_remove(newspapers_cleaner$date,"[:digit:][:digit:].[:digit:][:digit:]."),
                          newspapers_cleaner$year)
newspapers_cleaner$year <- as.numeric(newspapers_cleaner$year)
#expand two-digit years
newspapers_cleaner$year <- ifelse(newspapers_cleaner$year<80,
                          newspapers_cleaner$year + 2000,
                          ifelse(newspapers_cleaner$year<100,
                                 newspapers_cleaner$year + 1900,
                                 newspapers_cleaner$year)
)
#impute missing years based on neighbouring articles
for (i in 1:length(newspapers_cleaner$id_2)){
  if (is.na(newspapers_cleaner$year[i])){
    newspapers_cleaner$year[i] <- ifelse(newspapers_cleaner$year[i-1]==newspapers_cleaner$year[i+1]
                                         &!is.na(newspapers_cleaner$year[i+1]),
                                         newspapers_cleaner$year[i+1],
                                         ifelse(is.na(newspapers_cleaner$year[i+1]),
                                                newspapers_cleaner$year[i-1],
                                                ifelse(is.na(newspapers_cleaner$year[i-1]),
                                                       newspapers_cleaner$year[i+1],
                                                       NA)
                                 )
    )
  }
}
#manually add still missing values
newspapers_cleaner$year[1435] <- newspapers_cleaner$year[1434]
newspapers_cleaner$year[1825] <- newspapers_cleaner$year[1824]
newspapers_cleaner$year[2095] <- newspapers_cleaner$year[2094]
newspapers_cleaner$year[3262] <- newspapers_cleaner$year[3263]

#write out
save(newspapers_cleaner,file="data_for_analysis/newspapers_clean.RData")
write.csv(newspapers_cleaner,"newspapers_data_clean.csv",row.names=F,fileEncoding="UTF-8")