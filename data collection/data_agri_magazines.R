#########################################
#data extraction script for texts or files of all articles mentioning "nachhaltige landwirtschaft" from German agricultural magazines Top Agrar, AgrarHeute and DLG Mitteilungen for use in a topic modelling analysis of sustainable agriculture discourses
#contact: Bartosz Bartkowski, bartosz.bartkowski@ufz.de 
#########################################
require(here)
require(rvest)
require(stringr)
require(xml2)
require(XML)
require(dplyr)

setwd(here())

################### TopAgrar ##################################
##### Search for "nachhaltige landwirtschaft" on 19.10.2021 --> 1359 entries
#### Note: for some reason, the same search returns only 212 articles on 11.05.2022
#create empty object to save a list of search pages in
search_pages <- NULL
#create a vector of search page addresses (6 articles per page --> 227 pages)
for (j in 1:227){
  search_pages[j] <- paste("https://www.topagrar.com/?-type=standardbeitrag&doSearch=1&query=%22nachhaltige%20landwirtschaft%22&search=&sortfieldwithorder=date_desc&date_years=&filterbytree=&page=",j,sep="")
}
search_pages <- unlist(search_pages)
##extract links to articles for all search pages
#create an empty object to save list of links in
link_list <- NULL
for (k in 1:length(search_pages)){
  search_page <- read_html(search_pages[k]) #access the page
  entries <- html_elements(search_page,"h2.m-teaser__title") #extract article entries
  links <- html_elements(entries,"a") %>% html_attr("href") #extract links to all articles listed on page
  link_list <- append(link_list,links) #append links to the vector link_list for later use
}
#special TopAgrar: make full links
links_list <- NULL
for (l in 1:length(link_list)){
  links_list[l] <- paste("https://www.topagrar.com",link_list[l],sep="")
}
#generate an empty object to be used later to save results
topagrar_texts <- as.data.frame(matrix(ncol=6,nrow=0))
#extract article date, title, teaser and text from all 50 articles on the page based on HTML tags
for (i in 1:length(links_list)){
  page <- read_html(links_list[i]) #navigate to each article
  topagrar_texts[i,1] <- i #running number of entry
  topagrar_texts[i,2] <- html_text2(html_element(page,"time.m-metadata__item")) #extract date as text
  topagrar_texts[i,3] <- html_text2(html_element(page,"a.m-metadata__item.m-metadata__authorLink")) #extract author as text
  topagrar_texts[i,4] <- html_text2(html_element(page,"h1.a-title")) #extract article title as text
  topagrar_texts[i,5] <- html_text2(html_element(page,"p.a-lead")) #extract teaser as text
  topagrar_texts[i,6] <- html_text2(html_element(page,"div.l-articleview__pagetext")) #extract main text as text
}
colnames(topagrar_texts) <- c("No","Date","Author","Title","Abstract","Main")
write.csv(topagrar_texts,"topagrar_texts.csv",row.names=F,fileEncoding="UTF-8")


################### AgrarHeute ##################################
##### Search for "nachhaltige landwirtschaft" on 28.04.2022 --> 175 entries
#create empty object to save a list of seearch pages in
search_pages <- NULL
#create a vector of search page addresses (25 articles per page --> 7 pages; first page is page=0 in this case)
for (j in 1:7){
  search_pages[j] <- paste('https://www.agrarheute.com/suche?qs=%22nachhaltige+landwirtschaft%22&page=',j-1,sep="")
}
search_pages <- unlist(search_pages)
##extract links to articles for all search pages
#create an empty object to save list of links in
link_list <- NULL
for (k in 1:length(search_pages)){
  search_page <- read_html(search_pages[k]) #access the page
  links <- html_elements(search_page,"a.teaser.sucherergebnis.row.mb-3") %>% html_attr("href") #extract article entry links
  link_list <- append(link_list,links) #append links to the vector link_list for later use
}
#special AgrarHeute: make full links
links_list <- NULL
for (l in 1:length(link_list)){
  links_list[l] <- paste("https://www.agrarheute.com",link_list[l],sep="")
}
#generate an empty object to be used later to save results
agrarheute_texts <- as.data.frame(matrix(ncol=5,nrow=0))
colnames(agrarheute_texts) <- c("No","Author","Date","Title","Main")
#extract article date, title, teaser and text from all articles based on HTML tags
for (i in 1:length(links_list)){
  page <- read_html(links_list[i]) #navigate to each article
  author <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/span[2]/a/div/span")) #extract author as text
  author_alt <- html_text2(html_element(page,"div.profil-nowrap")) #alternative author location
  author <- ifelse(is.na(author),author_alt,author)
  date <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/span[3]")) #extract date as text
  date_alt <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/span[2]")) #alternative date location
  date_alt_2 <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/span[1]")) #another alternative date location
  date <- ifelse(is.na(date)|date=="",date_alt,date)
  date <- ifelse(is.na(date)|date=="",date_alt_2,date)
  title <- html_text2(html_element(page,"h1")) #extract article title as text 
  text_1 <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/div[5]")) #extract teaser as text (not always the teaser)
  text_2 <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/div[6]")) #extract main text or teaser (depending on page) as text
  text_3 <- html_text2(html_element(page,xpath="/html/body/div[3]/section/div/div/div/div[2]/div/article/div[7]")) #alternative main text location
  text <- paste0(text_1," ",text_2," ",text_3)
  agrarheute_texts[i,] <- c(i,author,date,title,text)
}
#correct dates
agrarheute_texts$Date <- str_extract(agrarheute_texts$Date,"\\d+.\\d+.\\d+")
#remove "agrarheute" from author names
agrarheute_texts$Author <- str_remove(agrarheute_texts$Author,", agrarheute")
#remove empty articles
agrarheute_texts <- subset(agrarheute_texts,Main!="Mail\nTeilen\nTwittern\nPinnen\nMail  ")
#clean main text
agrarheute_texts$Main <- str_remove(agrarheute_texts$Main,"Mail\nTeilen\nTwittern\nPinnen\nMail")

#write out
write.csv(agrarheute_texts,"Web scraping/agrarheute_texts.csv",row.names=F,fileEncoding="UTF-8")

################### DLG Mitteilungen ##################################
##### Code adapted thanks to Ponraj Arumugam (Wageningen University & Research)
##### Search for "nachhaltigkeit" on 30.11.2021 (same number on 28.04.2022) --> 827 entries
#define urls for later use
main_url <- "https://dlgarchiv.lv.de"
url1 <- "https://dlgarchiv.lv.de/suchergebnis?searchkey=nachhaltigkeit&start=" #first part of url of search result pages
url2 <- "&autor=&monat=&jahr=&sortbydate=1" #second part

#create empty data frames for later use
table_new <-data.frame()
links <- data.frame()
df <- data.frame()
df1 <- data.frame()

##create a data frame of individual articles
i <- 0 #number of entries in search 
while (i<827) {
  #first, the title, author and issue:
  new_webpage<- read_html(sprintf(paste0(url1,i,url2)))
  table_new <- rvest::html_table(new_webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  df <- rbind(df,table_new)
  #next, the links:
  doc <- htmlParse(new_webpage)
  links <- data.frame(paste0(main_url,xpathSApply(doc, "//td/a/@href")))
  df1 <- rbind(df1,links)
  i=i+50
  print(paste0("Extracting information from files ",i-50,"-",i))
}
#combine both sets of information about articles
names(df1) <- "url"
df_f <- cbind(df,df1)

#use only articles with all information available
df_all <- df_f[complete.cases(df_f),]
df_all_1 <- df_all[!(is.na(df_all$Titel) | df_all$Titel==""), ]
print(paste0("After removing empty links we remained with ",nrow(df_all_1)," data from ",nrow(df_f)," data"))

#add filenames for later matching with data extracted from the PDFs
x <- seq(1:nrow(df_all_1))
df_all_1$filename <- paste0("DLG_",x,".pdf")

#save article information dataset
save(df_all_1,file="dlgm_info.RData")
write.csv(df_all_1,"dlgm_info.csv",row.names=F,fileEncoding="UTF-8")

##download PDFs
#set directory for download
setwd(paste0(here(),"/dlg_downloads"))
#download files
for (i in 1:nrow(df_all_1)){
  web <- read_html(df_all_1$url[i]) #navigate to article web page
  doc <- htmlParse(web)
  links <- paste0(main_url,xpathSApply(doc, "//a/@href"))
  d <- grep("*.pdf", links) #find and extract PDF link
  link <- links[d]
  login <- link
  pgsession <- session(login) #start web session
  pgform <- html_form(pgsession)[[1]] #fill login form
  filled_form <- html_form_set(pgform, customerno="*******", plz="*****") #login credentials must be filled
  main_page <- submit_form(pgsession,filled_form)
  download <- session_jump_to(main_page, login)
  print(paste0("Downloading file from ",link,". Downloading completed --- ", round(i/nrow(df_all_1)*100,2),"%"))
  writeBin(download$response$content, basename(login)) #download
  file.rename(substr(login,29,nchar(link)),paste0("DLG_",i,".pdf")) #numbering matches row numbers in df_all_1
}

##### Extract relevant data from PDFs
##### Partly adapted from code provided by Mariana Madruga de Brito
#load basic article info if necessary
load(paste0(here(),"/Web scraping/dlgm_info.RData"))
#rename
dlgm_base <- df_all_1

#prepare PDFs
files <- list.files(paste0(here(),"/Web scraping/dlg_downloads/"),full.names = T)
dlgm <- as.data.frame(matrix(c(""),length(files),4))
colnames(dlgm) <- c("id","filename","Main","Source")

#create loop to extract all relevant (and accessible) information
for(i in 1:length(files)){
  imported_text <- readtext::readtext(file = files[i])
  filename <- imported_text$doc_id
  text <- imported_text$text
  #extract main text by removing page numbers and headers
  main <- stringr::str_remove_all(string=text,pattern="([:digit:][:digit:]|[:digit:])[ ]+DLG-Mitteilungen ([:digit:]|[:digit:][:digit:])/[:digit:][:digit:][:digit:][:digit:]")
  main <- stringr::str_remove_all(string=main,pattern="^[ -���A-Z]{2,30} (| )?[ -�������a-zA-Z]{2,40}(\n)+")
  #remove \n by replacing them by spaces
  main <- stringr::str_replace_all(string=main,pattern="[\n]+",replacement=" ")
  dlgm[i,] <- c(i,filename,main,"DLG-Mitteilungen")
}

#combine with basic article information
dlgm_full <- merge(dlgm_base,dlgm)
#add year
dlgm_full$year <- stringr::str_extract(string=dlgm_full$Ausgabe,pattern="[:digit:][:digit:][:digit:][:digit:]")

#write out
save(dlgm_full,file="data_for_analysis/dlgm.RData")
write.csv(dlgm_full,"dlgm_data.csv",row.names=F,fileEncoding="UTF-8")

######################## Combine all three
#read in Agrarheute articles scraped from web if necessary
agrarheute <- read.csv("Web scraping/agrarheute_texts.csv")
#add column with magazine name
agrarheute$source <- rep("Agrarheute",length(agrarheute$Main))

#same for TopAgrar articles
topagrar <- read.csv("Web scraping/topagrar_texts.csv")
topagrar$source <- rep("TopAgrar",length(topagrar$Main))
#remove entries without date (events and recipes)
topagrar <- subset(topagrar,!is.na(Date))
#merge Abstract and Main to match agrarheute
topagrar$Main <- paste0(topagrar$Abstract," ",topagrar$Main)
topagrar <- topagrar[c(1,3,2,4,6,7)]

#merge topagrar and agrarheute
magazines <- rbind(topagrar[c(2:6)],agrarheute[c(2:6)])

#simplify dates in magazines
magazines$year <- str_extract(magazines$Date,"[:digit:][:digit:][:digit:][:digit:]")

#load dlgm_full if necessary
load("data_for_analysis/dlgm.RData")

#add unique IDs
magazines$ID <- c(seq(1:1301),seq(1:172))
magazines$ID <- paste0(magazines$source,"_",magazines$ID)
dlgm_full$ID <- paste0(dlgm$source,"_",dlgm$id)

#remove unnecessary columns from dlgm
dlgm_simple <- dlgm_full[,c(10,3,9,2,7)]
colnames(dlgm_simple) <- c("ID","Author","Date","Title","Main")

#combine DLG with topagrar and agrarheute
ag_magazines <- rbind(magazines[,c(6,1,2,3,4)],dlgm_simple)

#write out
save(ag_magazines,file="data_for_analysis/ag_magazines.RData")
write.csv(ag_magazines,"ag_magazines_data.csv",row.names=F,fileEncoding="UTF-8")
