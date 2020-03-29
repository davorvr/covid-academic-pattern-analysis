#################################################################
# Preliminary analysis of COVID-19 academic information patterns: 
# A call for open science in the times of closed borders
# Homolak, Kodvanj, Virag
#################################################################

library(roadoi)
library(fulltext)
library(rcrossref)
library(rorcid)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggsci)
library(RISmed)
library(spatstat.utils)
library(stringr)
library(smps)
library(devEMF)
library(plotly)
library(rjson)
library(bibliometrix)

tema1 <- theme_bw() + theme( plot.subtitle = element_text(vjust = 1), 
                            plot.caption = element_text(vjust = 1), 
                            panel.grid.major = element_line(colour = "gray97"), 
                            panel.grid.minor = element_line(size = 0.25), 
                            axis.title = element_text(size = 9), 
                            axis.text = element_text(size = 8, colour = "black"), 
                            panel.background = element_rect(fill = NA),
                            
)
tema <- theme_bw() + theme( plot.subtitle = element_text(vjust = 1), 
                            plot.caption = element_text(vjust = 1), 
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(), 
                            axis.title = element_text(size = 9), 
                            axis.text = element_text(size = 8, colour = "black"), 
                            panel.background = element_rect(fill = NA),
                            panel.border = element_rect(linetype = "solid", fill = NA, size = 1)
                            
)


theme_set(tema)





search_topic_TopicSpecified <- '(((((((Case Reports[Publication Type]) OR (English Abstract[Publication Type])) OR (Guideline[Publication Type])) OR (Journal Article[Publication Type])) OR (Multicenter Study[Publication Type])) OR (Retracted Publication[Publication Type])) OR (Review[Publication Type])) AND ((COVID-19) OR (Sars-CoV-2))'
search_query <- EUtilsSummary(search_topic_TopicSpecified)
summary(search_query)
search1time <- Sys.time()
Sys.sleep(3)
records <- EUtilsGet(search_query)


# ovo iduce izvuce iz records sve sto sam napisao...
pubmed_data_full <- data.frame('PMID'=as.character(PMID(records)),
                               'DayAccepted' = DayAccepted(records),
                               'MonthAccepted' = MonthAccepted(records),
                               'YearAccepted' = YearAccepted(records),
                               'DayReceived' = DayReceived(records),
                               'MonthReceived' = MonthReceived(records),
                               'YearReceived' = YearReceived(records),
                               'Journal' = Title(records),
                               'PublicationStatus' = PublicationStatus(records),
                               'Country' = Country(records),
                               'Language' = Language(records)
)




# izbacimo one podatke koji imaju NA kod datuma prihvacanja ili objave
pm <- na.omit(pubmed_data_full)



# izracuna PR vrijeme
pm <- pm %>%
  mutate(YMD_Accepted = ymd(paste(YearAccepted,MonthAccepted, DayAccepted))) %>%
  mutate(YMD_Received = ymd(paste(YearReceived,MonthReceived, DayReceived))) %>%
  mutate(InReview =  YMD_Accepted - YMD_Received)

pm$JournalCount <- table(pm$Journal)[pm$Journal]  


plot <- ggplot(pm) + geom_bar(aes(x = YMD_Accepted), inherit.aes = FALSE, fill = "Red" ,color = "Red", alpha = 0.4) + 
  geom_bar(aes(x = YMD_Received), inherit.aes = FALSE, fill = "Blue", color = "Blue", alpha = 0.4) + 
  scale_x_date(limits = c(ymd("2020-01-15"),ymd("2020-03-21"))) 
plotAccepted <- ggplot(pm) + 
  geom_bar(aes(x = YMD_Accepted), inherit.aes = FALSE, fill = "Red" ,color = "Red", alpha = 0.4) + 
  scale_x_date(limits = c(ymd("2020-01-15"),ymd("2020-03-21"))) + xlab("Date Accepted") + ylab("Count") 
plotReceived <- ggplot(pm) + 
  geom_bar(aes(x = YMD_Received), inherit.aes = FALSE, fill = "Blue", color = "Blue", alpha = 0.4) + 
  scale_x_date(limits = c(ymd("2020-01-15"),ymd("2020-03-21"))) + xlab("Date Received") + ylab("Count")
emf("0 Accepted.emf", width = 3.5, height = 3)
print(plotAccepted)
dev.off()
emf("0 Received.emf", width = 3.5, height = 3)
print(plotReceived)
dev.off()




data.frame(pubmed_data_full$Country) %>%
  group_by(pubmed_data_full.Country) %>%
  summarize(PublishedPapers = n()) %>%
  ggplot(aes(x = reorder(pubmed_data_full.Country, PublishedPapers), y = PublishedPapers, fill = pubmed_data_full.Country)) + 
  geom_col() + theme(axis.title.y = element_blank(), legend.position = "None") + ylab("Published Papers") + 
  geom_text(aes(label = PublishedPapers, y = PublishedPapers +13),
            position = position_dodge(0.9),
            vjust = 0.50, size = 3) +
  coord_flip()

data.frame(pubmed_data_full$Country, pubmed_data_full$Language) %>%
  filter(pubmed_data_full.Language == "eng") %>%
  group_by(pubmed_data_full.Country) %>%
  summarize(PublishedPapers = n()) %>%
  ggplot(aes(x = reorder(pubmed_data_full.Country, PublishedPapers), y = PublishedPapers, fill = pubmed_data_full.Country)) + 
  geom_col() + theme(axis.title.y = element_blank(), legend.position = "None") + ylab("Published Papers") + 
  geom_text(aes(label = PublishedPapers, y = PublishedPapers +13),
            position = position_dodge(0.9),
            vjust = 0.50, size = 3) +
  coord_flip()





x <- pm %>%
  filter(JournalCount >4)
popis <- levels(factor(x$Journal))
search <- paste('$', popis[1], '$',"[Journal]", sep = "")
for (i in 2:length(popis)) {
  search <- paste(search, paste('$', popis[i], '$',"[Journal]", sep = ""), sep = " OR ")
}
write.csv(search, "searchphrase.csv")









###############

# search phrase is construced from searchphrase.csv
search_phrase2 <- '("Frontiers of medicine"[Journal] OR "Intensive care medicine"[Journal] OR "International journal of antimicrobial agents"[Journal] OR "International journal of environmental research and public health"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Korean journal of radiology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "Travel medicine and infectious disease"[Journal]) AND ("2018/12/01"[Date - Publication] : "2019/04/01"[Date - Publication])'
search_query2 <- EUtilsSummary(search_phrase2, retmax = 6000)
summary(search_query2)
search2time <- Sys.time()
Sys.sleep(3)
records2 <- EUtilsGet(search_query2)


# ovo iduce izvuce iz records sve sto sam napisao...
pubmed_data_full2 <- data.frame('PMID'=as.character(PMID(records2)),
                                'DayAccepted' = DayAccepted(records2),
                                'MonthAccepted' = MonthAccepted(records2),
                                'YearAccepted' = YearAccepted(records2),
                                'DayReceived' = DayReceived(records2),
                                'MonthReceived' = MonthReceived(records2),
                                'YearReceived' = YearReceived(records2),
                                'Journal' = Title(records2),
                                'PublicationStatus' = PublicationStatus(records2),
                                'Country' = Country(records2),
                                'Language' = Language(records2)
)
pm2 <- na.omit(pubmed_data_full2)
pm2 <- pm2 %>%
  mutate(YMD_Accepted = ymd(paste(YearAccepted,MonthAccepted, DayAccepted))) %>%
  mutate(YMD_Received = ymd(paste(YearReceived,MonthReceived, DayReceived))) %>%
  mutate(InReview =  YMD_Accepted - YMD_Received)
pm2$JournalCount <- table(pm2$Journal)[pm2$Journal]  

# pm <- pm %>% mutate(Journal = paste(Journal, "(COVID-19)", sep = " "))
# pm2 <- pm2 %>% mutate(Journal = paste(Journal, "", sep = " "))


pl1 <- pm %>% 
  filter(JournalCount > 4) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + 
  scale_y_continuous(limits = c(0,365)) + scale_fill_rickandmorty(alpha = 0.7)

emf("1 PR po casopisu desni - F.emf", width = 5, height = 5)
print(pl1)
dev.off()

pl1_summary_by_group <- pm %>% 
  filter(JournalCount > 4) %>%
  mutate(InReview = as.numeric(InReview)) %>%
  select(Journal, InReview) %>%
  group_by(Journal) %>%
  summarize(SP = median(InReview), SD = sd(InReview), n = n())

pl2 <- pm2 %>% 
  filter(JournalCount > 4) %>%
  ggplot(aes(x = Journal, y = InReview, fill = Journal)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = "jitter", size = 0.1) +
  theme(legend.position = "None") + coord_flip() +
  ylab("SP time [days]") + theme(axis.title.y = element_blank()) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_y_reverse(limits = c(350,0))+ scale_fill_rickandmorty(alpha = 0.7)


emf("2 PR po casopisu lijevi - F.emf", width = 5, height = 5)
print(pl2)
dev.off()

pl2_summary_by_group <- pm2 %>% 
  filter(JournalCount > 4) %>%
  mutate(InReview = as.numeric(InReview)) %>%
  select(Journal, InReview) %>%
  group_by(Journal) %>%
  summarize(SP = median(InReview), SD = sd(InReview), n = n())

pl1_summary <- pm %>% 
  filter(JournalCount > 4) %>%
  mutate(InReview = as.numeric(InReview)) %>%
  select(Journal, InReview) %>%
  summarize(SP = median(InReview), SD = sd(InReview), n = n())
pl2_summary <- pm2 %>% 
  filter(JournalCount > 4) %>%
  mutate(InReview = as.numeric(InReview)) %>%
  select(Journal, InReview) %>%
  summarize(SP = median(InReview), SD = sd(InReview), n = n())




pm_affiliation <- list(
  'PMID'=as.character(PMID(records)),
  'Author' = Author(records),
  'Affiliation' = Affiliation(records),
  'Country' = Country(records),
  'Journal' = Title(records),
  'PublicationType' = PublicationType(records),
  'Language' = Language(records)
)


X <- data.frame(pm_affiliation$Country, pm_affiliation$Language) %>%
  group_by(pm_affiliation.Country) %>%
  summarize(PublishedPapers = n())
xx <- data.frame(pm_affiliation$Country, pm_affiliation$Language) %>%
  left_join(X, by = "pm_affiliation.Country")
xx$pm_affiliation.Language <- factor(xx$pm_affiliation.Language)
xx$pm_affiliation.Language <- plyr::revalue(xx$pm_affiliation.Language, 
         c("chi"="Chinese", "eng"="English", "fre" = "French", "ger" = "German", "ita" = "Italian", "spa" = "Spanish"))

pl3 <- ggplot(xx, aes(x = reorder(pm_affiliation.Country, PublishedPapers), fill = pm_affiliation.Language)) + 
  geom_bar() + theme(axis.title.y = element_blank(), legend.title = element_blank(), 
                     axis.text = element_text(size = 8, colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ylab("Published Papers") + 
  coord_flip() + scale_fill_startrek(alpha = 0.8) + scale_y_continuous(expand = c(0,10)) 

emf("3 Broj publikaicja po zemljama 2 - F.emf", width = 4, height = 5)
print(pl3)
dev.off()

pl3_summary <- xx %>%
  group_by(pm_affiliation.Country, pm_affiliation.Language) %>%
  summarize(n = n())











############################################################################################################
search_topic3 <- '"COVID-19" AND ("Frontiers of medicine"[Journal] OR "Intensive care medicine"[Journal] OR "International journal of antimicrobial agents"[Journal] OR "International journal of environmental research and public health"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Korean journal of radiology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "Travel medicine and infectious disease"[Journal])'
search_query3 <- EUtilsSummary(search_topic3,  retmax = 6000)
summary(search_query3)
search3time <- Sys.time()
Sys.sleep(3)
records3 <- EUtilsGet(search_query3)  


pm_affiliation <- list(
  'PMID'=as.character(PMID(records3)),
  'Author' = Author(records3),
  'Affiliation' = Affiliation(records3),
  'Country' = Country(records3),
  'Journal' = Title(records3),
  'PublicationType' = PublicationType(records3),
  'PublicationStatus' = PublicationStatus(records3)
)





# publication status

pubstatus <- pm_affiliation$PublicationStatus

# Author count today

pm_aff <- pm_affiliation$Affiliation

AffiliationCount <- c()
for (i in 1:length(pm_aff)){
  x <- 0
  
  temp <- unique(pm_aff[[i]])
  try(
    for (j in 1:length(temp)){
      
      if (str_length(temp[j]) > 1) {
        x <- x + 1
      }
      
    }
  )
  AffiliationCount <- c(AffiliationCount,x)
  
}

AffiliationCount_Today <- AffiliationCount



# Affiliation today


pm_aff <- pm_affiliation$Author
AuthorCount <- c()
for (i in 1:length(pm_aff)){
  
  AuthorCount <- c(AuthorCount, dim(data.frame(pm_aff[i]))[1])
  
}

AuthorCount_Today <- AuthorCount





### 2

search_topic4 <- '("2018/12/01"[Date - Publication] : "2019/04/01"[Date - Publication]) AND ("Frontiers of medicine"[Journal] OR "Intensive care medicine"[Journal] OR "International journal of antimicrobial agents"[Journal] OR "International journal of environmental research and public health"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Korean journal of radiology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "Travel medicine and infectious disease"[Journal])'
search_query4 <- EUtilsSummary(search_topic4,  retmax = 6000)
summary(search_query4)
search4time <- Sys.time()
Sys.sleep(3)
records4 <- EUtilsGet(search_query4)  


pm_affiliation <- list(
  'PMID'=as.character(PMID(records4)),
  'Author' = Author(records4),
  'Affiliation' = Affiliation(records4),
  'Country' = Country(records4),
  'Journal' = Title(records4),
  'PublicationType' = PublicationType(records4),
  'PublicationStatus' = PublicationStatus(records4)
)







# pubstatusBefore <- pm_affiliation$PublicationStatus
pm_aff <- pm_affiliation$Affiliation

AffiliationCount <- c()
for (i in 1:length(pm_aff)){
  x <- 0
  
  temp <- unique(pm_aff[[i]])
  try(
    for (j in 1:length(temp)){
      
      if (str_length(temp[j]) > 1) {
        x <- x + 1
      }
      
    }
  )
  AffiliationCount <- c(AffiliationCount,x)
  
}


AffiliationCount_LastYear <- AffiliationCount


# Merging data for today and last year
AffiliationCount <- rbind(data.frame(AffiliationCount = AffiliationCount_LastYear, Time = "Other"), data.frame(AffiliationCount = AffiliationCount_Today, Time = "COVID-19"))

pl4 <- ggplot(AffiliationCount, aes(x = AffiliationCount, fill = Time)) + 
  geom_density(alpha = 0.3) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_jco() + 
  xlab("Affiliation Count Distribution")

emf("4AffiliationDistribution.emf", width = 4, height = 3)
print(pl4)
dev.off()



# Extracting the author count - Before

pm_aff <- pm_affiliation$Author
AuthorCount <- c()
for (i in 1:length(pm_aff)){
  
  AuthorCount <- c(AuthorCount, dim(data.frame(pm_aff[i]))[1])
  
}

AuthorCount_LastYear <- AuthorCount


# Merging data for today and last year and ploting
AuthorCount <- rbind(data.frame(AuthorCount = AuthorCount_LastYear, Time = "Other"), data.frame(AuthorCount = AuthorCount_Today, Time = "COVID-19"))

pl5 <- ggplot(AuthorCount, aes(x = AuthorCount, fill = Time)) + 
  geom_density(alpha = 0.3) + 
  scale_x_continuous(limits = c(NA, 40)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  scale_fill_jco() +
  xlab("Author Count Distribution")



emf("5AuthorDistribution.emf", width = 4, height = 3)
print(pl5)
dev.off()













#############################################################################################################################

search_topic_other <- '("2020/01/01"[Date - Publication] : "2029/01/01"[Date - Publication]) AND ("Frontiers of medicine"[Journal] OR "Intensive care medicine"[Journal] OR "International journal of antimicrobial agents"[Journal] OR "International journal of environmental research and public health"[Journal] OR "International journal of infectious diseases : IJID : official publication of the International Society for Infectious Diseases"[Journal] OR "Journal of medical virology"[Journal] OR "Journal of microbiology, immunology, and infection = Wei mian yu gan ran za zhi"[Journal] OR "Korean journal of radiology"[Journal] OR "Lancet (London, England)"[Journal] OR "The Journal of hospital infection"[Journal] OR "The Journal of infection"[Journal] OR "The Lancet. Infectious diseases"[Journal] OR "Travel medicine and infectious disease"[Journal]) NOT ("COVID-19")'
search_query_other <- EUtilsSummary(search_topic_other,  retmax = 6000)
summary(search_query_other)
search5time <- Sys.time()
Sys.sleep(3)
records_other <- EUtilsGet(search_query_other)  


pm_affiliation <- list(
  'PMID'=as.character(PMID(records_other)),
  'Author' = Author(records_other),
  'Affiliation' = Affiliation(records_other),
  'Country' = Country(records_other),
  'Journal' = Title(records_other),
  'PublicationType' = PublicationType(records_other),
  'PublicationStatus' = PublicationStatus(records_other)
)







pubstatusother <- pm_affiliation$PublicationStatus



PubStatus <- rbind(data.frame(Status = pubstatus, Time = "COVID-19"), data.frame(Status = pubstatusother, Time = "Other"))
PubStatus$Status <- factor(PubStatus$Status)
PubStatus$Status <- plyr::revalue(PubStatus$Status, 
          c("aheadofprint"="Ahead of Print", "epublish"="E-publish", "ppublish" = "P-publish"))

pl6 <- ggplot(PubStatus, aes(x = Time, fill = Status)) +geom_bar(position = "fill", width = 0.7) + 
  ylab("%") + theme(axis.title.x = element_blank()) + scale_fill_jco() + scale_y_continuous(labels = scales::percent) +
  theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")

emf("6 PubType.emf", width = 3.5, height = 3)
print(pl6)
dev.off()






## rXiv 

# load JSON, output data from python script
json <- fromJSON(file = "article_list.json")


df <- data.frame("rxiv_date" = c(NA), "injournal" = c(NA), "rxiv_site" = c(NA), 
                 "J_received" = c(NA), "J_accepted" = c(NA), "J_published" = c(NA),
                 "journal" = c(NA), "doi" = c(NA))


for (i in json){
  if(is.null(i$journal_date_received)){
    NA
  } else {  i$journal_date_received }
  df <- rbind(df, data.frame("rxiv_date" =  i$rxiv_date, "injournal" = !is.null(i$journal_doi),
                             "rxiv_site" = i$rxiv_site, 
                             "J_received" =   if(is.null(i$journal_date_received)){NA} else { i$journal_date_received }, 
                             "J_accepted" = if(is.null(i$journal_date_accepted)){NA} else { i$journal_date_accepted }, 
                             "J_published" = if(is.null(i$journal_date_published)){NA} else { i$journal_date_published },
                             "journal" = if(is.null(i$journal)){NA} else { i$journal },
                             "doi" = if(is.null(i$journal_doi)){NA} else { i$journal_doi }))
}

df<- df[-1,]

df$rxiv_date <- lubridate::ymd(df$rxiv_date)


pl7 <- ggplot(df, aes(x = rxiv_date, fill = injournal)) + geom_bar() + 
  scale_x_date(limits = c(ymd("2020-01-10"), NA)) + scale_fill_startrek() +
  theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal") + xlab("Date") + ylab("Count")

emf("7 rXiv.emf", width = 3.5, height = 3.5)
print(pl7)
dev.off()




## Bibliometrix
# based on search results for search phrase: 
# importing bib file from scopus
# Search phrase on scopus ( "COVID-19"  OR  "severe acute respiratory syndrome coronavirus 2"  OR  "2019-nCoV"  OR  "SARS-CoV-2"  OR  "2019nCoV"  OR  "COVID"  OR  "COVID19"  OR  "SARS-CoV2" ) 
# Accessed on 2020-03-29 19:35 CET


D <- readFiles("scopus20200329_1935.bib")

M <- convert2df(D, dbsource = "scopus", format = "bibtex")

M <- M %>%
  filter(DT == "ARTICLE" | DT == "REVIEW")

results_bib <- biblioAnalysis(M, sep = ";")

options(width=100)

S <- summary(object = results_bib, k = 20, pause = FALSE)


plot_list <- plot(x = results_bib, k = 10, pause = FALSE)

pl8 <- plot_list$MostProdCountries + scale_fill_startrek() + theme_bw() + 
  theme(legend.title = element_blank(), plot.title = element_blank(),
         axis.title.y = element_blank(),
         plot.caption = element_blank(),
         plot.subtitle = element_text(vjust = 1), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         axis.text = element_text(size = 8, colour = "black"), 
         panel.background = element_rect(fill = NA)) +
  ylab("Published papers")

emf("8 Scopus_countryproductivity.emf", width = 3.5, height = 3.5)
print(pl8)
dev.off()




## used to create a map.
biblioshiny()



