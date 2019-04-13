library(rvest)
library(tidyverse)
library(kableExtra)
library(stringr)
library(xml2)
library(httr)
library(dplyr)
library(data.table)
library(rowr)
library(maps)
data(world.cities)

# 1 link example 
cv<-read_html("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2018")
cv_table<-cv%>%
  html_nodes(xpath="//*[@class='link12']")%>%
  html_text()
cv_frame_2018<-as.data.frame(cv_table)
cv_frame_2018a<-as.data.frame(cv_frame_2018[sapply(gregexpr("\\W+", cv_frame_2018$cv_table), length) >5,])
cv_frame_2018b<-as.data.frame(cv_frame_2018[sapply(gregexpr("\\d", cv_frame_2018$cv_table), length) >5,])
cv_frame_2018xi<-cbind.fill(cv_frame_2018b,cv_frame_2018a)
colnames(cv_frame_2018xi)[1:2] <- c("date", "event")

#string detect cities and then combine to get country, lat, long
world.cities1<-world.cities%>%
  filter(nchar(name)>4)%>%
  filter(pop>55000)
cities<-as.list(world.cities1$name)
cities_str<-paste(unlist(cities), collapse='|')
citieslist<-as.data.frame(str_match(cv_frame_2018xi$event,cities_str))
country<-as.list(world.cities$country.etc)
country_str<-paste(unlist(country), collapse='|')
countrylist<-as.data.frame(str_extract(cv_frame_2018xi$event,country_str))
xi2018travel<-cbind(cv_frame_2018xi,citieslist,countrylist)
colnames(xi2018travel)[3:4] <- c("name", "visitorvisitor")
xi2018travel$name<-as.character(xi2018travel$name)
xi2018travel$name[xi2018travel$name=="China"]<-"Beijing"
combinedframe<-left_join(xi2018travel,world.cities1,by="name")

cv_scrapeR<-function(webpage){
  data("world.cities")
  cv<-read_html(webpage)
  cv_table<-cv%>%
    html_nodes(xpath="//*[@class='link12']")%>%
    html_text()
  cv_frame_2018<-as.data.frame(cv_table)
  cv_frame_2018a<-as.data.frame(cv_frame_2018[sapply(gregexpr("\\W+", cv_frame_2018$cv_table), length) >5,])
  cv_frame_2018b<-as.data.frame(cv_frame_2018[sapply(gregexpr("\\d", cv_frame_2018$cv_table), length) >5,])
  cv_frame_2018xi<-cbind.fill(cv_frame_2018b,cv_frame_2018a)
  colnames(cv_frame_2018xi)[1:2] <- c("date", "event")
  world.cities1<-world.cities%>%
    filter(nchar(name)>4)%>%
    filter(pop>55000)
  cities<-as.list(world.cities1$name)
  cities_str<-paste(unlist(cities), collapse='|')
  citieslist<-as.data.frame(str_match(cv_frame_2018xi$event,cities_str))
  country<-as.list(world.cities$country.etc)
  country_str<-paste(unlist(country), collapse='|')
  countrylist<-as.data.frame(str_extract(cv_frame_2018xi$event,country_str))
  xi2018travel<-cbind(cv_frame_2018xi,citieslist,countrylist)
  colnames(xi2018travel)[3:4] <- c("name", "visitorvisitor")
  xi2018travel$name<-as.character(xi2018travel$name)
  xi2018travel$name[xi2018travel$name=="China"]<-"Beijing"
  combinedframe<-left_join(xi2018travel,world.cities1,by="name")
}

# this is still subject to visual inspection because some cities are not in database, so check names = NA 
framexi19<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2019")
framexi18<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2018")
framexi17<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2017")
framexi16<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2016")
framexi15<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2015")
framexi14<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2014")
framexi13<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2013")
framehu13<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2013")
framehu12<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2012")
framehu11<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2011")
framehu10<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2010")
framehu09<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2009")
framehu08<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2008")
framehu07<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2007")
framehu06<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2006")
framehu05<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2005")
framehu04<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2004")
framehu03<-cv_scrapeR("http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2003")


## DOMESTIC scraper to vomit out province names for me, then create a year column
## then count (i) number of days spent in province per year

combined_xi<-rbind(framexi19,framexi18,framexi17,framexi16,framexi15,framexi14,framexi13)
combined_xi$country.etc[is.na(combined_xi$country.etc)==TRUE]<-"China"
combined_xiprovince<-combined_xi%>%
  filter(str_detect(tolower(combined_xi$event),"travelled")==TRUE)%>%
  filter(country.etc=="China")
##split and retain string prior to the occurence of the word "Province" or "Municipality"
keywords<-"(province|autonomous)(.)*"
xi_NOTbeijing<-combined_xiprovince%>%
  filter(str_detect(tolower(combined_xiprovince$event),keywords)==TRUE)
province<-as.data.frame(sub(keywords,"",tolower(xi_NOTbeijing$event)))
colnames(province)[1] <- "province"
as.character(province$province)
xi_provinces<-as.data.frame(stringi::stri_extract_last_words(province$province))
xi_domestic<-cbind(xi_NOTbeijing,xi_provinces)
xi_domestic<-xi_domestic %>% distinct(date, .keep_all = TRUE)
colnames(xi_domestic)[10] <- "province"
xi_provincevisits<-xi_domestic%>%
  select("date","province")

sar<-"sar(.)*"
xi_sarvisits<-combined_xiprovince%>%
  filter(str_detect(tolower(combined_xiprovince$event),"\\bsar\\b(.)*")==TRUE)
xi_SAR<-as.data.frame(grep("\\bsar\\b(.)*",tolower(combined_xiprovince$event), value=T))
colnames(xi_SAR)[1]<-"event"
SARs<-as.data.frame(sub(sar,"",tolower(xi_SAR$event)))
colnames(SARs)[1] <- "province"
SARs$province<-as.character(SARs$province)
xi_sar_visit<-as.data.frame(stringi::stri_extract_last_words(SARs$province))
colnames(xi_sar_visit)[1] <- "province"
xi_sar_visit$province<-str_replace_all(xi_sar_visit$province,"kong","hong kong")
xi_sar<-cbind(xi_sarvisits,xi_sar_visit)
## xi_sar<-xi_sar %>% distinct(date, .keep_all = TRUE)
colnames(xi_sar)[10] <- "province"
xi_sar_final<-xi_sar%>%
  select("date","province")

xi_allchina_visits<-rbind(xi_provincevisits,xi_sar_final)

# Count for Xi
xi_provincecount<-xi_allchina_visits %>% 
  group_by(province) %>% 
  tally()
xi_provincecount$province<-str_replace_all(xi_provincecount$province,"mongolia","inner mongolia")
xi_provincecount$province<-str_replace_all(xi_provincecount$province,"^hui","ningxia")
xi_provincecount$province<-str_replace_all(xi_provincecount$province,"uyghur","xinjiang")
xi_provincecount$province<-str_replace_all(xi_provincecount$province,"zhuang","guangxi")

# Do the same for Hu 
combined_hu<-rbind(framehu13,framehu12,framehu11,framehu10,framehu09,framehu08,framehu07,framehu06,framehu05,framehu04,framehu03)
combined_hu$country.etc[is.na(combined_hu$country.etc)==TRUE]<-"China"
combined_huprovince<-combined_hu%>%
  filter(str_detect(tolower(combined_hu$event),"travelled")==TRUE)%>%
  filter(country.etc=="China")
##split and retain string prior to the occurence of the word "Province" or "Municipality"
keywords<-"(province|autonomous)(.)*"
hu_NOTbeijing<-combined_huprovince%>%
  filter(str_detect(tolower(combined_huprovince$event),keywords)==TRUE)
province<-as.data.frame(sub(keywords,"",tolower(hu_NOTbeijing$event)))
colnames(province)[1] <- "province"
as.character(province$province)
hu_provinces<-as.data.frame(stringi::stri_extract_last_words(province$province))
hu_domestic<-cbind(hu_NOTbeijing,hu_provinces)
hu_domestic<-hu_domestic %>% distinct(date, .keep_all = TRUE)
colnames(hu_domestic)[10] <- "province"
hu_provincevisits<-hu_domestic%>%
  select("date","province")

sar<-"sar(.)*"
hu_sarvisits<-combined_huprovince%>%
  filter(str_detect(tolower(combined_huprovince$event),"\\bsar\\b(.)*")==TRUE)
hu_SAR<-as.data.frame(grep("\\bsar\\b(.)*",tolower(combined_huprovince$event), value=T))
colnames(hu_SAR)[1]<-"event"
SARs<-as.data.frame(sub(sar,"",tolower(hu_SAR$event)))
colnames(SARs)[1] <- "province"
SARs$province<-as.character(SARs$province)
hu_sar_visit<-as.data.frame(stringi::stri_extract_last_words(SARs$province))
colnames(hu_sar_visit)[1] <- "province"
hu_sar_visit$province<-str_replace_all(hu_sar_visit$province,"kong","hong kong")
hu_sar<-cbind(hu_sarvisits,hu_sar_visit)
hu_sar<-hu_sar %>% distinct(date, .keep_all = TRUE)
hu_sar<-subset(hu_sar,!duplicated(hu_sar$date))
colnames(hu_sar)[10] <- "province"
hu_sar_final<-hu_sar%>%
  select("date","province")

hu_allchina_visits<-rbind(hu_provincevisits,hu_sar_final)

# Count for Xi
hu_provincecount<-hu_allchina_visits %>% 
  group_by(province) %>% 
  tally()
hu_provincecount$province<-str_replace_all(hu_provincecount$province,"mongolia","inner mongolia")
hu_provincecount$province<-str_replace_all(hu_provincecount$province,"^hui","ningxia")
hu_provincecount$province<-str_replace_all(hu_provincecount$province,"uyghur","xinjiang")
hu_provincecount$province<-str_replace_all(hu_provincecount$province,"zhuang","guangxi")
hu_provincecount<-hu_provincecount[!grepl("ukraine", hu_provincecount$province),]

# Analyse distribution across years and months 
sum(xi_provincecount$n) 
sum(hu_provincecount$n)



## Biography scraper
biodata_scrapeR<-function(webpage){
cv<-read_html(webpage)
cv_table<-cv%>%
  html_nodes(xpath="//*[@id='dataPanel']/table")%>%
  html_table()
cv_frame_2018<-as.data.frame(cv_table)
bioframe<-cv_frame_2018[,c(1,2)]
}

xibiodata<-biodata_scrapeR("http://www.chinavitae.com/biography/Xi_Jinping/career")
hubiodata<-biodata_scrapeR("http://www.chinavitae.com/biography/Hu_Jintao/career")
jiangbiodata<-biodata_scrapeR("http://www.chinavitae.com/biography/Jiang_Zemin/career")
dengbiodata<-biodata_scrapeR("http://www.chinavitae.com/biography/Deng_Xiaoping/career")
huabiodata<-biodata_scrapeR("http://www.chinavitae.com/biography/Hua_Guofeng/career")
#Mao probs from wiki 
#LiuSQ probs from wiki 



