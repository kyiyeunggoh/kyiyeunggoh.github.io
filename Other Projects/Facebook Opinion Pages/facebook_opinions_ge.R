### FB post-GE script

library(readr)
library(readxl)
library(zoo)
library(stringi)
library(dplyr)
library(readxl)
library(DT)
library(stringr)
library(pdftools)
library(qdapRegex)
library(maps)
library(SnowballC)
library(tm)
library(openxlsx)
library(tidytext)
library(qdapRegex)
library(lubridate)
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(plotly)
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringr)
library(topicmodels)
library(tidyverse)
library(corrplot)
library(reshape2)
library(forecast)
library(sandwich)
library(ggplot2)
library(zoo)
library(lmtest)
library(car)
library(fUnitRoots)
library(stargazer)
library(kableExtra)
library(wordcloud)
library(purrrlyr)
library(syuzhet)
library(viridis)
library(hrbrthemes)
library(pheatmap)
library(ggthemes)

fbposts<-read_xlsx("combined_fbposts_postge.xlsx")

post_numbers<-fbposts%>%
  group_by(source) %>%
  summarise(source_count = length(source))%>%
  arrange(desc(source_count))

datatable(post_numbers)

### Site activity
## Activity over time 
# cleaning dates 

fbposts$adDate<-str_extract(as.character(fbposts$time), "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
fbposts$adDate<-as.Date(fbposts$adDate)
fbposts$adYear<-year(fbposts$adDate)

#some descriptive - across time volume

### Post date
date_posted<-fbposts%>%
  select(adDate)%>%
  group_by(adDate)

activity_date_ppap <- fbposts %>%
  dplyr::filter(adYear>2014)%>%
  group_by(source,adDate)%>%
  summarise(post_volume=log(length(adDate)))%>%
  na.omit()


ggplot(activity_date_ppap, aes(x=adDate,y=post_volume,group=source))+
  geom_line(aes(color=source))+
  ggtitle("Post activity since page inception")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

activity_date_ppap %>%
  ggplot(aes(x=adDate,y=post_volume,group=source)) +
  geom_line(aes(color=source))+
  stat_smooth(method = "loess", formula = y ~ x, size = 1)+
  facet_wrap(~ source) +
  labs(title = "Post activity from 2015 onwards") +
  labs(x = "year", y = "Post\nVolume\n(logged)") +
  theme_economist()

#time posted - by hour, by maximum frequency, by clustering
### Post time
fbposts$spectime <- as.POSIXct(fbposts$time, 
                               format="%Y-%m-%d %H:%M:%S", 
                               tz="UTC")

fbposts$spectime <-with_tz(fbposts$spectime, "Asia/Hong_Kong")
fbposts$hour<-str_extract(as.character(fbposts$spectime), "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]?")
fbposts$hour<-hour(fbposts$spectime)
fbposts$mins<-minute(fbposts$spectime)

activity_hour_ppap <- fbposts %>%
  group_by(source,hour)%>%
  summarise(post_volume=length(hour))%>%
  na.omit()%>%
  group_by(source) %>% 
  mutate(new = post_volume/max(post_volume))


ggplot(activity_hour_ppap, aes(x=hour,y=post_volume,group=source))+
  geom_line(aes(color=source))+
  scale_x_continuous(breaks = seq(0, 23, 1), labels = paste(seq(0, 23, 1), "00", sep = ":"))+
  ggtitle("Post activity over one day since page inception (by hour)")+
  theme_economist()+ 
  ylab("Number of posts")


activity_hour_ppap %>%
  ggplot(aes(x=hour,y=post_volume,group=source)) +
  geom_line(aes(color=source))+
  facet_wrap(~ source) +
  labs(title = "Post activity over one day since page inception (by hour)") +
  labs(x = "Hours", y = "Post\nVolume\n(raw)") +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste(seq(0, 23, 2), "00", sep = ":"))+
  theme_economist()

activity_hour_ppap %>%
  ggplot(aes(x=hour,y=new,group=source)) +
  geom_line(aes(color=source))+
  facet_wrap(~ source) +
  labs(title = "Post activity over one day since page inception (by hour)") +
  labs(x = "Hours", y = "Post\nVolume\n(normalised)") +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste(seq(0, 23, 2), "00", sep = ":"))+
  theme_economist()


activity_min_ppap <- fbposts %>%
  group_by(source,mins)%>%
  summarise(post_volume=length(mins))%>%
  na.omit()%>%
  group_by(source) %>% 
  mutate(min_norm = post_volume/max(post_volume))

activity_min_ppap %>%
  ggplot(aes(x=mins,y=min_norm,group=source)) +
  geom_line(aes(color=source))+
  facet_wrap(~ source) +
  labs(title = "Post activity over an hour since page inception (by minutes)") +
  labs(x = "Minutes", y = "Post\nVolume\n(normalised)") +
  scale_x_continuous(breaks = seq(0, 59, 10), labels = seq(0, 59, 10))+
  theme_economist()


## Filter to GE period - Jun 24th to July 10th
ge_filter<-fbposts%>%
  dplyr::filter(adDate >= "2020-06-24", adDate <= "2020-07-10")

activity_date_ge <- ge_filter %>%
  group_by(source,adDate)%>%
  summarise(post_volume=log(length(adDate)))%>%
  na.omit()

activity_hour_ge <- ge_filter %>%
  group_by(source,hour)%>%
  summarise(post_volume=length(hour))%>%
  na.omit()%>%
  group_by(source) %>% 
  mutate(new = post_volume/max(post_volume))

activity_date_ge %>%
  ggplot(aes(x=adDate,y=post_volume,group=source)) +
  geom_line(aes(color=source))+
  stat_smooth(method = "loess", formula = y ~ x, size = 1)+
  facet_wrap(~ source) +
  labs(title = "Post activity during 2020 GE period") +
  labs(x = "days", y = "Post\nVolume\n(logged)") +
  scale_x_date(date_breaks = 'day', 
               date_labels = '%b %d\n%a')+
  theme_economist()


### look at posting activity by hour - are there differences? 
activity_hour_ge %>%
  ggplot(aes(x=hour,y=new,group=source)) +
  geom_line(aes(color=source))+
  facet_wrap(~ source) +
  labs(title = "Post activity over one day since page inception (by hour)") +
  labs(x = "Hours", y = "Post\nVolume\n(normalised)") +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste(seq(0, 23, 2), "00", sep = ":"))+
  theme_economist()


### Day before cooling off (8th)
cod_filter<-fbposts%>%
  dplyr::filter(adDate >= "2020-07-08", adDate <= "2020-07-08")

activity_hour_cod <- cod_filter %>%
  group_by(source,hour)%>%
  summarise(post_volume=length(hour))%>%
  na.omit()%>%
  group_by(source) %>% 
  mutate(new = post_volume/max(post_volume))

### look at posting activity by hour - are there differences? 
activity_hour_cod %>%
  ggplot(aes(x=hour,y=new,group=source)) +
  geom_line(aes(color=source))+
  facet_wrap(~ source) +
  labs(title = "Post activity over one day on day before Cooling Day (by hour)") +
  labs(x = "Hours", y = "Post\nVolume\n(normalised)") +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste(seq(0, 23, 2), "00", sep = ":"))+
  theme_economist()

### Coverage of elections 

### Hot topics across all sites 
get_lda_cluster<-function(x,y){
  source_frame<-dplyr::filter(ge_filter,ge_filter$source==x)
  text_corpus <- VCorpus(VectorSource(source_frame$text))
  text_corpus_clean <- tm_map(text_corpus,
                              content_transformer(tolower))
  text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
  
  text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
  text_corpus_clean <- tm_map(text_corpus_clean,
                              removeWords, stopwords())
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  
  text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)
  
  text_dtm <- DocumentTermMatrix(text_corpus_clean)
  text_dtm
  findFreqTerms(text_dtm, lowfreq = 20)
  
  #remove zero entries
  ui = unique(text_dtm$i)
  text_dtm.new = text_dtm[ui,]
  
  
  text_lda <- LDA(text_dtm.new, k = y, method = "VEM", control = NULL)
  text_topics <- tidy(text_lda, matrix = "beta")
  
  text_top_terms <- text_topics %>%
    group_by(topic) %>%
    top_n(12, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  theme_update(plot.title = element_text(hjust = 0.5))
  p<-text_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    ggtitle(paste0("Topic cluster generated by LDA of posts from ", x))+
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  png(paste("plot_", i, ".png", sep = ""), width=1100, height=800, res=120) # start export
  print(p) 
  dev.off() # finish export
  
}

sites<-unique(ge_filter$source)

for (i in sites){
  get_lda_cluster(i,5)
}

### Memes 

### Ivan Lim
find<-c("\\bivan\\b") #find words
ivan_list = list()

ivan_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(ivan_posts$source)

### Mentions over time during election period
activity_date_ivan <- ivan_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_ivan, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about Ivan Lim over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### Sentiment
find<-c("\\bivan\\b") #find words
ivan_list = list()

ivan_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(ivan_posts$source)

for (i in sites){
  filtered_frame<-dplyr::filter(ivan_posts,ivan_posts$source==i)
  emo_baro <- get_nrc_sentiment(filtered_frame$text)
  emocol<-colSums(emo_baro)
  ivan_list[[i]] <- emocol
}
ivan_data = do.call(cbind, ivan_list)
emo_sum_ivan<-scale(ivan_data[-c(2,8,9,10),1:7])
pheatmap(emo_sum_ivan, treeheight_row = 0, treeheight_col = 0, main="Heatmap of sentiments when discussing Ivan Lim")

### Jamus Lim
find<-c("\\bjamus\\b") #find words
jamus_list = list()

jamus_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(jamus_posts$source)

### Mentions over time during election period
activity_date_jamus <- jamus_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_jamus, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about Jamus over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### Sentiment
find<-c("\\bjamus\\b") #find words
jamus_list = list()

jamus_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(jamus_posts$source)

for (i in sites){
  filtered_frame<-dplyr::filter(jamus_posts,jamus_posts$source==i)
  emo_baro <- get_nrc_sentiment(filtered_frame$text)
  emocol<-colSums(emo_baro)
  jamus_list[[i]] <- emocol
}
jamus_data = do.call(cbind, jamus_list)
emo_sum_jamus<-scale(jamus_data[-c(2,8,9,10),-c(3)])
pheatmap(emo_sum_jamus, treeheight_row = 0, treeheight_col = 0, main="Heatmap of sentiments when discussing Jamus Lim")


### Raeesah Khan
find<-c("raeesah","khan") #find words
raeesah_list = list()

raeesah_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(raeesah_posts$source)

### Mentions over time during election period
activity_date_raeesah <- raeesah_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_raeesah, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about Raeesah Khan over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### Sentiment
find<-c("raeesah","khan") #find words
raeesah_list = list()

raeesah_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(raeesah_posts$source)

for (i in sites){
  filtered_frame<-dplyr::filter(raeesah_posts,raeesah_posts$source==i)
  emo_baro <- get_nrc_sentiment(filtered_frame$text)
  emocol<-colSums(emo_baro)
  raeesah_list[[i]] <- emocol
}
raeesah_data = do.call(cbind, raeesah_list)
emo_sum_raeesah<-scale(raeesah_data[-c(2,8,9,10),1:8])
pheatmap(emo_sum_raeesah, treeheight_row = 0, treeheight_col = 0, main="Heatmap of sentiments when discussing Raeesah Khan")

### Sengkang GRC
find<-c("sengkang") #find words
sengkang_list = list()

sengkang_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(sengkang_posts$source)

### Mentions over time during election period
activity_date_sengkang <- sengkang_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_sengkang, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about Sengkang GRC over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### East Coast GRC
find<-c("east coast") #find words
eastcoast_list = list()

eastcoast_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(eastcoast_posts$source)

### Mentions over time during election period
activity_date_eastcoast <- eastcoast_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_eastcoast, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about East Coast GRC over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### The East Coast Plan
### East Coast Plan
find<-c("\\beast coast plan\\b","\\bwe care\\b","\\bat east coast\\b", "\\bwe have a together\\b") #find words
ECP_list = list()

ECP_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(ECP_posts$source)

### Mentions over time during election period
activity_date_ECP <- ECP_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_ECP, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about ECP over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 

### Charles Yeo

### Charles Yeo
find<-c("\\bcharles\\b","\\bcharles yeo\\b") #find words
charles_list = list()

charles_posts<-ge_filter%>%
  dplyr::filter(grepl(paste(find,collapse="|"), tolower(text)))

sites<-unique(charles_posts$source)

### Mentions over time during election period
activity_date_charles <- charles_posts %>%
  group_by(adDate) %>% 
  count()

ggplot(activity_date_charles, aes(x=adDate,y=n))+
  geom_line()+
  geom_point()+
  ggtitle("Discussion about Charles Yeo over GE period")+
  theme_economist() +
  labs(x = "Date", y = "Number of posts") 


