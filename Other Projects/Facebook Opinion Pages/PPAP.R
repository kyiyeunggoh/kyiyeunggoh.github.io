library(readr)
library(readxl)
library(zoo)
library(stringi)
library(dplyr)
library(tidyverse)
library(readxl)
library(DT)
library(stringr)
library(pdftools)
library(qdapRegex)
library(maps)
library(SnowballC)
library(tm)
library(openxlsx)
library(quanteda)
library(tidytext)
library(qdapRegex)
library(lubridate)
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(plotly)
library(tidyverse)
library(readxl)
library(gutenbergr)
library(tidytext)
library(stringr)
library(stringi)
library(topicmodels)
library(readr)
library(readxl)
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

propap<-read_xlsx("propap_fbposts.xlsx")

### Do the pages have unique identities - LDA to cluster, bi/n-grams, sentiment analysis by personalities - split by year?
### Policy markers 

### Clean the data
propap$year<-year(as.POSIXlt(propap$time, format="%Y/%m/%d"))


### function to get LDA
get_lda_cluster<-function(x,y,z){
  source_frame<-dplyr::filter(propap, grepl(x, source))
  source_frame<-dplyr::filter(source_frame,year < z)
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
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  p<-text_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  print(p)
}

### gen word cloud 
gen_word_cloud<-function(x){
  frame<-dplyr::filter(propap, grepl(x, text))
  text_corpus <- VCorpus(VectorSource(frame$text))
  text_corpus_clean <- tm_map(text_corpus,
                              content_transformer(tolower))
  text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
  
  text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
  text_corpus_clean <- tm_map(text_corpus_clean,
                              removeWords, stopwords())
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  
  text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)
  
  wordcloud(text_corpus_clean, min.freq = 8, random.order = FALSE,
            colors=brewer.pal(8, "Dark2"))
  
}

gen_word_cloud_precovid<-function(x){
  frame<-dplyr::filter(propap, grepl(x, text))
  frame<-dplyr::filter(frame,year < 2020)
  text_corpus <- VCorpus(VectorSource(frame$text))
  text_corpus_clean <- tm_map(text_corpus,
                              content_transformer(tolower))
  text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
  
  text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
  text_corpus_clean <- tm_map(text_corpus_clean,
                              removeWords, stopwords())
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  
  text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)
  
  wordcloud(text_corpus_clean, min.freq = 8, random.order = FALSE,
            colors=brewer.pal(8, "Dark2"))
  
}

gen_word_cloud("kirsten")
gen_word_cloud("jolovan")
gen_word_cloud("thum")
gen_word_cloud("cherian")
gen_word_cloud("roy")
gen_word_cloud("sylvia")
gen_word_cloud("juan")
gen_word_cloud_precovid("josephine")


### LDA

get_lda_cluster("factuallysg",4)
get_lda_cluster("GTS",3)
get_lda_cluster("everydaysg",4)


### Trigram visualisations 
n_gramming <- 3
trigrams <- propap %>%
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)%>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%               
  dplyr::filter(
    !word1 %in% stop_words$word,                 # remove stopwords from both words in bi-gram
    !word2 %in% stop_words$word,
    !word3 %in% stop_words$word,
    !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word2, pattern = "[[:digit:]]"),
    !str_detect(word3, pattern = "[[:digit:]]"),
    !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word2, pattern = "[[:punct:]]"),
    !str_detect(word3, pattern = "[[:punct:]]"),
    !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word2, pattern = "(.)\\1{2,}"),
    !str_detect(word3, pattern = "(.)\\1{2,}"),
    !str_detect(word1, pattern = "\\b(.)\\b"),   # removes any remaining single letter words
    !str_detect(word2, pattern = "\\b(.)\\b"),
    !str_detect(word3, pattern = "\\b(.)\\b"),
  )%>%
  unite("trigram", c(word1,word2,word3), sep = " ")

trigramdigram<-function(a,b){
start_words <- c(a, b)
pattern <- str_c("^", start_words, " ", collapse = "|")
top_words <- trigrams %>%
  dplyr::filter(str_detect(trigram, pattern)) %>%
  count(trigram, sort = TRUE) %>%
  pull(trigram)

trigrams <- trigrams %>%
  dplyr::filter(trigram %in% top_words)

str_nth_word <- function(x, n, sep = " ") {
  str_split(x, pattern = " ") %>%
    map_chr(~ .x[n])
}

nodes <- map_df(seq_len(n_gramming),
                ~ trigrams %>%
                  mutate(word = str_nth_word(trigram, .x)) %>%
                  count(word, sort = TRUE) %>%
                  slice(seq_len(n_word)) %>% 
                  mutate(y = seq(from = n_word + 1, to = 0, 
                                 length.out = n() + 2)[seq_len(n()) + 1],
                         x = .x))

nodes %>% 
  ggplot(aes(x, y, label = word)) +
  geom_text()

sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                       x_space = 0) {
  
  from_word <- from_word %>%
    select(-n) %>%
    set_names(c("from", "y_from", "x_from"))
  
  to_word <- to_word %>%
    select(-n) %>%
    set_names(c("to", "y_to", "x_to"))
  
  links <- crossing(from = from_word$from, 
                    to = to_word$to) %>%
    mutate(word_pair = paste(from, to),
           number = map_dbl(word_pair, 
                            ~ sum(str_detect(trigram$trigram, .x)))) %>%
    left_join(from_word, by = "from") %>%
    left_join(to_word, by = "to")
  
  links %>%
    purrrlyr::by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                     x_to = .x$x_to - 0.05, 
                     y_from = .x$y_from, y_to = .x$y_to, 
                     scale = scale, n = n) %>%
             mutate(word_pair = .x$word_pair,
                    number = .x$number,
                    from = .x$from)) %>%
    pull(.out) %>%
    bind_rows()
}

egde_lines(trigram = trigrams, 
           from_word = dplyr::filter(nodes, x == 1), 
           to_word = dplyr::filter(nodes, x == 2)) %>%
  dplyr::filter(number > 0) %>%
  ggplot(aes(x, y, group = word_pair, alpha = number, color = from)) +
  geom_line()

# egdes between first and second column
egde1 <- egde_lines(trigram = trigrams, 
                    from_word = dplyr::filter(nodes, x == 1), 
                    to_word = dplyr::filter(nodes, x == 2), 
                    n = 50) %>%
  dplyr::filter(number > 0) %>%
  mutate(id = word_pair)

# Words in second colunm
## That start with kirsten
second_word_he <- nodes %>%
  dplyr::filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      dplyr::filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with jolovan
second_word_she <- nodes %>%
  dplyr::filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      dplyr::filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))


# Words in third colunm
## That start with he
third_word_he <- nodes %>%
  dplyr::filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      dplyr::filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
third_word_she <- nodes %>%
  dplyr::filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      dplyr::filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# egdes between second and third column that starts with he
egde2_he <- egde_lines(dplyr::filter(trigrams, 
                              str_detect(trigram, paste0("^", start_words[1], " "))), 
                       second_word_he, third_word_he, n = 50) %>%
  mutate(y = y + 0.05,
         from = start_words[1],
         id = str_c(from, word_pair, sep = " ")) %>%
  dplyr::filter(number > 0)

# egdes between second and third column that starts with she
egde2_she <- egde_lines(dplyr::filter(trigrams, 
                               str_detect(trigram, paste0("^", start_words[2], " "))), 
                        second_word_she, third_word_she, n = 50) %>%
  mutate(y = y - 0.05,
         from = start_words[2],
         id = str_c(from, word_pair, sep = " ")) %>%
  dplyr::filter(number > 0)


# All edges
edges <- bind_rows(egde1, egde2_he, egde2_she)


p <- nodes %>% 
  ggplot(aes(x, y, label = word, size = n)) +
  geom_text(hjust = 0, color = "#DDDDDD") +
  theme_void() +
  geom_line(data = edges,
            aes(x, y, group = id, color = from, alpha = sqrt(number)),
            inherit.aes = FALSE) +
  theme(plot.background = element_rect(fill = "#666666", colour = 'black'),
        text = element_text(color = "#EEEEEE", size = 15)) +
  guides(alpha = "none", color = "none", size = "none") +
  xlim(c(0.9, 3.2)) +
  scale_color_manual(values = c("#5EF1F1", "#FA62D0")) +
  labs(title = " Visualizing depictions of personalities on pro-PAP pages") + 
  scale_size(range = c(3, 8))

print(p)
}

## Activity over time 
# cleaning dates 

propap$adDate<-str_extract(as.character(propap$time), "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
propap$adDate<-as.Date(propap$adDate)

#some descriptive - across time volume

### Post date
date_posted<-propap%>%
  dplyr::filter(source=="factuallysg")%>%
  select(adDate)%>%
  group_by(adDate)

date_posted<-as.data.frame(as.Date(date_posted$adDate))
colnames(date_posted)[1]="date"

date_posted1<-date_posted%>%
  group_by(date)%>%
  summarise(post_volume=length(date))


p <- ggplot(data = date_posted1, aes(x =as.Date(date), y = post_volume))+
  geom_line(color = "#00AFBB", size = 1)

p

#time posted
### Post time
propap$spectime <- as.POSIXct(propap$time, 
                    format="%Y-%m-%d %H:%M:%S", 
                    tz="UTC")

propap$spectime <-with_tz(propap$spectime, "Asia/Hong_Kong")
propap$hour<-str_extract(as.character(propap$spectime), "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]?")
propap$hour<-hms(as.character(propap$hour))
propap$hour<-hour(propap$spectime)

time_posted<-propap%>%
  dplyr::filter(source=="sgmatters")%>%
  select(hour)%>%
  group_by(hour)

time_posted<-as.data.frame(time_posted$hour)
colnames(time_posted)[1]="hour"

time_posted1<-time_posted%>%
  group_by(hour)%>%
  summarise(post_volume=length(hour))


p <- ggplot(data = time_posted1, aes(x =hour, y = post_volume))+
  geom_line(color = "#00AFBB", size = 1)

p
