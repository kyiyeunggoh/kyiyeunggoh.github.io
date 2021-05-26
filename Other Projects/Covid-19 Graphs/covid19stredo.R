library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(gganimate)
library(gifski)
library(bbplot)
library(hrbrthemes)
options(scipen=10000)

covidall <-
  list.files(pattern = "*-2021.csv") %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

population<-read_xlsx("population.xlsx")

##cleanup datetime
covid21$date_reported <- lubridate::date(covid21$Last_Update)

##convert type
covid21$Confirmed<-as.numeric(covid21$Confirmed)
covid21$Deaths<-as.numeric(covid21$Deaths)

target <- c("Brunei", "Cambodia","Indonesia",
            "Laos","Malaysia","Burma","Philippines",
            "Singapore","Thailand","Vietnam")

covidst<-covid21%>%
  filter(Country_Region %in% target)

##merge population 
covidst_pop<-left_join(covidst,population, by=c("Country_Region" = "Country"))


## calculate rate of change
covidst_cleaned<-covidst_pop%>%
  arrange(Country_Region,date_reported)%>%
  mutate(Diff_day = date_reported - lag(date_reported), 
         daily_death = Deaths-lag(Deaths),
         daily_case = Confirmed - lag(Confirmed), 
         daily_caserate = 100-((daily_case)/lag(daily_case)*100),
         daily_deathrate = 100-((daily_death)/lag(daily_death)*100),
         dailycase_weighted= daily_case/prevpeak,
         dailydeath_weighted=daily_death/prevdeath,
         confirmed_pop=Confirmed/`Population (2020)`,
         death_pop=Deaths/`Population (2020)`)%>%
  filter(date_reported!="2021-01-02")

covidst_cleaned$dailycase_weighted[is.nan(covidst_cleaned$dailycase_weighted)]<-0
covidst_cleaned$dailycase_weighted[is.na(covidst_cleaned$dailycase_weighted)]<-0

##normalise 0-1
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

scaled_data <- 
  covidst_cleaned %>%
  group_by(Country_Region) %>%
  mutate(dailycaseweighted.d=normalize(dailycase_weighted),
         dailydeathweighted.d=normalize(dailydeath_weighted))%>%
  select(date_reported,officialnames,dailycaseweighted.d,dailydeathweighted.d)

scaled_data[is.na(scaled_data)]<-0
scaled_data<-rename(scaled_data,Cases=4)
scaled_data<-rename(scaled_data,Deaths=5)

scaled_data<-melt(scaled_data, id.vars=c("Country_Region","date_reported","officialnames"))


labels <- data.frame(officialnames=c("Brunei","Cambodia","Indonesia","Laos","Malaysia","Myanmar",
                                     "Philippines","Singapore","Thailand","Vietnam"),
                     label=c("236 cases, 3 deaths","25,761 cases, 179 deaths","1,786,187 cases, 49,627 deaths",
                             "1,822 cases, 2 deaths", "525,889 cases, 2,369 deaths", "143,262 cases,  3,216 deaths","1,188,672 cases, 20,019 deaths", "61,890 cases, 32 deaths", "135,439 cases, 832 deaths",
                             "5,404 cases, 44 deaths"))
                             

## facet country (case growth rate)
covidcasesea<-ggplot(scaled_data, aes(x=date_reported, y=value,group=variable,
                                 colour=variable)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#5574a8", "#d8474a")) +
  facet_wrap( ~ officialnames,ncol=2)+
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  #scale_alpha_manual(values=c(1,0.2,1,0.2,0.2,0.2,0.2,0.2,0.2,0.2,1)) +
  bbc_style()+
  labs(title="A new Covid-19 wave has been engulfing most countries in South-east Asia",
       subtitle = "Daily cases and deaths reported as percentage of previous all-time peak (normalised), 2021")

finalise_plot(plot_name = covidcasesea,
              source = "Sources: Our World In Data, John Hopkins University, Worldometer.info",
              save_filepath = "seacovidcaseloads.png",
              width_pixels = 1600,
              height_pixels = 1400)

