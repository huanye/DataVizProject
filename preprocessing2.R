library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
setwd("/Users/huanyeliu/Documents/DataVisualization/DataVizProject/")
h1b = read_csv("H1B_FY17.csv")
cities = read_csv("us-cities.csv")
industry_code =c("index11","index21","index22","index23","index31","index42","index44","index48","index51","index52","index53","index54","index55","index56","index61","index62","index71","index72","index81","index92")
h1b_2017 = h1b%>%
  select(WORKSITE_CITY,H1B_DEPENDENT,WAGE_RATE_OF_PAY,PW_UNIT_OF_PAY,NAIC_CODE)%>%
  filter(!is.na(WORKSITE_CITY) & !is.na(H1B_DEPENDENT) & !is.na(WAGE_RATE_OF_PAY) & !is.na(NAIC_CODE)) %>%
  mutate(WAGE_RATE_OF_PAY = as.numeric(str_extract(WAGE_RATE_OF_PAY,"[^\\s]+")))%>%
  filter(PW_UNIT_OF_PAY=="Year")%>%
  mutate(industry=str_sub(as.character(NAIC_CODE),1,2))%>%
  mutate(industry=ifelse(industry%in%c("31","32","33"),"31",industry))%>%
  mutate(industry=ifelse(industry%in%c("44","45"),"44",industry))%>%
  mutate(industry=ifelse(industry%in%c("48","49"),"48",industry))%>%
  mutate(industry=paste("index",industry,sep=''))%>%
  filter(industry%in%industry_code)%>%
  group_by(WORKSITE_CITY, H1B_DEPENDENT,industry)%>%
  summarize(median_wage = median(WAGE_RATE_OF_PAY,na.rm=T))%>%
  rename(dependent = H1B_DEPENDENT)%>%
  mutate(city = str_to_title(WORKSITE_CITY))%>%
  spread(industry,median_wage)%>%
  right_join(cities,by=(c("city"="place")))%>%
  filter(!is.na(dependent))%>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))
 # select(city,population,dependent,median_wagelat,lon)

h1b_2017_dependent = h1b_2017 %>%
  filter(dependent=="Y")
h1b_2017_independent = h1b_2017 %>%
  filter(dependent == "N")

write.csv(h1b_2017_dependent,"h1b_2017_dependent.csv")
write.csv(h1b_2017_independent,"h1b_2017_independent.csv")
