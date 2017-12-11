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
  select(WORKSITE_CITY,H1B_DEPENDENT,WAGE_RATE_OF_PAY,WAGE_UNIT_OF_PAY,NAIC_CODE,EMPLOYER_NAME)%>%
  filter(!is.na(WORKSITE_CITY) & !is.na(H1B_DEPENDENT) & !is.na(WAGE_RATE_OF_PAY) & !is.na(NAIC_CODE) & !is.na(EMPLOYER_NAME)) %>%
  mutate(WAGE_RATE_OF_PAY = as.numeric(str_extract(WAGE_RATE_OF_PAY,"[^\\s]+")))%>%
  filter(WAGE_UNIT_OF_PAY=="Year")%>%
  mutate(industry=str_sub(as.character(NAIC_CODE),1,2))%>%
  mutate(industry=ifelse(industry%in%c("31","32","33"),"31",industry))%>%
  mutate(industry=ifelse(industry%in%c("44","45"),"44",industry))%>%
  mutate(industry=ifelse(industry%in%c("48","49"),"48",industry))%>%
  mutate(industry=paste("index",industry,sep=''))%>%
  filter(industry%in%industry_code)%>%
  mutate(employer_name = str_replace(EMPLOYER_NAME, "INC.","INC"))%>%
  mutate(employer_name = str_replace(employer_name, "&AMP;","&"))%>%
  mutate(employer_name = str_replace(employer_name, "AND","&"))%>%
  mutate(employer_name = str_replace(employer_name, " & ","&"))%>%
  mutate(employer_name = str_replace(employer_name, ",",""))%>%
  mutate(employer_name = str_replace(employer_name, "UNIVERSTY","UNIVERSITY"))%>%
  group_by(WORKSITE_CITY, H1B_DEPENDENT,industry,employer_name)%>%
  summarize(median_wage = median(WAGE_RATE_OF_PAY,na.rm=T))%>%
  rename(dependent = H1B_DEPENDENT)%>%
  mutate(city = str_to_title(WORKSITE_CITY))%>%
  right_join(cities,by=(c("city"="place")))%>%
  filter(!is.na(dependent))%>%
  arrange(city, dependent,industry,desc(median_wage))%>%
  group_by(city, dependent,industry)%>%
  top_n(n=5,median_wage)%>%
  select(city,dependent,industry,employer_name,median_wage,lat,lon)

h1b_2017_dependent_top5 = h1b_2017 %>%
  filter(dependent=="Y")
h1b_2017_independent_top5 = h1b_2017 %>%
  filter(dependent == "N")

write.csv(h1b_2017_dependent_top5,"h1b_2017_dependent_top5.csv")
write.csv(h1b_2017_independent_top5,"h1b_2017_independent_top5.csv")
