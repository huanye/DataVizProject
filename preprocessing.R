library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
setwd("/Users/huanyeliu/Documents/DataVisualization/DataVizProject/")
perm_2011 = read_csv("PERM_Disclosure_Data_FY11.csv")
top10_11 <- perm_2011%>%
  filter(!is.na(COUNTRY_OF_CITIZENSHIP) & !is.na(JOB_INFO_WORK_STATE))%>%
  mutate(COUNTRY_OF_CITIZENSHIP=str_to_title(COUNTRY_OF_CITIZENSHIP))%>%
  mutate(COUNTRY_OF_CITIZENSHIP=str_replace(COUNTRY_OF_CITIZENSHIP,"United Kingdom","UK"))%>%
  group_by(COUNTRY_OF_CITIZENSHIP)%>%
  summarise(NUMBER=n())%>%
  arrange(desc(NUMBER))%>%
  head(10)

top10_11_state<-perm_2011%>%
  select(JOB_INFO_WORK_STATE,COUNTRY_OF_CITIZENSHIP)%>%
  filter(!is.na(COUNTRY_OF_CITIZENSHIP) & !is.na(JOB_INFO_WORK_STATE))%>%
  mutate(COUNTRY_OF_CITIZENSHIP=str_to_title(COUNTRY_OF_CITIZENSHIP))%>%
  mutate(COUNTRY_OF_CITIZENSHIP=str_replace(COUNTRY_OF_CITIZENSHIP,"United Kingdom","UK"))%>%
  #mutate(state = str_to_title(JOB_INFO_WORK_STATE))%>%
  left_join(bind_cols(fname=state.name,abbname=state.abb),by=c("JOB_INFO_WORK_STATE"="abbname"))%>%
  filter(!is.na(fname))%>%
  mutate(state = fname)%>%
  group_by(COUNTRY_OF_CITIZENSHIP,state)%>%
  summarise(NUMBER=n())%>%
  select(COUNTRY_OF_CITIZENSHIP,state,NUMBER)%>%
  spread(state,NUMBER)%>%
  left_join(top10_11,by="COUNTRY_OF_CITIZENSHIP")%>%
  filter(!is.na(NUMBER))%>%
  select(-NUMBER)%>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))%>%
  gather("state","value",2:51)
  
write.csv(top10_11_state,"top10_11_state.csv")
write.csv(top10_11,"top10_11.csv")
