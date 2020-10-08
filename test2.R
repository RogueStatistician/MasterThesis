#reading data
library(tidyverse)
library(here)
library(parallel)
library(plyr)

base <- paste0(c('Simone','Uni','Tesi','erasmus','code','MasterThesis'), collapse= '/')
source(here::here(base,'customPooledModels.R'))
orig <- star_data <- read.csv(here::here(base,'long_star_filtered2.csv'),header = T,stringsAsFactors = F,na.strings = '*')

maxn <- 62
View(star_data%>%group_by(HDnumber)%>%dplyr::summarise(n()))
subsample <- star_data%>%group_by(HDnumber) %>% filter(n()>50)
# datasets <- subsample %>% ungroup()%>%filter(HDnumber==51756) %>% select(.imp) 
# subsample <- subsample%>%filter(.imp %in% datasets$.imp)
subsample <- subsample%>%group_by(HDnumber)%>%sample_n(maxn)
subsample <- subsample%>%group_by(HDnumber) %>% mutate(.imp=rep(1:maxn,66))
subsample <- subsample%>%arrange(.imp)%>%select(-one_of('X'))

subsample_nested <- subsample %>% group_by(.imp) %>% nest()  

subsample_nested <- subsample_nested %>% 
  mutate(m1 = map(data, function(df) lm(X10~X1,data=df)))%>% 
  mutate(m2 = map(data, function(df) lm(X10~X2,data=df)))%>% 
  mutate(m3 = map(data, function(df) lm(X10~X3,data=df)))%>% 
  mutate(m4 = map(data, function(df) lm(X10~X4,data=df)))%>% 
  mutate(m5 = map(data, function(df) lm(X10~X5,data=df)))%>% 
  mutate(m6 = map(data, function(df) lm(X10~X6,data=df)))%>% 
  mutate(m7 = map(data, function(df) lm(X10~X7,data=df)))%>% 
  mutate(m8 = map(data, function(df) lm(X10~X8,data=df)))%>% 
  mutate(m9 = map(data, function(df) lm(X10~X9,data=df)))

subsample_nested


pools <- subsample_nested %>% ungroup() %>% select(-one_of('.imp','data')) %>% apply(2,pool.lms)
lapply(pools,summary)
