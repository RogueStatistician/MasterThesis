#reading data
library(tidyverse)
library(here)
library(parallel)
library(plyr)

#base <- paste0(c('Simone','Uni','Tesi','erasmus','code','MasterThesis'), collapse= '/')
#base <- '.'
source(here::here(base,'customPooledModels.R'))
orig <- star_data <- read.csv(here::here(base,'long_star_filtered2.csv'),header = T,stringsAsFactors = F,na.strings = '*')

maxn <- 62
#checking which stars have not enough valid samples, i will remove them
View(star_data%>%group_by(HDnumber)%>%dplyr::summarise(n()))
subsample <- star_data%>%group_by(HDnumber) %>% filter(n()>=maxn)

#solution 1: i just extract all the imputed datasets for the star with the lowest number of
#good imputations, this doesn't work because i have a lot of datasets that don't include 
#many stars
# datasets <- subsample %>% ungroup()%>%filter(HDnumber==51756) %>% select(.imp) 
# subsample <- subsample%>%filter(.imp %in% datasets$.imp)

#solution 2: i group by star and sample for each star a number of observations equal to the 
#number of good samples for the star with fewer samples and then combine them together. 
#this way i am not using "coherent" datasets because i'm joining samples from different datasets
subsample <- subsample%>%group_by(HDnumber)%>%sample_n(maxn)
subsample <- subsample%>%group_by(HDnumber) %>% mutate(.imp=rep(1:maxn,66))
subsample <- subsample%>%arrange(.imp)%>%select(-one_of('X'))

#nested version of the dataset in a tibble with the imputation number on the first column and
#the imputed dataset on the second
subsample_nested <- subsample %>% group_by(.imp) %>% nest()  

#applying the same linear model to all the datasets i've created before
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

#using my custom functions (which basically just mimic the original mice ones) to pool the models
#and get a summary
pools <- subsample_nested %>% ungroup() %>% select(-one_of('.imp','data')) %>% apply(2,pool.lms)
lapply(pools,summary)
