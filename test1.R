#reading data
library(mice)
library(tibble)
library(here)
#library(VIM)
library(dplyr)
library(stringr)
library(magrittr)
library(data.table)
#functions to extract and remove the bounds for the variables
to_limit <- function(column,type='<'){
  require(dplyr)
  if(!is.character(column)){
    column[!is.na(column)] <- ifelse(type=='<',Inf,0)
    return(column)
  }
  column[!grepl(type,column)] <- ifelse(type=='<',Inf,0)
  column <- column %>% str_remove(type) %>% as.numeric()
  return(column)
}

remove_limits <- function(column){
  require(dplyr)
  column[grepl('<',column)] <- NA
  column[grepl('>',column)] <- NA
  column <- as.numeric(column)
  return(column)
}
#base <- paste0(c('Simone','Uni','Tesi','erasmus','code','MasterThesis'), collapse= '/')
#base <- '.'
#reading the data as tibble
orig <- star_data <- read.csv(here::here(base,'Aerts-Molenberghs-Kenward-Neiner-Table.dat'),header = T,stringsAsFactors = F,na.strings = '*')
star_data <- tibble(star_data)
#getting the limits for the variables with missing values
lower_limit <- upper_limit <- star_data%>% select(X2,X3,X4,X5,X6,X7,X10)
upper_limit<- upper_limit %>% apply(2,to_limit) %>% as_tibble()
lower_limit<- lower_limit %>% apply(2,to_limit,type='>') %>% as_tibble()

#clean dataset with no < or > observation in it
star_data <- apply(star_data,2,remove_limits) %>% as_tibble()

#creating a new dataset with only upper and lower limit for each of the imputed variables
colnames(lower_limit) <- paste0(colnames(lower_limit),'_l')
colnames(upper_limit) <- paste0(colnames(upper_limit),'_u')
limit <- cbind(lower_limit,upper_limit)

#copy of the dataset, i will apply the transformations here
star_data2 <- tibble(star_data)
star_data2 <- star_data2 %>% mutate(
  X2 = sqrt(X2),
  X3 = sqrt(X3),
  X4 = sqrt(X4),
  X5 = sqrt(X5),
  X6 = sqrt(X6),
  X10 = log(((X10-6.8)/2.2)/(1-(X10-6.8)/2.2))
  )

#removing the star id and the binary condition from the predictors for the MI procedure
imp0 <- mice(star_data2,maxit =0)
pred <- imp0$predictorMatrix
pred[1,] <- 0
pred[,1] <- 0
pred[12,] <- 0
pred[,12] <- 0

#multithreading version of mice, 5000 datasets imputed on 10 cores, so it's 50000 total complete
#datasets in the end, twice as many as you had
imp1 <-  parlmice(star_data2,method = 'norm',n.imp.core = 5000,n.core=10,predictorMatrix=pred)

#creating a complete dataset with all the imputations, going back to the original variables
#and removing all the observations with at least one variable not in the limits
long_star <- mice::complete(imp1,'long')
long_star <- tibble(long_star)
long_star <- cbind(long_star,limit) 
long_star <- long_star %>% mutate(
  X2 = X2^2,
  X3 = X3^2,
  X4 = X4^2,
  X5 = X5^2,
  X6 = X6^2,
  X10 = 2.2*(exp(X10)/(1+exp(X10)))+6.8
)
long_star_filtered <- long_star %>% group_by(HDnumber) %>% 
  filter(X2 %between% c(unique(X2_l),unique(X2_u)))%>%
  filter(X3 %between% c(unique(X3_l),unique(X3_u)))%>%
  filter(X4 %between% c(unique(X4_l),unique(X4_u)))%>%
  filter(X5 %between% c(unique(X5_l),unique(X5_u)))%>%
  filter(X6 %between% c(unique(X6_l),unique(X6_u)))%>%
  filter(X7 %between% c(unique(X7_l),unique(X7_u)))%>%
  filter(X10 %between% c(unique(X10_l),unique(X10_u)))

write.csv(long_star_filtered,here::here(base,'long_star_filtered3.csv'))
