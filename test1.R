#reading data
library(mice)
library(tibble)
library(here)
#library(VIM)
library(dplyr)
library(stringr)
library(magrittr)
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
base <- '.'
orig <- star_data <- read.csv(here::here(base,'Aerts-Molenberghs-Kenward-Neiner-Table.dat'),header = T,stringsAsFactors = F,na.strings = '*')
star_data <- tibble(star_data)
lower_limit <- upper_limit <- star_data%>% select(X2,X3,X4,X5,X6,X7,X10)
upper_limit<- upper_limit %>% apply(2,to_limit) %>% as_tibble()
lower_limit<- lower_limit %>% apply(2,to_limit,type='>') %>% as_tibble()
star_data <- apply(star_data,2,remove_limits) %>% as_tibble()
colnames(lower_limit) <- paste0(colnames(lower_limit),'_l')
colnames(upper_limit) <- paste0(colnames(upper_limit),'_u')
limit <- cbind(lower_limit,upper_limit)
#colSums(apply(star_data,2,is.na))/68

#md.pattern(star_data)
#md.pattern(orig)

# columns <- colnames(star_data)
# columns <- columns[-1]
# grid <- t(combn(columns,2))
# apply(grid,1,function(x){
#   png(here(base,'plots','comissing',paste0(x[1],'vs',x[2],'.png',collapse='')),width = 600,height = 600)
#   marginplot(star_data[, c(x[1],x[2])], col = mdc(1:2), cex = 1.2,
#              cex.lab = 1.2, cex.numbers = 1.3, pch = 19,alpha = 0.4)
#   dev.off()
# })

star_data2 <- tibble(star_data)
star_data2 <- star_data2 %>% mutate(
  X2 = sqrt(X2),
  X3 = sqrt(X3),
  X4 = sqrt(X4),
  X5 = sqrt(X5),
  X6 = sqrt(X6),
  X10 = log(((X10-6.8)/2.2)/(1-(X10-6.8)/2.2))
  )
star_data2
imp0 <- mice(star_data2,maxit =0)
pred <- imp0$predictorMatrix
pred[1,] <- 0
pred[,1] <- 0
pred[12,] <- 0
pred[,12] <- 0
imp1 <-  parlmice(star_data2,method = 'norm',n.imp.core = 5000,n.core=10,predictorMatrix=pred)

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
