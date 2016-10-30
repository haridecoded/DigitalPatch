# CIGARETTE USAGE
# Profiles of interest:
# 1) Smokers with high risk behavior
# 2) Smoker's daily cigarette consumption
# 3) Smoker initiation age
# a) first-time use
# b) first-time of daily use
library(dplyr)
library(tidyr)
View(cigarettes)

cig <- cigarettes %>% mutate_each(funs(as.numeric(substr(.,2,2))))
#Adding in an ID column so that we can join the datasets
cigarettes <- cbind(ID = c(1:nrow(cigarettes)), cigarettes)

cigt <- cigarettes %>% 
  mutate(CIGYR2 = '1') %>% 
  mutate(CIGYR2 = ifelse(grepl('not', CIGYR), '0', CIGYR2)) 
cigarettes$CIGYR <- cigt$CIGYR2

cigt <- cigarettes %>% 
  mutate(CDUFLAG2 = '1') %>% 
  mutate(CDUFLAG2 = ifelse(grepl('Never', CDUFLAG), '0', CDUFLAG2)) 
cigarettes$CDUFLAG <- cigt$CDUFLAG2

#-How many cigarettes a day was the average ex-smoker smoking?
#An exsmoker is defined as CDUFLAG =1 and CIGYR = 0
cignum<-cigarettes %>% 
  group_by(CIGPDAY) %>% 
  filter(CDUFLAG==1|CIGYR==0 ) %>% 
  summarise(count=n()) %>%
  mutate(percent= round(count/sum(count)*100)) %>% 
  arrange(desc(percent))
  #' about 75% reported no daily use; 10% smoked 6-15 cigarettes; 7% smoked 16-25; 6% fewer than 6 and 2%  more than 26. 

#Is there a threshold where after you begin smoking a certain number of cigarettes each day your chances of cessation markedly decrease?
#-What is the relationship between age of initiation and ex-smoker status? 
age_smoke <- cigarettes %>% 
  group_by(CIGAFU) %>% 
  filter(CDUFLAG==1|CIGYR==0) %>% 
  summarise(count=n()) %>% 
  mutate(total=sum(count)) %>% 
  mutate(percent=(count/total*100)) %>% 
  arrange(desc(percent))
  #' 38.49% of ex-smoker initiated smoking at age of 14 or younger; 34.39% at age of 15-17 and 27.11% at 18 years or older
  
#-What is the relationship between age of initiation of daily use and ex-smoker status? 
age_daily <- cigarettes %>% 
  filter(CDUFLAG==1|CIGYR==0) %>% 
  group_by(DCIGAFU) %>% 
  summarise(count=n()) %>% 
  mutate(total=sum(count)) %>% 
  mutate(percent=(count/total*100)) %>% 
  arrange(desc(percent))
#' 9.39 % of ex-smoker started smoking daily at the age of 14 or younger; 21.2 % at age 15-17 and 30.69% at age 18 and above.
#' This is opposite to the age of smoking initiation. 


#-What percentage of people who haven’t smoked in the past 30days also haven’t smoked in the past year?
mth_yr <-cigarettes %>% 
  group_by(CIGFLAG) %>% 
  filter(CIGMON==0|CIGYR==0) %>% 
  summarize(count=n())
  
  
  

  
 


  

  
