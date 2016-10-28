install.packages('dplyr')
library(dplyr)

## INVESTIGATING THE DEMOGRAPHICS OF SMOKERS/QUITTERS ##

#Adding in an ID column so that we can join the datasets
cigarettes <- cbind(ID = c(1:nrow(cigarettes)), cigarettes)
demographics <- cbind(ID = c(1:nrow(demographics)), demographics)
alcohol <- cbind(ID = c(1:nrow(alcohol)), alcohol)

demt <- demographics %>% 
  mutate(AGE3 = 0) %>% 
  mutate(AGE3 = ifelse(grepl('12', AGE2), '12', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('13', AGE2), '13', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('14', AGE2), '14', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('15', AGE2), '15', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('16', AGE2), '16', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('17', AGE2), '17', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('18', AGE2), '18', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('19', AGE2), '19', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('20', AGE2), '20', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('21', AGE2), '21', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('22', AGE2), '22 or 23', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('24', AGE2), '24 or 25', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('26', AGE2), '26 to 29', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('30', AGE2), '30 to 34', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('35', AGE2), '35 to 49', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('50', AGE2), '50 to 64', AGE3)) %>% 
  mutate(AGE3 = ifelse(grepl('65', AGE2), '65 or older', AGE3)) 

demographics$AGE2 <- demt$AGE3  

# DEMOGRAPHICS
# Profiles of interest:
# 1) Smoker educational level
# 2) Smoker's current age
# 3) Smoker's gender
# 4) Smoker's employment status
# 5) Smoker's marital status

# 1) Typical age for a smoker and a quitter?
demographics %>% 
  full_join(cigarettes) %>% 
  filter(CIGMON == '1') %>% 
  select(AGE2) %>% 
  group_by(AGE2) %>% 
  summarize(count = n()) %>%
  mutate(smoker = sum(count)) %>% 
  mutate(smoker_pop = round((count/smoker *100), digits = 2)) %>% 
  arrange(desc(smoker_pop))
#The typical age of smoker based on smoking behavior in the past month: between 35 to 49 (at 24.88%). The next highest amount is aged 30 to 34 (at 12.07%)

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  filter(CIGYR == '0') %>% 
  select(AGE2) %>% 
  group_by(AGE2) %>% 
  summarize(count = n()) %>%
  mutate(quitr = sum(count)) %>% 
  mutate(smoker_pop = round((count/quitr *100), digits = 2)) %>% 
  arrange(desc(smoker_pop))
#The typical age of quitter: between 35 to 49 (at 30.65%). The next highest is 65+ (at 24.47%)

# 2) What is the avg quit rate for the population?
demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(CIGYR) %>% 
  group_by(CIGYR) %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
# 33.6% of smokers quit on average.

# 3) What are the percentages of smokers within each age group? 
demographics %>% 
  full_join(cigarettes) %>% 
  select(AGE2, CIGMON) %>% 
  group_by(AGE2, CIGMON) %>%
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  ungroup %>% 
  filter(CIGMON == '1') %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(rate))
#About 31.94% of people aged 22-23 have smoked in the past month. This rate is similar throughout the 20s and early 30s. Etc.  
  
# 4) Which age (group) has the highest cessation rates (those who have ever smoked daily but haven't smoked in the past year)?
cigt <- cigarettes %>% 
  mutate(CIGYR2 = '1') %>% 
  mutate(CIGYR2 = ifelse(grepl('not', CIGYR), '0', CIGYR2)) 
cigarettes$CIGYR <- cigt$CIGYR2

cigt <- cigarettes %>% 
  mutate(CDUFLAG2 = '1') %>% 
  mutate(CDUFLAG2 = ifelse(grepl('Never', CDUFLAG), '0', CDUFLAG2)) 
cigarettes$CDUFLAG <- cigt$CDUFLAG2

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(AGE2, CIGYR) %>% 
  group_by(AGE2, CIGYR) %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  arrange(CIGYR) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
  arrange(desc(cess_rate))
# Those aged 65+ have the highest cessation rate at 76.95%. The cessation rates drop markedly before the next highest at 49.81% for ages 50-64. A 17 year old who has been smoking daily has a very low cessation rate at 2.56%. (Note: sample size is quite small)

# 5) Based on those who have succesfully quit smoking, at what age did they begin smoking?
cigt <- cigarettes %>% 
  mutate(CIGAFU2 = 'non-user') %>% 
  mutate(CIGAFU2 = ifelse(grepl('Younger', CIGAFU), '14 or younger', CIGAFU2)) %>% 
  mutate(CIGAFU2 = ifelse(grepl('Old', CIGAFU), '15 to 17', CIGAFU2)) %>% 
  mutate(CIGAFU2 = ifelse(grepl('Older', CIGAFU), '18 or older', CIGAFU2)) 
cigarettes$CIGAFU <- cigt$CIGAFU2  

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  filter(CIGYR == '0') %>% 
  select(CIGAFU) %>% 
  table()
# About 39% of quitters (of which there are 5,129) smoked for the first time at age 14 or younger. 37% between ages 15-17. 24% at age 18 or above.

cigt <- cigarettes %>% 
  mutate(DCIGAFU2 = 'non-user') %>% 
  mutate(DCIGAFU2 = ifelse(grepl('Younger', DCIGAFU), '14 or younger', DCIGAFU2)) %>% 
  mutate(DCIGAFU2 = ifelse(grepl('Old', DCIGAFU), '15 to 17', DCIGAFU2)) %>% 
  mutate(DCIGAFU2 = ifelse(grepl('Older', DCIGAFU), '18 or older', DCIGAFU2)) 
cigarettes$DCIGAFU <- cigt$DCIGAFU2 

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  filter(CIGYR == '0') %>% 
  select(DCIGAFU) %>% 
  table()
# About 53% of quitters began smoking daily for the first time at age 18 or above. 34% between ages 15 to 17. 13% at age 14 or younger. [So the younger you are when you began smoking heavily, the less likely you are to quit later.]
  
# 6) Who smokes more, men or women? 
cigt <- cigarettes %>% 
  mutate(CIGMON2 = '1') %>% 
  mutate(CIGMON2 = ifelse(grepl('not', CIGMON), '0', CIGMON2)) 
cigarettes$CIGMON <- cigt$CIGMON2

demographics %>% 
  full_join(cigarettes) %>% 
  select(IRSEX, CIGMON) %>% 
  group_by(IRSEX, CIGMON) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(CIGMON)) %>% 
  ungroup %>% 
  slice(1:2) %>% 
  mutate(rate = round((count/smokey *100), digits = 2))
# Based on smoking behavior in the past month: 23.25% of men smoke compared to 18.95% of women.

# 7) Is there any age at which there are more female smokers than male? 
demographics %>% 
  full_join(cigarettes) %>% 
  filter(CIGMON == '1') %>% 
  select(IRSEX, AGE2) %>% 
  group_by(IRSEX, AGE2) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(count)) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  ungroup %>% 
  group_by(AGE2) %>% 
  slice(1) %>% 
  arrange(desc(rate)) 
# There are more female smokers in the 50-64 age range as well as at ages 13 and 14.

# 8) At what age do women generally begin smoking? What about for men? 
demographics %>% 
  full_join(cigarettes) %>% 
  select(IRSEX, CIGAFU) %>% 
  filter(CIGAFU != 'non-user') %>% 
  group_by(IRSEX, CIGAFU) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(count)) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(IRSEX))
#  Based on first time use: 35.13% of women began smoking at age 14 or younger and 38.08% of men began smoking at age 14 or younger.

demographics %>% 
  full_join(cigarettes) %>% 
  select(IRSEX, DCIGAFU) %>% 
  filter(DCIGAFU != 'non-user') %>% 
  group_by(IRSEX, DCIGAFU) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(count)) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(IRSEX))
# Based on first time of daily use: 49.80% of women began smoking daily at age 18 or older. And 50.33% of men began smoking daily at age 18+.

# 9) Who are better quitters? Men or women?
demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(IRSEX, CIGYR) %>% 
  group_by(IRSEX, CIGYR) %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  arrange(CIGYR) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
  arrange(desc(cess_rate))
#Men and women both have equal quitting rates at 33% and 34% respectively.

# 10) Do those who have been in the armed forces smoke more?
cigt <- cigarettes %>% 
  mutate(CIGMON2 = '1') %>% 
  mutate(CIGMON2 = ifelse(grepl('not', CIGMON), '0', CIGMON2)) 
cigarettes$CIGMON <- cigt$CIGMON2

demographics %>% 
  full_join(cigarettes) %>% 
  filter(SERVICE == "(1) Yes" | SERVICE == "(2) No") %>% 
  select(SERVICE, CIGMON) %>% 
  group_by(SERVICE, CIGMON) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(CIGMON)) %>% 
  ungroup %>% 
  slice(1:2) %>% 
  mutate(rate = round((count/smokey *100), digits = 2))
# Based on smoking behavior in the past month: Military service doesn't seem to be a contributing factor to cigarette use. Non-military smoke at rate of 25.38%, while military smoke at 25.19%.

# 11) What is the relationship between race & smoking rates, race & cessation rates?
demographics %>% 
  full_join(cigarettes) %>% 
  select(NEWRACE2, CIGMON) %>% 
  group_by(NEWRACE2, CIGMON) %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(CIGMON)) %>% 
  ungroup %>% 
  slice(1:7) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(rate))
# Based on smoking behavior in the past month: Native Americans smoke the most at 35.94% Asians smoke the least at 10.79%.

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(NEWRACE2, CIGYR) %>% 
  group_by(NEWRACE2, CIGYR) %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  arrange(CIGYR) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
  arrange(desc(cess_rate))
# Asians had the highest cessation rate at 38.29%. Native Americans had the lowest cessation rate at 19.30%. [Asian quit rate is higher than avg quit rate]

# 12) Relationship between smoking and employment status for adults?
demt <- demographics %>% 
  mutate(EMPSTAT4_2 = NA) %>% 
  mutate(EMPSTAT4_2 = ifelse(grepl('full', EMPSTAT4), 'full-time', EMPSTAT4_2))  %>% 
  mutate(EMPSTAT4_2 = ifelse(grepl('part', EMPSTAT4), 'part-time', EMPSTAT4_2)) %>% 
  mutate(EMPSTAT4_2 = ifelse(grepl('Unemployed', EMPSTAT4), 'unemployed', EMPSTAT4_2)) 
demographics$EMPSTAT4 <- demt$EMPSTAT4_2

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(EMPSTAT4, CIGYR) %>% 
  group_by(EMPSTAT4, CIGYR) %>% 
  na.omit() %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  arrange(CIGYR) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
  arrange(desc(cess_rate))
#There isn't a significant difference between cessation rates for full or part time employed individuals. But those who are unemployed have a quit rate of only about 13%.

demographics %>% 
  full_join(cigarettes) %>% 
  select(EMPSTAT4, CIGMON) %>% 
  group_by(EMPSTAT4, CIGMON) %>% 
  na.omit() %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(CIGMON)) %>% 
  ungroup %>% 
  slice(1:3) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(rate))
# Based on smoking behavior in the past month: the unemployed smoke the most at 40.86%. The rate is comparable for those employed full or part time at 25.82% and 23.22% respectively.

# 13) Relationship between smoking and marital status for adults?
demt <- demographics %>% 
  mutate(IRMARIT2 = NA) %>% 
  mutate(IRMARIT2 = ifelse(grepl('Married', IRMARIT), 'married', IRMARIT2))  %>% 
  mutate(IRMARIT2 = ifelse(grepl('Widowed', IRMARIT), 'widowed', IRMARIT2)) %>% 
  mutate(IRMARIT2 = ifelse(grepl('Divorced', IRMARIT), 'divorced or separated', IRMARIT2)) %>% 
  mutate(IRMARIT2 = ifelse(grepl('Never', IRMARIT), 'never been married', IRMARIT2)) 
demographics$IRMARIT <- demt$IRMARIT2

demographics %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '1') %>% 
  select(IRMARIT, CIGYR) %>% 
  group_by(IRMARIT, CIGYR) %>% 
  na.omit() %>% 
  summarize(count = n()) %>%
  mutate(smoke_yr = sum(count)) %>% 
  arrange(CIGYR) %>% 
  ungroup %>% 
  filter(CIGYR == '0') %>% 
  mutate(cess_rate = round((count/smoke_yr *100), digits = 2)) %>% 
  arrange(desc(cess_rate))
#Those who are widowed have the highest cessation rate at 57.09%. Married individuals quit at a rate of 50.59%. For other inidividuals, that rate (of about 50%) nearly halves

demographics %>% 
  full_join(cigarettes) %>% 
  select(IRMARIT, CIGMON) %>% 
  group_by(IRMARIT, CIGMON) %>% 
  na.omit() %>% 
  summarize(count=n()) %>% 
  mutate(smokey = sum(count)) %>% 
  arrange(desc(CIGMON)) %>% 
  ungroup %>% 
  slice(1:4) %>% 
  mutate(rate = round((count/smokey *100), digits = 2)) %>% 
  arrange(desc(rate))
# Based on smoking behavior in the past month: those who are divorced or separated smoke the most at 37.87%. The next highest group is those who have never been married at 25.52%.

# 14) What percentage of quitters have used alcohol in the past month?
alct <- alcohol %>% 
  mutate(ALCMON2 = NA) %>% 
  mutate(ALCMON2 = ifelse(grepl('not', ALCMON), '0', '1'))
alcohol$ALCMON <- alct$ALCMON2

alct <- alcohol %>% 
  mutate(HVYDRK2_2 = NA) %>% 
  mutate(HVYDRK2_2 = ifelse(grepl('Never/No', HVYDRK2), '0', '1'))
alcohol$HVYDRK2 <- alct$HVYDRK2_2

alcohol %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '0') %>% 
  filter(CIGYR == '1') %>% 
  select(ALCMON) %>% 
  group_by(ALCMON) %>% 
  summarize(count=n()) %>% 
  mutate(both = sum(count)) %>% 
  mutate(rate = round((count/both *100), digits = 2)) %>% 
  arrange(desc(rate)) %>% 
  slice(1)
#70.77% of quitters have used alcohol in the past month. 

alcohol %>% 
  full_join(cigarettes) %>% 
  filter(CDUFLAG == '0') %>% 
  filter(CIGYR == '1') %>% 
  select(ALCMON, HVYDRK2) %>% 
  group_by(ALCMON, HVYDRK2) %>% 
  summarize(count=n()) %>% 
  mutate(both = sum(count)) %>% 
  mutate(rate = round((count/both *100), digits = 2)) %>% 
  ungroup() %>% 
  arrange(desc(rate)) %>% 
  slice(2:3)
#And of that, 19.9% engaged in heavy alcohol use (defined as drinking 5+ drinks on one occassion on 5+ days in the past month).