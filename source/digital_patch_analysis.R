# MIDTERM
#Yippie!

# Gita - Cigarettes, Adaeze - Demographics, Hari -  Treatment

#Adding in an ID column so that we can join the datasets
cigarettes <- cbind(ID = c(1:nrow(cigarettes)), cigarettes)
demographics <- cbind(ID = c(1:nrow(demographics)), demographics)

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

# (Way to change an existing column with pipes instead of having to mutate to create a new column?)

# 1) Which age (group) has the highest cessation rates (those who have ever smoked daily but haven't smoked in the past year)?

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

# Those aged 65+ have the highest cessation rate at 76.95%. The cessation rates drop markedly before the next highest at 49.81%. A 17 year old who has been smoking daily has a very low cessation rate at 2.56%. (Note: sample size is quite small)

# 2) Who smokes more, men or women? Is there any age at which there are more female smokers than male?
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

# Any age at which there are more female smokers than male: 
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

# 3) Do those who have been in the armed forces smoke more?
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


# 4) What is the relationship between race and smoking rates and smoking cessation rates?
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

# Based on smoking behavior in the past month: Native Americans smoke the most at 35.94%. Asians smoke the least at 10.79%.

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

# Asians had the highest cessation rate at 38.29%. Native Americans had the lowest cessation rate at 19.30%.

# 5) Relationship between smoking and employment status?
# 6) At what age do women begin smoking on average? What about for men?


#' BELOW ARE SOME INTERESTING Q'S FOR EXPLORATION
#-How many cigarettes a day was the average ex-smoker smoking? Is there a threshold where after you begin smoking a certain number of cigarettes each day your chances of cessation markedly decrease?
#-What is the relationship between age of initiation and ex-smoker status? At what age do women begin smoking on average? What about for men?
#-What percentage of people who haven’t smoked in the past 30days also haven’t smoked in the past year?
#-Do cigarettes smokers tend to smoke marijuana more than non cigarette smokers? What about compared to ex-smokers?
#-Do people who enjoy engaging in risky behavior tend to also smoke? Are they less likely to quit?
#-Are those who received treatment for drug use more likely to quit?
#-How many who felt they needed treatment ended up quitting? 
#-What is the major reason people don’t seek treatment for substance dependence/abuse?

# ALCOHOL
#-How much alcohol does the average smoker tend to consume? How does this relate to quitting rates?
#-Is binge or heavy alcohol use associated with smoking a pack of cigarettes each day?
-#Are social drinkers who smoke, more likely to quit?
  
# MENTAL HEALTH
#-How does being on psychiatric medication in the past year relate to whether or not someone has smoked in the past year?
#-Do people who received outpatient mental health treatment smoke more than those who got inpatient help?
#-Are those in counseling less likely to smoke?
#-What percentage of smokers have sought a doctor for mental health medication?
#-Relevance of depression in female/male smokers