# ALCOHOL
# Profiles of interest:
# 1) Smoker with co-existing alcohol use:
# a) addictive drinking
# b) social drinking

#-How much alcohol does the average smoker tend to consume? How does this relate to quitting rates?
#-Is binge or heavy alcohol use associated with smoking a pack of cigarettes each day?
#-Are social drinkers who smoke, more likely to quit?

# How likely are people to quit smoking, when they start smoking and consuming alcohol at the same age?
# Do people who quit smoking, quit drinking as well? What the the correlation between quitting smoking and drinking?



library(dplyr)
library(tidyr)
library(ggplot2)

cig <- cigarettes %>% mutate_each(funs(as.numeric(substr(.,2,2))))
cig <- cbind(ID = c(1:nrow(cig)), cig)
all_smokers = cig %>% filter(CIGFLAG == 1) %>% mutate(HasQuit = ifelse(CIGYR == 0, TRUE, FALSE))


alc <- alcohol %>% select(c(1:9)) %>% mutate_each(funs(as.numeric(substr(.,2,2))))
alc <- cbind(ID = c(1:nrow(alc)), alc)

cig_alcohol <- all_smokers %>% left_join(alc,by="ID") %>% mutate(cig_start = factor(CIGAFU, labels = c("14 years or younger","15 - 17 years", "18 years or older"))) %>%
  mutate(alc_start = factor(ALCAFU2,labels = c("13 Years or Younger", "14 -17 years", "18 -20 Years", "18 years or older","Non User" ))) %>% 
  mutate(HasQuitAlc = ifelse(ALCYR == 0, TRUE, FALSE))



# ALCMDays, CIGMDAYS, ALC_START, CIG_START -> QUIT SMOKING

library(FactoMineR)
result <- PCA(cig_alcohol %>% select(c(2:12,16:19,21:29)))


result <- PCA(cig_alcohol %>% select(c(9,3,22,27)))


