install.packages('dplyr')
install.packages('ggplot2')
install.packages('MASS')
install.packages('colorRamps')
install.packages('RColorBrewer')
install.packages('devtools')
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
devtools::install_github("pablo14/clusplus")

library(dplyr)
library(ggplot2)
library(MASS)
library(colorRamps)
library(RColorBrewer)
library(ggradar)
library(clusplus)

smokers <- samsha %>% filter(CIGFLAG == '(1) Ever used (IRCIGRC = 1-4)') #Selecting for people who have ever smoked
smokers <- cbind(ID = c(1:nrow(smokers)), smokers) #Adding an ID to link across datasets

# We consider the following factors to investigate smoker profiles: age, overall health, gender, if they've ever used alcohol, severity of alcohol use in the past month, those who get a kick out of doing dangerous things, those who like to test themselves by doing risky things, if they were calculated to have a nicotine dependence, if they received treatment for drug/alcohol in their lifetime, and their level of psychological distress in the past year. These are the variables AGE2, HEALTH, IRSEX, ALCFLAG, BINGEHVY, RKFQDNGR, RKFQRSKY, NDSSDNSP, TXILALEV, SPDYR, respectively.

# NOTE (Hari): removing RKFQDNGR because the profiles almost align with RKFQRSKY

#AGE2 : 1 - 17, 12 year, 65 + years
#IRSEX : 1 - 2,Male, Female

#HEALTH : 0,1 - 5, dontknow/ excellent, poor *
#ALCFLAG : 0 - 1, never used, used
#BINGEHVY : 1 - 4, Heavy Use, Did not use *
#RKFQRSKY : 0,1 -4, dont know/ Never, Always
#NDSSDNSP : 0 -1, No, Yes
#TXILALEV : 0 - 1, No, Yes
#SPDYR : 0 -1, No, Yes
# * need to reverse scale so parallel plot is easier to read. 


profiles <- smokers[,c('ID','AGE2', 'IRSEX','HEALTH', 'ALCFLAG', 'BINGEHVY', 'RKFQRSKY', 'NDSSDNSP', 'TXILALEV', 'SPDYR')]

# converting string to numeric values
profiles <- profiles %>%  
    mutate(AGE2 = as.numeric(substr(AGE2,2,3))) %>% 
    mutate(HEALTH = as.numeric(substr(HEALTH,2,2))) %>% 
    mutate(IRSEX = as.numeric(substr(IRSEX,2,2))) %>% 
    mutate(ALCFLAG = as.numeric(substr(ALCFLAG,2,2))) %>% 
    mutate(BINGEHVY = as.numeric(substr(BINGEHVY,2,2))) %>% 
    mutate(RKFQRSKY = as.numeric(substr(RKFQRSKY,2,2))) %>% 
    mutate(TXILALEV = as.numeric(substr(TXILALEV,2,2))) %>% 
    mutate(NDSSDNSP = as.numeric(substr(NDSSDNSP,2,2))) %>%
    mutate(SPDYR = as.numeric(substr(SPDYR,2,2)))


# reverse scale HEALTH and BINGEHVY
profiles <- profiles %>% 
  mutate(HEALTH = ifelse(HEALTH == 0,0,6-HEALTH)) %>% 
  mutate(BINGEHVY = 5-BINGEHVY)

  
# replacing all NA with 0
profiles[is.na(profiles)] <- 0

# check to make sure there are no NA
apply(profiles, 2, function(x) any(is.na(x)))


# rename columns to more human readable

names(profiles)[4:10] =
  c("HEALTH  RISK","ALCOHOL", "HEAVY DRINKING", "RISKY", "NICOTINE", "TREATMENT", "DEPRESSION")


#Considered using Principal Component Analysis to see if there were any variables signigicantly driving the difference. However based on the plot, there isn't a marked difference.
pca <- prcomp(profiles[,4:10],center = TRUE,scale. = TRUE)
plot(pca, type = "l")
summary(pca)

# Performing cluster analysis by excluding ID, and AGE2... gets too noisy with AGE2
set.seed(20) #helps set reproducible random numbers

#My last point is here
clusters <- kmeans(profiles[,4:10],8,nstart = 20,algorithm = "Hartigan-Wong")

# Assign the cluster back to the profiles dataframe
profiles["CLUSTER"] <- clusters$cluster

#----------------- PARALLEL PLOTS TO VISUALIZE CLUSTERS----------------------
plot_clus_coord(clusters, profiles[,4:10])


#----------------- RADAR PLOTS FOR CIGARETTE USE ---------------------------------

cigarettes <- data.frame(smokers[1],smokers[587:589], smokers[682:685], smokers[787:788], smokers[799], smokers[805:808], smokers[810:813])








