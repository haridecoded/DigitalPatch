library(dplyr)
library(ggplot2)
library(MASS)
library(colorRamps)
library(RColorBrewer)

smokers <- samsha %>% filter(CIGFLAG == '(1) Ever used (IRCIGRC = 1-4)')
smokers <- cbind(ID = c(1:nrow(smokers)), smokers)

# We consider the following factors to investigate smoker profiles: AGE2, HEALTH, IRSEX, ALCFLAG, BINGEHVY, RKFQDNGR, RKFQRSKY, NDSSDNSP, TXILALEV, K6SCMON

profiles <- smokers[,c('ID','AGE2', 'IRSEX','HEALTH', 'ALCFLAG', 'BINGEHVY', 'RKFQDNGR', 'RKFQRSKY', 'NDSSDNSP', 'TXILALEV', 'K6SCMON')]


# converting string to nummeric values
profiles <- profiles %>%  
    mutate(AGE2 = as.numeric(substr(AGE2,2,3))) %>% 
    mutate(HEALTH = as.numeric(substr(HEALTH,2,2))) %>% 
    mutate(IRSEX = as.numeric(substr(IRSEX,2,2))) %>% 
    mutate(ALCFLAG = as.numeric(substr(ALCFLAG,2,2))) %>% 
    mutate(BINGEHVY = as.numeric(substr(BINGEHVY,2,2))) %>% 
    mutate(RKFQDNGR = as.numeric(substr(RKFQDNGR,2,2))) %>% 
    mutate(RKFQRSKY = as.numeric(substr(RKFQRSKY,2,2))) %>% 
    mutate(TXILALEV = as.numeric(substr(TXILALEV,2,2))) %>% 
    mutate(NDSSDNSP = as.numeric(substr(NDSSDNSP,2,2)))

# replacing all NA with 0
profiles[is.na(profiles)] <- 0

# check to make sure there are no NA
apply(profiles, 2, function(x) any(is.na(x)))


pca <- prcomp(profiles[,3:11],center = TRUE,scale. = TRUE)
plot(pca, type = "l")
summary(pca)

# Performing cluster analysis by excluding ID, and AGE2... gets too noisy with AGE2
set.seed(20)
clusters <- kmeans(profiles[,3:11],8,nstart = 20,algorithm = "Hartigan-Wong")

# Assign the cluster back to the profiles dataframe
profiles["CLUSTER"] <- clusters$cluster

# PARALLEL PLOTS... need to pretty
k <- adjustcolor(brewer.pal(3, "Set1")[profiles$CLUSTER], alpha=.2)
parcoord(profiles[,3:11], col=k,var.label= TRUE)

# Cigarettes
cigarettes <- data.frame(smokers[1],smokers[587:589], smokers[682:685], smokers[787:788], smokers[799], smokers[805:808], smokers[810:813])








