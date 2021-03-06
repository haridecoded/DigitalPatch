install.packages('dplyr')
install.packages('ggplot2')
install.packages('MASS')
install.packages('colorRamps')
install.packages('RColorBrewer')
install.packages('devtools')
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
devtools::install_github("pablo14/clusplus")
install.packages('fmsb')
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("corybrunson/ggalluvial")
devtools::install_github("mbojan/alluvial")

library(dplyr)
library(ggplot2)
library(MASS)
library(colorRamps)
library(RColorBrewer)
library(fmsb)
library(clusplus)
library(scales)
library(ggradar)
library(devtools)
library(alluvial)
library(ggalluvial)

smokers <- samsha %>% filter(CIGFLAG == '(1) Ever used (IRCIGRC = 1-4)') #Selecting for people who have ever smoked
smokers <- cbind(ID = c(1:nrow(smokers)), smokers) 

# We consider the following factors to investigate smoker profiles: age, overall health, gender, if they've ever used alcohol, severity of alcohol use in the past month, those who like to test themselves by doing risky things, if they were calculated to have a nicotine dependence, if they received treatment for drug/alcohol in their lifetime, and their level of psychological distress in the past year. These are the variables AGE2, HEALTH, IRSEX, ALCFLAG, BINGEHVY, RKFQRSKY, NDSSDNSP, TXILALEV, SPDYR, respectively.

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


#-----------------CLUSTER ANALYSIS---------------------------------------------

#Considered using Principal Component Analysis to see if there were any variables signigicantly driving the difference. However based on the plot, there isn't a marked difference.
pca <- prcomp(profiles[,4:10],center = TRUE,scale. = TRUE)
plot(pca, type = "l")
summary(pca)

# Performing cluster analysis by excluding ID, and AGE2... gets too noisy with AGE2
set.seed(20) #helps set reproducible random numbers

clusters <- kmeans(profiles[,4:10],8,nstart = 20,algorithm = "Hartigan-Wong")

# Assign the cluster back to the profiles dataframe
profiles["CLUSTER"] <- clusters$cluster

#----------------- PARALLEL PLOTS TO VISUALIZE CLUSTERS--------------------------

plot_clus_coord(clusters, profiles[,4:10])


#----------------- PLOTS FOR DEMOGRAPHICS ---------------------------------

#For each of the identified smoker profiles we explore demographics based on age, gender, race, employment status, and marital status (these are the variables AGE2, IRSEX, NEWRACE2, EMPSTAT4, IRMARIT) and compared them to quit vs not-quit condition. 

# be sure to re-scale if necessary

#CIGAFU: 1-4 scale, 14 or younger to 18+
#IRSEX: 1-2 scale, male or female
#NEWRACE2: 1-7 scale, white, black, native american, native/pacific, asian, multi, hispanic
#EMPSTAT4: 1-4 scale, full-time, part-time, unemployed, ignore 4
#IRMARIT: 1-4 scale, married, widowed, divorced/separated, never married

demographics <- samsha[,c('CASEID', 'IRSEX','AGE2', 'NEWRACE2', 'IRMARIT','EMPSTAT4')]
demographics2 <- semi_join(cbind(demographics[-3], samsha$CIGAFU), smokers, by='CASEID')

demographics2 <- cbind(ID = c(1:nrow(smokers)), demographics2) #Add ID for demographics to link to clusters

demographics2 <- demographics2 %>%   
  mutate(IRSEX = as.numeric(substr(IRSEX,2,2))) %>% 
  mutate(NEWRACE2 = as.numeric(substr(NEWRACE2,2,2))) %>% 
  mutate(EMPSTAT4 = as.numeric(substr(EMPSTAT4,2,2))) %>% 
  mutate(IRMARIT = as.numeric(substr(IRMARIT,2,2))) %>% 
  mutate(`samsha$CIGAFU` = as.numeric(substr(`samsha$CIGAFU`,2,2)))

CIGYR2 <- as.character(cigarettes$CIGYR)
demographics2['Quit'] <- cigarettes$CIGYR  
demographics2['Freq'] <- 1
#length(CIGYR2[which(CIGYR2 == '0')])/length(CIGYR2)
#Freq should be quit over non-quit
demographics2["CLUSTER"] <- clusters$cluster

colnames(demographics2)[3:7] <- c('Sex', 'Race', 'Marital_Status', 'Work', 'Initial_Age')
demographics2 <- demographics2[,c(1, 2, 7, 3:6, 8:10)]
demographics2$Quit <- ifelse(demographics2$Quit == '1', "No", "Yes")
demographics2$Sex <- ifelse(demographics2$Sex == '2', "Female", "Male")

demographics2$Marital_Status[is.na(demographics2$Marital_Status)] <- 0
demographics2$Work[is.na(demographics2$Work)] <- 0

# reverse scale Marital_Status and Work
demographics2 <- demographics2 %>% 
  mutate(Marital_Status = ifelse(Marital_Status == 0,0,5-Marital_Status)) %>% 
  mutate(Work = ifelse(Work == 0, 0, 5-Work))

#demographics2 %>% group_by(Initial_Age, Sex, Marital_Status, Work, Quit) %>% mutate(has_quit = ifelse(Quit == "Yes", TRUE, FALSE)) %>% 
#  mutate(n = length(which(has_quit))) %>% 
#  ungroup() -> x

demographics2 %>% group_by(Initial_Age, Sex, Marital_Status, Work, Quit) %>% mutate(has_quit = ifelse(Quit == "Yes", TRUE, FALSE)) %>% 
  mutate(n = sum(Freq)) %>% 
  ungroup() -> x


x <- aggregate( Freq ~ Initial_Age + Sex + Marital_Status + Quit, data=demographics2, sum)

alluvial( x[,1:4], freq=x$Freq, border=NA,
          hide = x$Freq < quantile(x$Freq, .50),
          col=ifelse( x$Quit == "Yes", "darkseagreen2", "gray"),
          alpha = 0.8)

#alluvial(x[,1:5], freq = x$n, col = ifelse(x$Quit == "Yes", "darkseagreen2", "grey"), border = ifelse(x$Quit == "Yes", "darkseagreen2", "grey"), layer #= x$Quit != "Yes", alpha = 0.8, blocks=TRUE)



#, Quit, Sex, Age, Race, Marital_Status, Work

#----------------- RADAR PLOTS FOR CIGARETTE USE ---------------------------------

#For each of the smoker profile we identified we explore cigarette usage along the following variables:  CIGYR, CIGALCMO, CIGAFU, DCIGAFU, CIGAVGD

#CIGYR: 0 - 1, did not use in past year, used within past year*
#CIGALCMO : 1 - 4 , c & a, c & !a, !c & a, !c & !a *
#CIGAFU : 1 - 4, -14, 15- 17, 18 +,  ** 4 is non user, but irrelevent here because of filter condition 
#DCIGAFU : 1 - 4, -14, 15- 17, 18 +, 4 non-daily user
#CIGAVGD : 0 - 50 , none/ less than 1, 35 + 

cigarettes <- data.frame(smokers[,c('ID','CIGAVGD')], smokers[,c('CIGYR', 'CIGALCMO', 'CIGAFU', 'DCIGAFU')] %>% 
                           mutate_each(funs(as.numeric(substr(.,2,2)))))

cigarettes[is.na(cigarettes)] <- 0

cigarettes["CLUSTER"] <- clusters$cluster

#names(cigarettes)[2:6] =
#  c("DAILY-AVG", " HAS-QUIT", "CIG&ALCOHOL", "AGE-FIRST-USE", "AGE-DAILY-USE")

# groups are of different sizes, so we have to normalize data to make meaningful comparisons between groups on the radar plot

radar_data <- cigarettes %>% 
  mutate_each(funs(rescale), -CLUSTER,-ID) %>% 
  group_by(CLUSTER) %>% 
  summarise(has_quit = mean(CIGYR),first_use_age = mean(CIGAFU),daily_use_age = mean(DCIGAFU), avg_cig = mean(CIGAVGD), cig_alc = mean(CIGALCMO)) %>% 
  arrange(CLUSTER)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
radar_data=rbind(rep(1,6), rep(0,6) , radar_data)

#------------ CLUSTER 1 ---------------

radarchart( radar_data[1:3,2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 2 ---------------

radarchart( radar_data[c(1:2,4),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 3 ---------------

radarchart( radar_data[c(1:2,5),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 4 ---------------

radarchart( radar_data[c(1:2,6),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 5 ---------------

radarchart( radar_data[c(1:2,7),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 6 ---------------

radarchart( radar_data[c(1:2,8),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 7 ---------------

radarchart( radar_data[c(1:2,9),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#------------ CLUSTER 8 ---------------

radarchart( radar_data[c(1:2,10),2:6] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

