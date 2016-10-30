install.packages('dplyr')
install.packages('ggplot2')
install.packages("stringr", dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)

# NOTE: I just loaded the csv file directly on RStudio
playstore <- cbind(ID = c(1:nrow(playstore)), playstore) # adding a primary key column

# filter only apps that contain smoking/nicotine/ cigarette
filteredApps <- playstore %>% 
  mutate(relevance = ifelse(grepl("smoking",title) | grepl("smoking",summary) | grepl("cigarette",title) | grepl("cigarette",summary) | grepl("nicotine",title) | grepl("nicotine",summary), TRUE, FALSE )) %>% 
  filter(relevance == TRUE)


# synonyms lookup table for matching

lookup <- vector(mode="list",length = 5)
names(lookup) <- c("smoking", "alcohol", "depression", "health", "substance")
lookup[["smoking"]] <- c("smoking", "nicotine", "cigarette")
lookup[["alcohol"]] <- c("alcohol", "drinking", "drunk")
lookup[["depression"]] <- c("depression", "mental")
lookup[["health"]] <- c("health", "cancer", "medicine","treatment")
lookup[["substance"]] <- c("substance", "drug")

# construct dataframe for HEATMAP

x <- c('smoking', 'alcohol', 'depression', 'health', 'substance')
y <- c('smoking', 'alcohol', 'depression', 'health', 'substance')
heatmap_data <- expand.grid(X=x, Y=y) # generates a data frame with all combinations of X and Y
heatmap_data <- cbind(ID = c(1:nrow(heatmap_data)), heatmap_data) # adding primary key column


# function to return count of apps that match

getMatches <- function(x,y){
 
  matches <- filteredApps %>% 
    filter(grepl(paste(lookup[[x]], collapse = "|"),paste(title, summary, sep= " ")) & grepl(paste(lookup[[y]], collapse = "|"),paste(title, summary, sep= " ")))
   
  return(nrow(matches))
}


heatmap_data <- heatmap_data %>% group_by(ID) %>% 
    mutate(total = getMatches(X,Y)) %>% 
    ungroup()

ggplot(heatmap_data, aes(X, Y, z= total)) + geom_tile(aes(fill = total)) + 
  theme_bw() + 
  scale_fill_gradient(low="gray100", high="cadetblue") 





