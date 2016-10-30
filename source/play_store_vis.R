install.packages('dplyr')
install.packages('ggplot2')
install.packages("stringr", dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)


x <- c('smoking', 'nicotine', 'alcohol', 'depression', 'health', 'substance')
y <- c('smoking', 'nicotine', 'alcohol', 'depression', 'health', 'substance')
apps <- data <- expand.grid(X=x, Y=y)

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  }
  return(temp)
}

playstore <- cbind(ID = c(1:nrow(playstore)), playstore)


playstore <- playstore  %>% 
  group_by(ID) %>% 
  mutate(terms = I(list(Clean_String(summary)))) %>% 
  ungroup()









