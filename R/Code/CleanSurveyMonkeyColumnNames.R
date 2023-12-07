# Function to clean column names of survey data that comes out of SurveyMonkey

# This function renames columns with the text from the first row of the dataset, unless that first row is empty or contains text that you specify in the replaceable_names vector. The first row is then deleted. The resulting column names are cleaned with the clean_names() function from the janitor package. This helps to handle data coming out of SurveyMonkey or similar tools, where the survey questions are split across the top two rows of a CSV or Excel file.

# library(tidyverse) should be loaded when you call this function.

# For SurveyMonkey, you might set the following for text to ignore in the first row:
# replaceable_names <- c("Response", "Open-Ended Response")

# Created by Dawn Macdonald, 2023-12-06

# Last modified 2023-12-06


library(tidyverse)
library(janitor)


clean_survey_monkey_column_names <- function(dataset, replaceable_names = c("Response", "Open-Ended Response"))  {
  
  new_names <- dataset %>%
    slice(1) %>%
    unlist(., use.names=FALSE)
  
  old_names <- colnames(dataset)
  
  for (i in 1:length(new_names)) {
    
    if (new_names[i] %in% replaceable_names | is.na(new_names[i])) {
      
      new_names[i] <- old_names[i]
      
    }
    
  }
  
  names(dataset) <- new_names
  
  dataset <- dataset %>%
    janitor::clean_names()
  
  dataset <- dataset %>%
    slice(-1)
  
  return(dataset)
  
}
