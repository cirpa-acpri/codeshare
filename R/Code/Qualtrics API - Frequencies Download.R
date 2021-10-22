# Qualtrics API Grabbing and Frequencies Generation
# -----------------------------------------------------------------------
# Contributed by: Fraser Hay - Conestoga College
# -----------------------------------------------------------------------
# You will need:
# - To define your working directory immediately below.
# - A text file with:
#    > Your Qualtrics base URL
#    > Your Qualtrics API User Token
# - To put that text file reference in the "temp" object defined below.
# - After viewing available survey IDs, grab the one you want to summarize, enter it in the third section below.
# -----------------------------------------------------------------------

# Set working directory - for output
setwd("C:/Users/fhay/Desktop")

# Libraries
library(tidyverse)
library(qualtRics)
library(Hmisc)
devtools::source_url("https://github.com/cirpa-acpri/codeshare/blob/master/R/Code/Survey%20Data%20Functions.R?raw=TRUE")

# **************************************************************************************************************************
# Define our access variables --- *** ENTER YOUR REFERENCE FILE BELOW ***
# **************************************************************************************************************************
temp = file("C:/Users/Fraser/Dropbox/Python/Qualtrics API - R.txt", "r")  # Open the file for reading. This is where I keep my variables, because I don't want to put them in my script. It's basically a password, so storing it in here I think is bad form.
url = readLines(temp, n = 1, encoding = "UTF-8") # Read the top line
key = readLines(temp, n = 2, encoding = "UTF-8") %>% # Read the next line
  .[1] # But filter it just cuz.
close.connection(temp) # Done with the file.

# Qualtrics API
qualtrics_api_credentials(api_key = key, base_url = url) # Get access to my Qualtrics account via API.
surveys <- all_surveys() # Request a list of all surveys

# **************************************************************************************************************************
# Extract/load --- *** ENTER YOUR SURVEY ID BELOW: ***
# **************************************************************************************************************************
dataset <- fetch_survey(surveyID = "SV_br8iOUumgHfhZc1", verbose = FALSE) # Save a survey data extract as dataset.

Questions = tibble(Q = names(dataset), Text = Hmisc::contents(dataset)[["contents"]][["Labels"]], x = "") %>%  # Generate Questions helper dataset.
  mutate(Text = gsub("[\r]", "", Text), Text = gsub("[\n]", " ", Text))
# The Hmisc package is able to list variable labels, so that's why I use it, since the traditional "first column" isn't there in these extracts. (Which might be smart, but I don't know enough on how to interface with named things.)
detach(package:Hmisc, unload = TRUE) # But it interferes with dplyr::summarize, so I don't want it going forward.

# -----------------------------------------------------------------------

# # Analyze away...
# Q = "Q9"
# ftw(dataset, Q)
# frq_g_simple(ftw(dataset, Q), Questions$Text[which(Questions$Q == Q)])

# Frequencies output file, all questions
ftws = list()
for (x in 1:length(dataset)) {
  temp = ftw(dataset, Questions$Q[x]) %>% 
    mutate(across(1, as.character))
  temp = rbind(c(Questions$Text[x],NA,NA),temp)
  ftws[[x]] = temp
}
names(ftws) = Questions$Q

openxlsx::write.xlsx(ftws, "Basic Survey Frequencies.xlsx")