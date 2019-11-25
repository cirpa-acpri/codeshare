# Stolen from Mark Kane, Conestoga College (Kitchener, Ontario), mkane{at}conestogac.on.ca
# An example of unpivoting a dataframe.

# Import relevant packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Set working directory, just for convenience.
setwd("S:/Institutional_Research/Special Projects/Ad hoc Requests/OCAS Int Diversification")

# Load our data file into a data frame (table) (called "xls_data") by using the "read_excel" function.
xls_data <- read_excel("Copy of Enrolment Cube - Int Country Nov 11, 2019.xlsx", sheet="DATA")

# Open the data frame in a read-only viewable tab - note the capital "V"
View(xls_data)
# For convenience, list the names of the columns in the data frame.
names(xls_data)

# Unpivot / make data long - turn the columns ("2016", "2017", "2018", "2019") into a single variable ("Year"), with the values for those years encoded as "Count".
data_long <- gather(xls_data, key="Year", value="Count", "2016":"2019")
View(data_long)

# Write the "data_long" data frame into an Excel file (saved in the working directory).
write.xlsx(data_long, "output.xlsx")