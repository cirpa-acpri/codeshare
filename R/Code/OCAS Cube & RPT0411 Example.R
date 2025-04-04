# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Making OCAS Data "from Wide to Long" format - Cube and 0411.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# By Fraser Hay (Conestoga) [fhay at conestogac.on.ca], Winter 2025
# Code also uploaded to CIRPA CodeShare (https://github.com/cirpa-acpri/codeshare)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script is for Ontario College members. Meant to operate on an example OCAS Cube output and 0411 output.
# If you'd like a more live demo / walkthrough, feel free to connect.


library(tidyverse)
consfile = function(path, share.basepath = 'Conestoga College/Institutional Research - File Resources - Shared Resources') { base <- if (dir.exists('C:/OneDrive/Conestoga College')) 'C:/OneDrive' else gsub('\\\\', '/', Sys.getenv('USERPROFILE')); if (grepl('^C:/', path)) { n <- if (grepl('C:/OneDrive/', path)) 2 else 3; pattern <- paste0('^([^/]+/){', n, '}'); file.path(sub(pattern, paste0(base, '/'), path)) } else { if (!is.null(share.basepath)) file.path(base, share.basepath, path) else file.path(base, path) } }


# A simple rejig - Cube Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_Cube_raw = readxl::read_excel(consfile("LMI/~Source Data & Reference Files/OCAS/LMI Machine (New) - Applications.xlsx"))
view(data_Cube_raw)

# We will need: Key column names. Not defined in the file. Silly cube!
key_colnames = c("College Name", "MTCU Code and Title", "Program Title", "Term", "Choice", "Catchment") 

# Where is the header row? Two ways to get this...
header_row = pull(data_Cube_raw, 1) %>% {which(. == "Number of Program Choices")} # Specifically spell it out: row with <label>... or...
header_row = grep("^[A-Z]+$", pull(data_Cube_raw, 1), value = TRUE) %>% .[1] %>% { which(pull(data_Cube_raw, 1) == .) - 1 } # Detect the first all-caps entry and readjust.

# Detect: 
year_columns = data_Cube_raw %>% .[header_row,] %>% as.numeric() %>% {. ->> key_cols} %>% na.omit() %>% as.vector() %>% as.character()
key_col_count = length(key_cols) - length(year_columns); rm(key_cols) # Grouping / breakdown columns
year_columns_select = tail(sort(year_columns), 4) # Last four years

data_Cube = data_Cube_raw %>% 
  select(1:(key_col_count+length(year_columns))) %>% # Select only detected data columns - just for greater certainty.
  .[-c(1:header_row, nrow(data_Cube_raw)),] %>% # Clean down to header row
  `colnames<-`(c(key_colnames, year_columns)) %>% # Assign column names
  select(all_of(key_colnames), all_of(year_columns_select)) %>% # Only the years we want
  fill(all_of(key_colnames), .direction = "down") %>% # Fill data down, for key columns
  pivot_longer(cols = all_of(year_columns_select), names_to = "Year", values_to = "value") %>% # Pivot longer
  filter(value != 0) # Remove 0 lines, save space

# Final note: OCAS cube exports cap out at 65,000 data rows per page - keep in mind if you have a big pull!



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complicated multi-row header: RPT0411
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_0411_raw = readxl::read_excel(consfile("LMI/~Source Data & Reference Files/~Archive/Official Datasets/OCAS Extracts/RPT00411- Comprehensive Report - ALL Catchment - Fall (Jan 25, 2024).xlsx"))
view(data_0411_raw)

# Proposed solution: 
#   1. Assign headers - by concatenating METRIC and YEAR (character delineated).
#   2. Pivot the data.

# ~~~~~~~~~
# Objective 1: Create appropriate column names that we can use with separate() later on.
# ~~~~~~~~~
# Remove unneeded header, and footer rows. ! Hopefully this stays the same !
temp = data_0411_raw %>% 
  .[-c(1,3,(nrow(data_0411_raw)-1):nrow(data_0411_raw)),-c(5:6)] 

header_row = which(!is.na(pull(temp, 1)))[1] # Key column names are on the first row without NA's
key_colnames = temp[header_row,] %>% # Grab the row
  unlist() %>% unname() %>% # Flatten into a vector
  .[1:(which(is.na(.))[1] - 1)] # Chop at the first NA. Now we have our main key (breakdown) columns.

# Remaining columns will be a combination of metric-year. So let's grab them / line them up.
headers_metric = unname(unlist(zoo::na.locf0(temp[1,]))) # Metric header - "Fill-right", first row
headers_year = unname(unlist(temp[2,])) # Year header, second row
# Raw header data: `names<-`(headers_year, headers_metric)

headers = NULL # Placeholder
for (i in 1:length(headers_metric)) { # Loop to concatenate [headers_metric]+[headers_year] vectors together ('a.b')
  headers = c(headers, paste0(headers_metric[i], ".", headers_year[i]))
}
# Delineated headers are now ready.
col_names = c(key_colnames, headers[-c(1:4)]) # Combine. Now we have our full set of column names.
temp_named = temp %>% 
  `colnames<-`(col_names) %>% # Assign
  .[-c(1:header_row),] # Remove header rows - clear!

# ~~~~~~~~~
# Objective 2: Pivot, get column names with separate(), clean up, and fill down.
# ~~~~~~~~~
data_0411 = temp_named %>% 
  fill(all_of(key_colnames), .direction = "down") %>% # Fill data for header vars 
  pivot_longer(cols = 5:ncol(.)) %>% # Pivot to long data
  drop_na(value) %>% # Remove empty rows
  separate_wider_delim(name, delim = ".", names = c("Metric", "Year")) # Separate based on [.]

# Profit.