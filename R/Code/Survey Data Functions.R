# Survey Data Functions
# --------------------------------------------------------------------------------------------
# For working with data and surveys from Qualtrics

# By: Fraser Hay, Conestoga College
# With some things stolen from: Mark Kane, Conestoga College
# --------------------------------------------------------------------------------------------

# These functions are probably inefficiently coded, but they seem to work. They also save me a lot of time.
# They in part trace my slow learning / development in the R language. Feel free to suggest improvements.

# Most of the time when we work with surveys at Conestoga we're using categorical data out of Qualtrics,
# and end up creating a weighting variable, so most of the functions here that summarize data [eg. ftw()] 
# have a weight option. In the cases where a weight isn't specified, it assumes all cases have a weight of
# 1 and proceeds as normal.

# For one-response questions, ftw() and the frq_g_simple() bar graph is the main approach.
# For multiple-response questions, we typically employ mc_ftw() with frq_g_simple().
# For battery questions, we use battery_ftw() and frq_g_battery(), which is our 100% stacked bar.

# Hope these prove useful. Thanks obviously to the many, many people who contribute to the
# R community on the internet / Stack Overflow, who have supplied answers to my questions over many
# hours of Googling and head-scratching.

# --------------------------------------------------------------------------------------------

# Gotta have some libraries...
library(tidyverse)
library(knitr)
library(readxl)
library(DT)
library(patchwork)

# Suppress that "groups" warning from the summarize() function.
options(dplyr.summarise.inform=F)

# Percent Formatting
# --------------------------------------------------------------------------------------------
pct_format = function(value, decimals = 0) {
  ifelse(!is.na(value), sprintf(paste0("%.", decimals, "f%%"), (value * 100)), NA)
}

# Remove percent, return number. Basically the reverse of pct_format.
# --------------------------------------------------------------------------------------------
pct_n = function(value) {
  ifelse(!is.na(value), as.numeric(gsub("\\%", "", value)) / 100, NA)
}

# Comma-Number Formatting
# --------------------------------------------------------------------------------------------
n_format = function(n, decimals = 0) {
  ifelse(!is.na(n), formatC(n,format="f", big.mark=",", digits=decimals), n)
}

# Remove commas, return number. Basically the reverse of n_format. (Credit: https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r)
# --------------------------------------------------------------------------------------------
comma_n <- function(x){ 
  as.numeric(gsub("\\,", "", x))
}

# Label Wrapping (Credit: https://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels) 
# ---------------------------------------------------------------------
# Super useful for ensuring strings wrap at particular character counts.
# ---------------------------------------------------------------------
wrap.labels <- function(x, len) {
  if (is.list(x))
  {
    lapply(x, sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE), len)
  } else {
    sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
  }
}

# Copy a dataframe to paste in Excel (Credit: https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet)
# ---------------------------------------------------------------------
# Because copying out of an R dataframe in to Excel is, for reasons beyond my comprehension, not built-in.
# ---------------------------------------------------------------------
cb <- function(df, sep="\t", dec=".", max.size=(200*1000)) {
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

# Paste a table you just copied in Excel into R (Credit: https://www.youtube.com/watch?v=4Y_UhaZj-5I)
# ---------------------------------------------------------------------
# As above, this should be built-in... Example usage: 1) Copy data with headers in Excel. 2) data = r.cb()
# ---------------------------------------------------------------------
cb.read = function(sep="\t", header=TRUE) {
  read.table("clipboard", sep=sep, header=header)  
}

# Set values - so I can adjust labels or values while still in a pipe.
# --------------------------------------------------------------------------------------------
# Useful programmatic example: set(which(data$student_fee == "Regular"), 2, "Domestic")
# --------------------------------------------------------------------------------------------
set = function(df, r, c, value) {
  df[r, c] = value
  ungroup(df) # This is basically because occasionally the output comes back grouped? No idea why.
  return(df)
}

# (F)requency (T)able, (W)eighted - Default sort is by count.
# --------------------------------------------------------------------------------------------
# Creates a basic frequency table (with percentages) based on a categorical question.
# Arguments:
#   count_round = Whether you want counts rounded to whole numbers.
# --------------------------------------------------------------------------------------------
ftw = function(df, Question_var_in_Quotes, Weight_var_in_Quotes = NULL, count_round = TRUE) {
  df = df %>% 
    { if (is.null(Weight_var_in_Quotes)) select(., Question_var_in_Quotes) else select(., Question_var_in_Quotes, Weight_var_in_Quotes) } %>% 
    rename(resp = Question_var_in_Quotes) %>% 
    { if (is.null(Weight_var_in_Quotes)) mutate(., w = 1) else rename(., w = Weight_var_in_Quotes) } %>% 
    drop_na %>%
    group_by(resp) %>% 
    summarize(count = sum(w)) %>% 
    mutate(pct = count/sum(count)) %>% 
    arrange(desc(count))
  if (count_round == TRUE) {
    mutate_at(df, vars(count), round) 
  }
  return(df)
}

# (C)ross (T)abulation
# --------------------------------------------------------------------------------------------
# Creates a simple crosstab count table from two variables.
# Arguments:
#   Weight_var_in_Quotes = Specific weighting variable, optional.
#   pct = Set to TRUE to change output to row percentages
#   count = Set to TRUE to return a total N for each row - usually only used in conjunction with pct.
#   decimals = Specify an integer to round to that many places
#   include_N = Adds an "N" column with row totals.
#   sort = Either set to TRUE to return the columns ordered highest (n) to lowest, or specify a label vector for the ordering.
# --------------------------------------------------------------------------------------------
ct = function(dataset, Row_Demo_quoted, Column_Question_quoted, Weight_var_in_Quotes = NULL, pct = FALSE, decimals = NULL, count = FALSE, include_N = TRUE, sort = TRUE) {
  Data = dataset %>% 
    { if (is.null(Weight_var_in_Quotes)) select(., Column_Question_quoted, Row_Demo_quoted) else select(., Column_Question_quoted, Row_Demo_quoted, Weight_var_in_Quotes) } %>%
    rename(Response = 1, Field = 2) %>% 
    { if (is.null(Weight_var_in_Quotes)) mutate(., weight = 1) else rename(., weight = 3) }
  if ("list" %in% sapply(data, class)) message("Caution: The function can run into problems with non-traditional data types.")
  CrossTab = Data %>% 
    group_by(Field, Response) %>% 
    drop_na %>%
    summarize(count = sum(weight)) %>% 
    ungroup() %>%
    pivot_wider(names_from = Response, values_from = count, names_prefix = "cnt_") 
  if (include_N == TRUE) {
    CrossTab = CrossTab %>% 
      mutate(N = round(rowSums(select(., 2:ncol(CrossTab)), na.rm = TRUE)), Field = paste0( Field," (N = ",N,")" ))
  } 
  CrossTab = select(CrossTab, Field, starts_with("cnt_")) 
  names(CrossTab) = str_replace(names(CrossTab), "cnt_", "")
  Overall = Data %>%  
    group_by(Response) %>% 
    drop_na %>%
    summarize(count = sum(weight)) %>%
    ungroup() %>%
    pivot_wider(names_from = Response, values_from = count, names_prefix = "cnt_") %>% 
    mutate(Field = "Overall") %>% 
    select(Field, starts_with("cnt_"))
  names(Overall) <- str_replace(names(Overall), "cnt_", "")
  Output = bind_rows(Overall,CrossTab)
  names(Output)[1] = Row_Demo_quoted
  if (count == TRUE) {
    Output = Output %>%
      mutate(count = rowSums(select(., 2:ncol(.)), na.rm = TRUE)) %>%
      .[,c(1,ncol(.),2:(ncol(.)-1))]
  }
  if (pct == TRUE) {
    if (count == TRUE) {
      temp = Output[,2]
      Output = Output %>% 
        .[,c(1,3:ncol(.))]
    }
    Output = Output %>% 
      pivot_longer(cols=2:ncol(.), names_to="cat", values_to = "val") %>% 
      group_by_at(1) %>% 
      mutate(pct = val / sum(val, na.rm = TRUE)) %>% 
      select(1:2,4) %>% 
      pivot_wider(names_from = cat, values_from = pct)
    if (count == TRUE) {
      Output = bind_cols(Output, temp) %>% 
        .[,c(1,ncol(.),2:(ncol(.)-1))]
    }
  }
  if (!is.null(decimals)) {
    Output = Output %>% mutate_at(vars(-1), ~ round(., digits = decimals))
  }
  # Sort, if activated
  if (all(sort == TRUE)) {
    x = Output
    a = x[,1] # Pop-off the top
    b = x[,2:ncol(x)] # Sort the rest
    b = b[,rev(order(b[1,]))]
    Output = bind_cols(a, b) # Recombine
    return(Output)
  } else if (length(sort) > 1) { # If a vector is provided, sort the output by the vector.
    return(Output[,c(Row_Demo_quoted,sort)])
  }
  return(Output)
}
  
# Row Percents
# ------------------------------------------------------------------------------------------------------------
# Transforms a frequency table input to row percentages. Remember ct() above can also do this.
# Arguments:
#   Col1Cats = Whether the first column is the categories. Set to FALSE if not. 
#   count = Whether you want a count column maintained - either as column 1, or column 2 if Col1Cats = TRUE.
# ------------------------------------------------------------------------------------------------------------
row_percents = function(df, Col1Cats = TRUE, count = FALSE) {
  if (Col1Cats == FALSE) {
    df = df %>% 
      rowid_to_column()
  }
  if (count == TRUE) {
    temp_counts = df %>% 
      ungroup() %>% 
      mutate(count = rowSums(select(., 2:ncol(.)), na.rm = TRUE)) %>% 
      .[,ncol(.)]
  }
  df = df %>%
    ungroup() %>% 
    pivot_longer(cols=2:ncol(.),names_to="cat", values_to = "val") %>% 
    group_by_at(1) %>% 
    mutate(pct = val/sum(val, na.rm = TRUE)) %>%
    select(1:2,4) %>% 
    pivot_wider(names_from = cat, values_from = pct)
  if (Col1Cats == FALSE) {
    df = df %>% 
      .[,-1]
  }
  if (count == TRUE) {
    df = bind_cols(df, temp_counts) %>% 
      { if (Col1Cats == FALSE) .[,c(ncol(.), 1:(ncol(.)-1))] else .[,c(1,ncol(.),2:(ncol(.)-1))] }
  }
  return(ungroup(df))
}

# Column Percents
# ------------------------------------------------------------------------------------------------------------
# Transforms a frequency table input to column percentages.
# Arguments:
#   Col1Cats = Whether the first column is the categories. Set to FALSE if not. 
# ------------------------------------------------------------------------------------------------------------
col_percents = function(df, Col1Cats = TRUE) {
  if (Col1Cats == TRUE) {
    mutate(df, across(.cols = -1,  ~ ./sum(., na.rm = TRUE)))
  } else {
    mutate(df, across(.cols = everything(), ~ ./sum(., na.rm = TRUE)))
  }
}


# (C)rosstabulation (Table) Rendering
# ------------------------------------------------------------------------------------------------------------
# Renders output from the ct function above, optionally with chi-square test (if supplied frequency counts).
# Arguments:
#   Col1Name = Name of the table's 1st column - typically the demo / category title
#   Col1Width = How wide should the first column (categories) be? Especially in relation to...
#   OtherColWidths = How wide should the other columns be? Adjusting these allows you to fit more or less in the table nicely.
#   chi2 = Toggle the chi2 test and colouring in the table (TRUE vs. FALSE)
#   freq = Toggle reporting the results as the raw counts, rather than making them into row percentages (T/F)
#   decimals = Assignment for how many decimals to include in numerical output.
#   title = Specify a title for the top of your table.
#   font = Font size for the table container.
# ------------------------------------------------------------------------------------------------------------
ctable = function(df, Col1Name = NULL, Col1Width = "20%", OtherColWidths = "10%", chi2 = TRUE, freq = FALSE, decimals = NULL, title = "", font = "15") {
  # Assign Col1Name, if not default to supplied variable title
  if (!is.null(Col1Name)) { names(df)[1] = Col1Name } 
  if (chi2 == TRUE) {
    # Check for NA rows - kills chi2
    data = df %>% 
      mutate(NAcheck = rowSums(select(., 2:ncol(.)), na.rm = TRUE))
    if (0 %in% data$NAcheck) {
      chi2 = FALSE
      message("Message: Dataframe supplied contains a row of NA's - disabling chi-square test results colouring.")
    } else {
      # Prepare data as table object - needed for chi-square test.
      data = df %>% 
        mutate_at(vars(-1), ~ replace_na(., 0))
      if (data[[1,1]] == "Overall") { data = data[-1,] } # Ignore "Overall" row, if found
      if (nrow(data) > 1) {
        data_table = as.matrix(data[,2:ncol(data)])
        data_table = as.table(data_table)
        rownames(data_table) = data[[1]]
        # Chi-square test
        wtest = chisq.test(data_table)
        wtest$Prop <- prop.table(wtest$observed, 1)
        if (wtest$p.value > 0.05 ) { 
          wtest$ColFlag = 9
          chi2 = FALSE # Chi-square non-significant, so disable.
        } else {
          wtest$ColFlag <- ifelse(wtest$expected >= 5, 1, 0) * ifelse(wtest$stdres >= 2, 1, ifelse(wtest$stdres <= -2, -1, 0))  
        }
      } else {
        chi2 = FALSE
        message("Message: Dataframe supplied only contains one effective row - disabling chi-square test results colouring.")
      }
    }
  }
  # Transform to row percentages
  if (freq == FALSE) {
    if (mean(as.vector(rowMeans(df[,-1])), na.rm = TRUE) >= 1.5) {
      df = df %>% row_percents(count = TRUE)
      row_totals = df[,2] %>% # Split off the row totals for later
        set(1, 1, 0)
      df = df[,c(1,3:ncol(df))]
    } else {
      if (chi2 == TRUE) {
        print("Message: You appear to possibly be running this function on a percentage table. The chi-square colouring will not return effective results. To silence this message, either change your input data to be a frequency table, or disable the chi-square test by using the parameter: 'chi2 = FALSE' in your function call.")
      }
    }
  } else {
    row_totals = df %>% 
      transmute(count = rowSums(select(., 2:ncol(.)), na.rm = TRUE)) %>% 
      set(1, 1, 0)
    freq_n = sum(df[1,-1], na.rm = TRUE)
  }
  # Rounding if activated
  if (!is.null(decimals)) {
    if (freq == FALSE) {
      decimals = decimals + 2
    }
    df = df %>% mutate_at(vars(-1), ~ round(., digits = decimals))
  }
  # Append row totals
  df = bind_cols(df, row_totals)
  max_cat = max(row_totals)
  # Append chi2 formatting
  if (chi2 == TRUE) {
    df = bind_cols(as_tibble(df), rename_all(as_tibble(rbind(rep(10, ncol(wtest[["ColFlag"]])), wtest[["ColFlag"]])), function(x) paste0(x,"_chi")))
  } else {
    df = bind_cols(as_tibble(df), as_tibble(rbind(matrix(10L, nrow = 1, ncol = dim(df)[2]-2), matrix(9L, nrow = dim(df)[1]-1, ncol = dim(df)[2]-2))))
  }
  N <- (ncol(df) - 2) / 2 # Number of columns, will be useful for the table render, relating to the dataframe.
  CrossTab = datatable(df, class = 'row-border', rownames = FALSE, 
                       caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',title),
                       options = list(
                         initComplete = htmlwidgets::JS("function(settings, json) {", paste0("$(this.api().table().container()).css({'font-size': '", paste0(font, "px"), "'});"), "}"), # Source: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
                         scrollX = FALSE, paging = FALSE, searching = FALSE, dom = 't', ordering = F, autoWidth = TRUE,
                         columnDefs = list(list(visible=FALSE, targets = (N+1):(2*N+1)),
                                           list(className = 'dt-center', targets = "_all"),
                                           list(width = Col1Width, targets = 0),
                                           list(width = OtherColWidths, targets = 1:N)))) %>%
    formatStyle(1, target = "row", # "Overall" row formatting
                backgroundColor = styleEqual("Overall", "#337ab7"),
                fontWeight = styleEqual("Overall", 'bold'),
                color = styleEqual("Overall", 'white')) %>%
    formatStyle(2:(N+1), # Data bars - table values
                background = styleColorBar({if (freq == FALSE) as.integer(0:1) else as.integer(c(0,freq_n))}, "#d4d4d485", angle = -90),
                backgroundSize = '98% 80%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
    formatStyle(1, (N+2), # Data bars - 1st column N's
                background = styleColorBar(as.integer(0:max_cat), "#dfe6f585", angle = -90),
                backgroundSize = '98% 80%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
    formatPercentage(ifelse(freq==FALSE,2,0):ifelse(freq==FALSE,(N+1),0), ifelse(!is.null(decimals),ifelse(freq==FALSE,decimals-2,decimals), 0)) %>% # Percentages
    formatStyle(1:(N+1), border = '1px solid #ddd') %>% 
    # Chi2 formatting
    formatStyle(2:(N+1), 
                valueColumns = (N+3):(2*N+2), 
                color = styleEqual(c('-1', '0', '1', '9', '10'), 
                                   c('#C00000', 'black', '#76933C', '#858585', 'white')))
  return(CrossTab)
}


# (C)ross(t)abulation (Tabset) for (D)ata(t)ables
# ------------------------------------------------------------------------------------------------------------
# Generates the knit code (and related materials) to create a dynamically-defined series of tabs with ctable() renderings.
# It does this by committing two supposed cardinal sins of programming: a) interpreting a string (that you pass) as code (for generating the 
# data behind the ctable() calls); and b) assigning a static global variable - but at least you can specify the name!
# It returns 3 lists - the knit code to be run, the DTs (list), and the raw table input (with the row_percents() function run on it) (list).
# Requires the ctable() and row_percents() functions. This code was inspired by: https://gist.github.com/StevenMMortimer/e54ec050d97d79996189.
# Arguments:
#   ct_code_in_quotes = The literal code that should be used to generate the underlying data tables to be fed into the ctable() calls.
#   Demos = A vector of all the demographic crosstabulation variables you want to be looped through.
#   Tabs = A vector of all the titles of the tabs you want generated for each respective demo-ctable() output.
#   Params = A vector of any ctable() parameters you want passed for each respective demo-ctable() output.
#   HeadingLvl = How many #'s to include as part of the rendered tabsets.
#   output = Set to TRUE to have the output returned by the function call.
#   assign = String to name the variable assignment for the output; set to NULL to disable.
#
# The code includes an annoying message to remind you of some things. Namely, it wants you to know that the output from the function 
# is actually "knit code" that needs to be rendered using a knit(text) command. You also need to ensure you end the tabset 
# (that the code creates) after you render this. Here's what the message looks like:
# --- To preview the output, paste and run the following:
# for (i in 1:length(ct.dt.out[['dt']])) {print(ct.dt.out[['dt']][[i]])}
# --- Include knit command *in-text* below for rendering (remember to end the tabset):
# `r paste(knit(text = ct.dt.out[['knit_code']]), collapse = '\\n')`"
# ------------------------------------------------------------------------------------------------------------
ct_tabset_dt = function(ct_code_in_quotes_x_as_demo, Demos, Tabs, Params = NULL, HeadingLvl = 4, output = FALSE, assign = "ct.dt.out") {
  if (is.null(Params)) Params = rep("", length(Demos))
  output_raw = list() # List for the raw tables
  output_dt = list() # List for the datatables
  for (x in Demos) { # For each demo variable...
    temp = eval(parse(text = ct_code_in_quotes_x_as_demo)) # Evaluate the defined code literally...
    output_dt[[which(Demos == x)]] = eval(parse(text = paste0("ctable(temp, ", Params[[which(Demos == x)]],")"))) # Create the ctable() call string and run...
    output_raw[[which(Demos == x)]] = suppressMessages(temp %>% bind_cols(row_percents(.)[-1] %>% rename_at(vars(everything()),function(x) paste0(x,"_%")))) # Store the result in the raw tables list...
  }
  names(output_dt) = Demos # Assign names for clarity
  names(output_raw) = Demos
  out = NULL # Variable for knit statements
  for (i in 1:length(output_dt)) { # For every DT we have...
    knit_expanded <- paste0("\n\n", paste0(rep("#", HeadingLvl), collapse = "")," ", Tabs[i], " {-}\n\n```{r results='asis', echo=FALSE, message=FALSE, warning=FALSE}\n\n",assign,"[['dt']][[", i, "]]\n\n```\n\n&nbsp;\n\n") # Make the knit statement
    out = c(out, knit_expanded) # Append so as to create a vector.
  }
  message("\n--- To preview the output, paste and run the following:\nfor (i in 1:length(",assign,"[['dt']])) {print(",assign,"[['dt']][[i]])}\n--- Include knit command *in-text* below for rendering (remember to end the tabset):\n`r paste(knit(text = ",assign,"[['knit_code']]), collapse = '\\n')`")
  ctabs = list(out, output_dt, output_raw)
  names(ctabs) = c("knit_code", "dt", "raw")
  if (!is.null(assign)) {
    assign(assign,ctabs,envir = globalenv())
  }
  if (output == TRUE) {
    return(ctabs)
  }
}

# Mark's MultiCoding Function, for multi-response questions from Qualtrics (comma-separated options).
# --------------------------------------------------------------------------------------------
# Credit: Mark Kane, Conestoga College
# Used for multiple-response questions (from Qualtrics) where each option is a separate column, but shares a common column
# name prefix (eg. 'Q7_'). It will grab all of these columns and, where a value exists, code a 1 or a 0 if a value is present
# or not, coding rows NA if the respondent didn't complete any question of the series. (Might thereby be important that you
# include a "none of the above" option for your survey questions.)
# Arguments:
#   label = Set your own prefix (eg. "abc" = "abc_item") for the output; otherwise, the current prefix will be preserved. Setting to "" removes all prefixes.
# --------------------------------------------------------------------------------------------
MultiCoding <- function(data, Q, label = NULL) {
  label <- ifelse(is.null(label),paste0(Q,"_"),ifelse(label == "", "", paste0(label,"_")))
  data <- select(data, all_of(Q)) %>% 
    rename(Q = 1)
  temp = data %>% drop_na()
  temp$Q <- trimws(as.character(temp$Q))
  resp <- unique(trimws(unlist(strsplit(temp$Q, ",(?!\\s)", perl = TRUE))))
  dummies <- matrix(NA, nrow(data), length(resp)) # Create empty matrix to populate dummies.
  for (i in 1:length(resp)) {
    dummies[,i] <- ifelse(str_detect(data$Q, pattern = coll(resp[i])), 1, 0)
  }
  colnames(dummies) <- paste0(rep(label, length(resp)), resp)
  as.data.frame(dummies) # Return the dataframe
}

# (M)ulti(C)oding (F)requency (T)able, (W)eighted - Default sort is by count.
# --------------------------------------------------------------------------------------------
# Makes a count frequency table from a multiple-response question (from Qualtrics). 
# Function Dependencies: Mark's MultiCoding function.
# Default raw_mode (FALSE) takes the default Qualtrics output of a comma-separated-mulit-response "Q" and calls MultiCode to get an array of dummies that it then summarizes, using the weights supplied.
# Modified raw_mode (TRUE) ignores weight calculations - simply creates a frequency table based on supplied values in the df - they need to be numbers that can be summed. N = number of rows.
#   > Often used for qualitative coding outputs.
# return_N: Sometimes for graphs you need to get an N out of here - as in, how many people answered these multi-select questions in any fashion. For this, use the return_N feature (set to TRUE).
# --------------------------------------------------------------------------------------------
mc_ftw = function(df, Q = NULL, Weight_var_in_Quotes = NULL, raw_mode = FALSE, return_N = FALSE) {
  if (raw_mode == FALSE) {
    mc = MultiCoding(df, Q)
    df = { if (is.null(Weight_var_in_Quotes)) transmute(df, weight = 1) else select(df, all_of(Weight_var_in_Quotes)) } %>%
      rename(weight = 1) %>% 
      bind_cols(., mc) %>% 
      mutate_at(vars(contains(paste0(Q, "_"))), ~ . * weight) %>%
      drop_na() 
    Num_Resp = sum(df$weight)
    df = df %>%
      select(contains(Q)) %>% 
      pivot_longer(cols=everything(),names_to="Category", values_to = "Response") %>%
      group_by(Category) %>%
      summarize(count = sum(Response)) %>%
      mutate(pct = count/Num_Resp, Category = {str_remove(.$Category, paste0(Q, "_"))}) %>%
      rename(resp = Category) %>%
      mutate_at(vars(count), round) %>%
      arrange(desc(count))
  } else {
    Num_Resp = nrow(df)
    df = df %>% 
      pivot_longer(cols=everything(),names_to="Category", values_to = "Response") %>%
      drop_na() %>%
      group_by(Category) %>%
      summarize(count = sum(Response)) %>%
      mutate(pct = count/Num_Resp) %>%
      rename(resp = Category) %>%
      arrange(desc(count))
  }
  if (return_N == TRUE) {
    return(list(df, Num_Resp))
  } else {
    return(df)
  }
}

# (M)ulti(C)oding (C)ross(t)abulation - Because I do this too often to not have a function.
# --------------------------------------------------------------------------------------------
# Basically just handles a specific selection of a demographic and weighting variable (required) and a column from a MultiCoding call,
# running a CrossTabulation [ct()] call for them.
# Function Dependencies: Mark's MultiCoding function, ct().
# Arguments:
#   First few are pretty self-explanatory, excepting...
#   Item_from_Q_in_quotes is the actual column title. You may need to run a {MultiCoding(df, Question_in_quotes} to get this straight.
#   label_1 / label_0 = What you want 1 or 0 to be titled in the output.
# --------------------------------------------------------------------------------------------
mc_ct = function(df, Question_in_quotes, Item_from_Q_in_quotes, Demo_in_quotes, Weight_var_in_Quotes = NULL, label_1 = "Yes", label_0 = "No") {
  if (is.null(Weight_var_in_Quotes)) { 
    temp = select(df, Demo_in_quotes) %>% 
      mutate(weight = 1) %>% 
      .[,2:1]
  } else { 
    temp = select(df, Weight_var_in_Quotes, Demo_in_quotes)
  }
  bind_cols(temp, MultiCoding(df, Question_in_quotes, label = "")[Item_from_Q_in_quotes]) %>%
    ct(2, 3, 1, sort = FALSE) %>%
    rename(!!label_0 := 2, !!label_1 := 3, !!Demo_in_quotes := 1) %>%
    .[,c(1,3,2)]
}

# (Battery) Question (F)requency (T)able, (W)eighted
# --------------------------------------------------------------------------------------------
# Takes your question battery eg. ("Q10" refers to "Q10_1, _2, etc.") and makes it into a frequency table.
# It's important that you then view said table and re-arrange it properly for the rendering in frq_g_battery().
# * For best results, needs access to a "Questions" dataframe in order to get the category text, since that needs to be added for the output to be useful. This is an artifact of how Qualtrics encodes question-battery category text into the column headers.
# Note: I tried to make this have conditional pipes instead of all these IF statements; sum() kept giving me errors for some unknown reason.
# Arguments:
#   Q_prefix_quoted = The prefix of the question battery you're wanting to summarize, eg. "Q10" refers to "Q10_1, _2, etc."
#   Weight_var_in_Quotes = Weighting variable. Required.
#   freq = Set to TRUE to return frequency counts instead of row percents (default, FALSE)
#   round_freq = Round frequencies / counts to integers
#   Questions_df = Specify your "Questions" dataset. If you don't have a "Questions" dataset, set to NULL - this will skip looking up the category text.
#     > Questions is supposed to be a dataframe with the following columns (In Qualtrics output, this is the first two rows of an excel export):
#       + Q = The column name in the reference dataset
#       + Text = The actual question text
# --------------------------------------------------------------------------------------------
battery_ftw = function(df, Q_prefix_quoted, Weight_var_in_Quotes = NULL, freq = FALSE, round_freq = TRUE, Questions_df = Questions) {
  if (is.null(Weight_var_in_Quotes)) { 
    temp = select(df, contains(Q_prefix_quoted)) %>% 
      mutate(weight = 1) %>% 
      select(weight, everything())
  } else { 
    temp = select(df, Weight_var_in_Quotes, contains(Q_prefix_quoted))
  }
  temp = temp %>% 
    rename(weight = 1) %>% 
    pivot_longer(cols = contains(Q_prefix_quoted), names_to = "Q", values_to = "resp") %>% 
    drop_na %>% 
    group_by(Q, resp) %>% 
    summarize(count = sum(weight, na.rm = TRUE))
  if (freq == FALSE) {
    temp = temp %>% 
      mutate(pct = count/sum(count, na.rm = TRUE), count = sum(count, na.rm = TRUE))
  }
  if (round_freq == TRUE) { 
    temp = temp %>% 
      mutate_at(vars(count), round)
  }
  if (freq == FALSE) {
    temp = temp %>% 
      pivot_wider(names_from = resp, values_from = pct) 
  } else {
    temp = temp %>% 
      pivot_wider(names_from = resp, values_from = count)
  } 
  if (is.null(Questions_df)) {
    temp = temp %>% 
      rename(resp = 1)
  } else {
    temp = temp %>% 
      mutate(resp = gsub(".*-\\s", "", Questions_df$Text[which(Questions_df$Q == Q)])) %>% 
      .[,-1] %>% 
      select(resp, everything())
  }
  return(temp)
}

# (Fr)e(q)uency (G)raph, (Simple)
# --------------------------------------------------------------------------------------------
# Makes a simple horizontal bar graph.
# Function Dependencies: wrap.labels, pct_format
# Requires a frequency table with dimensions: "resp" [category], and either "count" or "pct" [n, decimal]
# Arguments:
#   Title_in_Quotes = Title of the graph
#   Title_wrap_length = How long the text of the title should be allowed to run before spilling to a new line. Uses the function dependency.
#   Subtitle_font_size = Self-described
#   Value_font_size = How big should the numbers on the bars be?
#   Cat_wrap_length = How long should the categories on the bars run before they spill to a new line?
#   Cat_font_size = Font size for categories on the bars
#   Custom_N = Specify a custom N to appear in the subtitle
#   subtitle = Specify a custom subtitle - overrides Custom_N
#   scale = Set to "pct" (default) for percentages, "count" or specify a max number for counts. (Don't over-exceed the highest count by much or the last gridline won't display.)
#   decimals = Number of decimals to include for percentage rounding.
#   pos = function for positioning text labels. Eg. position_nudge(x = 0, y = 0.05)
# --------------------------------------------------------------------------------------------
frq_g_simple = function(df, Title_in_Quotes = "", Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Value_font_size = 6, Cat_wrap_length = 25, Cat_font_size = 20, Custom_N = NULL, subtitle = NULL, scale = "pct", decimals = 0, colour = "#6baed6", pos = position_stack(vjust = 0.9)) {
  df = df %>%  
    mutate(resp = wrap.labels(resp, Cat_wrap_length))
  if(!"pct" %in% colnames(df)) {
    df = df %>% mutate(pct = count / sum(count, na.rm = TRUE))
  }
  df %>% 
    ggplot(aes(x=resp, y={if (scale == "pct") pct else count})) +
    geom_bar(stat = 'identity', fill = colour) + 
    xlim(rev(df$resp)) + 
    geom_text(aes(label = {if (scale == "pct") pct_format(pct, decimals) else n_format(count)}), size = Value_font_size, position = pos) +
    coord_flip() +
    {if (scale == "pct") scale_y_continuous(labels = scales::percent_format(accuracy = 1)) else scale_y_continuous(breaks = unname(round(quantile(c(0,ifelse(scale == "count", max(df$count), scale))))), labels = unname(round(quantile(c(0,ifelse(scale == "count", max(df$count), scale))))))} + 
    patchwork::plot_annotation(title = wrap.labels(Title_in_Quotes, Title_wrap_length), 
                               subtitle = ifelse(!is.null(subtitle), subtitle,
                                                 ifelse(is.null(Custom_N), paste0("N = ",formatC(sum(df$count),format="f", big.mark=",", digits=0)),
                                                        ifelse(Custom_N == FALSE, "", paste0("N = ",formatC(Custom_N,format="f", big.mark=",", digits=0))))), 
                               theme = theme(plot.title = element_text(hjust = 0.5, size = Title_font_size, margin = margin(0,0,8,0)), plot.subtitle = element_text(hjust = 0.5, size = Subtitle_font_size, color="#525252", face = "italic", margin = margin(0,0,5,0)))) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(size = Cat_font_size), 
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour ="grey"),
      axis.ticks = element_blank()
    )
}

# (Fr)e(q)uency (G)raph, (Battery)
# --------------------------------------------------------------------------------------------
# Creates a stacked bar chart.
# Function Dependencies: wrap.labels
# Required input: a frequency table with dimensions: "resp" [category], "count" [n], and a column with the percentages [decimals] for each response option, properly labelled with the complete response text.
# Arguments:
#   N_mode = Specify the mode to display N values in the graph. Can be either ("t")itle or ("c")ategory. FALSE disables N's.
#   subtitle = Manually specify subtitle content. Overrides N_mode.
#   Fcolour = R palette being used.
#   colours = Allows you to specify a vector [c()] of colours (labels or hex) to be displayed (left to right) in the graph. Ensure you have as many colours as there are legend categories.
#   Cat_font_size / Cat_wrap_length = Refer to categories (items on the left, not percentages).
#   Label_font_size = Font size of the percentage labels.
#   YRightMargin = Margin between categories and bars.
#   Legend_Rows = Number of rows in the legend.
#   Legend_Padding = Play with to adjust the spacing of the legend on the graph.
#   decimals = Number of decimals to include for percentage rounding.
# --------------------------------------------------------------------------------------------
frq_g_battery = function(df, Title_in_Quotes = NULL, Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Cat_font_size = 12, Cat_wrap_length = 34, YRightMargin = 0, Label_font_size = 5, N_mode = "t", decimals = 0, Legend_Rows = 2, Legend_Padding = 100, subtitle = NULL, Fcolour = "Blues", colours = NULL, border = NULL) { 
  if("try-error" %in% class(try(select(df, count, resp), silent = TRUE))) stop('This function requires a dataframe with columns "resp" (categories) and "count" (n). Supplied dataframe is incomplete.')
  if (N_mode == "t") N = paste0(formatC(min(df$count),format="f", big.mark=",", digits=0)," - ",formatC(max(df$count),format="f", big.mark=",", digits=0)) else N = ""
  if (N_mode == "c") df = df %>% mutate(df, resp = paste0(df$resp, " (N=",count,")"))
  df = df %>% 
    rename("Question" = resp) %>%   
    select(-count)
  df = df %>%
    mutate(Question = wrap.labels(Question, Cat_wrap_length))
  Qlabels = unique(df$Question)
  FillOrder = names(df[,-c(1)])
  Table = df %>% 
    pivot_longer(cols=-Question, names_to="Response", values_to = "pct")
  Table %>% 
    ggplot(aes(x=Question, y=pct, fill=factor(Response, levels = rev(FillOrder)))) + 
    {if (is.null(border)) geom_bar(stat = 'identity') else geom_bar(stat = 'identity', color = border) } +
    geom_text(aes(label = ifelse(pct>=0.045,sprintf(paste0("%.", decimals, "f%%"), (pct*100)),"")), size = Label_font_size, position = position_stack(vjust = 0.5)) +
    coord_flip() +
    scale_x_discrete(limits=rev(Qlabels)) +
    {if (is.null(colours)) scale_fill_brewer(palette = Fcolour) else scale_fill_manual(values = rev(colours)) } +
    scale_y_continuous(breaks = seq(0,1,0.1), labels = pct_format(seq(0,1,0.1))) +
    guides(fill=guide_legend(nrow=Legend_Rows,byrow=TRUE,reverse=TRUE)) +
    patchwork::plot_annotation(title = wrap.labels(Title_in_Quotes, Title_wrap_length), subtitle=ifelse(!is.null(subtitle), subtitle, ifelse(N_mode == "t", paste0("N = ",N), "")), theme = theme(plot.title = element_text(hjust = 0.5, size = Title_font_size, margin = margin(0,0,8,0)), plot.subtitle = element_text(hjust = 0.5, size = Subtitle_font_size, color="#525252", face = "italic", margin = margin(0,0,ifelse(any(!is.null(subtitle) | N_mode == "t"),5,-10),0)))) +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.margin = margin(5, 0, 5, (Legend_Padding * -1)),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=Cat_font_size, margin=margin(0,YRightMargin,0,0)),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(colour = "grey"),
          axis.ticks = element_blank())
}
