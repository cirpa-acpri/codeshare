# Survey Data Functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For working with data and surveys from Qualtrics

# By: Fraser Hay, Conestoga College
# With some things stolen from: Mark Kane, Conestoga College, and the Internet in general.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# In mid-to-late 2022 I've tried to retrofit some functions to accept both "quoted" and `unquoted` 
# variable names. Haven't completed this, nor do I think my approach is a good one, but it's working for
# now.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gotta have some libraries...
library(tidyverse)
library(clipr)
library(DT)
library(patchwork)

# Suppress that "groups" warning from the summarize() function.
options(dplyr.summarise.inform=F)

# Percent Formatting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pct_format = function(value, decimals = 0) {
  ifelse(!is.na(value), sprintf(paste0("%.", decimals, "f%%"), (value * 100)), NA)
}

# Comma-Number Formatting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n_format = function(n, decimals = 0) {
  ifelse(!is.na(n), formatC(n,format="f", big.mark=",", digits=decimals), n)
}

# Value -- Remove commas / percents, return value. Basically the reverse of pct_format / n_format.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
value = function(x) {
  x = as.character(x)
  ifelse(grepl("%", x, fixed = TRUE), parse_number(x) / 100, parse_number(x))
}

# Label Wrapping (Credit: https://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Super useful for ensuring strings wrap at particular character counts.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrap.labels <- function(x, len) {
  if (is.list(x))
  {
    lapply(x, sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE), len)
  } else {
    sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
  }
}

# Copy a dataframe to paste in Excel (Credit: https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Because copying out of an R dataframe in to Excel is, for reasons beyond my comprehension, not built-in.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cb <- function(df, headers = TRUE) {
  clipr::write_clip(df, col.names = headers)
}

# Paste a table you just copied in Excel into R (Credit: https://www.youtube.com/watch?v=4Y_UhaZj-5I)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As above, this should be built-in... Example usage: 1) Copy data with headers in Excel. 2) data = cb.read()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cb.read = function(sep="\t", header=TRUE) {
  read.table("clipboard", sep=sep, header=header, colClasses = "character", check.names = FALSE)
}

# Set values - so I can adjust labels or values while still in a pipe.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Useful programmatic example: set(which(data$student_fee == "Regular"), 2, "Domestic")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set = function(df, r, c, value) {
  df[r, c] = value
  ungroup(df) # This is basically because occasionally the output comes back grouped? No idea why.
  return(df)
}


# (F)requency (T)able, (W)eighted
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates a basic frequency table (with percentages) based on a categorical question. Sorts by count.
# Arguments:
#   n_round = TRUE; Whether you want counts rounded to whole numbers.
#   sort = TRUE; Whether you want the output sorted by values (counts) vs. alphabetical (response).
#   drop_na = TRUE; Whether you want NA categories to be returned.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ftw = function(df, question, weight = weight, n_round = TRUE, sort = TRUE, drop_na = TRUE) {
  question = if("try-error" %in% class(try(class(question), silent = TRUE))) substitute(question) else if("function" %in% class(question)) as.character(substitute(question)) else question # This is just my standard approach of accepting quoted ("x") and unquoted (`x` - deparse/substitute) arguments to specify the target column.
  if(missing(weight)) { df = df %>% mutate(weight = 1) } else if("try-error" %in% class(try(class(weight), silent = TRUE))) { weight = deparse(substitute(weight)) } else if("function" %in% class(weight)) { weight = as.character(substitute(weight)) } else { weight = weight } # Accept both `x` (deparse/substitute) and "x" - and if not specified, set weights to 1.
  count(x = df, !!ensym(question), wt = !!ensym(weight), sort = sort) %>% # Tabulate
    { if (drop_na) drop_na(.) else . } %>% # (Optionally) Drop NA's
    mutate(pct = n/sum(.$n)) %>% # Generate percentages
    { if (n_round) mutate_at(., vars(n), round) else . } %>% # (Optionally) Round to integers
    rename(count = n, resp = {{question}}) # Until I change over my graphing functions...
}

# (C)ross (T)abulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates a simple crosstab count table from two variables. Updated in Dec. 2021 to account for Qualtrics multi-coding (comma-no-space-separated) stuff. As a result:
# Function Dependencies: Mark's MultiCoding function.
# Random note: could possibly re-write this with dplyr::tally() https://www.statology.org/dplyr-crosstab/ (though pivot_wider > spread)
# Arguments:
#   weight = Specific weighting variable, optional. (Note: 'Weight_var_in_Quotes' is also accepted (to not break old stuff).)
#   pct = FALSE; Set to TRUE to change output to row percentages
#   count = FALSE; Set to TRUE to return a total N for each row - usually only used in conjunction with pct to get both worlds.
#   decimals = NULL; Specify an integer to round to that many places
#   include_N = TRUE; Adds "(N=)" indicator text to the first (category) column, with row totals.
#   N_adjust = FALSE; Only for when you're crosstabbing against a multiple-response variable, toggle TRUE to return only the number of cases (by row) in the N= counts.
#   sort = TRUE; Returns the columns ordered highest (n) to lowest. Alternately specify a label vector for the column ordering. FALSE returns as-is.
#   order = NULL; Specify a vector of row labels to return the categories in that order.
#   row_recodes = NULL; Specify a named vector (eg. c("New" = "Original")) to recode row categories.
#   delim = delimiter for detecting multiple-response question and for MultiCoding() calls. Note: perl = TRUE.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ct = function(df, rows, cols, weight = NULL, pct = FALSE, decimals = NULL, count = FALSE, include_N = TRUE, N_adjust = FALSE, sort = TRUE, order = NULL, row_recodes = NULL, Weight_var_in_Quotes = NULL, delim = ",(?!\\s)") {
  rows = if("try-error" %in% class(try(class(rows), silent = TRUE))) deparse(substitute(rows)) else if("function" %in% class(rows)) as.character(substitute(rows)) else rows # Accept both `var` (deparse/substitute) and "var".
  cols = if("try-error" %in% class(try(class(cols), silent = TRUE))) deparse(substitute(cols)) else if("function" %in% class(cols)) as.character(substitute(cols)) else cols # Accept both `var` (deparse/substitute) and "var".
  if(!missing(weight)) { if("try-error" %in% class(try(class(weight), silent = TRUE))) { weight = deparse(substitute(weight)) } else if("function" %in% class(weight)) { weight = as.character(substitute(weight)) } else { weight = weight } } # Accept both `var` (deparse/substitute) and "var" - and if not specified, revert to NULL.
  x = MultiCoding(df, cols, label = "x", delim = delim)
  y = MultiCoding(df, rows, label = "y", delim = delim)
  data = {if (is.null(weight)) bind_cols(x, y, weight = 1) else bind_cols(x, y, select(df, all_of(weight)) %>% rename(weight = weight))} %>% 
    pivot_longer(cols=-c(weight, names(y)), names_to="Column", values_to = "selected") %>% 
    filter(selected != 0) %>% 
    select(-selected) %>% 
    pivot_longer(cols=-c(weight, Column), names_to="Row", values_to = "selected") %>% 
    filter(selected != 0) %>% 
    select(-selected) %>% 
    mutate(across(Column:Row, ~ gsub("x_|y_", "", .)))
  CrossTab = data %>% 
    group_by(Column, Row) %>% 
    drop_na %>%
    summarize(count = sum(weight)) %>% 
    ungroup() %>%
    pivot_wider(names_from = Column, values_from = count) %>%
    { if (!is.null(row_recodes)) mutate(Row = recode(Row, !!!row_recodes)) else . } %>% 
    { if (!is.null(order)) arrange(., factor(Row, levels = order)) else . }
  if (include_N == TRUE) {
    multi_flag = ifelse(max(unlist(lapply(strsplit(as.character(pull(df, cols)), delim, perl = TRUE), length))) > 1, TRUE, FALSE)
    if (multi_flag == TRUE & N_adjust == FALSE) message("** NOTE ** -  Multiple-response independent variable detected. In some cases you may want to use the 'N_adjust = TRUE' to make the row N= counts be the number of actual observations in each variable category. Default N= behaviour is row sums across all response options (respondent duplicate counting possible).")
    CrossTab = CrossTab %>%
      { if (multi_flag == TRUE & N_adjust == TRUE) bind_cols(., tibble(N = round(select(df, all_of(c(rows, cols))) %>% drop_na() %>% pull(rows) %>% table() %>% unname()))) else mutate(., N = round(rowSums(select(., 2:ncol(CrossTab)), na.rm = TRUE))) } %>% 
      mutate(Row = paste0(Row," (N = ",n_format(N),")")) %>% 
      select(-N)
  }
  Overall = data %>%  
    group_by(Column) %>% 
    drop_na %>%
    summarize(count = sum(weight)) %>%
    ungroup() %>%
    pivot_wider(names_from = Column, values_from = count) %>% 
    mutate(Row = "Overall") %>% 
    select(Row, everything())
  Output = bind_rows(Overall,CrossTab)
  names(Output)[1] = rows
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
    a = x[,1] # Pop-off the 'top'
    b = x[,2:ncol(x)] # Grab the rest
    b = b[,rev(sort(as.numeric(as.vector(b[1,])), index.return = TRUE)$ix)] # Sort the columns for the rest based on the overall. NOTES: obviously this line is more complicated than that from the previous iteration (b = b[,rev(order(b[1,]))]), however since early 2021 I was getting an annoying warning message - "In xtfrm.data.frame(x) : cannot xtfrm data frames". Apparently the original approach is simply not recommended anymore. https://win-vector.com/2021/02/07/it-has-always-been-wrong-to-call-order-on-a-data-frame/
    Output = bind_cols(a, b) # Recombine
    return(Output)
  } else if (length(sort) > 1) { # If a vector is provided, sort the output by the vector.
    return(select(Output, rows, any_of(sort)))
  }
  return(Output)
}

# Row Percents
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transforms a frequency table input to row percentages. Remember ct() above can also do this.
# Arguments:
#   Col1Cats = Whether the first column is the categories. Set to FALSE if not. 
#   count = Whether you want a count column maintained - either as column 1, or column 2 if Col1Cats = TRUE.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transforms a frequency table input to column percentages.
# Arguments:
#   Col1Cats = Whether the first column is the categories. Set to FALSE if not. 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col_percents = function(df, Col1Cats = TRUE) {
  if (Col1Cats == TRUE) {
    mutate(df, across(.cols = -1,  ~ ./sum(., na.rm = TRUE)))
  } else {
    mutate(df, across(.cols = everything(), ~ ./sum(., na.rm = TRUE)))
  }
}


# (C)rosstabulation (Table) Rendering
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#   scrollX = If your table is very wide, use this to add a scroll-bar so it doesn't spill. Also activates DT's fixedColumns option (first column stays put when scrolling), and disables data bars for the first column (which show N-sizes) (this is because the data bars work by filling the bar with the colour and making the end of the bar transparent) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctable = function(df, Col1Name = NULL, Col1Width = "20%", OtherColWidths = "10%", chi2 = TRUE, freq = FALSE, decimals = NULL, title = "", font = "15", scrollX = FALSE) {
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
        # First we must check as to whether there's only one column, which would cause an error.
        if("try-error" %in% class(try(wtest$Prop <- prop.table(wtest$observed, 1), silent = TRUE))) {
          message("Message: Dataframe supplied only contains one column of data. Without co-variates, chi2 results cannot be returned. Disabling chi-square test results colouring.")
          chi2 = FALSE # Described above.
        } else { # Cleared for take-off...
          wtest$Prop <- prop.table(wtest$observed, 1)  
        }
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
  CrossTab = datatable(df, class = 'row-border', rownames = FALSE, extensions = 'FixedColumns', width = '100%',
                       caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',title),
                       options = list(
                         initComplete = htmlwidgets::JS("function(settings, json) {", paste0("$(this.api().table().container()).css({'font-size': '", paste0(font, "px"), "'});"), "}"), # Source: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
                         scrollX = scrollX, fixedColumns = scrollX, paging = FALSE, searching = FALSE, dom = 't', ordering = F, autoWidth = TRUE,
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
    { if (scrollX == FALSE) formatStyle(., 1, (N+2), # Data bars - 1st column N's
                                        background = styleColorBar(as.integer(0:max_cat), "#dfe6f585", angle = -90),
                                        backgroundSize = '98% 80%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'left') else formatStyle(., 1) } %>%
    formatStyle(1, target = "cell", background = styleEqual("Overall", "#337ab7")) %>% # This manual colouring of the "Overall" cell needed due to 'FixedColumns' extension. For some reason it has to go below the data bars...
    formatPercentage(ifelse(freq==FALSE,2,0):ifelse(freq==FALSE,(N+1),0), ifelse(!is.null(decimals),decimals-2, 0)) %>% # Percentages
    formatRound(ifelse(freq==TRUE,2,0):ifelse(freq==TRUE,(N+1),0), ifelse(!is.null(decimals),decimals, 0)) %>% # Frequency decimals
    formatStyle(1:(N+1), border = '1px solid #ddd') %>% 
    # Chi2 formatting
    formatStyle(2:(N+1), 
                valueColumns = (N+3):(2*N+2), 
                color = styleEqual(c('-1', '0', '1', '9', '10'), 
                                   c('#C00000', 'black', '#76933C', '#858585', 'white')))
  return(CrossTab)
}


# (C)ross(t)abulation (Tabset) for (D)ata(t)ables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generates the knit code (and related materials) to create a dynamically-defined series of tabs with ctable() renderings.
# It does this by committing two supposed cardinal sins of programming: a) interpreting a string (that you pass) as code (for generating the 
# data behind the ctable() calls); and b) assigning a static global variable - but at least you can specify the name!
# It returns 3 lists - the knit code to be run, the DTs (list), and the raw table input (with the row_percents() function run on it) (list).
# Requires the ctable() and row_percents() functions. This code was inspired by: https://gist.github.com/StevenMMortimer/e54ec050d97d79996189.
# Arguments:
#   ct_code_in_quotes = The literal code that should be used to generate the underlying data tables to be fed into the ctable() calls. 
#                       NOTE: 'x' should be included as the demographic variable.
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    knit_expanded <- paste0("\n\n", paste0(rep("#", HeadingLvl), collapse = "")," ", Tabs[i], " {-}\n\n```{r results='asis'}\n\n",assign,"[['dt']][[", i, "]]\n\n```\n\n&nbsp;\n\n") # Make the knit statement
    out = c(out, knit_expanded) # Append so as to create a vector.
  }
  message("\n--- To preview the output, paste and run the following:\n",assign,"[['dt']]\n--- Include knit command *in-text* below for rendering (remember to end the tabset):\n`r paste(knit(text = ",assign,"[['knit_code']]), collapse = '\\n')`")
  ctabs = list(out, output_dt, output_raw)
  names(ctabs) = c("knit_code", "dt", "raw")
  if (!is.null(assign)) {
    assign(assign,ctabs,envir = globalenv())
  }
  if (output == TRUE) {
    return(ctabs)
  }
}


# (C)ross(t)abulation (Tabset) for (D)ata(t)ables - (Battery) Edition
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLEASE FIX: Cases where there's a battery with a _TEXT entry, and so Questions (df) keeps looking for it even if it's not in data.
# This is basically a fancy way of looping ct_tabset_dt() calls. This approach is used when I want to make the crosstabs for a battery-type question
# without needing to iterate over each question. The result of the call is a series of output elements from the various ct_tabset_dt's and a single
# output vector of knit code, to be used for showing it all in your .rmd. I typically make the holding section a .tabset-pills.
# Assumes you have a 'Questions' dataframe, which I often have (for looking up Question titles)...
# [Musing] ... This could possibly be retrofitted to accommodate looping over multiple questions (such as when working with multiple-select batteries);
# I'd need to tinker with the labels part (at the top), which is where the 'Questions' df comes in by default. ... Or allow you to define the Q's...
# Arguments:
#   dataset_in_quotes = Your dataframe. Someday I'll get smart enough to dispense with the quotes... (Piped into ct() calls)
#   Q = The question (battery-prefix) you want to iterate over. (eg. Q2 would look for contains("Q2"), so it would grab Q2_1, Q2_2, etc.) (Piped into ct() calls)
#   Demos = A vector of all the demographic crosstabulation variables you want to be looped through. (Piped into ct() calls)
#   Tabs = A vector of all the titles of the tabs you want generated for each respective demo-ctable() output. (Piped into ct() calls)
#   Params = A vector of any ctable() parameters you want passed for each respective demo-ctable() output. (Piped into ct() calls)
#   row_recodes = NULL; A named vector (eg. c("recoded value" = "original value")) for the recode-pair transformations you want to conduct for rows. Because we're doing demo tabsets, you'll need to do this as "one big vector" accommodating all the recodes you want to do for all demo variables. This would be if you have response categories that are ridiculously long and should be short-formed.
#   sort = NULL; Specify a label vector for column ordering. FALSE returns as-is. (Piped into ct() calls) 
#   order = NULL; Specify a vector of row labels to return the categories in that order. (Piped into ct() calls) Because we're doing demo tabsets, you'll need to do this as "one big vector" accommodating all the ordering you want to do for all demo variables.
#   output_var = String to name the variable assignment for the output.
#   HeadingLvl = How many #'s to include as part of the rendered tabsets.
#   delim = Delimiter for ct()'s MultiCoding() calls. Note: perl = TRUE.
#   tab_recodes = Same as row_recodes above, but for the names of the categories / tabs people will click on. Specify a named vector to recode.
#   Questions_df = Specify your questions dataframe (Q = Column name, Text = Question text)
#
# Remember to include a knit command below your chunk to render the output after running this!
# Example: `r paste(knit(text = out), collapse = '\n')`
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ct_tabset_dt_battery = function(dataset_in_quotes = "dataset", Q, weight = "weight", Demos, Tabs, Params, row_recodes = NULL, sort = NULL, order = NULL, output_var = "out", HeadingLvl = 4, tab_recodes = NULL, Questions_df = "Questions", delim = NULL) {
  labels = get(Questions_df) %>%
    filter(startsWith(Q, !!Q)) %>% 
    mutate(labels = gsub(".*-\\s", "", .$Text[which(.$Q == Q)])) %>%
    select(`Q`, labels)
  if(is.null(tab_recodes)) Categories = unique(labels$labels) else Categories = recode(unique(labels$labels), !!!tab_recodes)
  out = NULL
  for (c in Categories) {
    Q = labels[[which(c == Categories),1]]
    ct_tabset_dt(ct_code = paste0('ct(', dataset_in_quotes,', x, "', Q, '", weight = "', weight ,'"', if(!is.null(row_recodes)) paste0(', row_recodes = ', row_recodes), 
                                  if(!is.null(order)) paste0(", order = c(", paste0('"', order, '"', collapse = ", "), "),"),
                                  if(!is.null(delim)) paste0(", delim = c(", paste0('"', delim, '"', collapse = ", "), "),"),
                                  ifelse(!is.null(sort), paste0(", sort = c(", paste0('"', sort, '"', collapse = ", "), "))"), ")")), Demos = Demos, Tabs = Tabs, Params = Params, HeadingLvl = (HeadingLvl + 1), output = TRUE, assign = paste0("t", which(c == Categories)))
    a = paste0("t",which(c == Categories))
    out = c(out, paste0(paste0(rep("#", HeadingLvl), collapse = "")," ",c," {.tabset .tabset-fade -}\n\n"),
            get(a)[['knit_code']],
            "\n\n")
  }
  assign(output_var, out, envir = globalenv())
}


# Mark's (MultiCoding) Function, for multi-response questions from Qualtrics (comma-separated options).
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Original credit: Mark Kane, Conestoga College
# This function is used for multiple-response questions (from Qualtrics), but is applicable anytime you want to generate dummies.
# It takes a question / variable and returns a matrix of 1/0's (T/F) depending on what values exist in that column for each case.
# By default, it will be looking for "comma no space" as a delimiter in case there are multiple values on the same row - which 
# is the default for Qualtrics whenever it provides multiple-response data. (Eg. "A,B") Any case that didn't answer the question
# is coded as NA's across the matrix (removing them from the denominator in most typical analysis that follows - it might be
# advisable to have a "None of the above" option in your surveys). 
# Arguments:
#   label = Set your own prefix (eg. "abc" = "abc_item") for the output; otherwise, the current prefix will be preserved. Setting to "" removes all prefixes.
#   delim = ",(?!\\s)"; Specify a PERL expression for a delimiter to detect multiple-response data options. Default is "comma-no-space".
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MultiCoding = function(df, Q, label = NULL, delim = ",(?!\\s)") {
  Q = if("try-error" %in% class(try(class(Q), silent = TRUE))) deparse(substitute(Q)) else if("function" %in% class(Q)) as.character(substitute(Q)) else Q # This is just my standard approach of accepting quoted ("x") and unquoted (`x`) arguments to specify the target column.
  label = ifelse(is.null(label), paste0(Q, "_"), ifelse(label == "", "", paste0(label, "_")))
  resp = pull(df, {{Q}}) %>% .[!is.na(.)] %>% trimws(.) %>% strsplit(., delim, perl = TRUE) %>% unlist() %>% unique() # Identify the unique selection options using the delimiter
  dummies = matrix(NA, nrow(df), length(resp)) # Create empty matrix to populate dummies.
  if (max(unlist(lapply(strsplit(as.character(pull(df, {{Q}})), delim, perl = TRUE), length))) > 1) {  # This dense command checks whether the df itself contains delimited (default comma-no-space-separated) items that are being multi-selected. If that's the case, we need to approach things differently in the for loop below.)
    message(paste0("MultiCoding: Creating dummy variables for response options - Multiple-response question (", Q, ")"))
    for (i in 1:length(resp)) { dummies[,i] <- ifelse(str_detect(pull(df, {{Q}}), pattern = coll(resp[i])), 1, 0) }
  } else {
    message(paste0("MultiCoding: Creating dummy variables for response options - Single-response question (", Q, ")"))
    for (i in 1:length(resp)) { dummies[,i] <- ifelse(trimws(pull(df, {{Q}})) == resp[i], 1, 0) }
  }
  colnames(dummies) <- paste0(rep(label, length(resp)), resp) # Column names
  as.data.frame(dummies) # Return the dataframe
}



# (M)ulti(C)oding (F)requency (T)able, (W)eighted - Default sort is by count.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Makes a count frequency table from a multiple-response question (from Qualtrics). 
# Function Dependencies: Mark's MultiCoding function.
# Default raw_mode (FALSE) takes the default Qualtrics output of a comma-separated-mulit-response "Q" and calls MultiCode to get an array of dummies that it then summarizes, using the weights supplied.
# Modified raw_mode (TRUE) ignores weight calculations - simply creates a frequency table based on supplied values in the df - they need to be numbers that can be summed. N = number of rows.
#   > Often used for qualitative coding outputs.
# return_N: Sometimes for graphs you need to get an N out of here - as in, how many people answered these multi-select questions in any fashion. For this, use the return_N feature (set to TRUE).
# delim = ",(?!\\s)"; Specify a PERL expression for a delimiter to be passed to the MultiCoding() call. Default is "comma-no-space".
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mc_ftw = function(df, Q = NULL, Weight_var_in_Quotes = NULL, raw_mode = FALSE, return_N = FALSE, delim = ",(?!\\s)") {
  if (raw_mode == FALSE) {
    mc = MultiCoding(df, Q, delim = delim)
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basically just handles a specific selection of a demographic and weighting variable (required) and a column from a MultiCoding call,
# running a CrossTabulation [ct()] call for them.
# Function Dependencies: Mark's MultiCoding function, ct().
# Arguments:
#   First few are pretty self-explanatory, excepting...
#   Item_from_Q_in_quotes is the actual column title. You may need to run a {MultiCoding(df, Question_in_quotes} to get this straight.
#   label_1 / label_0 = What you want 1 or 0 to be titled in the output.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mc_ct = function(df, Question_in_quotes, Item_from_Q_in_quotes, Demo_in_quotes, Weight_var_in_Quotes = NULL, label_1 = "Yes", label_0 = "No") {
  if (is.null(Weight_var_in_Quotes)) { 
    temp = select(df, Demo_in_quotes) %>% 
      mutate(weight = 1) %>% 
      .[,2:1]
  } else { 
    temp = select(df, Weight_var_in_Quotes, Demo_in_quotes)
  }
  bind_cols(temp, MultiCoding(df, Question_in_quotes, label = "")[Item_from_Q_in_quotes]) %>%
    ct(names(.)[2], names(.)[3], names(.)[1], sort = FALSE) %>%
    rename(!!label_0 := 2, !!label_1 := 3, !!Demo_in_quotes := 1) %>%
    .[,c(1,3,2)]
}


# (Battery) Question (F)requency (T)able, (W)eighted
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Takes your question battery eg. ("Q10" refers to "Q10_1, _2, etc.") and makes it into a frequency table.
# It's important that you then view said table and re-arrange it properly for the rendering in frq_g_battery().
# * For best results, needs access to a "Questions" dataframe in order to get the category text, since that needs to be added for the output to be useful. This is an artifact of how Qualtrics encodes question-battery category text into the column headers.
# Note: I tried to make this have conditional pipes instead of all these IF statements; sum() kept giving me errors for some unknown reason.
# Arguments:
#   Q_prefix = The prefix of the question battery you're wanting to summarize, eg. "Q10" refers to "Q10_1, _2, etc."
#   weight = Weighting variable. Required.
#   freq = FALSE; set to TRUE to return frequency counts instead of row percents
#   round_freq = TRUE; round frequencies / counts to integers
#   Questions_df = Questions; Specify your "Questions" dataset. If you don't have a "Questions" dataset, set to NULL - this will skip looking up the category text.
#     > Questions is supposed to be a dataframe with the following columns (In Qualtrics output, this is the first two rows of an excel export):
#       + Q = The column name in the reference dataset
#       + Text = The actual question text (for labelling)
#   contains = TRUE, Whether to try to grab the entire battery (using contains()) or only use the "Q_prefix" as one question.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
battery_ftw = function(df, Q_prefix, weight = NULL, freq = FALSE, round_freq = TRUE, Questions_df = Questions, contains = TRUE) {
  Q_prefix = if("try-error" %in% class(try(class(Q_prefix), silent = TRUE))) deparse(substitute(Q_prefix)) else Q_prefix # Accept both `var` (deparse/substitute) and "var".
  if(missing(weight)) { weight = NULL } else { if("try-error" %in% class(try(class(weight), silent = TRUE))) { weight = deparse(substitute(weight)) } else { weight = sym(weight) } } # Accept both `var` (deparse/substitute) and "var" - and if not specified, revert to NULL.
  if (is.null(weight)) { 
    temp = { if (contains) select(df, contains(Q_prefix)) else select(df, all_of(Q_prefix)) } %>% 
      mutate(weight = 1) %>% 
      select(weight, everything())
  } else { 
    temp = { if (contains) select(df, all_of(weight), contains(Q_prefix)) else select(df, all_of(weight, Q_prefix)) }
  }
  temp = temp %>% 
    rename(weight = 1) %>% 
    { if (contains) pivot_longer(., cols = contains(Q_prefix), names_to = "Q", values_to = "resp") else pivot_longer(., cols = Q_prefix, names_to = "Q", values_to = "resp") } %>% 
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
    temp = tryCatch(temp %>% 
                      mutate(resp = gsub(".*-\\s", "", Questions_df$Text[which(Questions_df$Q == Q)])) %>% 
                      .[,-1] %>% 
                      select(resp, everything()),
                    error = function(e){ message("Error: Problem detected with Questions_df parameter / lookup - check that questions (Q/Text) exist in supplied or default dataframe, or set to NULL.") } )
  }
  return(ungroup(temp))
}


# (Fr)e(q)uency (G)raph, (Simple)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Makes a simple horizontal bar graph.
# Function Dependencies: wrap.labels, pct_format
# Requires a frequency table with dimensions: "resp" [category], and either "count" or "pct" [n, decimal]
# Arguments:
#   Title_in_Quotes = Title of the graph
#   Title_wrap_length = How long the text of the title should be allowed to run before spilling to a new line. Uses the wrap.labels() function.
#   Subtitle_font_size = Self-described
#   Value_font_size = How big should the numbers on the bars be?
#   Cat_wrap_length = How long should the categories on the bars run before they spill to a new line?
#   Cat_font_size = Font size for categories on the bars
#   Custom_N = Specify a custom N to appear in the subtitle
#   subtitle = Specify a custom subtitle - overrides Custom_N
#   scale = Set to "pct" (default) for percentages, "count" or specify a max number for counts. (The graph will only display 4 gridlines + 0. Your scale typically needs to be cleanly divisable by 4 for gridlines to not look awkward. Don't over-exceed the highest count by much or the last gridline won't display.)
#   decimals = Number of decimals to include for percentage rounding.
#   pos = Function for positioning text labels. Eg. position_nudge(x = 0, y = 0.05)
#   border = TRUE; the colour you want the border to be, if applicable. Default is black. Disable with NULL.
#   width = How thick do you want the bars? (In %, basically.)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
frq_g_simple = function(df, Title_in_Quotes = "", Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Value_font_size = 6, Cat_wrap_length = 25, Cat_font_size = 20, Custom_N = NULL, subtitle = NULL, scale = "pct", decimals = 0, colour = "#6baed6", pos = position_stack(vjust = 0.9), border = TRUE, width = 0.8) {
  df = df %>% 
    rename(resp = 1) %>% 
    mutate(resp = wrap.labels(resp, Cat_wrap_length))
  if(!"pct" %in% colnames(df)) {
    df = df %>% mutate(pct = count / sum(count, na.rm = TRUE))
  }
  df %>% 
    ggplot(aes(x=resp, y={if (scale == "pct") pct else count})) +
    geom_bar(stat = 'identity', fill = colour, color = ifelse(is.null(border), "#ffffff00", border), width = width) + 
    xlim(rev(df$resp)) + 
    geom_text(aes(label = {if (scale == "pct") pct_format(pct, decimals) else n_format(count, decimals)}), size = Value_font_size, position = pos) +
    coord_flip() +
    {if (scale == "pct") scale_y_continuous(labels = scales::percent_format(accuracy = 1)) else scale_y_continuous(breaks = unname(round(quantile(c(0,ifelse(scale == "count", max(df$count, na.rm = TRUE), scale))))), labels = unname(round(quantile(c(0,ifelse(scale == "count", max(df$count, na.rm = TRUE), scale))))))} + 
    patchwork::plot_annotation(title = tryCatch(wrap.labels(Title_in_Quotes, Title_wrap_length), error = function(e){stop("Error: Title is not a string. Are you accidentally piping in a dataframe here?")}), 
                               subtitle = ifelse(!is.null(subtitle), subtitle,
                                                 ifelse(is.null(Custom_N), paste0("N = ",formatC(sum(df$count, na.rm = TRUE),format="f", big.mark=",", digits=0)),
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates a stacked bar chart.
# Function Dependencies: wrap.labels
# Required input: a frequency table with dimensions: "resp" [category], "count" [n], and a column with the percentages [decimals] for each response option, properly labelled with the complete response text.
# Arguments:
#   Title_in_Quotes = Title of the graph
#   Title_wrap_length = How long the text of the title should be allowed to run before spilling to a new line. Uses the wrap.labels() function.
#   N_mode = Specify the mode to display N values in the graph. Can be either ("t")itle or ("c")ategory. FALSE disables N's.
#   subtitle = Manually specify subtitle content. Overrides N_mode.
#   Fcolour = R palette being used - see https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
#   colours = Allows you to specify a vector [c()] of colours (labels or hex) to be displayed (left to right) in the graph. Ensure you have as many colours as there are legend categories.
#   Cat_font_size / Cat_wrap_length = Refer to categories (items on the left, not percentages).
#   Label_font_size = Font size of the percentage labels.
#   YRightMargin = Margin between categories and bars.
#   Legend_Font = Legend font size
#   Legend_Rows = Number of rows in the legend.
#   Legend_Padding = Play with to adjust the spacing of the legend on the graph.
#   Legend_Preserve = TRUE; If your graph has empty (NA) columns, by setting NA's to 0's, this preserves the categories in the legend from being dropped if they have no data. Disable by setting to NULL.
#   decimals = Number of decimals to include for percentage rounding.
#   border = TRUE; Set a single border colour for all categories / bars in the stack. Default is black. Disable with NULL.
#   divergent = NULL; This reorganizes the stack to a "divergent" set-up, with bars opposing each other from a central axis line. You'll need to supply a list of two vectors: 'left' and 'right', containing the quoted names of the columns you want on each side of the line, in order. Only those columns (bars) will be returned. Eg: divergent = list(left = c("Poor", "Fair"), right = c("Good", "Excellent")) -- Credit: Mark Kane, who came up the proof-of-concept here.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
frq_g_battery = function(df, Title_in_Quotes = NULL, Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Cat_font_size = 12, Cat_wrap_length = 34, YRightMargin = 0, Label_font_size = 5, N_mode = "t", decimals = 0, Legend_Font = 11, Legend_Rows = 2, Legend_Padding = 100, Legend_Preserve = TRUE, subtitle = NULL, Fcolour = "Blues", colours = NULL, border = TRUE, divergent = NULL) { 
  if("try-error" %in% class(try(select(df, count, resp), silent = TRUE))) stop('This function requires a dataframe with columns "resp" (categories) and "count" (n). Supplied dataframe is incomplete.') # Error-checking the dataframe
  # N-mode labeling
  if (N_mode == "t") { 
    if (min(df$count) == max(df$count)) {
      N = n_format(max(df$count))
    } else {
      N = paste0(n_format(min(df$count))," - ", n_format(max(df$count)))
    }
  }
  if (N_mode == "c") df = df %>% mutate(df, resp = paste0(df$resp, " (N=",n_format(count),")"))
  # Setting up the data
  df = df %>% 
    rename("Question" = resp) %>%   
    mutate(Question = wrap.labels(Question, Cat_wrap_length)) %>% 
    select(-count) %>%
    { if (!is.null(divergent)) select(., c("Question", rev(divergent[['left']]), divergent[['right']])) else .[] } %>% # For divergent graphs: select columns in order
    { if (!is.null(divergent)) mutate(., across(divergent[['left']], ~ . * -1)) else .[] }  # For divergent graphs: Make the left-side values negative
  if (is.null(colours)) colours = RColorBrewer::brewer.pal(n = (ncol(df) - 1), name = Fcolour) # Colours
  Qlabels = unique(df$Question) # Question (bar) labels
  FillOrder = names(df[,-c(1)]) # The order of the bars
  # Table = final data for ggplot - needs to be long format
  Table = df %>%  
    pivot_longer(cols=-Question, names_to="Response", values_to = "pct")
  { if (Legend_Preserve) Table[is.na(Table)] = 0 } # Legend preserve option
  # ggplot call
  Table %>% 
    ggplot(aes(x=Question, y=pct, fill=factor(Response, levels = rev(FillOrder)))) + 
    { if (is.null(border)) geom_bar(stat = 'identity') else geom_bar(stat = 'identity', color = border) } + # Borders
    { if (!is.null(divergent)) geom_hline(yintercept=0, colour="black", size=1.25) } + # For divergent graphs: X-axis line
    { if (!is.null(divergent)) geom_text(aes(label = ifelse(abs(pct)>=0.045,sprintf(paste0("%.", decimals, "f%%"), (abs(pct)*100)),"")), size = Label_font_size, position = position_stack(vjust = 0.5)) else geom_text(aes(label = ifelse(pct>=0.045, sprintf(paste0("%.", decimals, "f%%"), (pct * 100)), "")), size = Label_font_size, position = position_stack(vjust = 0.5)) } + # For divergent graphs: we have to use the absolute value of the pcts for graphing
    coord_flip() + # Horizontal graph
    scale_x_discrete(limits=rev(Qlabels)) +
    { if (!is.null(divergent)) scale_fill_manual(breaks = rev(c(rev(FillOrder[1:length(divergent[['left']])]), FillOrder[(length(divergent[['left']])+1):sum(lengths(divergent))])), values = rev(colours)) else scale_fill_manual(values = rev(colours)) } + # For divergent graphs: additionally flipping the order of the "left side" columns.
    { if (!is.null(divergent)) scale_y_continuous(breaks = seq(-1,1,0.2), labels = pct_format(abs(seq(-1,1,0.2))), limits = c(-1,1)) else scale_y_continuous(breaks = seq(0,1,0.1), labels = pct_format(seq(0,1,0.1))) } + # For divergent graphs: the scale needs to be -1 to +1, rather than 0-1.
    guides(fill=guide_legend(nrow=Legend_Rows,byrow=TRUE,reverse=TRUE)) +
    patchwork::plot_annotation(title = tryCatch(wrap.labels(Title_in_Quotes, Title_wrap_length), error = function(e){stop("Error: Title is not a string. Are you accidentally piping in a dataframe here?")}), subtitle=ifelse(!is.null(subtitle), subtitle, ifelse(N_mode == "t", paste0("N = ",N), "")), theme = theme(plot.title = element_text(hjust = 0.5, size = Title_font_size, margin = margin(0,0,8,0)), plot.subtitle = element_text(hjust = 0.5, size = Subtitle_font_size, color="#525252", face = "italic", margin = margin(0,0,ifelse(any(!is.null(subtitle) | N_mode == "t"),5,-10),0)))) + # Title & subtitle
    # Theme (aesthetic) settings
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = Legend_Font),
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


# (Fr)e(q)uency (G)raph, (Overlap)ped
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates a "grouped" bar chart where there's the ability to overlap them. Note: by default, this function only accommodates up to 5 data columns. But
# you could probably hotwire it to accommodate as many as you like by inspecting the geom_bar/geom_text calls.
# Function Dependencies: wrap.labels
# Required input: a frequency table with dimensions: "resp" [category], and a column with the counts (or percentages) for each response option.
# Arguments:
#   title = Title of the graph
#   Title_wrap_length = 55; How long the text of the title should be allowed to run before spilling to a new line. Uses the wrap.labels() function.
#   groups; A character vector with the value columns you want displayed in the stacked bar, in order.
#   pct = "col"; Whether to run a col_percents() ("col") or row_percents() ("row") call on the values. If you're piping in counts, this is good. If you've already converted things to percents, set this to FALSE.
#   decimals = Number of decimals on value labels
#   scale = If you're wanting to specify your own scale, provide the breaks you want on the X axis.
#   border = "#00000060"; Border colour of the bars
#   colours; A character vector with the colours you want used for the bars. REMEMBER to include the transparency level (percentage) as the last two numbers if you want to see through things.
#   Cat_font_size / Cat_wrap_length = Refer to these characteristics of the category text (items on the left, not the percentages).
#   label_font_size = Font size of the percentage labels.
#   label_nudge = 0.04; Value to offset labels horizontally from the bar.
#   bar_width = 0.4; How thick the bars should be.
#   spacing = 0.2; Spacing between individual bars (and therefore how big category groups will be).
#   Fcolour = "Blues"; R palette being used - see https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
frq_g_overlap = function(df, title = "Chart", Title_wrap_length = 55, groups = names(df)[-1], pct = "col", decimals = 0, scale = NULL, border = "#00000060", colours = NULL, Cat_font_size = 14, Cat_wrap_length = 40, label_font_size = 4, label_nudge = 0.04, bar_width = 0.4, spacing = 0.2, Fcolour = "Blues") {
  # browser()
  if (is.null(colours)) { if(ncol(df) > 3) { colours = RColorBrewer::brewer.pal(n = (ncol(df) - 1), name = Fcolour) } else { colours = RColorBrewer::brewer.pal(n = 3, name = Fcolour)[c(1,3)] } } 
  if (length(colours) != length(groups)) stop(paste0("Omission: You have specified colours, but you need to specify a vector of colours (with transparency numbers trailing, if applicable) *the same length as the number of bars (specified groups) in your dataframe* (Groups detected: ", length(groups), ")"))
  # Data reshaping: the function takes output similar to ct(), but ggplot needs a column for category, group, and value in this case.
  x_pos = seq(spacing, (spacing * -1), length = length(groups))
  BarData_Long = df %>%
    { if (pct == "col") col_percents(.) else . } %>%
    { if (pct == "row") row_percents(.) else . } %>% 
    pivot_longer(cols = 2:ncol(df), names_to = "Group") %>% 
    rename(resp = 1) %>% # Rename first column
    mutate(resp = wrap.labels(resp, Cat_wrap_length)) %>% 
    mutate(resp = factor(resp, levels = rev(unique(.$resp))))
  # The ggplot-call
  ggplot(BarData_Long, aes(x=resp, y=value, fill = Group)) +
    # Here I've conditionally accommodated up to 5 of these geom_bar/geom_text combos. You could add more if you wanted to go crazy.
    geom_bar(data= ~ filter(., Group == groups[1]), stat = "identity", width = bar_width, position = position_nudge(x = x_pos[1]), colour=border) +
    geom_bar(data= ~ filter(., Group == groups[2]), stat = "identity", width = bar_width, position = position_nudge(x = x_pos[2]), colour=border) +
    { if (length(groups) > 2) geom_bar(data= ~ filter(., Group == groups[3]), stat = "identity", width = bar_width, position = position_nudge(x = x_pos[3]), colour=border) } + 
    { if (length(groups) > 3) geom_bar(data= ~ filter(., Group == groups[4]), stat = "identity", width = bar_width, position = position_nudge(x = x_pos[4]), colour=border) } +
    { if (length(groups) > 4) geom_bar(data= ~ filter(., Group == groups[5]), stat = "identity", width = bar_width, position = position_nudge(x = x_pos[5]), colour=border) } +
    geom_text(data= ~ filter(., Group == groups[1]), {if (pct != F) aes(label = ifelse(value>0,paste0(round(value*100,decimals),"%"),"")) else aes(label = ifelse(value>0,formatC(value,format="f",big.mark=",",digits=decimals),""))}, size = label_font_size, position = position_nudge(x = x_pos[1], y = label_nudge)) +
    geom_text(data= ~ filter(., Group == groups[2]), {if (pct != F) aes(label = ifelse(value>0,paste0(round(value*100,decimals),"%"),"")) else aes(label = ifelse(value>0,formatC(value,format="f",big.mark=",",digits=decimals),""))}, size = label_font_size, position = position_nudge(x = x_pos[2], y = label_nudge)) +
    { if (length(groups) > 2) geom_text(data= ~ filter(., Group == groups[3]), {if (pct != F) aes(label = ifelse(value>0,paste0(round(value*100,decimals),"%"),"")) else aes(label = ifelse(value>0,formatC(value,format="f",big.mark=",",digits=decimals),""))}, size = label_font_size, position = position_nudge(x = x_pos[3], y = label_nudge)) } +
    { if (length(groups) > 3) geom_text(data= ~ filter(., Group == groups[4]), {if (pct != F) aes(label = ifelse(value>0,paste0(round(value*100,decimals),"%"),"")) else aes(label = ifelse(value>0,formatC(value,format="f",big.mark=",",digits=decimals),""))}, size = label_font_size, position = position_nudge(x = x_pos[4], y = label_nudge)) } +
    { if (length(groups) > 4) geom_text(data= ~ filter(., Group == groups[5]), {if (pct != F) aes(label = ifelse(value>0,paste0(round(value*100,decimals),"%"),"")) else aes(label = ifelse(value>0,formatC(value,format="f",big.mark=",",digits=decimals),""))}, size = label_font_size, position = position_nudge(x = x_pos[5], y = label_nudge)) } +
    scale_fill_manual(values=colours, limits = groups) +
    geom_vline(xintercept=seq(-0.5, length(groups)+1.5, 1), colour="gray") + # Gridlines
    patchwork::plot_annotation(title = tryCatch(wrap.labels(title, Title_wrap_length), error = function(e){stop("Error: Title is not a string. Are you accidentally piping in a dataframe here?")}), theme = theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(0,0,8,0)))) +
    {if (pct != F) scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%","25%","50%","75%","100%")) else {if (is.null(scale)) scale_y_continuous(breaks = seq(0, ceiling(max(select_if(BarData_Long, is.numeric), na.rm = T)), ceiling(max(select_if(BarData_Long, is.numeric), na.rm = T) / 5))) else scale_y_continuous(breaks = scale)}} +
    coord_flip() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=11),
          legend.margin = margin(5, 0, 5, -100),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = Cat_font_size, margin=margin(0,0,0,0)),
          text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, margin = margin(15,0,8,-990)),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(colour ="grey"),
          panel.grid.minor.x = element_line(colour ="grey90"),
          axis.ticks = element_blank())
}



# (Histo)gram
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Currently a bit broken, needs work... I wonder if someone has done this better... :P
# Feed in a dataframe and a numeric variable, and this function will make a histogram of it. By default, the function will attempt to figure out what's 
# the max value of your data and make a bin for each value. Obviously for best results, you should specify some of the parameters of your data.
# Note: If your supplied column from the dataframe isn't numeric, it'll try to coerce it.
# Arguments:
#   start = The minimum value included in a bin. Auto-detected based on data minimum by default.
#   stop = The maximum value included in a bin. Auto-detected based on data maximum by default.
#   by = Specify the interval size to traverse the space between start/stop. Bins created based on each interval.
#   bins = Specify the number of bins in the plot. 
#   discrete = TRUE; Whether each bin is labeled as a discrete value (T), or between two values (F). The latter is more useful for percent ranges.
#   frq = FALSE; Specify whether you want the Y-axis for the bins to be frequency counts or percent of total?
#   x_axis = TRUE; Show or hide (FALSE) the x-axis.
#   x_label = Manually specify the label on the x-axis.
#   x_labels_type = "frq"; Whether the x-axis labels should be shown as raw values, or ("pct") percent-formatted.
#   x_pct_accuracy = 1; Decimal format for x-axis percent labels, if activated. See scales::label_percent for syntax, but basically, 0.001 shows 3 decimal places, etc.
#   y_label = Manually specify the label on the y-axis.
#   font = Font size of the y-scale and the axes.
#   data_labels = TRUE; Show or hide (FALSE) data-labels over each bin.
#   colour = Specify the colour of the bars. Defaults to light blue.
#   title = Specify the string title over the plot.
#   title_font = Font size of the title.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
histo = function(df, var, font = 14, start = NULL, stop = NULL, bins = NULL, by = NULL, discrete = TRUE, x_axis = TRUE, x_size = 12, x_pct_accuracy = 1, x_label = NULL, x_label_font = 4, frq = FALSE, y_label = NULL, data_labels = TRUE, x_labels_type = "frq", colour = "lightblue", title = NULL, title_font = 16) {
  df = select(df, {{var}}) # Select the data
  # Coerce to numeric if necessary...
  if (lapply(select(df, {{var}}), class) != "numeric") {
    message("Warning: Data supplied is not numeric - attempting to coerce...")
    v = deparse(substitute(var))
    df[v] = as.numeric(as.character(pull(df, v)))
  }
  # Define some things if not provided in the call.
  { if(is.null(start)) start = as.numeric(df %>% pull({{var}}) %>% min(na.rm = TRUE)) }
  { if(is.null(stop)) stop = as.numeric(df %>% pull({{var}}) %>% max(na.rm = TRUE)) }
  { if(is.null(bins)) bins = ifelse(is.null(by), (stop - start), (stop - start) / by)}
  { if(is.null(by)) by = ((stop - start) / bins) }
  message(paste0("Start: ", start, " | Stop: ", stop, " | By: ", by, " | Bins: ", bins))
  df %>% 
    ggplot(aes(x={{var}})) + 
    # Have to make these layers conditionally since I can't seem to figure out how to conditionally invoke / define "bins =" vs. "breaks ="...
    { if (discrete) {
      geom_histogram({ if(frq == FALSE) aes(y = (..count..)/sum(..count..)) else aes(y = (..count..)) }, fill = colour, color = "black", bins = (bins + 1))
    } else {
      geom_histogram({ if(frq == FALSE) aes(y = (..count..)/sum(..count..)) else aes(y = (..count..)) }, fill = colour, color = "black", breaks = seq(start, stop, by = by))
    } } +
    # Frequency vs. percent scales.
    { if(frq) scale_y_continuous(expand = expansion(mult = c(0, 0.13))) else scale_y_continuous(expand = expansion(mult = c(0, 0.13)), labels = scales::percent) } +
    { if(x_labels_type == "pct") scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = x_pct_accuracy), 
                                                    breaks = seq(start, stop, by=by), minor_breaks = seq(start - (by / 2), stop + by, by=by))
      else if(x_labels_type == "frq") scale_x_continuous(breaks = seq(start, stop, by=by), minor_breaks = seq(start - (by / 2), stop + by, by=by)) } + 
    ylab(ifelse(is.null(y_label), ifelse(frq == FALSE, "Percentage", "Count"), y_label)) +
    { if(!is.null(title)) ggtitle(title) } +
    { if(!is.null(x_label)) labs(x = x_label) } + 
    theme(legend.position="bottom",
          legend.title = element_blank(),
          text = element_text(size=font),
          plot.title = element_text(hjust = 0.5, size=title_font),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour ="grey"),
          panel.grid.minor.x = { if(discrete) element_line(colour="grey") else element_blank() },
          panel.grid.major.x = { if(discrete) element_blank() else element_line(colour="grey") },
          axis.ticks.y = element_blank(),
          axis.ticks.x = { if(discrete) element_blank() else element_line(colour = "gray11") },
          axis.text.x = { if(x_axis) element_text(angle=90, vjust=0.5, colour = "black", size = x_size) else element_blank() },
          axis.text.y = element_text(colour = "black")) +
    # Hovering data / bin labels - have to do each layer conditionally again because of needing to invoke "bins =" vs. "breaks =" again.
    { if (data_labels) {
      if (discrete) {
        stat_bin(bins = bins + 1, geom = "text", { if(frq == FALSE) aes(y=(..count..)/sum(..count..), label=pct_format((..count..)/sum(..count..))) else aes(label=(..count..)) }, hjust = -0.25, angle = 90, size=x_label_font) 
      } else {
        stat_bin(breaks = seq(start, stop, by = by), geom = "text", { if(frq == FALSE) aes(y=(..count..)/sum(..count..), label=pct_format((..count..)/sum(..count..))) else aes(label=(..count..)) }, hjust = -0.25, angle = 90, size=x_label_font) 
      }
    } }
}








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEPRECIATED BUT KEPT FOR CONSISTENCY'S SAKE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# And so my old stuff doesn't break...
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove percent, return number. Basically the reverse of pct_format. Use value() instead
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pct_n = function(value) {
  parse_number(value) / 100
}

# Remove commas, return number. Basically the reverse of n_format. Use value() instead. (Credit: https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comma_n <- function(value) { 
  parse_number(value)
}