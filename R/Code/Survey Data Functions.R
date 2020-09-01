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

# Suppress that "groups" warning from the summarize() function.
options(dplyr.summarise.inform=F)

# Percent Formatting
# --------------------------------------------------------------------------------------------
pct_format = function(value, decimals = 0) {
  ifelse(!is.na(value), sprintf(paste0("%.", decimals, "f%%"), (value * 100)), NA)
}

# Comma-Number Formatting
# --------------------------------------------------------------------------------------------
n_format = function(n, decimals = 0) {
  formatC(n,format="f", big.mark=",", digits=decimals)
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

# Set values - so I can adjust labels while still in a pipe.
# --------------------------------------------------------------------------------------------
set = function(df, r, c, value) {
  df[r, c] = value
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
#   Weight_var_in_Quotes = Specific weighting variable, optional.
#   pct = Set to TRUE to change output to row percentages
#   count = Set to TRUE to return a total N for each row - usually only used in conjunction with pct.
#   decimals = Specify an integer to round to that many places
#   include_N = Adds an "N" column with row totals.
# --------------------------------------------------------------------------------------------
ct = function(dataset, Row_Demo_quoted, Column_Question_quoted, Weight_var_in_Quotes = NULL, pct = FALSE, decimals = NULL, count = FALSE, include_N = TRUE) {
  Data = dataset %>% 
    { if (is.null(Weight_var_in_Quotes)) select(., Column_Question_quoted, Row_Demo_quoted) else select(., Column_Question_quoted, Row_Demo_quoted, Weight_var_in_Quotes) } %>%
    rename(Response = 1, Field = 2) %>% 
    { if (is.null(Weight_var_in_Quotes)) mutate(., weight = 1) else rename(., weight = 3) }
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
  return(Output)
}

# Row Percents
# ------------------------------------------------------------------------------------------------------------
# Transforms a frequency table input to row percentages. Remember ct() above does most of this.
# Arguments:
#   Col1Label = Whether the first column is the categories. Set to FALSE if not. 
#   count = Whether you want a count column maintained - either as column 1, or column 2 if Col1Label = TRUE.
# ------------------------------------------------------------------------------------------------------------
row_percents = function(df, Col1Label = TRUE, count = FALSE) {
  if (Col1Label == FALSE) {
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
  if (Col1Label == FALSE) {
    df = df %>% 
      .[,-1]
  }
  if (count == TRUE) {
    df = bind_cols(df, temp_counts) %>% 
      { if (Col1Label == FALSE) .[,c(ncol(.), 1:(ncol(.)-1))] else .[,c(1,ncol(.),2:(ncol(.)-1))] }
  }
  return(ungroup(df))
}

# (C)rosstabulation (Table) Rendering
# ------------------------------------------------------------------------------------------------------------
# Renders output from the ct function above, optionally with chi-square test (if supplied frequency counts).
# NOTES: SWITCH field and response in the future, so it's RxC
# Arguments:
#   Col1Name = Name of the table's 1st column - typically the demo / category title
#   Col1Width = How wide should the first column (categories) be? Especially in relation to...
#   OtherColWidths = How wide should the other columns be? Adjusting these allows you to fit more or less in the table nicely.
#   chi2 = Toggle the chi2 test and colouring in the table (TRUE vs. FALSE)
#   freq = Toggle reporting the results as the raw counts, rather than making them into row percentages (T/F)
#   decimals = Assignment for how many decimals to include in numerical output.
# ------------------------------------------------------------------------------------------------------------
ctable = function(df, Col1Name = NULL, Col1Width = "20%", OtherColWidths = "10%", chi2 = TRUE, freq = FALSE, decimals = NULL, title = "") {
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
          wtest$ColFlag = ifelse(wtest$expected >= 5, 9, 9)
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
      df = df %>% 
        pivot_longer(cols=2:ncol(.),names_to="cat", values_to = "val") %>% 
        group_by_at(1) %>% 
        mutate(pct = val/sum(val, na.rm = TRUE)) %>% 
        select(1:2,4) %>% 
        pivot_wider(names_from = cat, values_from = pct)
    } else {
      if (chi2 == TRUE) {
        print("Message: You appear to possibly be running this function on a percentage table. The chi-square colouring will not return effective results. To silence this message, either change your input data to be a frequency table, or disable the chi-square test by using the parameter: 'chi2 = FALSE' in your function call.")
      }
    }
  } else {
    freq_n = sum(df[1,-1], na.rm = TRUE)
  }
  # Rounding if activated [though currently moot due to the rendering... We have to keep them as numbers to get the data bars]
  if (!is.null(decimals)) {
    if (freq == FALSE) {
      decimals = decimals + 2
    }
    df = df %>% mutate_at(vars(-1), ~ round(., digits = decimals))
  }
  # Append chi2 formatting
  if (chi2 == TRUE) {
    df = bind_cols(as_tibble(df), rename_all(as_tibble(rbind(rep(10, ncol(wtest[["ColFlag"]])), wtest[["ColFlag"]])), function(x) paste0(x,"_chi")))
  } else {
    df = bind_cols(as_tibble(df), as_tibble(matrix(10L, nrow = dim(df)[1], ncol = dim(df)[2]-1)))
  }
  N <-(ncol(df) - 1) / 2 # Number of columns, will be useful for the table render, relating to the dataframe.
  # The datatable call.
  CrossTab = datatable(df, class = 'row-border', rownames = FALSE, 
                       caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',title),
                       options = list(scrollX = FALSE, paging = FALSE, searching = FALSE, dom = 't', ordering = F, autoWidth = TRUE, 
                                      columnDefs = list(list(visible=FALSE, targets = (N+1):(2*N)),
                                                        list(className = 'dt-center', targets = "_all"),
                                                        list(width = Col1Width, targets = 0),
                                                        list(width = OtherColWidths, targets = 1:N)))) %>%
    formatStyle(columns = 1, target = "row", # "Overall" row formatting
                backgroundColor = styleEqual(c("Overall"), c("#337ab7")), 
                fontWeight = styleEqual("Overall", 'bold'), 
                color = styleEqual(c("Overall"), c("white"))) %>%
    formatStyle(2:(N+1), # Data bars
                background = styleColorBar({if (freq == FALSE) as.integer(0:1) else as.integer(c(0,freq_n))}, "#d4d4d485"),
                backgroundSize = '98% 80%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
    formatPercentage(ifelse(freq==FALSE,2,0):ifelse(freq==FALSE,(N+1),0), ifelse(!is.null(decimals),ifelse(freq==FALSE,decimals-2,decimals), 0)) %>% # Percentages
    formatStyle(1:(N+1), border = '1px solid #ddd') %>% 
    formatStyle(2:(N+1), valueColumns = (N+2):(2*N+1), color = styleEqual(c('-1', '0', '1', '9'), c('#C00000', 'black', '#76933C', '#858585')))
  return(CrossTab)
}

# Mark's MultiCoding Function, for multi-response questions from Qualtrics (comma-separated options).
# --------------------------------------------------------------------------------------------
# Credit: Mark Kane, Conestoga College
# Used for multiple-response questions (from Qualtrics) where each option is a separate column, but shares a common column
# name prefix (eg. 'Q7_'). It will grab all of these columns and, where a value exists, code a 1 or a 0 if a value is present
# or not, coding rows NA if the respondent didn't complete any question of the series. (Might thereby be important that you
# include a "none of the above" option.)
# Arguments:
#   label = Set your own prefix for the output; otherwise, the current prefix will be preserved.
# --------------------------------------------------------------------------------------------
MultiCoding <- function(data, Q, label = NULL) {
  label <- ifelse(is.null(label),Q,label)
  data <- select(data, all_of(Q)) %>% 
    rename(Q = 1)
  temp = data %>% drop_na()
  temp$Q <- as.character(temp$Q)
  resp <- unique(unlist(strsplit(temp$Q, ",(?!\\s)", perl = TRUE)))
  dummies <- matrix(NA, nrow(data), length(resp)) # Create empty matrix to populate dummies.
  for (i in 1:length(resp)) {
    dummies[,i] <- ifelse(str_detect(data$Q, pattern = coll(resp[i])), 1, 0)
  }
  colnames(dummies) <- paste0(rep(paste0(label, "_"), length(resp)), resp)
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
  bind_cols(temp, MultiCoding(df, Question_in_quotes)[which(names(MultiCoding(df, Question_in_quotes)) == Item_from_Q_in_quotes)]) %>%
    ct(2, 3, 1) %>%
    rename(!!label_0 := 2, !!label_1 := 3) %>%
    .[,c(1,3,2)]
}

# (Battery) Question (F)requency (T)able, (W)eighted
# --------------------------------------------------------------------------------------------
# Takes your question battery eg. ("Q10" refers to "Q10_1, _2, etc.") and makes it into a frequency table.
# It's important that you then view said table and re-arrange it properly for the rendering in frq_g_battery().
# * Requires access to a "Questions" dataframe in order to get the category text, since that needs to be added for the output to be useful. This is an artifact of how Qualtrics encodes question-battery category text into the column headers.
# Note: I tried to make this have conditional pipes instead of all these IF statements; sum() kept giving me errors for some unknown reason.
# Arguments:
#   Q_prefix_quoted = The prefix of the question battery you're wanting to summarize, eg. "Q10" refers to "Q10_1, _2, etc."
#   Weight_var_in_Quotes = Weighting variable. Required.
#   freq = Set to TRUE to return frequency counts instead of row percents (default, FALSE)
#   round_freq = Round frequencies / counts to integers
#   skip_Questions = If you don't have a "Questions" dataset, set to TRUE - this will skip looking up the category text
# --------------------------------------------------------------------------------------------
battery_ftw = function(df, Q_prefix_quoted, Weight_var_in_Quotes = NULL, freq = FALSE, round_freq = TRUE, skip_Questions = FALSE) {
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
  if (skip_Questions == FALSE) {
    temp = temp %>% 
      mutate(resp = gsub(".*-\\s", "", Questions$Text[which(Questions$Q == Q)])) %>% 
      .[,-1] %>% 
      select(resp, everything())
  }
  return(temp)
}

# (Fr)e(q)uency (G)raph, (Simple)
# --------------------------------------------------------------------------------------------
# Makes a simple horizontal bar graph.
# Function Dependencies: wrap.labels, pct_format
# Requires a frequency table with dimensions: "resp" [category], "pct" [decimal]
# Arguments:
#   Title_in_Quotes = Title of the graph
#   Title_wrap_length = How long the text of the title should be allowed to run before spilling to a new line. Uses the function dependency.
#   Subtitle_font_size = Self-described
#   Value_font_size = How big should the numbers on the bars be?
#   Cat_wrap_length = How long should the categories on the bars run before they spill to a new line?
#   Cat_font_size = Font size for categories on the bars
#   Custom_N = Specify a custom N to appear in the subtitle
#   Cat_pcts = If you want the graph to be of counts (not percentages), put your counts into the "pct" column and set the option Cat_pct = FALSE.
#   decimals = Number of decimals to include for percentage rounding.
# --------------------------------------------------------------------------------------------
frq_g_simple = function(df, Title_in_Quotes = "", Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Value_font_size = 6, Cat_wrap_length = 25, Cat_font_size = 20, Custom_N = NULL, Cat_pcts = TRUE, decimals = 0) {
  df = df %>%  
    mutate(resp = wrap.labels(resp, Cat_wrap_length)) 
  df %>% 
    ggplot(aes(x=resp, y=pct)) +
    geom_bar(stat = 'identity', fill = '#6baed6') + 
    xlim(rev(df$resp)) + 
    geom_text(aes(label = {if (Cat_pcts == TRUE) pct_format(pct, decimals) else pct}), size = Value_font_size, position = position_stack(vjust = 0.9)) +
    coord_flip() +
    {if (Cat_pcts == TRUE) scale_y_continuous(labels = scales::percent_format(accuracy = 1)) else scale_y_continuous(breaks = unname(round(quantile(c(0,max(df$pct))), 0)), labels = unname(round(quantile(c(0,max(df$pct))), 0)))} + 
    patchwork::plot_annotation(title = wrap.labels(Title_in_Quotes, Title_wrap_length), subtitle = ifelse(is.null(Custom_N), paste0("N = ",formatC(sum(df$count),format="f", big.mark=",", digits=0)), ifelse(Custom_N == FALSE, "", paste0("N = ",formatC(Custom_N,format="f", big.mark=",", digits=0)))), theme = theme(plot.title = element_text(hjust = 0.5, size = Title_font_size, margin = margin(0,0,8,0)), plot.subtitle = element_text(hjust = 0.5, size = Subtitle_font_size, color="#525252", face = "italic", margin = margin(0,0,5,0)))) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(size = Cat_font_size), 
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour ="grey"),
      axis.ticks = element_blank())
}

# (Fr)e(q)uency (G)raph, (Battery)
# --------------------------------------------------------------------------------------------
# Creates a stacked bar chart.
# Function Dependencies: wrap.labels
# Required input: a frequency table with dimensions: "resp" [category], "count" [n], and a column with the percentages [decimals] for each response option, properly labelled with the complete response text.
# Arguments:
#   N_mode = Specify the mode to display N values in the graph. Can be either (t)itle or (c)ategory. FALSE disables N's.
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
frq_g_battery = function(df, Title_in_Quotes = NULL, Title_wrap_length = 55, Title_font_size = 16, Subtitle_font_size = 14, Cat_font_size = 12, Cat_wrap_length = 34, YRightMargin = 0, Label_font_size = 5, N_mode = "t", decimals = 0, Legend_Rows = 2, Legend_Padding = 100, subtitle = NULL, Fcolour = "Blues", colours = NULL) { 
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
    geom_bar(stat = 'identity') +
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