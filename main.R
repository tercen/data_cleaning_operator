library(tercen)
library(dplyr)
library(tidyr)
library(lubridate)
library(caret)

ctx = tercenCtx()

# input parameters
file_type <- "Excel"
if(!is.null(ctx$op.value('file_type'))) file_type <- ctx$op.value('file_type')

max_missing_prop_rows <- 0.10
if(!is.null(ctx$op.value('max_missing_prop_rows'))) max_missing_prop_rows <- as.numeric(ctx$op.value('max_missing_prop_rows'))
max_missing_prop_cols <- 0.10
if(!is.null(ctx$op.value('max_missing_prop_cols'))) max_missing_prop_cols <- as.numeric(ctx$op.value('max_missing_prop_cols'))

min_categories <- 2
if(!is.null(ctx$op.value('min_categories'))) min_categories <- as.numeric(ctx$op.value('min_categories'))
max_categories <- 25
if(!is.null(ctx$op.value('max_categories'))) max_categories <- as.numeric(ctx$op.value('max_categories'))
numeric_encoding <- "one hot encoding" # or One-hot, or Label
if(!is.null(ctx$op.value('numeric_encoding'))) numeric_encoding <- (ctx$op.value('numeric_encoding'))

date_factor <- FALSE 
if(!is.null(ctx$op.value('date_factor'))) date_factor <- as.logical(ctx$op.value('numeric_encoding'))
date_time <- TRUE
if(!is.null(ctx$op.value('date_time'))) date_time <- as.logical(ctx$op.value('date_time'))
date_weekday <- TRUE
if(!is.null(ctx$op.value('date_weekday'))) date_weekday <- as.logical(ctx$op.value('date_weekday'))
date_day <- FALSE
if(!is.null(ctx$op.value('date_day'))) date_day <- as.logical(ctx$op.value('date_day'))
date_month <- FALSE
if(!is.null(ctx$op.value('date_month'))) date_month <- as.logical(ctx$op.value('date_month'))
date_year <- FALSE
if(!is.null(ctx$op.value('date_year'))) date_year <- as.logical(ctx$op.value('date_year'))

rows <- ctx$rselect()

vars_names <- rows[[grep("nam", colnames(rows), ignore.case = TRUE)[1]]]
vars_types <- rows[[grep("typ", colnames(rows), ignore.case = TRUE)[1]]]

## load input table
documentId <- ctx$cselect()[[1]]
client = ctx$client
schema = client$tableSchemaService$get(documentId)
df <- as_tibble(client$tableSchemaService$select(schema$id, list(), 0, schema$nRows))

## filter missing data
df <- df[, colSums(is.na(df)) / nrow(df) <  max_missing_prop_cols]
df <- df[rowSums(is.na(df)) / ncol(df) <  max_missing_prop_cols, ]

df_out <- data.frame(rowId = seq_len(nrow(df)))

## numeric data preprocessing
l1 <- vars_names[vars_types == "Numeric"]
l1 <- l1[l1 %in% colnames(df)]

if(length(l1) > 0) {
  df_num <- df %>%
    select(all_of(l1)) %>%
    transmute_all(as.numeric) %>%
    mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  
  df_out <- cbind(df_out, df_num)
}

## categories preprocessing
l2 <- vars_names[vars_types == "Categorical"]
l2 <- l2[l2 %in% colnames(df)]
## min max categories
lst <- unlist(lapply(df[, l2], function(x) length(unique(x))))
l2 <- names(which(lst >= min_categories & lst <= max_categories))

if(length(l2) > 0) {
  df_cat <- df %>% select(all_of(l2))
  if(numeric_encoding == "one hot encoding") {
    dv <- caret::dummyVars(~ ., data = df_cat)
    df_cat <- data.frame(predict(dv, newdata = df_cat))
  }
  df_out <- cbind(df_out, df_cat)
}
  
## date preprocessing
l3 <- vars_names[vars_types == "Date"]
l3 <- l3[l3 %in% colnames(df)]

if(length(l3) > 0) {
  # c. dates
  df_date_in <- df %>% select(all_of(l3))
  df_date <- list(id = seq_len(nrow(df_date_in)))
  
  if(date_time) {
    df_time <- lapply(df_date_in, function(x) { 
      x <- lubridate::as_date(x)
      out <- hour(x) + minute(x) / 60
      return(out)
    })
    names(df_time) <- paste0(names(df_time), "_time")
    df_date <- cbind(df_date, as.data.frame(df_time))
  }
  if(date_weekday) {
    df_weekday <- lapply(df_date_in, function(x) { 
      x <- lubridate::as_date(x)
      out <- wday(x, label = TRUE)
      return(out)
    })
    names(df_weekday) <- paste0(names(df_weekday), "_weekday")
    df_date <- cbind(df_date, as.data.frame(df_weekday))
  }
  
  df_date$id <- NULL
  
  df_out <- cbind(df_out, df_date)
  
}

# df_out$rowId <- NULL

df_out %>%
  mutate(.ci = 0) %>%
  ctx$addNamespace() %>%
  ctx$save()
