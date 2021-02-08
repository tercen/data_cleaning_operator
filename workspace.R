library(tercen)
library(dplyr)
require(tidyr)
library(lubridate)

options("tercen.workflowId" = "2553cb89b6ec3bc593e238e0df047713")
options("tercen.stepId"     = "f0f7c34d-e438-459f-be32-fcde2a205abe")

getOption("tercen.workflowId")
getOption("tercen.stepId")

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
numeric_encoding <- "None" # or One-hot, or Label
if(!is.null(ctx$op.value('numeric_encoding'))) numeric_encoding <- (ctx$op.value('numeric_encoding'))

date_factor <- FALSE 
if(!is.null(ctx$op.value('date_factor'))) date_factor <- as.logical(ctx$op.value('numeric_encoding'))
date_time <- TRUE
if(!is.null(ctx$op.value('date_time'))) date_time <- as.logical(ctx$op.value('date_time'))
date_weekday <- FALSE
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

## convert types

l1 <- vars_names[vars_types == "Numeric"]
l1 <- l1[l1 %in% colnames(df)]
l2 <- vars_names[vars_types == "Categorical"]
l2 <- l2[l2 %in% colnames(df)]
## min max categories
lst <- unlist(lapply(df[,l2], function(x) length(unique(x))))
l2 <- names(which(lst >= min_categories & lst <= max_categories))

l3 <- vars_names[vars_types == "Date"]
l3 <- l3[l3 %in% colnames(df)]

# c. dates
date_var <- var_info$Name[var_info$Type == "Date" & var_info$`X or Y` == "X"]
date_var <- date_var[date_var %in% colnames(df)]
df_date <- df[, date_var]

lst <- lapply(df_date, function(x) { hour(x) + minute(x) / 60})
names(lst) <- paste0(names(lst), "_time")
lst2 <- lapply(df_date, function(x) {
  wday(x, label = TRUE)
})
names(lst2) <- paste0(names(lst2), "_weekday")

df_date <- as.data.frame(list(lst, lst2))


df_out <-
  
df %>%
  select(l1, l2, l3) %>%
  mutate_each_(funs(as.numeric), l1) %>%
  mutate_each_(funs(factor), l2) %>%
  mutate_each_(funs(as.Date), l3) 
  


## numeric encoding
library(tidyr)
library(dplyr)
dt %>% mutate(value = 1) %>% #spread(subject, group, value,  fill = 0 ) 
 pivot_wider(#id_cols = group, 
             names_from = c(subject, group), 
             values_from = c(value,value),
             values_fill = 0)
do(pivot.all(.))
?pivot_wider_spec()

## date encoding


## return data
df_out %>% 
  ctx$addNamespace() %>%
  ctx$save()
