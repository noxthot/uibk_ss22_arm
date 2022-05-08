library(lubridate)
library(tidyverse)

PRJ_DIR <- "exercise_epftoolbox"

INDEX_COLS <- c("date", "hour")
TRAIN_COLS <- c("Exogenous.1", "Exogenous.2", "Price", "DayOfYear", "month")
TARGET_COLS <- c("PriceNextDay")

MAX_ORDER <- 5


getColsForLongDf <- function(longdf, col_template, excludeNextDayCols) {
    train_cols <- c()

    for (colsuffix in col_template) {
        train_cols <- c(train_cols, grep(colsuffix, names(longdf), value=TRUE))
    }

    if (excludeNextDayCols) {
        train_cols <- setdiff(train_cols, grep("NextDay", train_cols, value=TRUE))
    }

    return(train_cols)
}


## Set the seed for reproducibility.
set.seed(123)

# Read training and test data from CSVs
df_train <- read.csv(file.path(PRJ_DIR, "DE_train.csv"))
df_test <- read.csv(file.path(PRJ_DIR, "DE_test.csv"))


#' Adds date and hour column based on datetime column
#'  
#' @param df Dataframe containing columns X (carrying datetime as string)
#' @return Dataframe df extended by hour and date column 
#' @examples
#' transformDateCols(df)
transformDateCols <- function(df) {
    dff <- data.frame(df)
    dff$datetime <- as.POSIXct(dff$X, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    dff$hour <- format(dff$datetime, "%H")
    dff$date <- as.Date(format(dff$datetime, "%Y-%m-%d"))
    dff$dateMinus12Hours <- as.Date(format(dff$datetime - hours(12), "%Y-%m-%d"))
    dff <- select(dff, Price, Exogenous.1, Exogenous.2, hour, date, dateMinus12Hours)

    return(dff)
}


#' Prepares dataframe for training. This function transforms the data such that one row holds 
#' hourly prices and the hourly values of the two exogenous co-variates, aswell as the target 
#' values which are the prices hourly prices of the next day
#'  
#' @param df Dataframe containing columns date, Price, hour and various co-variates
#' @return Dataframe prepared for training
#' @examples
#' transformData(df)
transformData <- function(df) {
    dff <- transformDateCols(df)
    df_PrevDay <- data.frame(dff)

    df_PrevDay$prevdaydate <- df_PrevDay$date - 1
    df_PrevDay <- df_PrevDay %>% 
                    select(Price, hour, prevdaydate) %>% 
                    rename(PriceNextDay = Price)

    merged_df <- merge(dff, df_PrevDay, by.x=c("date", "hour"), by.y=c("prevdaydate", "hour"))

    long_df <- (reshape(merged_df, idvar = "dateMinus12Hours", timevar = "hour", direction = "wide"))
    long_df$DayOfYear <- yday(long_df$dateMinus12Hours)
    long_df$month <- month(long_df$dateMinus12Hours)

    return(select(long_df, getColsForLongDf(long_df, c(INDEX_COLS, TRAIN_COLS), FALSE)))
}


# transform training and test data
df_train <- transformData(df_train)
df_test <- transformData(df_test)

train_cols <- getColsForLongDf(df_train, TRAIN_COLS, TRUE)
target_cols <- getColsForLongDf(df_train, TARGET_COLS, FALSE)

# Set up grid search
trans_fun <- list(
                "log" = log,
                "sqrt" = sqrt,
                "exp" = exp,
                "inverse" = function(x) { 1 / x }
             )

## including polynomial functions of orders {1, .., MAX_ORDER}
for (order in seq_len(MAX_ORDER)) {
  poly_fun <- paste0("function(x) { poly(x,", order, ") }")
  trans_fun[[paste0("poly_", order)]] <- eval(parse(text = poly_fun))
}

## Get all combinations .
combos <- expand.grid(
  "f_exo1" = names(trans_fun),
  "f_exo2" = names(trans_fun),
  "f_price" = names(trans_fun),
  "f_DayOfYear" = names(trans_fun),
  "f_month" = names(trans_fun),
  stringsAsFactors = FALSE
)