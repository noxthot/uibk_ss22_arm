library(lubridate)
library(tidyverse)

PRJ_DIR <- "exercise_epftoolbox"

INDEX_COLS <- c("date", "hour")
TRAIN_COLS <- c("Exogenous.1", "Exogenous.2", "Price", "PriceTransf", "DayOfYear", "month", "weekday", "dayofyear")
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


enrichDataSetPriorReshape <- function(df) {
    dff <- data.frame(df)

    dff$PriceTransf <- log(dff$Price + sqrt(dff$Price ^ 2 + 1))  # expert feature

    return(dff)
}


enrichDataSetPastReshape <- function(df) {
    dff <- data.frame(df)

    dff$month <- format(dff$date, "%m")
    dff$weekday <- as.POSIXlt(dff$date)$wday
    dff$dayofyear <- as.POSIXlt(dff$date)$yday

    return(dff)
}


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
    dff <- enrichDataSetPriorReshape(dff)
    df_PrevDay <- data.frame(dff)
    df_NextDay <- data.frame(dff)
    df_PrevWeek <- data.frame(dff)

    df_NextDay$prevdaydate <- df_NextDay$date - 1
    df_NextDay <- df_NextDay %>%
                    select(Price, PriceTransf, Exogenous.1, Exogenous.2, hour, prevdaydate) %>%
                    rename(PriceNextDay = Price, PriceTransfNextDay = PriceTransf, Exogenous.1NextDay = Exogenous.1, Exogenous.2NextDay = Exogenous.2)

    df_PrevDay$nextdaydate <- df_PrevDay$date + 1
    df_PrevDay <- df_PrevDay %>%
                    select(Price, PriceTransf, Exogenous.1, Exogenous.2, hour, nextdaydate) %>%
                    rename(PricePrevDay = Price, PriceTransfPrevDay = PriceTransf, Exogenous.1PrevDay = Exogenous.1, Exogenous.2PrevDay = Exogenous.2)

    df_PrevWeek$nextweekdate <- df_PrevWeek$date + 7
    df_PrevWeek <- df_PrevWeek %>%
                    select(Price, PriceTransf, Exogenous.1, Exogenous.2, hour, nextweekdate) %>%
                    rename(PricePrevWeek = Price, PriceTransfPrevWeek = PriceTransf, Exogenous.1PrevWeek = Exogenous.1, Exogenous.2PrevWeek = Exogenous.2)

    merged_df <- dff %>%
                    merge(df_PrevDay, by.x=c("date", "hour"), by.y=c("nextdaydate", "hour")) %>%
                    merge(df_PrevWeek, by.x=c("date", "hour"), by.y=c("nextweekdate", "hour")) %>%
                    merge(df_NextDay, by.x=c("date", "hour"), by.y=c("prevdaydate", "hour"))

    long_df <- (reshape(merged_df, idvar = "date", timevar = "hour", direction = "wide"))

    long_df <- enrichDataSetPastReshape(long_df)

    cols_to_remove = c()

    # Removing unnecessary columns and afternoon of day of prediction (since this can not be used for prediction -> 12 hour gap)
    for (i in 0:23) {
        colcounter = sprintf("%02d", i)
        
        for (colpre in c("datetime.", "Exogenous.1.", "Exogenous.2.", "Price.", "PriceTransf.")) {
            if (colpre == "datetime." || i >= 12) {
                cols_to_remove = c(cols_to_remove, paste0(colpre, colcounter))
            }
        }
    }

    `%ni%` <- Negate(`%in%`)
    
    long_df <- subset(long_df, select = names(long_df) %ni% cols_to_remove)

    return(select(long_df, getColsForLongDf(long_df, c(INDEX_COLS, TRAIN_COLS), FALSE)))
}


## Set the seed for reproducibility.
set.seed(123)

# Read training and test data from CSVs
df_train <- read.csv(file.path(PRJ_DIR, "DE_train.csv"))
df_test <- read.csv(file.path(PRJ_DIR, "DE_test.csv"))

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