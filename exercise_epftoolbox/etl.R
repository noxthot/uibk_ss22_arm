library(DMwR)
library(lubridate)
library(rhdf5)
library(tidyverse)

PRJ_DIR <- "exercise_epftoolbox"
OUT_HDF5_FILENAME <- "DE_fulldata.hdf5"

WRITE_HDF5_FILE <- TRUE

INDEX_COLS <- c("date", "hour")

TRAIN_COLS <- c(#"CurrDayExogenous.1", 
                #"CurrDayExogenous.2", 
                "CurrDayExogenous.1Transf", 
                "CurrDayExogenous.2Transf", 
                #"CurrDayPrice", 
                "CurrDayPriceTransf", 
                #"L1Exogenous.1", 
                #"L1Exogenous.2", 
                "L1Exogenous.1Transf", 
                "L1Exogenous.2Transf", 
                #"L1Price", 
                "L1PriceTransf", 
                #"L2Exogenous.1", 
                #"L2Exogenous.2", 
                "L2Exogenous.1Transf", 
                "L2Exogenous.2Transf", 
                #"L2Price", 
                "L2PriceTransf", 
                #"L3Exogenous.1", 
                #"L3Exogenous.2", 
                "L3Exogenous.1Transf", 
                "L3Exogenous.2Transf", 
                #"L3Price", 
                "L3PriceTransf", 
                #"L7Exogenous.1", 
                #"L7Exogenous.2", 
                "L7Exogenous.1Transf", 
                "L7Exogenous.2Transf", 
                #"L7Price", 
                "L7PriceTransf", 
                "month", 
                "weekday", 
                "dayofyear")

TARGET_COLS <- c("NextDayPrice")

ENABLE_12_HOUR_GAP = FALSE  # TRUE .. Bids need to be given until 12:00 (midday); FALSE .. Give bids at 24:00 (midnight)


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
    
    for (colname in c("Price", "Exogenous.1", "Exogenous.2")) {
        mx <- mean(dff[[colname]])
        sdx <- sd(dff[[colname]])
        x <- (dff[[colname]] - mx) / sdx

        dff[paste0(colname, "Transf")] <- log(x + sqrt(x ^ 2 + 1))  # expert feature
    }

    return(dff)
}


enrichDataSetPastReshape <- function(df) {
    dff <- data.frame(df)

    dff$month <- as.integer(format(dff$date, "%m"))
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
transformDateCols <- function(df, plus1day=FALSE) {
    dff <- data.frame(df)
    dff$datetime <- as.POSIXct(dff$X, format="%Y-%m-%d %H:%M:%S", tz="UTC")

    if (plus1day) {
        dff$datetime <- dff$datetime + days(1)
    }

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
    dff <- transformDateCols(df, TRUE)  # add one day to have the same date reference as used in the paper's forecast csv
    dff <- enrichDataSetPriorReshape(dff)
    dfftmp <- data.frame(dff)
    df_NextDay <- data.frame(dff)

    merged_df <- dff %>% 
                    rename(CurrDayPrice = Price, CurrDayPriceTransf = PriceTransf, CurrDayExogenous.1 = Exogenous.1, CurrDayExogenous.2 = Exogenous.2, CurrDayExogenous.1Transf = Exogenous.1, CurrDayExogenous.2Transf = Exogenous.2)
    
    df_NextDay$prevdaydate <- df_NextDay$date - 1
    df_NextDay <- df_NextDay %>%
                    select(Price, hour, prevdaydate) %>%
                    rename(NextDayPrice = Price)
  
    merged_df <- merged_df %>% 
                    merge(df_NextDay, by.x=c("date", "hour"), by.y=c(paste0("prevdaydate"), "hour"))

    lagdays <- c(1, 2, 3, 7)

    for (l in lagdays) {
        df_lagday <- data.frame(dfftmp)

        df_lagday[paste0("L", l, "date")] <- df_lagday$date + l
        df_lagday <- df_lagday %>%
                        select(Price, PriceTransf, Exogenous.1, Exogenous.2, Exogenous.1Transf, Exogenous.2Transf, hour, paste0("L", l, "date"))
        
        for (colname in c("Price", "PriceTransf", "Exogenous.1", "Exogenous.2", "Exogenous.1Transf", "Exogenous.2Transf")) {
            colnames(df_lagday)[colnames(df_lagday) == colname] = paste0("L", l, colname)
        }

        merged_df <- merged_df %>%
                    merge(df_lagday, by.x=c("date", "hour"), by.y=c(paste0("L", l, "date"), "hour"))
    }

    long_df <- (reshape(merged_df, idvar = "date", timevar = "hour", direction = "wide"))

    long_df <- enrichDataSetPastReshape(long_df)

    cols_to_remove = c()

    # Removing unnecessary columns
    for (i in 0:23) {
        colcounter = sprintf("%02d", i)
        cols_to_remove = c(cols_to_remove, paste0("datetime.", colcounter))
        
        if ((ENABLE_12_HOUR_GAP) & (i >= 12)) {
            for (colpre in c("CurrDayExogenous.1.", "CurrDayExogenous.2.", "CurrDayPrice.", "CurrDayPriceTransf.")) {
                cols_to_remove = c(cols_to_remove, paste0(colpre, colcounter))
            }
        }
    }

    `%ni%` <- Negate(`%in%`)
    
    long_df <- subset(long_df, select = names(long_df) %ni% cols_to_remove)

    return(select(long_df, getColsForLongDf(long_df, unique(c(INDEX_COLS, TRAIN_COLS, "Price")), FALSE)))
}

scaleDataMeanStd <- function(df, mean=NULL, std=NULL) {
    if (is.null(mean)) {
        mean <- apply(df, 2, mean)
    }

    if (is.null(std)) {
        std <- apply(df, 2, sd)
    }

    retdf <- scale(df, center=mean, scale=std)

    return(list(df=retdf, mean=mean, std=std))
}

unscaleDataMeanStd <- function(df) {
    return(unscale(df, df))
}


## Set the seed for reproducibility.
set.seed(123)

# Read training and test data plus forecasts from CSVs
df_train <- read.csv(file.path(PRJ_DIR, "DE_train.csv"))
df_test <- read.csv(file.path(PRJ_DIR, "DE_test.csv"))
df_forecasts <- read.csv(file.path(PRJ_DIR, "DE_test_forecasts.csv"))

# transform forecast data
df_forecasts <- transformDateCols(df_forecasts)

# transform training and test data
df_train <- transformData(df_train)
df_test <- transformData(df_test)

train_cols <- getColsForLongDf(df_train, TRAIN_COLS, TRUE)
target_cols <- getColsForLongDf(df_train, TARGET_COLS, FALSE)

X_train <- select(df_train, all_of(train_cols))
y_train <- select(df_train, all_of(target_cols))

X_test <- select(df_test, all_of(train_cols))
y_test <- select(df_test, all_of(target_cols))

scaled_X_train_tmp <- scaleDataMeanStd(X_train)
scaled_y_train_tmp <- scaleDataMeanStd(y_train)

scaled_X_train <- scaled_X_train_tmp$df
mean_X_train <- scaled_X_train_tmp$mean
std_X_train <- scaled_X_train_tmp$std

scaled_y_train <- scaled_y_train_tmp$df
mean_y_train <- scaled_y_train_tmp$mean
std_y_train <- scaled_y_train_tmp$std

scaled_X_test_tmp <- scaleDataMeanStd(X_test, mean_X_train, std_X_train)
scaled_y_test_tmp <- scaleDataMeanStd(y_test, mean_y_train, std_y_train)

scaled_X_test <- scaled_X_test_tmp$df
scaled_Y_test <- scaled_y_test_tmp$df

if (WRITE_HDF5_FILE) {
    h5write(X_train, file=file.path(PRJ_DIR, OUT_HDF5_FILENAME), name="xtrain")
    h5write(X_test, file=file.path(PRJ_DIR, OUT_HDF5_FILENAME), name="xtest")
    h5write(y_train, file=file.path(PRJ_DIR, OUT_HDF5_FILENAME), name="ytrain")
    h5write(y_test, file=file.path(PRJ_DIR, OUT_HDF5_FILENAME), name="ytest")
    h5write(df_forecasts, file=file.path(PRJ_DIR, OUT_HDF5_FILENAME), name="forecasts")
}