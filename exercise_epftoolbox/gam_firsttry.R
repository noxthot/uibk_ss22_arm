library(lubridate)
library(tidyverse)

PRJ_DIR <- "exercise_epftoolbox"

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
    #dff$dateMinus12Hours <- as.Date(format(dff$datetime - hours(12), "%Y-%m-%d"))

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

    merged_df <- merge(dff, df_PrevDay, by.x=c("date", "hour"), by.y=c("df_PrevDay", "hour"))

    return(reshape(merged_df, idvar = "date", timevar = "hour", direction = "wide"))
}


# transform training and test data
df_train <- transformData(df_train)
df_test <- transformData(df_test)



poly_5 <- function(x) { poly(x, 5) }

## Estimate poly model.
f1 <- poly_5
f2 <- poly_5

b <- lm(rentsqm ~ f1(area) + f2(yearc) + location, data = MunichRent)  # location is a factor, so it transforms automatically

summary(b)

## Plot
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 0.1, 0.5))
termplot(b, partial.resid = TRUE, se = TRUE, cex = 0.1) 