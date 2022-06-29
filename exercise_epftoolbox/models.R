library(bamlss)
library(gamlss)
library(Metrics)
library(mgcv)
library(svMisc)

modelchoice = "stepwise"      # possibilities: stepwise

tp <- function(z, degree = 3, knots = seq(min(z), max(z), length = 10)) {
  ## If knots is integer.
  if(length(knots) < 2)
    knots <- seq(min(z), max(z), length = knots)
  ## Setup the columns for the global polynomials.
  Z <- outer(z, 0:degree, "^"); cn <- paste("z^", 0:degree, sep = "")
  ## Compute local polynomials.
  if(length(knots) > 2) {
    knots <- sort(unique(knots))
    for(j in 2:(length(knots) - 1)) {
      zk <- z - knots[j]
      check <- zk < 0
      zk <- zk^degree
      zk[check] <- 0
      Z <- cbind(Z, zk)
      cn <- c(cn, paste("(z-", round(knots[j], 2), ")^", degree, sep = ""))
    }
  }
  ## Assign column names.
  colnames(Z) <- cn
  return(Z)
}


buildFormulaRhs <- function(train_cols) {
    terms <- NULL

    for (train_col in train_cols) {
        sparam <- ""
        term_start <- "s("

        if (train_col %in% c("month", "weekday")) {
            sparam <- ", k=6"
        }

        if (train_col %in% c("month", "weekday", "dayofyear")) {
          term_start <- "pbc("
        }

        terms <- c(terms, paste0(term_start, train_col, sparam, ")"))
    }

    return(paste0(terms, collapse=" + "))
}


if (methodchoice == "stepwise") {
  cat("Your choice: stepwise")
  nr_threads <- 15
  ctrl <- list(nthreads=nr_threads)
  df_pred <- NULL

  for (target_hour in 0:23) {
    cat("Starting with target hour: ", target_hour, "\n")
    target_col <- paste0("PriceNextDay.", str_pad(target_hour, 2, pad="0"))

    f <- as.formula(paste0(target_col, " ~ ", 1))
    min_b <- gam(f, data=df_train)
    p <- predict(min_b, newdata=df_test)

    min_err <- smape(df_test[[target_col]], p)

    selected_cols <- NULL
    idx <- 0

    while (idx < 15) {
        idx <- idx + 1
        cat("Iteration: ", idx, "\n", sep = "")
        err <- NULL
        unselected_cols <- setdiff(train_cols, selected_cols)
        max_iter <- length(unselected_cols)
        rhs_ls <- NULL

        for (train_col_idx in seq_along(unselected_cols)) {
            #txtProgressBar(train_col_idx, max_iter)
            progress(train_col_idx, max_iter)
            sparam <- ""

            train_col <- unselected_cols[train_col_idx]

            sparam <- ", k=6"

            rhs <- buildFormulaRhs(c(selected_cols, train_col))
            f <- as.formula(paste0(target_col, " ~ ", rhs))

            rhs_ls <- c(rhs_ls, rhs)
            b <- gam(f, data=df_train, control=ctrl)
            p <- predict(b, newdata=df_test)

            curr_err <- smape(df_test[[target_col]], p)

            err <- c(err, curr_err)
        }

        j <- which.min(err)
        err0 <- min(err)

        THRESHOLD_IMPROVEMENT <- 1e-3

        if ((err0 - min_err) < -THRESHOLD_IMPROVEMENT) {
            cat("Adding column:", unselected_cols[j], "\n", sep = " ")
            cat("Old error:", min_err, "\n", sep = " ")
            cat("New error:", err0, "\n", sep = " ")

            selected_cols <- c(selected_cols, unselected_cols[j])

            min_err <- err0
            min_f <- as.formula(paste0(target_col, " ~ ", rhs_ls[j]))
            #min_b <- bamlss(min_f, data=df_train, cores=nr_threads)
            min_b <- gam(min_f, data=df_train, control=ctrl)
            min_p <- predict(min_b, newdata=df_test)
        } else {
            break
        }
    }

    cat("Final error hour", target_hour, ":", min_err, "\n", sep = " ")
    cat("Selected columns:", selected_cols, "\n", sep = " ")

    err <- apply(min_p, 1, function(x) mean((df_test[[target_col]] - x) ^ 2))
    plot(err, type = "l")

    df_pred_onehour <- data.frame(min_p, df_test[[target_col]], df_test[["date"]], str_pad(target_hour, 2, pad="0"))
    names(df_pred_onehour) <- c("own_model", "real_data", "date", "hour")

    df_pred <- rbind(df_pred, df_pred_onehour)
  }
}

df_forecasts_condensed <- merge(df_forecasts, df_pred, by=c("date", "hour"))

compare_model <- "DNN.Ensemble"

plot(df_forecasts_condensed$own_model, df_forecasts_condensed$real_data)
points(df_forecasts_condensed[[compare_model]], df_forecasts_condensed$real_data, col=2)
abline(0, 1, lwd=3, col=4)

plot(df_forecasts_condensed$date, df_forecasts_condensed$own_model)
points(df_forecasts_condensed$date, df_forecasts_condensed[[compare_model]], col=2)
points(df_forecasts_condensed$date, df_forecasts_condensed$real_data, col=3)

plot(df_forecasts_condensed$date, df_forecasts_condensed$own_model - df_forecasts_condensed$real_data)
points(df_forecasts_condensed$date, df_forecasts_condensed[[compare_model]] - df_forecasts_condensed$real_data, col=2)
abline(h=0, col=4)

cat("Own model SMAPE:", smape(df_forecasts_condensed$own_model, df_forecasts_condensed$real_data), "\n", sep = " ")
cat("DNN Ensemble SMAPE:", smape(df_forecasts_condensed[[compare_model]], df_forecasts_condensed$real_data), "\n", sep = " ")