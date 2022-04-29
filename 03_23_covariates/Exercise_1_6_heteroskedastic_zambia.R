## Load the data.
load("ZambiaNutrition.rda")

bp_test <- function(formula, ...) {
  ## Estimate mean model.
  b <- lm(formula, ...)
  
  ## Extract and square residuals.
  e2 <<- residuals(b)^2

  ## Variance model formula.
  formula_sigma2 <- update(formula, e2 ~ .)

  ## Fit variance model.
  m <- lm(formula_sigma2, ...)

  ## Extract the test statistic.
  LM <- nobs(m) * summary(m)$r.squared

  ## Compute the p-value.
  df_LM <- length(coefficients(m)) - 1
  pval_LM <- 1 - pchisq(LM, df_LM)

  ## And the F-statistic.
  F <- as.numeric(summary(m)$fstatistic)
  pval_F <- pf(F[1L], F[2L], F[3L], lower.tail = FALSE)

  ## Clean up global environment.
  rm(list = "e2", envir = .GlobalEnv)

  rval <- list(
    "LM" = c("LM-statistic" = LM, "df" = df_LM, "p-value" = pval_LM),
    "F" = c("F-statistic" = F[1], "df" = F[3], "p-value" = pval_F)
  )

  return(rval)
}

## Heteroskedastic linear model fitting function.
## Argument eps controls stopping of the while loop.
hetlm <- function(formula, data = NULL, eps = 1e-05, ...) {
  ## Step 1, initial linear model.
  b <- lm(formula, data = data, ...)

  ## Extract fitted values to measure change of fit
  ## at the end of the while loop.
  fit0 <- fitted(b)

  ## The variance model formula,
  ## note e2 will be set up within the while loop.
  formula_sigma2 <- update(formula, log_e2 ~ .)

  ## Start the while loop, therefore initialize
  ## change parameter.
  change <- 1

  ## Save number of iterations.
  iter <- 0
  while(eps < change) {
    ## Extract log-squared residuals.
    ## Note that the doubled arrow "<<-" assigns variables
    ## to the global environment. This way object log_e2,
    ## and h below is available for fitting the model
    ## with lm().
    log_e2 <<- log(residuals(b)^2)

    ## Estimate variance model.
    m <- lm(formula_sigma2, data = data, ...)

    ## Compute the weights.
    h <<- exp(fitted(m))

    ## Estimate weighted linear model.
    b <- lm(formula, weights = 1/h, data = data, ...)

    ## Extract fitted values and compute
    ## relative change value.
    fit1 <- fitted(b)
    change <- mean(abs((fit1 - fit0) / fit0))

    ## Overwrite initial fitted values.
    fit0 <- fit1

    iter <- iter + 1
  }

  ## Delete log_e2 and h from global environment.
  rm(list = c("log_e2", "h"), envir = .GlobalEnv)

  ## Even a bit more technical, change the call of the "lm"
  ## object, so we can use the predict() method.
  cl <- match.call()
  b$call$data <- m$call$data <- cl$data

  ## Return models for the mean and the variance.
  rval <- list("mean" = b, "variance" = m, "iterations" = iter)
  return(rval)
}


## Use the Breusch-Pagen test.
bp_test(stunting ~ poly(mbmi, 3) + poly(agechild, 3), data = ZambiaNutrition)

model_corr <- hetlm(stunting ~ poly(mbmi, 3) + poly(agechild, 3), data = ZambiaNutrition)
model_uncorr <- lm(stunting ~ poly(mbmi, 3) + poly(agechild, 3), data = ZambiaNutrition)

par(mfrow = c(2, 2), mar = c(4.1, 4.1, 4.1, 1.1))
termplot(model_corr$mean, term = 1, partial.resid = TRUE, se = TRUE,  main = "Corrected", pch = ".")
termplot(model_uncorr, term = 1, partial.resid = TRUE, se = TRUE,  main = "Uncorrected", pch = ".")
termplot(model_corr$mean, term = 2, partial.resid = TRUE, se = TRUE,  main = "Corrected", pch = ".")
termplot(model_uncorr, term = 2, partial.resid = TRUE, se = TRUE,  main = "Uncorrected", pch = ".")