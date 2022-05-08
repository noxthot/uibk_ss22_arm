## Set the seed.
set.seed(456)

## Number of observations.
n <- 200

## Simulate simple regression model
## with heteroskedastic error term.
x <- runif(n, -1, 1)
y <- 1.2 + 0.1 * x + rnorm(n, sd = exp(-1 + 2 * x))

## Visualize.
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)

## Model without accounting for heteroskedasticity.
b0 <- lm(y ~ x)
summary(b0)

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

bp_test(y ~ x)