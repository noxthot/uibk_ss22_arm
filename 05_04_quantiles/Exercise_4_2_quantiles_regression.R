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


library("quantreg")

options(error = function() traceback(3))

## Set the seed for reproducibility.
set.seed(123)

# Load data
load("MunichRent.rda")

tau <- c(0.01, seq(0.1, 0.9, by = 0.1), 0.99)

poly_3 <- function(x) { poly(x, 3) }

b_quantile <- rq(rent ~ poly_3(area), data=MunichRent, tau=tau)
b_hetlm <- hetlm(rent ~ poly_3(area), data=MunichRent)

## Quantile regression fit.
fit_quantile <- predict(b_quantile)

## Heteroskedastic LM fit.
b_hetlm <- predict(b_hetlm$mean)
sigma_hetlm <- exp(predict(b_hetlm$variance))

fit_hetlm <- matrix(NA, nrow = length(p2), ncol=length(tau))

for(j in seq_along(tau)) {
  fit_hetlm[, j] <- p2 + qnorm(tau[j], mean=0, sd=sqrt(sigma_hetlm))
}

## Compare.
par(mfrow = c(1, 2))

ylim <- range(c(MunichRent$rent, fit_quantile, fit_hetlm))

plot(rent ~ area, data = MunichRent, col = rgb(0.1, 0.1, 0.1, alpha=0.4),
  main = "Quantile regression", ylim = ylim)

i <- order(MunichRent$area)

matplot(MunichRent$area[i], fit1[i, ], type="l", lty=1, lwd=2, col=4, add=TRUE)

plot(rent ~ area, data = MunichRent, col=rgb(0.1, 0.1, 0.1, alpha=0.4),
  main = "Heteroskedastic LM", ylim=ylim)
i <- order(MunichRent$area)
matplot(MunichRent$area[i], fit2[i, ], type="l", lty=1,
  col=4, lwd=2, add=TRUE)