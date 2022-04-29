## Set the seed for reproducibility.
set.seed(123)

# Load data
load("UsedCars.rda")

pred_inv <- function(object, y_fun) {
  yinv_fun <- switch(y_fun,
    "exp" = log,
    "log" = exp,
    "sqrt" = function(x) x^2,
    "inverse" = function(x) { 1 / x },
    "quadratic" = sqrt
  )

  p <- yinv_fun(predict(object))

  return(p)
}

## Function to compute the MAPE (mean absolute prediction error)
mape_fun <- function(object, y_fun) {
  warn <- getOption("warn")
  options("warn" = -1)
  on.exit(options("warn" = warn))

  p <- pred_inv(object, y_fun)

  y <- UsedCars$price

  mape <- mean(abs((p - y) / y), na.rm=TRUE)

  return(mape)
}

trans_fun <- list(
                "exp" = exp,
                "log" = log, 
                "sqrt" = sqrt,
                "inverse" = function(x) { 1 / x },
                "quadratic" = function(x) x ^ 2,
             )


## Get all combinations for Y() and B().
combos <- expand.grid(
  "Y" = names(trans_fun),
  "B" = names(trans_fun),
  stringsAsFactors = FALSE
)

print(combos)


## Estimate models and extract the adjusted R-squared
## to find the best fitting combination.
mape <- rep(NA, nrow(combos))

for(i in seq_len(nrow(combos))) {
  y <- trans_fun[[combos[i, "Y"]]](UsedCars$price)
  x <- trans_fun[[combos[i, "B"]]](UsedCars$age)
  b <- lm(y ~ x)
  mape[i] <- mape_fun(b, combos[i, "Y"])
}

## Extract the best fitting combination.
i <- which.min(mape)
print(combos[i, ])


## Reestimate model and plot with best fitting model
y <- trans_fun[[combos[i, "Y"]]](UsedCars$price)
x <- trans_fun[[combos[i, "B"]]](UsedCars$age)
b <- lm(y ~ x)
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(y ~ x, xlab = "sqrt(age)", ylab = "1/price")
abline(b, lwd = 2)

## Predict on inverse scale.
p <- pred_inv(b, combos[i, "B"])

## Plot again
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(price ~ age, data = UsedCars,
  ylim = range(c(UsedCars$price, p)))
i <- order(UsedCars$age)
lines(p[i] ~ age[i], data = UsedCars, lwd = 2)
