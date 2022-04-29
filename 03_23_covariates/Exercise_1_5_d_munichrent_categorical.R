## Set the seed for reproducibility.
set.seed(123)

# Load data
load("MunichRent.rda")

poly_5 <- function(x) { poly(x, 5) }

MunichRent$location <- relevel(MunichRent$location, ref = "top")  # top as intercept

## Estimate linear model.
f1 <- poly_5
f2 <- poly_5

b <- lm(rentsqm ~ f1(area) + f2(yearc) + location, data = MunichRent)  # location is a factor, so it transforms automatically

summary(b)

## Plot
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 0.1, 0.5))
termplot(b, partial.resid = TRUE, se = TRUE, cex = 0.1) 