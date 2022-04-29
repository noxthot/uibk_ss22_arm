## Set the seed for reproducibility.
set.seed(123)

# Load data
load("MunichRent.rda")

poly_3 <- function(x) { poly(x, 3) }

MunichRent$location <- relevel(MunichRent$location, ref = "top")  # top as intercept

## Estimate linear model.
b <- lm(rentsqm ~ poly_3(area) * location, data=MunichRent)  # location is a factor, so it transforms automatically

summary(b)

## Predict
nd <- MunichRent[, c("area", "location", "rentsqm")]

nd$location <- "normal"
nd$pnormal <- predict(b, newdata = nd, interval = "confidence")

nd$location <- "good"
nd$pgood <- predict(b, newdata = nd, interval = "confidence")

nd$location <- "top"
nd$ptop <- predict(b, newdata = nd, interval = "confidence")

## Scatterplot.
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)
lines(p[i] ~ x[i], col=2, lwd=2)

## Plot
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 0.1, 0.5))
termplot(b, partial.resid = TRUE, se = TRUE, cex = 0.1)