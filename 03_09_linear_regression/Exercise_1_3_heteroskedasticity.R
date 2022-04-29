## Set the seed for reproducibility.
set.seed(123)

## Simulate data.
n <- 100
x <- runif(n)
y <- 2 + 1.5 * x + rnorm(n, sd = 0.2) * exp(-2 + 4 * x)

## Scatterplot.
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)

## Estimate linear model and add the fitted line
## to the scatterplot.
b <- lm(y ~ x)
abline(b)

summary(b)$sigma

img_dens <- function(object) {
  xlim <- par()$usr[1:2]
  ylim <- par()$usr[3:4]
  yp <- seq(ylim[1], ylim[2], length = 100)
  xp <- seq(xlim[1], xlim[2], length = 100)
  xm <- xp[-100] + diff(xp[1:2]) / 2
  ym <- yp[-100] + diff(yp[1:2]) / 2
  sd <- summary(object)$sigma
  p <- predict(object, newdata = data.frame("x" = xm))
  d <- matrix(NA, 99, 99)
  for(i in 1:99) {
    d[i, ] <- dnorm(ym, mean = p[i], sd = sd)
  }
  image(xm, ym, d, add = TRUE, col = rev(colorspace::heat_hcl(99)))
}

## Add density to plot
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)
img_dens(b)
abline(b)
points(x, y)