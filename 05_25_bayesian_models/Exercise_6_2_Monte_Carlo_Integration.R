set.seed(123)

f <- function(x){
  return(0.4 * dnorm(x, mean=-1, sd=1) + 0.6 * dgamma(x, shape=6, rate=1))
}

MIN_Y <- 0
MAX_Y <- 0.2
MIN_X <- -10
MAX_X <- 20

SAMPLE_SIZE <- 1000000

par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(f, MIN_X, MAX_X, ylim = c(MIN_Y, MAX_Y), lwd = 4)
rect(MIN_X, MIN_Y, MAX_X, MAX_Y, border = 4, lwd = 4)

p <- data.frame("x" = runif(SAMPLE_SIZE, MIN_X, MAX_X), "y" = runif(SAMPLE_SIZE, MIN_Y, MAX_Y))

fx <- f(p$x)
col <- rep(1, SAMPLE_SIZE)
col[p$y <= fx] <- 2
points(p, col = col)

area_rect <- (MAX_Y - MIN_Y) * (MAX_X - MIN_X)

x <- p[p$y <= fx, ]$x

mean(x)
integrate(function(x) f(x) * x, -Inf, Inf)

qx <- quantile(x, prob = c(0.05, 0.95))
print(qx)

## Add to the plot.
abline(v = qx, col = 3, lwd = 2)