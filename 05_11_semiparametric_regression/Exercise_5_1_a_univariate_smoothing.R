data("mcycle", package = "MASS")

## Truncated powers
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


degree <- 0:20
aic <- rep(0, length(degree))
for(i in seq_along(degree)) {
  b <- lm(accel ~ tp(times, degree = degree[i]) - 1, data = mcycle)
  aic[i] <- AIC(b)
}

## Re-estimate best fitting model.
i <- which.min(aic)
print(degree[i])

b <- lm(accel ~ tp(times, degree = degree[i]) - 1, data = mcycle)
p <- predict(b, interval = "confidence")

## Plot.
par(mfrow = c(1, 2), mar = c(4.1, 4.1, 0.5, 0.5))
plot(aic ~ degree, type = "l")
abline(v = degree[i], lty = 2, col = "lightgray")
plot(mcycle, col =  rgb(0.1, 0.1, 0.1, alpha = 0.3))
matplot(mcycle$times, p, type = "l", lty = c(1, 2, 2),
  add = TRUE, lwd = 1.5, col = 1)