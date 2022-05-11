load("MunichRent.rda")

mars <- function(y, z, degree = 3, knots = 60,
  k = log(length(y)), ...)
{
  Z <- tp(z, degree, knots)
  colnames(Z) <- paste("z", 1:ncol(Z), sep = "")
  f <- paste("y", paste(c("-1", colnames(Z)), collapse = "+"),
    sep = "~")
  f <- as.formula(f)
  d <- cbind("y" = y, as.data.frame(Z))
  b <- step(lm(f, data = d), k = k, trace = FALSE, ...)
  b$model <- cbind(b$model, "zterm" = z)
  class(b) <- c("mars", "lm")
  return(b)
}


plot.mars <- function(x, add = FALSE, ...)
{
  mf <- model.frame(x)
  y <- mf[, 1]
  z <- mf$zterm
  if(!add)
    plot(z, y, ...)
  i <- order(z)
  p <- predict(x, se.fit = TRUE,
    interval = "confidence", type = "response")
  pol <- rbind(
    cbind(z[i], p$fit[i, "lwr"]),
    cbind(rev(z[i]), rev(p$fit[i, "upr"]))
  )
  polygon(pol, col = rgb(1, 0.55, 0, alpha = 0.5), border = NA)
  lines(p$fit[i, "fit"] ~ z[i])
  invisible(NULL)
}

# MARS
## AIC penalty.
b1 <- with(MunichRent, mars(rentsqm, yearc, k = 2))
print(coef(b1))

## BIC penalty.
b2 <- with(MunichRent, mars(rentsqm, yearc, k = log(nrow(MunichRent))))
print(coef(b2))

# Plot.
par(mfrow = c(1, 2))
plot(b1, xlab = "yearc", ylab = "rentsqm", main = "MARS AIC")
plot(b2, xlab = "yearc", ylab = "rentsqm", main = "MARS BIC")