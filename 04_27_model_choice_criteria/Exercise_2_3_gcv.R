## Set the seed for reproducibility.
set.seed(123)

MAX_ORDER = 21

GCV <- function(object) {
    H <- influence(object)$hat
    I <- diag(dim(H)[1])
    tr <- sum(1 - H)
    v0 <- length(object$residuals) * sum(object$residuals ^ 2) / (tr ^ 2)

    return(v0)
}

# Load data
load("MunichRent.rda")

## Estimate polynomial models
rsquared <- rep(NA, MAX_ORDER)
gcv <- rep(NA, MAX_ORDER)

for (order in seq_len(MAX_ORDER)) {
  model <- lm(rentsqm ~ poly(area, order), data = MunichRent)
  rsquared[order] <- summary(model)$adj.r.squared
  gcv[order] <- GCV(model)
}

best_order_rsqr <- which.max(rsquared)
best_order_gcv <- which.min(gcv)
print(paste0("Best polynomial according to r squared of order ", best_order_rsqr))
print(paste0("Best polynomial according to generalized cross validation of order ", best_order_gcv))

model_best_gcv <- lm(rentsqm ~ poly(area, best_order_gcv), data = MunichRent)
model_best_rsqr <- lm(rentsqm ~ poly(area, best_order_rsqr), data = MunichRent)

## Plot.
par(mfrow = c(1, 2), mar = c(4.1, 4.1, 0.5, 0.7))
plot(gcv ~ seq_len(MAX_ORDER), type = "l")
abline(v = which.min(gcv), lty = 2, col = "lightgray") 
plot(rentsqm ~ area, data = MunichRent,
  col = rgb(0.1, 0.1, 0.1, alpha = 0.2), cex = 0.3)
i <- order(MunichRent$area)
p_gcv <- predict(model_best_gcv, interval = "confidence")
p_rsqr <- predict(model_best_rsqr, interval = "confidence")
matplot(MunichRent$area[i], p_gcv[i, ], type = "l",
  lty = c(1, 2, 2), col = 2, add = TRUE, lwd = c(2, 1, 1))
matplot(MunichRent$area[i], p_rsqr[i, ], type = "l",
  lty = c(1, 2, 2), col = 3, add = TRUE, lwd = c(2, 1, 1))