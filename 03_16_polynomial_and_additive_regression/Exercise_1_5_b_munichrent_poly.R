## Set the seed for reproducibility.
set.seed(123)

MAX_ORDER = 21

# Load data
load("MunichRent.rda")

## Estimate polynomial models
rsquared <- rep(NA, MAX_ORDER)

for (order in seq_len(MAX_ORDER)) {
  b <- lm(rentsqm ~ poly(area, order), data = MunichRent)
  rsquared[order] <- summary(b)$adj.r.squared
}

best_order <- which.max(rsquared)
print(paste0("Best polynomial of order ", best_order))

b <- lm(rentsqm ~ poly(area, l), data = MunichRent)

## Visualize.
par(mar = c(4.1, 4.1, 0.1, 0.1))
termplot(b, partial.resid = TRUE, se = TRUE, cex = 0.1)