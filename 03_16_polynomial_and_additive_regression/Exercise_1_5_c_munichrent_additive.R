## Set the seed for reproducibility.
set.seed(123)

MAX_ORDER = 21

# Load data
load("MunichRent.rda")

trans_fun <- list(
                "log" = log,
                "sqrt" = sqrt,
                "exp" = exp,
                "inverse" = function(x) { 1 / x }
             )


for (order in seq_len(MAX_ORDER)) {
  poly_fun <- paste0("function(x) { poly(x,", order, ") }")
  trans_fun[[paste0("poly_", order)]] <- eval(parse(text = poly_fun))
}

## Get all combinations for Y() and B().
combos <- expand.grid(
  "f1" = names(trans_fun),
  "f2" = names(trans_fun),
  stringsAsFactors = FALSE
)

MunichRent$area_s <- scale(MunichRent$area)
MunichRent$yearc_s <- scale(MunichRent$yearc)


## Estimate polynomial models
rsquared <- rep(0, nrow(combos))

for (i in seq_len(nrow(combos))) {
  f1 <- trans_fun[[combos[i, "f1"]]]
  f2 <- trans_fun[[combos[i, "f2"]]]
  b <- lm(rentsqm ~ f1(area_s) + f2(yearc_s), data = MunichRent)
  rsquared[i] <- summary(b)$adj.r.squared
}

best_comb_idx <- which.max(rsquared)
print(paste0("Best combination for ", best_comb_idx))
print(combos[best_comb_idx, ])
print(rsquared[best_comb_idx])

best_f1 <- trans_fun[[combos[best_comb_idx, "f1"]]]
best_f2 <- trans_fun[[combos[best_comb_idx, "f2"]]]

b <- lm(rentsqm ~ best_f1(area_s) + best_f2(yearc_s), data = MunichRent)

## Visualize.
par(mfrow = c(1, 2), mar = c(4.1, 4.1, 0.1, 0.5))
termplot(b, partial.resid = TRUE, se = TRUE, cex = 0.1)