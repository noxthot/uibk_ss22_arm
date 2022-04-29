## Set the seed for reproducibility.
set.seed(123)

# Load data
load("MunichRent.rda")

## Estimate linear model.
x <- MunichRent$area
y <- MunichRent$rentsqm
b_id <- lm(y ~ x)
b_log <- lm(y ~ log(x))
b_inv <- lm(y ~ I(1 / x))
b_sqrt <- lm(y ~ sqrt(x))


rsq_id <- summary(b_id)$adj.r.squared
rsq_log <- summary(b_log)$adj.r.squared
rsq_inv <- summary(b_inv)$adj.r.squared
rsq_sqrt <- summary(b_sqrt)$adj.r.squared

## Add scaled basis function.
i <- order(x)
p_id <- predict(b_id)
p_log <- predict(b_log)
p_inv <- predict(b_inv)
p_sqrt <- predict(b_sqrt)

## Scatterplot.
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)
lines(p_id[i] ~ x[i], col=2, lwd=2)
lines(p_log[i] ~ x[i], col=3, lwd=2)
lines(p_inv[i] ~ x[i], col=4, lwd=2)
lines(p_sqrt[i] ~ x[i], col=5, lwd=2)
legend("topright", c("id", "log", "inv", "sqrt"), lwd=2, col=2:5)