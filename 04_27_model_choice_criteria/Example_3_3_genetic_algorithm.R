## First, load the data.
load("UsedCars.rda")

## The base formula.
f <- price ~ poly(age, 3) + poly(kilometer, 3) +
  poly(TIA, 3) + abs + sunroof

## Now, create a big formula with all pairwise interactions.
f <- update(f, . ~ .^2)
print(f)

## Function to create a new formula based on the
## original large formula f from above.
make_formula <- function(formula, which) {
  tl <- attr(terms(formula), "term.labels")
  if(length(which)) {
    tl <- tl[which]
    tl <- paste(tl, collapse = "+")
    f <- as.formula(paste("price", tl , sep = "~"))
  } else {
    f <- price ~ 1
  }
  return(f)
}

## The final fitness function. The function
## gets a binary string, creates a new formula,
## estimates a linear model and returns the
## negative BIC, since ga() searches for the
## maximum.
fitness <- function(string) {
  i <- which(string == 1)
  f <- make_formula(f, i)
  b <- lm(f, data = UsedCars)
  return(-1 * BIC(b))
}

nterms <- length(attr(terms(f), "term.labels"))
print(nterms)

## Load the package.
library("GA")

## Set the seed.
set.seed(123)

## Start the GA.
b <- ga("binary", fitness = fitness, nBits = nterms)

## Extract the solution.
i <- which(b@solution == 1)

## Create the final formula
f <- make_formula(f, i)
print(f)

## Re-estimate the model.
b <- lm(f, data = UsedCars)

## Plot effects.
par(mfrow = c(1, 2), mar = c(4.1, 4.1, 0.1, 1.1))
termplot(b, partial.resid = TRUE, se = TRUE)