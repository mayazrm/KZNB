# simulate the data
N <- 200
b <- c(0.5, 0.6, 0.7)
x1 <- rnorm(N)  # continuous predictor
x2 <- rep(0:1, each = 100)  # categorical predictor with 2 levels  
g <- rep(1:20, each = 10)
library(mvtnorm)
r <- rmvnorm(20, sigma = matrix(c(1,0.5,0.5,1), 2, 2))
e <- rnorm(N, sd = 0.5)
y <- rep(NA, 200)
for (i in 1:N) {
  y[i] <- 2 + b[1]*x1[i] + b[2]*x2[i] + b[3] * x1[i] * x2[i] +
    sum(r[g[i], ] * c(1, x1[i])) + e[i]
}
data <- data.frame(y = y, x1 = x1, x2 = factor(x2), g = g)

# fit the model
library(brms)
model <- brm(y~ x1*x2 +(1+x1|g), data = data, chains=2, cores=4)
summary(model)

# plot marginal effects separately for each level of g
plot(marginal_effects(model, conditions = data.frame(g = 1:20, x1 = 0, x2 = 1),
                      re_formula = NULL), points = TRUE)

# shows the effect x2 for some values of x1 ignoring random effects
conditions1 <- data.frame(x1 = seq(-2, 2, 0.5))
rownames(conditions1) <- paste("x1 =", conditions1$x1)
plot(marginal_effects(model, effects = "x2", conditions = conditions1),
     points = TRUE)
# shows the effect x1 for both values of x2 ignoring random effects
conditions2 <- data.frame(x2 = 0:1)
rownames(conditions2) <- paste("x2 =", conditions2$x2)
plot(marginal_effects(model, effects = "x1", conditions = conditions2,
                      re_formula = NULL), points = TRUE)

