n <- 1
m <- 450
M <- 1      ## Huber threshold
p <- 0.1    ## Fraction of responses with sign flipped

## Generate problem data
set.seed(1289)
beta_true <- 5 * matrix(stats::rnorm(n), nrow = n)
X <- matrix(stats::rnorm(m * n), nrow = m, ncol = n)
y_true <- X %*% beta_true
eps <- matrix(stats::rnorm(m), nrow = m)

#We will randomly flip the sign of some responses to illustrate the robustness.

factor <- 2*stats::rbinom(m, size = 1, prob = 1-p) - 1
y <- factor * y_true + eps


#We can solve this problem both using ordinary least squares and huber regression to compare.

beta <- Variable(n)
rel_err <- norm(beta - beta_true, "F") / norm(beta_true, "F")

## OLS
obj <- sum((y - X %*% beta)^2)
prob <- Problem(Minimize(obj))
result <- solve(prob)
beta_ols <- result$getValue(beta)
err_ols <- result$getValue(rel_err)

## Solve Huber regression problem
obj <- sum(CVXR::huber(y - X %*% beta, M))
prob <- Problem(Minimize(obj))
result <- solve(prob)
beta_hub <- result$getValue(beta)
err_hub <- result$getValue(rel_err)
beta_hub
beta_true
beta_ols

err_hub
err_ols
