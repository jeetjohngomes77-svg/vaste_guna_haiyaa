#Randomized Tests
# Setup for Problem 1
alpha <- 0.05
n <- 10
lambda_0 <- 1.2
lambda_T <- n * lambda_0 # T follows Poisson(12) under H0

# Find the critical value c
# We want the smallest c such that P(T > c) <= alpha
c <- qpois(1 - alpha, lambda_T)
if (1 - ppois(c, lambda_T) > alpha) {
  c <- c + 1
}

# Calculate probabilities needed for gamma
prob_greater_than_c <- 1 - ppois(c, lambda_T)
prob_equal_to_c <- dpois(c, lambda_T)

# Calculate gamma
gamma <- (alpha - prob_greater_than_c) / prob_equal_to_c

# Output results
cat("Critical Value (c):", c, "\n")
cat("Randomization Probability (gamma):", round(gamma, 4), "\n")

# Setup for Problem 2
alpha <- 0.05
N <- 10
K_0 <- 10 * 0.5 # Number of red pencils under H0
n_draw <- 4

# Cumulative probabilities under H0
# phyper(q, m (white balls), n (black balls), k (draws))
cum_probs <- phyper(0:4, K_0, N - K_0, n_draw)

# Find c for left-tailed test: smallest value where CDF > alpha
# Since R indices start at 1 (for X=0), we subtract 1
c <- min(which(cum_probs > alpha)) - 1

# Probabilities for gamma calculation
prob_less_than_c <- ifelse(c == 0, 0, phyper(c - 1, K_0, N - K_0, n_draw))
prob_equal_to_c <- dhyper(c, K_0, N - K_0, n_draw)

# Calculate gamma
gamma <- (alpha - prob_less_than_c) / prob_equal_to_c

cat("Critical Value (c):", c, "\n")
cat("Randomization Probability (gamma):", round(gamma, 4), "\n")
