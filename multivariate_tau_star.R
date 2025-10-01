# Install ggplot2 if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load necessary libraries
library(ggplot2)
library(parallel)

# Define the multivariate tau* function (simplified version)
tau_star <- function(X, Y) {
  n <- nrow(X)
  ranks_X <- apply(X, 2, rank)
  ranks_Y <- apply(Y, 2, rank)
  
  combs <- combn(n, 4)
  
  a_val <- function(M) {
    # M is a 4 x p or 4 x q matrix of ranks for one quadruple
    prod_signs <- apply(M, 2, function(col) {
      (col[1] - col[3]) * (col[2] - col[4]) - (col[1] - col[4]) * (col[2] - col[3])
    })
    sign(prod(prod_signs))
  }
  
  # Use parallel processing over all quadruples
  stat <- mclapply(1:ncol(combs), function(i) {
    idx <- combs[, i]
    ax <- a_val(ranks_X[idx, , drop = FALSE])
    ay <- a_val(ranks_Y[idx, , drop = FALSE])
    ax * ay
  }, mc.cores = detectCores())
  
  mean(unlist(stat))
}

# Set seed for reproducibility
seed <- round(runif(1, 1, 1e6))
set.seed(seed)
cat("Using seed:", seed, "\n")

# Simulation parameters
n_replications <- 100
n <- 100
p <- 2
q <- 2

tau_star_values <- numeric(n_replications)

# Run simulation under null hypothesis (X independent of Y)
for (r in 1:n_replications) {
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * q), n, q)
  tau_star_values[r] <- tau_star(X, Y)
  cat("Replication:", r, "tau* =", tau_star_values[r], "\n")
}

# Prepare data frame for plotting
df <- data.frame(tau_star = tau_star_values)

# Plot histogram of tau* values under null
p1 <- ggplot(df, aes(x = tau_star)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = expression(paste("Null Distribution of Multivariate ", tau^"*")),
       x = expression(tau^"*"),
       y = "Frequency") +
  theme_minimal()

# Save plot as PNG file
ggsave("null_distribution.png", plot = p1, width = 7, height = 5)

# Also display plot in R session
print(p1)
