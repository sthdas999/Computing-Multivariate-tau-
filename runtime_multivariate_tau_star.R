# Install ggplot2 if not installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)
library(parallel)

# Simplified tau_star function (same as before)
tau_star <- function(X, Y) {
  n <- nrow(X)
  ranks_X <- apply(X, 2, rank)
  ranks_Y <- apply(Y, 2, rank)
  
  combs <- combn(n, 4)
  
  a_val <- function(M) {
    prod_signs <- apply(M, 2, function(col) {
      (col[1] - col[3]) * (col[2] - col[4]) - (col[1] - col[4]) * (col[2] - col[3])
    })
    sign(prod(prod_signs))
  }
  
  stat <- mclapply(1:ncol(combs), function(i) {
    idx <- combs[, i]
    ax <- a_val(ranks_X[idx, , drop = FALSE])
    ay <- a_val(ranks_Y[idx, , drop = FALSE])
    ax * ay
  }, mc.cores = detectCores())
  
  mean(unlist(stat))
}

seed <- round(runif(1, 1, 1e6))
set.seed(seed)
cat("Using seed:", seed, "\n")

sample_sizes <- c(20, 30, 40, 50, 60)  # Smaller sizes to keep runtime reasonable
p <- 3
q <- 3
n_reps <- 5

results <- data.frame(
  SampleSize = integer(),
  RuntimeSec = numeric()
)

for (n in sample_sizes) {
  cat("Measuring runtime for n =", n, "\n")
  times <- numeric(n_reps)
  for (rep in 1:n_reps) {
    X <- matrix(rnorm(n * p), n, p)
    Y <- matrix(rnorm(n * q), n, q)
    start_time <- Sys.time()
    tau_star(X, Y)
    end_time <- Sys.time()
    times[rep] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  avg_time <- mean(times)
  results <- rbind(results, data.frame(SampleSize = n, RuntimeSec = avg_time))
  cat(sprintf("Average runtime for n=%d: %.3f seconds\n", n, avg_time))
}

# Plot runtime vs sample size
p_runtime <- ggplot(results, aes(x = SampleSize, y = RuntimeSec)) +
  geom_point(size = 3, color = "darkred") +
  geom_line(color = "darkred") +
  labs(title = "Runtime of Multivariate Tau* Computation vs Sample Size",
       x = "Sample Size (n)",
       y = "Average Runtime (seconds)") +
  theme_minimal()

ggsave("runtime.png", plot = p_runtime, width = 7, height = 5)
print(p_runtime)
