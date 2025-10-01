library(microbenchmark)

# Assume tauStarCompute is your optimized function
benchmark_runtime <- function(sample_sizes, reps=10) {
  results <- data.frame(SampleSize = integer(), Runtime = numeric())
  
  for (n in sample_sizes) {
    X <- matrix(rnorm(n * 3), nrow = n, ncol = 3)
    Y <- matrix(rnorm(n * 3), nrow = n, ncol = 3)
    
    mbm <- microbenchmark(
      tauStarCompute(X, Y),
      times = reps
    )
    
    avg_time <- mean(mbm$time) / 1e9  # Convert nanoseconds to seconds
    results <- rbind(results, data.frame(SampleSize = n, Runtime = avg_time))
  }
  results
}
seed <- round(runif(1, 1, 1e6)) 
set.seed(seed) 
cat("Using seed:", seed, "\n")
sample_sizes <- seq(20, 200, by=20)
runtime_results <- benchmark_runtime(sample_sizes)
print(runtime_results)
