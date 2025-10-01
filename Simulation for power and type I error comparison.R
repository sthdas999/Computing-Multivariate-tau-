library(energy)    # for dcor
library(kernlab)   # for HSIC
# Assume tauStarTest is your custom function for multivariate tau*

simulate_power <- function(n, scenario, reps=1000) {
  power_tau <- power_dcor <- power_hsic <- numeric(reps)
  
  for (i in 1:reps) {
    if (scenario == "nonlinear") {
      X <- runif(n)
      Y <- sin(X) + rnorm(n, sd=0.3)
    } else if (scenario == "independent") {
      X <- runif(n)
      Y <- runif(n)
    }
    
    power_tau[i] <- tauStarTest(X, Y)$p.value < 0.05
    power_dcor[i] <- dcor.test(X, Y)$p.value < 0.05
    power_hsic[i] <- hsic.test(X, Y)@p.value < 0.05
  }
  
  c(
    tauPower = mean(power_tau) * 100,
    dcorPower = mean(power_dcor) * 100,
    hsicPower = mean(power_hsic) * 100
  )
}

# Example usage:
seed <- round(runif(1, 1, 1e6)) 
set.seed(seed) 
cat("Using seed:", seed, "\n")
simulate_power(100, "nonlinear")
