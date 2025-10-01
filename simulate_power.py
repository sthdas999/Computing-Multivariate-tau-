import numpy as np
from dcor import distance_correlation_test
# For HSIC, use your preferred implementation or custom function
# tau_star_test is user-defined

def simulate_power(n, model, reps=1000):
    tau_results = []
    dcor_results = []
    hsic_results = []
    
    for _ in range(reps):
        if model == "linear":
            X = np.random.normal(size=n)
            Y = 2 * X + np.random.normal(size=n)
        elif model == "nonlinear":
            X = np.random.uniform(size=n)
            Y = np.sin(2 * np.pi * X) + np.random.normal(scale=0.3, size=n)
        
        pval_tau = tau_star_test(X, Y)
        pval_dcor = distance_correlation_test(X, Y).p_value
        pval_hsic = hsic_test(X, Y)  # Replace with your HSIC p-value function
        
        tau_results.append(pval_tau < 0.05)
        dcor_results.append(pval_dcor < 0.05)
        hsic_results.append(pval_hsic < 0.05)
    
    return {
        "tauPower": np.mean(tau_results) * 100,
        "dCorPower": np.mean(dcor_results) * 100,
        "hsicPower": np.mean(hsic_results) * 100,
    }

seed = np.random.randint(1, 1e6)
np.random.seed(seed)
print("Using seed:", seed)
print(simulate_power(100, "linear"))
print(simulate_power(100, "nonlinear"))
