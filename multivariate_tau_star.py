import numpy as np
from itertools import combinations
from joblib import Parallel, delayed

def a_val(M):
    diff = M[:2, :] - M[2:, :]
    return np.sign(np.linalg.det(diff))

def tau_star(X, Y, n_jobs=4):
    n = X.shape[0]
    X_rank = np.argsort(np.argsort(X, axis=0), axis=0)
    Y_rank = np.argsort(np.argsort(Y, axis=0), axis=0)
    quadruples = list(combinations(range(n), 4))

    def compute(idx):
        i, j, k, l = idx
        ax = a_val(X_rank[[i, j, k, l], :])
        ay = a_val(Y_rank[[i, j, k, l], :])
        return ax * ay

    values = Parallel(n_jobs=n_jobs)(delayed(compute)(idx) for idx in quadruples)
    return np.mean(values)
