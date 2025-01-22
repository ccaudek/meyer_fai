data {
  int<lower=1> N; // Observations
  int<lower=1> K; // Categories
  int<lower=1> P; // Covariates
  array[N] int<lower=1, upper=K> y; // Response
  matrix[N, P] X; // Predictors
  vector[N] weights; // Observation weights
}
parameters {
  vector[P] beta_common; // Shared effects, dimension: P
  matrix[K, P] beta_raw; // Deviations, dimension: K x P
  vector[K] alpha; // Intercepts, dimension: K
}
model {
  // Prior on deviations
  for (k in 1 : K) {
    beta_raw[k] ~ normal(0, 1);
  }
  
  // Priors on other parameters
  beta_common ~ normal(0, 2);
  alpha ~ normal(0, 2);
  
  // Likelihood
  for (n in 1 : N) {
    // beta_common' is now (1 x P), so rep_matrix(beta_common', K) = (K x P)
    // beta_raw is also (K x P), so their addition is (K x P)
    // X[n]' is (P x 1), so (K x P) * (P x 1) = (K x 1)
    vector[K] logits = alpha
                       + (beta_raw + rep_matrix(beta_common', K)) * X[n]';
    target += weights[n] * categorical_logit_lpmf(y[n] | logits);
  }
}
