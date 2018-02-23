data {
    int<lower=0> N; // Number of binomial samples
    int<lower=0> n[N]; // Number of trials per sample
    int<lower=0> y[N]; // Number of deaths
    vector[N] x; // Concentration
}

transformed data {
    vector[N] centered_x;
    real mean_x;
    mean_x = mean(x);
    centered_x = x - mean_x;
}

parameters {
    real alpha_star;
    real beta;
}

transformed parameters {
    vector[N] linpred;
    linpred = alpha_star + beta * centered_x;
}

model {
  alpha_star ~ normal(0.0, 1.0E4); // prior for alpha_star
  beta ~ normal(0.0, 1.0E4); // prior for beta
  y ~ binomial_logit(n, linpred);  // model for y
}

generated quantities {  // these are optional - just of interest
  real alpha; 
  real p[N];
  real yhat[N];
  for (i in 1:N)  {
    p[i] = inv_logit(linpred[i]);
    yhat[i] = p[i]*n[i];  // fitted values
  }
  alpha = alpha_star - beta*mean_x; // original alpha
} 

