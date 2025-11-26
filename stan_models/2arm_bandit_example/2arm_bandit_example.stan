// source: https://github.com/CCS-Lab/hBayesDM/blob/develop/commons/stan_files/bandit2arm_delta.stan

// data specification; relates to the format of the data that you supply
// to the model when running it
data {
  int<lower=1> N;   // number of subjects
  int<lower=1> T;   // max number of trials
  array[N] int<lower=1, upper=T> Tsubj;       // number of trials per subject
  array[N, T] int<lower=-1, upper=2> choice;  // 1 or 2
  array[N, T] real outcome;
}

// data that are not supplied to the model, but created here
transformed data {
  vector[2] initQ;  // initial Q-values for the two options: 0.0
  initQ = rep_vector(0.0, 2);
}

// parameters that the model depends on. when you run the model you
// specify initial values (i.e. priors) for these
parameters {
  // group-level mean and SD for our free parameters: LR and tau
  vector[2] mu_pr;
  vector<lower=0>[2] sigma;

  // subject-level parameters raw  (for noncentered parameterization)
  vector[N] LR_pr;   // learning rate
  vector[N] tau_pr;  // inverse temperature
}

// parameters that are not supplied to the model, but created here
transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] LR;   // theoretical bounds [0, 1]
  vector<lower=0, upper=5>[N] tau;  // "practical" bounds [0, 5]

  for (i in 1:N) {
    // Phi: normal cumulative distr
    LR[i]  = Phi_approx(mu_pr[1] + sigma[1] * LR_pr[i]);
    tau[i] = Phi_approx(mu_pr[2] + sigma[2] * tau_pr[i]) * 5;
  }
}

model {
  // PRIORS
  // group-level parameters
  mu_pr ~ normal(0, 1);
  sigma ~ normal(0, 0.2);

  // subject-level parameters
  LR_pr  ~ normal(0, 1);
  tau_pr ~ normal(0, 1);

  // MODEL
  // subject loop
  for (i in 1:N) {
    vector[2] Q; // value
    real PE;     // prediction error

    Q = initQ;

    // trial loop
    for (t in 1:(Tsubj[i])) {
      // compute action probabilities
      choice[i, t] ~ categorical_logit(tau[i] * Q);

      // prediction error
      PE = outcome[i, t] - Q[choice[i, t]];

      // value updating (learning)
      Q[choice[i, t]] += LR[i] * PE;
    }
  }
}

generated quantities {
  // for group level parameters
  real<lower=0, upper=1> mu_LR;
  real<lower=0, upper=5> mu_tau;

  // for log likelihood calculation
  array[N] real log_lik;

  // for posterior predictive check
  array[N, T] real y_pred;

  // set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_LR  = Phi_approx(mu_pr[1]);
  mu_tau = Phi_approx(mu_pr[2]) * 5;

  { // local section: variables declared here are not part of the output
    for (i in 1:N) {
      vector[2] Q;  // value
      real PE;      // prediction error

      // Initialize values
      Q = initQ;

      log_lik[i] = 0;

      for (t in 1:(Tsubj[i])) {
        // compute log likelihood of current trial
        log_lik[i] += categorical_logit_lpmf(choice[i, t] | tau[i] * Q);

        // generate posterior prediction for current trial
        y_pred[i, t] = categorical_rng(softmax(tau[i] * Q));

        // prediction error
        PE = outcome[i, t] - Q[choice[i, t]];

        // value updating (learning)
        Q[choice[i, t]] += LR[i] * PE;
      }
    }
  }
}
