data {
  int<lower=1> n_part;
  int<lower=1> n_trials;
  array[n_part, n_trials] int<lower=1, upper=2> choice;
  array[n_part, n_trials] int<lower=1, upper=10> R;
}

// transformed data {
// }

parameters {
  // group-level parameters
  vector[2] means_probit;
  vector<lower=0>[2] sigmas;

  // participant-level parameters
  vector[n_part] LR_probit;
  vector[n_part] inv_temp_probit;
}

transformed parameters {
  // participant-level parameters
  vector<lower=0, upper=1>[n_part] LR; 
  vector<lower=0, upper=5>[n_part] inv_temp;

  for (j in 1:n_part) {
    LR[j] = Phi_approx(means_probit[1] + sigmas[1] * LR_probit[j]);
    inv_temp[j] = Phi_approx(means_probit[2] + sigmas[2] * inv_temp_probit[j]) * 5;
  }
}

model {
  // priors
  means_probit ~ normal(0, 1);
  sigmas ~ normal(0, 0.2);
  LR_probit ~ normal(0, 1);
  inv_temp_probit ~ normal(0, 1);

  // participant loop
  for (j in 1:n_part) {
    
    // initialization
    array[n_trials, 2] real Q;
    Q[1, 1] = 8;
    Q[1, 2] = 2;
    vector[2] Q_t;
    real pred_err;

    // trial loop
    for (t in 1:n_trials) {
      Q_t = to_vector(Q[t]);

      // sample choice (1 is F, 2 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp[j] * Q_t);

      // prediction error
      pred_err = R[j, t] - Q[t, choice[j, t]];

      // update value (learn)
      if (t < n_trials) {    // no updating in the very last trial
        if (choice[j, t] == 1) {
          Q[t+1, 1] = Q[t, 1] + LR[j] * pred_err;
          Q[t+1, 2] = Q[t, 2];
        } else {
          Q[t+1, 1] = Q[t, 1];
          Q[t+1, 2] = Q[t, 2] + LR[j] * pred_err;
        }
      }
    }
  }
}

generated quantities {
  vector[2] means;
  means[1] = Phi_approx(means_probit[1]);
  means[2] = Phi_approx(means_probit[2]) * 5;
}
