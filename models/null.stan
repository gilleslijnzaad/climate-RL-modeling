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
  vector[1] means_probit;
  vector<lower=0>[1] sigmas;

  // participant-level parameters
  vector[n_part] inv_temp_probit;
}

transformed parameters {
  // participant-level parameters
  vector<lower=0, upper=5>[n_part] inv_temp;

  for (j in 1:n_part) {
    inv_temp[j] = Phi_approx(means_probit[2] + sigmas[2] * inv_temp_probit[j]) * 5;
  }
}

model {
  // priors
  means_probit ~ normal(0, 1);
  sigmas ~ normal(0, 0.2);
  
  inv_temp_probit ~ normal(0, 1);

  // participant loop
  for (j in 1:n_part) {

    // trial loop
    for (t in 1:n_trials) {
      Q_t = to_vector(Q[t]);

      // sample choice (1 is F, 2 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp[j] * Q_t);
    }
  }
}

generated quantities {
  vector[4] means;
  means[1] = Phi_approx(means_probit[1]);
  means[2] = Phi_approx(means_probit[2]) * 5;
  means[3] = Phi_approx(means_probit[3]) * 9 + 1;
  means[4] = Phi_approx(means_probit[4]) * 9 + 1;
}
