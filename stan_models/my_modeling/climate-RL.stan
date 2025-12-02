data {
  int<lower=1> n_part;
  int<lower=1> T;
  array[n_part, T] int<lower=1, upper=2> choice;
  array[n_part, T] int<lower=0, upper=10> R;
}

parameters {
  real<lower=0, upper=1> LR_raw;
  real<lower=0, upper=5> inv_temp_raw;
  real<lower=0, upper=10> initQF_raw;
  real<lower=0, upper=10> initQU_raw;
  real<lower=0, upper=10> mu_R_raw;
  real<lower=0, upper=10> sigma_R_raw;
}

transformed parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=10> inv_temp;
  real<lower=0, upper=10> initQF;
  real<lower=0, upper=10> initQU;
  real<lower=0, upper=10> mu_R;
  real<lower=0, upper=10> sigma_R;

  LR = inv_logit(LR_raw);
  inv_temp = inv_logit(inv_temp_raw) * 10.0;
  initQF = inv_logit(initQF_raw) * 10.0;
  initQU = inv_logit(initQU_raw) * 10.0;
  mu_R = inv_logit(mu_R_raw) * 10.0; 
  sigma_R = inv_logit(sigma_R_raw) * 10.0;
}

model {
  // priors: all uninformative
  LR_raw ~ normal(0, 1);
  inv_temp_raw ~ normal(0, 1);
  initQF_raw ~ normal(0, 1);   
  initQU_raw ~ normal(0, 1);   
  mu_R_raw ~ normal(0, 1);          
  sigma_R_raw ~ normal(0, 1);

  for (j in 1:n_part) {
    array[T, 2] real Q;
    Q[1, 1] = initQF;
    Q[1, 2] = initQU;
    vector[2] Q_t;

    real pred_err;

    for (t in 1:T) {
      Q_t = to_vector(Q[t]);

      // sample choice (0 is F, 1 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp * Q_t);

      // rate
      R[j, t] ~ normal(mu_R, sigma_R);

      // prediction error
      if (choice[j, t] == 0) {
        pred_err = R[j, t] - Q[t, 1];
      } else {
        pred_err = R[j, t] - Q[t, 2];
      }

      // update value (learn)
      if (t < T) {    // no updating in the very last trial
        if (choice[j, t] == 0) {
          Q[t+1, 1] = Q[t, 1] + LR * pred_err;
          Q[t+1, 2] = Q[t, 2];
        } else {
          Q[t+1, 1] = Q[t, 1];
          Q[t+1, 2] = Q[t, 2] + LR * pred_err;
        }
      }
    }
  }
}

