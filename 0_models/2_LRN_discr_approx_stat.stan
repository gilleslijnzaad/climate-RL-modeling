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
  vector[5] means_probit;
  vector<lower=0>[5] sigmas;

  // participant-level parameters
  vector[n_part] LR_conf_probit;
  vector[n_part] LR_disconf_probit;
  vector[n_part] inv_temp_probit;
  vector[n_part] initQF_probit;
  vector[n_part] initQU_probit;
}

transformed parameters {
  // participant-level parameters
  vector<lower=0, upper=1>[n_part] LR_conf; 
  vector<lower=0, upper=1>[n_part] LR_disconf; 
  vector<lower=0, upper=5>[n_part] inv_temp;
  vector<lower=1, upper=10>[n_part] initQF;
  vector<lower=1, upper=10>[n_part] initQU;

  for (j in 1:n_part) {
    LR_conf[j] = Phi_approx(means_probit[1] + sigmas[1] * LR_conf_probit[j]);
    LR_disconf[j] = Phi_approx(means_probit[2] + sigmas[2] * LR_disconf_probit[j]);
    inv_temp[j] = Phi_approx(means_probit[3] + sigmas[3] * inv_temp_probit[j]) * 5;
    initQF[j] = Phi_approx(means_probit[4] + sigmas[4] * initQF_probit[j]) * 9 + 1;
    initQU[j] = Phi_approx(means_probit[5] + sigmas[5] * initQU_probit[j]) * 9 + 1;
  }
}

model {
  real margin = 2;
  
  // priors
  means_probit ~ normal(0, 1);
  sigmas ~ normal(0, 0.2);
  
  LR_conf_probit ~ normal(0, 1);
  LR_disconf_probit ~ normal(0, 1);
  inv_temp_probit ~ normal(0, 1);
  initQF_probit ~ normal(0, 1); 
  initQU_probit ~ normal(0, 1); 

  // participant loop
  for (j in 1:n_part) {
    
    // initialization
    array[n_trials, 2] real Q;
    Q[1, 1] = initQF[j];
    Q[1, 2] = initQU[j];
    vector[2] Q_t;
    real pred_err;
    real LR;

    // trial loop
    for (t in 1:n_trials) {
      Q_t = to_vector(Q[t]);

      // sample choice (1 is F, 2 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp[j] * Q_t);

      // prediction error
      pred_err = R[j, t] - Q[t, choice[j, t]];

      // update value (learn)
      if (t < n_trials) {    // no updating in the very last trial

        // choice is F
        if (choice[j, t] == 1) {
          if (abs(R[j, t] - initQF[j]) <= margin) {
            LR = LR_conf[j];
          } else {
            LR = LR_disconf[j];
          }
          Q[t+1, 1] = Q[t, 1] + LR * pred_err;
          Q[t+1, 2] = Q[t, 2];
        } 
        
        // choice is U
        else {
          if (abs(R[j, t] - initQU[j]) <= margin) {
            LR = LR_conf[j];
          } else {
            LR = LR_disconf[j];
          }
          Q[t+1, 1] = Q[t, 1];
          Q[t+1, 2] = Q[t, 2] + LR * pred_err;
        }
      }
    }
  }
}

generated quantities {
  vector[5] means;
  means[1] = Phi_approx(means_probit[1]);
  means[2] = Phi_approx(means_probit[2]);
  means[3] = Phi_approx(means_probit[3]) * 5;
  means[4] = Phi_approx(means_probit[4]) * 9 + 1;
  means[5] = Phi_approx(means_probit[5]) * 9 + 1;
}
