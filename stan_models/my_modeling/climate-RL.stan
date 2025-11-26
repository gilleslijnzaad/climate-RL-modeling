// CURRENT version 1.0: just one participant, and ratings go from 0 to 10
// next version 1.1: ratings go from 1 to 10

// next version 2.0: multiple participants. we supply the group-level means in the parameters block, and then draw them for each participant in the transformed parameters block

// random idea: if we are interested in values for pred_err we should make it a vector

// we supply to the model:
// (1) the number of trials
// (2) the choices that a participant made
// (3) the ratings (analogous to "outcome" in the 2arm_bandit example)
data {
  int<lower=1> T;
  array[T] int<lower=1, upper=2> choice;
  array[T] int<lower=0, upper=10> R;
}

// we have five free parameters, of which 1 vector: 
// (1) learning rate 
// (2) inverse temperature 
// (3) initial Q for friendly and unfriendly, respectively
// (4) rating mean 
// (5) rating std dev
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

  array[T, 2] real Q;
  Q[1, 1] = initQF;
  Q[1, 2] = initQU;
  vector[2] Q_t;

  real pred_err;

  // model
  for (t in 1:T) {
    Q_t = to_vector(Q[t]);
    // sample choice (0 is F, 1 is U) via softmax
    choice[t] ~ categorical_logit(inv_temp * Q_t); // ??

    // rate
    R[t] ~ normal(mu_R, sigma_R);  // does this need to be truncated still?

    // prediction error
    if (choice[t] == 0) {
      pred_err = R[t] - Q[t, 1];
    } else {
      pred_err = R[t] - Q[t, 2];
    }

    // update value (learn)
    if (t < T) {
      if (choice[t] == 0) {
        Q[t+1, 1] = Q[t, 1] + LR * pred_err;
        Q[t+1, 2] = Q[t, 2];
      } else {
        Q[t+1, 1] = Q[t, 1];
        Q[t+1, 2] = Q[t, 2] + LR * pred_err;
      }
    }

    // I hope this below works too, would be more elegant
    // Q[t + 1, choice[t]] = Q[t, choice[t]] + LR * pred_error
    // Q[t + 1, !choice[t]] = Q[t, !choice[t]]
  }
}
