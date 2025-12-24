data {
  int<lower=1> n_part;
  int<lower=1> n_trials;
  array[n_part, n_trials] int<lower=1, upper=2> choice;
  array[n_part, n_trials] int<lower=0, upper=10> R;
  int<lower=0, upper=10> initQF;
  int<lower=0, upper=10> initQU;
}

parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=5> inv_temp;
}

// transformed parameters {
// }

model {
  // this is where priors would go. leaving them empty leads to uninformative priors

  for (j in 1:n_part) {
    array[n_trials, 2] real Q;
    Q[1, 1] = initQF;
    Q[1, 2] = initQU;
    vector[2] Q_t;

    real pred_err;

    for (t in 1:n_trials) {
      Q_t = to_vector(Q[t]);

      // sample choice (1 is F, 2 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp * Q_t);

      // prediction error
      if (choice[j, t] == 1) {
        pred_err = R[j, t] - Q[t, 1];
      } else {
        pred_err = R[j, t] - Q[t, 2];
      }

      // update value (learn)
      if (t < n_trials) {    // no updating in the very last trial
        if (choice[j, t] == 1) {
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

