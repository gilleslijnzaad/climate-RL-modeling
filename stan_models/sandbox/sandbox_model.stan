data {
  int<lower=1> N; // participants
  int<lower=1> T; // trials
  array[N, T] int<lower=0, upper=2> choice;
  array[N, T] int<lower=0, upper=10> outcome;
}

transformed data {
  vector[2] initQ;  // initial Q-values for the two options: 0.0
  initQ = rep_vector(5.0, 2);
}

parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=5> inv_temp;
}

model {
  // PRIORS
  // leave empty to use uninformative priors

  // MODEL
  // subject loop
  for (i in 1:N) {
    vector[2] Q; // value
    real PE;     // prediction error

    Q = initQ;

    // trial loop
    for (t in 1:T) {
      // compute action probabilities
      choice[i, t] ~ categorical_logit(inv_temp * Q);

      // prediction error
      PE = outcome[i, t] - Q[choice[i, t]];

      // value updating (learning)
      Q[choice[i, t]] += LR * PE;
    }
  }
}

generated quantities {
  array[T] real choice_pred;
  vector[2] Q;

  { // local section: variables declared here are not part of the output
    real PE;      // prediction error
    int i = 1;

    // Initialize values
    Q = initQ;

    for (t in 1:T) {

      // generate posterior prediction for current trial
      choice_pred[t] = categorical_rng(softmax(inv_temp * Q));

      // prediction error
      PE = outcome[i, t] - Q[choice[i, t]];

      // value updating (learning)
      Q[choice[i, t]] += LR * PE;
    }
  }
}
