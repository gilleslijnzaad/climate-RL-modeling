// we run the model again, this time not adding to the log posterior, but generating a posterior predictive distribution for choice
generated quantities {
  real log_lik;
  array[T] real Q_pred;
  array[T] real choice_pred;

  { // local section: variables declared here are not part of the output
    array[T] real R;
    for (t in 1:T) {
      // sample choice (0 is F, 1 is U) via softmax
      choice_pred[t] = categorical_rng(softmax(inv_temp * Q[t]));

      // rate
      R[t] = normal_rng(mu_R, sigma_R);  // does this need to be truncated still?

      // prediction error
      pred_err = R[t] - Q[t, choice[t]];

      // update value (learn)
      if (choice[t] == 0) {
        Q_pred[t + 1, 0] = Q_pred[t, 0] + LR * pred_err;
        Q_pred[t + 1, 1] = Q_pred[t, 1];
      } else {
        Q_pred[t + 1, 0] = Q_pred[t, 0];
        Q_pred[t + 1, 1] = Q_pred[t, 1] + LR * pred_err;
      }
    }
  }
}