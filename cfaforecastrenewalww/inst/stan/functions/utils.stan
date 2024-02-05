// Functions to convert natural scale means to corresponding lognormal means
real convert_to_logmean(real mean, real sd) {
  real logmean;
  logmean = log(mean ^ 2 / sqrt(sd ^ 2 + mean ^ 2));
  return logmean;
}

real convert_to_logsd(real mean, real sd) {
  real logsd;
  logsd = sqrt(log(1 + (sd ^ 2 / mean ^ 2)));
  return logsd;
}
