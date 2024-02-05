// This code was adapted from code written
// (under an MIT license) as part of the `EpiNow2`
// package (https://github.com/epiforecasts/EpiNow2)

// convolve two vectors as a backwards dot product
// y vector shoud be reversed
// limited to the length of x and backwards looking for x indexes
// designed for use convolve a case vector and a delay pmf
vector convolve_dot_product(vector x, vector y, int len) {
  int ylen = num_elements(y);
  vector[len] z;
  for (s in 1 : len) {
    z[s] = dot_product(x[max(1, s - ylen + 1) : s], tail(y, min(ylen, s)));
  }
  return z;
}
