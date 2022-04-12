#' Simulate the two-sample t-test with equal variances
#'
#' @param rep number of replicates
#' @param mean1 true mean of the first population
#' @param mean2 true mean of the second population
#' @param n1 sample size drawn from the first population
#' @param n2 sample size drawn from the second population
#' @param sd standard deviation for both populations
#'
#' @importFrom tibble tibble
simulate_t = function(rep, mean1, mean2, n1, n2, sd) {
  tibble(t = replicate(rep, {
    t_sim(mean1, mean2, n1, n2, sd)
  }))
}

t_sim = function(mean1, mean2, n1, n2, sd) {
  samp1 = rnorm(n1, mean1, sd)
  samp2 = rnorm(n2, mean2, sd)
  xbar1 = mean(samp1)
  xbar2 = mean(samp2)
  diff_xbar = xbar1 - xbar2
  s1 = sd(samp1)
  s2 = sd(samp2)
  s = sig_p(n1, n2, s1, s2)
  #t.test(samp1, samp2, var.equal = TRUE)
  t_stat(diff_xbar, diff_mean = 0, n1, n2, s)
}

sig_p = function(n1, n2, s1, s2) {
  sqrt(((n1 - 1) * (s1^2) + (n2 - 1) * (s2^2)) / (n1 + n2 - 2))
}

t_stat = function(diff_xbar, diff_mean, n1, n2, sig_pool) {
  (diff_xbar - diff_mean) / (sig_pool * sqrt((1 / n1) + (1 / n2)))
}
