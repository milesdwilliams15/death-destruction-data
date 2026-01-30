# Extra helpers ----

## mean_ci_boot() ----

mean_ci_boot <- function(var, ci = .95) {
  vbar <- mean(var, na.rm = T)
  vboot <- replicate(
    n = 1000,
    expr = mean(sample(var, length(var), T), na.rm = T)
  )
  data.frame(
    mean = vbar,
    lower = quantile(vboot, .5 - ci / 2),
    upper = quantile(vboot, .5 + ci / 2)
  )
}