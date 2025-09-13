N <- 250
isZero <- rbinom(N, 1, 0.15)
wh <- ifelse(isZero == 1, 0, rlnorm(N, 3.5, 0.4))

tibble(
  isZero = rbinom(N, 1, 0.15),
  wh = ifelse(isZero == 1, 0, rlnorm(N, 3.5, 0.4))
)
