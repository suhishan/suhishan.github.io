N <- 250
isZero <- rbinom(N, 1, 0.15)
wh <- ifelse(isZero == 1, 0, rlnorm(N, 3.5, 0.4))

tibble(
  isZero = rbinom(N, 1, 0.15),
  wh = ifelse(isZero == 1, 0, rlnorm(N, 3.5, 0.4))
)
d_small <- d %>% filter(isZero == 0)

summary(lm(wh ~ 1, data = d))


model_1 <- brm(
  data = d,
  family = hurdle_lognormal,
  bf(wh ~ 1,
     hu ~ 1),
  prior = c(
    prior(normal(3, 0.5), class = Intercept, lb = 0),
    prior(normal(0, 1), dpar = "hu", class = Intercept),
    prior(exponential(4), class = sigma)
  ),
  iter = 2e3, warmup = 1e3, chains = 4, cores = 4,
  sample_prior = T,
  file = "fits/model_1"
  
)

# Looking at the priors:

prior <- prior_draws(model_1)
prior %>% 
  mutate(mu_original_scale = exp(Intercept + sigma^2 )) 
  ggplot(aes(x = mu_original_scale))+
  geom_histogram(bins = 30)


model_2 <- update(
  model_1,
  newdata = d_small
)



# Thinking about the mean and variance of a log normal distribution.

x <- rlnorm(N, 2, 1)

# Prior Predictive Simulation
tibble(
  Intercept = rnorm(N, 3, 0.5),
  sigma = rexp(N, 4)
) %>% 
  ggplot(aes(x = exp(Intercept + (sigma^2)/2)))+
  geom_histogram(bins = 100)+
  coord_cartesian(xlim = c(0, 100))

tibble(
  o = rnorm(N, -1, 1),
  probs = inv_logit_scaled(o)
) %>% 
  ggplot(aes(x = probs))+
  geom_histogram(bins = 100)

