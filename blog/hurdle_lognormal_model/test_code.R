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

# Group-Wise Simulation

a_0 <-  3.7
a_post <-  -0.05
a_treat <-  -0.05
a_pt <- -0.2

g <- tibble(
  post = rep(c(0, 1), each = 500), # time period 0 is before and time period 1 is after.
  treat = rep(c(0, 1), times = 500 ), # 0 is no-war, 1 is war.
  isZero = rbinom(N, 1, prob = 0.1 + 0.02 * post  + 
                    (0.03) * treat + 0.1 * (post * treat)),
  
  wh = (1 - isZero) * rlnorm(N, a_0 + a_post * post + 
                               a_treat * treat + a_pt * (post * treat),
                             sdlog = 0.4)
)

g %>% group_by(treat, post) %>% summarize(mean(isZero))
g %>% group_by(treat, post) %>% summarize(mean(wh))
sd(g$wh)

#Understanding log normal stuff.

# Let's make charts to show this stuff.


g <- g %>% mutate(
  group = factor(1 + post + 2 * treat, 
                 labels = c("Pre-Control", "Post-Control",
                            "Pre-Treatment","Post-Treatment"))
)

g %>% 
  group_by(group) %>% 
  summarize(avg_wh = mean(wh)) %>% 
  ggplot(aes(x = group, y = avg_wh))+
  geom_linerange(aes(ymin = 0, ymax = avg_wh),color = "black",
           linewidth = 1)


g %>% 
  group_by(group) %>% 
  summarize(avg_isZero = mean(isZero)) %>% 
  ggplot(aes(x = group, y = avg_isZero))+
  geom_linerange(aes(ymin = 0, ymax = avg_isZero),color = "black",
           linewidth = 1)

# A normal OLS model.

ols_group <- lm(wh ~ 0 + group, data =g)
summary(ols_group)

g %>% 
  filter(isZero !=1) %>% 
  group_by(group) %>% 
  summarise(mean(wh))



