library(tidyverse)
library(conflicted)
library(brms)
library(dagitty)
library(ggdag)


# Model Dag.

model_dag <- dagitty('dag{
CD -> CDP;
CI -> CD;
E -> CI;
E -> F;
F -> CI;
P -> CD;
P -> CDP;
}')

dag <- dagitty('dag{
CD -> CDP;
CI -> CD;
E -> CI;
E -> F;
F -> CI;
P -> CD;
P -> CDP;
F -> P;
}')

coordinates(model_dag) <- list(
  x = c(F = 0, E = 0.5,  CI = 1, CD = 2, P = 2.5,  CDP = 3),
  y = c(F = 1, E = 0,  CI = 1, CD = 1, P = 0 ,  CDP = 1)
)

ggdag(model_dag)+
  theme_dag_blank()

adjustmentSets(dag, exposure = "F", outcome  = "CDP")
?adjustmentSets

nd <- d %>% mutate(e_s2 = e^2,
                   f = ifelse(f > 3, 2.14, f) )
m3 <- brm(
  data = ,
  family = gaussian,
  cd ~ 1 + f + e,
  prior = c(
    prior(normal(0, 0.2), class = Intercept),
    prior(normal(0, 0.5), class = b)
  ),
  iter = 2e3, warmup = 1e3, chains = 4, cores = 8,
  fit = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m2"
)

m1 <- update(m1,
             newdata = nd,
             file = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m1" )

m2 <- update(m2,
             newdata = nd,
             file = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m2" )

m3 <- update(m1, 
             newdata = nd,
             formula = cd ~ 1 + e)


nd %>% ggplot(aes(x = e, y = f))+
  geom_point()

m4 <- update(m1,
             newdata = nd,
             formula = cd ~ 1 + f + e_s2)
print(m4)



nd <- nd %>% 
  mutate(pov = rethinking::standardize(pov_rate),
         cdp = rethinking::standardize( (best_est/TotalPopn)* 10000))

mpov <- update(m1,
               formula = cdp ~ 1 + f,
               newdata = nd)

print(mpov)

plot(nd$f, nd$cdp)




c_model <- bf(cdp ~ 1 + f + e)
e_model <- bf(f ~ 1 + e)
e_model_sq <- bf(f ~ 1 + e_s2)





m3 <- brm(
  data = nd,
  family = gaussian,
  c_model + e_model + set_rescor(FALSE),
  
  prior = c(
    prior(normal(0, 0.2), class = Intercept, resp = cdp),
    prior(normal(0, 0.5), class = b, resp = cdp),
    prior(exponential(1), class = sigma, resp = cdp),
    
    prior(normal(0, 0.2), class = Intercept, resp = f),
    prior(normal(0, 0.5), class = b, resp = f),
    prior(exponential(1), class = sigma, resp = f)),
  iter = 2000, warmup = 1000, seed = 5, cores = 4, chains = 4,
  fit = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m3"
  
)

m4 <- brm(
  data = nd,
  family = gaussian,
  c_model + e_model_sq + set_rescor(FALSE),
  
  prior = c(
    prior(normal(0, 0.2), class = Intercept, resp = cdp),
    prior(normal(0, 0.5), class = b, resp = cdp),
    prior(exponential(1), class = sigma, resp = cdp),
    
    prior(normal(0, 0.2), class = Intercept, resp = f),
    prior(normal(0, 0.5), class = b, resp = f),
    prior(exponential(1), class = sigma, resp = f)),
  iter = 2000, warmup = 1000, seed = 5, cores = 4, chains = 4,
  fit = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m4"
  
)

# f on e linear

m1.1 <- update(m1, 
               formula = f ~ 1 + e,
               newdata = nd)

fitted_1.1 <- fitted(m1.1) %>% 
  data.frame() %>% 
  bind_cols(nd)

# Plot the linear relationship and how it seems implausible.
nd %>% 
  ggplot(aes(x = e, y = f))+
  geom_point()+
  geom_smooth(stat = "identity",
              data = fitted_1.1,
              aes(y = Estimate, ymax = Q97.5, ymin = Q2.5))

# residuals
r_1 <- residuals(m1.1)
nd$r_1 <- r_1[,1]

m2.1 <- update(m1, 
               formula = cdp ~ 1 + r_1,
               newdata = nd)

fitted_2.1 <- fitted(m2.1) %>% 
  data.frame() %>% 
  bind_cols(nd)

nd %>% 
  ggplot(aes(x = r_1, y = cdp))+
  geom_point()+
  geom_smooth(stat = "identity",
              data = fitted_2.1,
              aes(y = Estimate, ymax = Q97.5, ymin = Q2.5))

# f on e non linear

m1.2 <- update(m1,
               formula = f ~ 1 + e + e_s2,
               newdata = nd)

nd %>% 
  ggplot(aes(x = e, y = f))+
  geom_point()+
  geom_smooth(stat = "identity",
              data = fitted_1.2,
              aes(y = Estimate, ymax = Q97.5, ymin = Q2.5))

# Residuals

fitted_1.2 <- fitted(m1.2) %>% 
  data.frame() %>% 
  bind_cols(nd)

r_2 <- residuals(m1.2)
nd$r_2 <- r_2[,1]

m2.2 <- update(m1, 
               formula = cdp ~ 1 + r_2,
               newdata = nd)

fitted_2.2 <- fitted(m2.2) %>% 
  data.frame() %>% 
  bind_cols(nd)

nd %>% 
  ggplot(aes(x = r_2, y = cdp))+
  geom_point()+
  geom_smooth(stat = "identity",
              data = fitted_2.2,
              aes(y = Estimate, ymax = Q97.5, ymin = Q2.5))


m3 <- brm(file = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m2")









# ------Full Luxury Bayes Type Model  -------------------------#


c_model <- bf(cdp ~ 1 + f)
e_model <- bf(f ~ 1 + e)
e_model_sq <- bf(f ~ 1 + e_s2)


m2_full <- brm(
  data = nd,
  family = gaussian,
  c_model + e_model + set_rescor(FALSE),
  
  prior = c(
    prior(normal(0, 0.2), class = Intercept, resp = cdp),
    prior(normal(0, 0.5), class = b, resp = cdp),
    prior(exponential(1), class = sigma, resp = cdp),
    
    prior(normal(0, 0.2), class = Intercept, resp = f),
    prior(normal(0, 0.5), class = b, resp = f),
    prior(exponential(1), class = sigma, resp = f)),
  iter = 2000, warmup = 1000, seed = 5, cores = 4, chains = 4,
  fit = "/home/suhishan/Documents/Small R projects/Conflict and Forest Cover Bayes/fits/m2_full"
  
)


# ------Non linear in the same model

m3 <- update(m2,
             formula = cdp ~ 1 + f + e + e_s2,
             newdata = nd)

print(m3)
