library(rethinking)
library(tidyverse)
library(conflicted)
N <- 1e3
u <- rbinom(N, 1, 0.6)
y1 <- rlnorm(N, meanlog = 2, sdlog = 0.5)
y2 <- rlnorm(N, meanlog = 2, sdlog = 1)
y3 <- rlnorm(N, meanlog = 2+u, sdlog = 0.5+(u/3))
plot(NULL, ylim = c(0, 0.15), xlim = c(0, 100))
dens(y1, add = TRUE)
dens(y2, add = TRUE)
abs_diff_gini <- sapply(y, function(a) sum(abs(a - y)) )
plot(abs_diff_gini~y, cex = 0.5, col = "blue",
     ylab = "Sum of absolute differences",
     xlab = "Income")
abline(v = 5, lty = 2)
abline(v = 25, lty = 2)

G <- sum(outer(y3,y3, FUN = function(a, b) abs(a-b))) / (2 * (N^2) * mean(y3))

