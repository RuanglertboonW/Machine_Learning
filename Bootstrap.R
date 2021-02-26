########  Bootstrap ########
# suppose the income distribution of your population is as follows

library(tidyverse)
library(dslabs)
set.seed(1995)
n = 10^6
income = 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, colour = I('black'))

m = median(income)
m
# in case we could not access to the entire population, but want to estimate the median m. 
# we take a sample of 100 and estimate the population median m with the sample median M
N = 100
X = sample(income, N)
median(X)

# can we construct a confidence interval? what is the distribution of M?
# because we are simulating the data, Monte Carlo can be used to learn the distribution of M

B = 10^4
M = replicate(B, {
  X = sample(income, N)
  median(X)
})
p1 = qplot(M, bins = 30, colour = I('black'))
p2 = qplot(sample = scale(M), xlab = 'Theoretical', ylab = 'sample') + 
  geom_abline()
p1
p2

# if we know this distribution, we can construct the Confidence interval. However, we may not able to access to the entire data in practice. in the past, CLT was used but not to estimate the median (CLT applies to averages)
# we can see that the CI based on CLT is
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1,1)



library(caret)
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
indexes = createResample(y,10)
q_75 = sapply(indexes, function(ind){
  y_star = y[ind]
  quantile(y_star, 0.75)
})
mean(q_75)
sd(q_75)















