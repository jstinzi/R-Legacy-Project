#Examples from https://nicercode.github.io/guides/mcmc/
#Monte Carlo methods are randomization methods
#There is no single good definition of them that's readily
#comprehensible to me, but likely because it is a class of
#methods, rather than a strictly defined technique
#Monte Carlo methods are particularly useful for Bayesian
#statistics and act as algorithms for making inferences in
#the same way the function optimization is used for making
#inferences in frequentist statistics.

#In Bayesian statistics, we need to quantify our beliefs
#about a parameter, known as the prior distribution. We
#then use our data to define the likelihood distribution
#and can calculate the posterior distribution from the
#prior and likelihood distributions.

#However, not all distributions are nice and tidy like
#a normal distribution. Suppose we wanted to use a
#distribution with 3 peaks? Markov Chain Monte Carlo
#(MCMC) methods come into play here. They allow us to
#estimate a distribution that we cannot calculate.

#Monte Carlo methods are useful for estimating parameters
#using a randomization approach. For example, if we wanted
#to estimate the area of a cloud in a picture, we could
#randomly generate a series of points to place on the
#picture and estimate the area of the cloud as the
#proportion of points lying within the cloud in the picture.

#Markov chains are event sequences where the prior event
#determines the next event. We could construct a Markov
#chain for daily activities. For example, if it is a
#weekend, there may be a 10% chance that you do homework,
#where if you don't, there's a 50% chance you run errands,
#but only a 15% chance of running errands if you do do
#homework. In this case, each subsequent event is
#dependent on the previous one. By running thousands of
#iterations, we can come up with a good estimate of how
#likely you are to run errands on a weekend. While the
#starting condition affects the path (i.e. are you doing
#homework?), in the long run, the starting condition no
#longer matters and we can calculate the long-term
#probability of an action. Also note that the sampling
#for Markov chains is not independent!!!

#We can use this strategy to generate the posterior
#distribution, by assessing the how likely the Markov
#chain output value is given our prior beliefs. We can
#then use statistics with our posterior distribution!

m <- 100
s <- 15
set.seed(1991)
d <- rnorm(n = 10000, mean = m, sd = s)
cummean <- function(x) {
  cumsum(x) / seq_along(x)
}

plot(cummean(d), type="l", xlab="Sample", ylab="Cumulative mean",
     las=1)
plot(cummean(d), type="l", xlab="Sample", ylab="Cumulative mean",
     las=1, log="x")
for (i in seq_len(30))
  lines(cummean(rnorm(10000, m, s)),
        col=rgb(runif(1), runif(1), runif(1), .5))


A <- c(10.05, 7.45, 4.86, 1.79, 0.04, 14.03, 15.83, 16.55, 19.83, 21.4, 23.7, 23.5)
Ci <- c(219, 165, 124, 74, 47, 290, 295, 296, 453, 618, 837, 1007)


d <- rnorm(n = 10000, mean = 10.05, sd = 0.01 * 10.05)
cummean <- function(x) {
  cumsum(x) / seq_along(x)
}

plot(cummean(d), type="l", xlab="Sample", ylab="Cumulative mean",
     las=1, log="x")
for (i in seq_len(30))
  lines(cummean(rnorm(10000, 10.05, 0.01 * 10.05)),
        col=rgb(runif(1), runif(1), runif(1), .5))
mean(A)


Qin <- rep(1500, 12)
Tleaf <- rep(25, 12)
data <- data.frame(cbind(A, Ci, Qin, Tleaf))
library(plantecophys)
fit <- fitaci(data, varnames = list(ALEAF = "A",
                                        Tleaf = "Tleaf",
                                        Ci = "Ci",
                                        PPFD = "Qin"))
summary(fit)
plot(fit)

A <- A[1:6]
Ci <- Ci[1:6]
library(MCMCpack)
m1 <- lm(A ~ Ci)
model <- MCMCregress(A ~ Ci, b0 = coef(lm(A ~ Ci))[[2]], B0 = coef(lm(A ~ Ci))[[2]] * 0.1, 
                     d0 = 0.1, c0 = var(A))
model2 <- MCMCregress(A ~ Ci)
summary(m1)
summary(model)
