#Demonstrate that the accumulation of many small random errors is normally distributed

#number of random errors to add
X = 10

#number of times to repeat
N = 50

#set up container for simulation output
out = matrix(nrow=N,ncol=X+1)

#create for loop
for (i in 1:N)
{
  out[i,1:X] = runif(X,min=0, max=50)
  out[i,X+1] = sum(out[i,1:X])
}

#plot a histogram to see if the simulation outputs look normally distributed
hist(out[,X+1])


#Plot samples from Poisson distributions
hist(rpois(500,500))

#Plot samples from Binomial distributions
hist(rbinom(500, 50, 0.95))

#Don Corleone's unfair coin
#Plot samples from Beta distribution with alpha-1 = 4 heads and beta-1 = 0 tails
hist(rbeta(500, 5, 1))

#Calculate the probability density to the left of 0.5; 1 minus this probability 
#is the probability that p(heads) > 0.5
pbeta(0.5, 5, 1)

#Using fitdistrplus to compare distributions from the simulation at the beginning

require("fitdistrplus")
data("groundbeef")

plotdist(out[,X+1], histo = TRUE, demp = TRUE)

fnorm <- fitdist(out[,X+1], "norm")
summary(fnorm)

fg <- fitdist(out[,X+1], "gamma")
fln <- fitdist(out[,X+1], "lnorm")
plot.legend <- c("normal", "lognormal", "gamma")
denscomp(list(fnorm, fln, fg), legendtext = plot.legend)