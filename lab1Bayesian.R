#SIMPLEST CASE

a<-1
b<-1

#sample evidence
n<-20
r<-15

a_prime<-a+r
b_prime<-n-r+b

#random sample from the pesterior
post_sample<-rbeta(n=1e6, shape1=a_prime, shape2=b_prime)
hist(post_sample, probability = TRUE)
curve(expr=dbeta(x,shape1=a_prime, shape2=b_prime), from=0, to=1, col=2, add=T)

mean(post_sample)

# summary statistics computation
mean(post_sample); a_prime / (a_prime + b_prime)
sd(post_sample); sqrt((a_prime * b_prime) / ((a_prime + b_prime) ^ 2 * (a_prime + b_prime + 1)))

#quantiles
p <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
quantile(post_sample, probs = p)
qbeta(p = p, shape1 = a_prime, shape2 = b_prime)


############### MCMC ################
## Example: conjugate Normal model ##
#####################################

# Inverse gamma prior for variance: hyperparameters
nu_0 <- 1
S_0 <- 1

# Normal prior for the mean: hyperparameters
theta_0 <- 0
phi_0 <- 100

# sample
n<- 40

set.seed(123)
y <- rnorm(n = n, mean = 1, sd = 1)

mean(y)
var(y)

# Posterior parameters 
a_1<-nu_0/2+n/2


################
## GIBBS SAMPLER
#number iterations
B <- 1e4
# Create vector for realizations
phi_sample <- numeric(B)#variance
theta_sample <- numeric(B)#mean

#Initialize
phi_sample[1] <- 1
theta_sample[1] <- 0

# loop
for (i in 2:B) {
  theta_1_den  <- (sum(y) / phi_sample[i - 1] + theta_0 / phi_0)
  phi_1  <- (n / phi_sample[i - 1] + 1 / phi_0) ^ (-1)
  theta_sample[i] <- rnorm(1, theta_1_den * phi_1, sqrt(phi_1))
  
  b_1  <- S_0 / 2 + sum((y - theta_sample[i]) ^ 2) / 2
  phi_sample[i] <- 1 / rgamma(1, a_1, b_1)
  
  
}

# Joint posterior distribution
plot(theta_sample, phi_sample, xlab = "theta", 
     ylab = "phi", main = "Joint posterior distribution", col=rgb(0.1,0.1,0.1,0.1), pch=19)
# Marginal posteriors
hist(phi_sample, breaks = 30, xlab = "phi", main = "Posterior of phi")
mean(phi_sample); sd(phi_sample)
quantile(phi_sample, probs = c(0.025, 0.25, 0.5, 0.75, 0.095))

hist(theta_sample, breaks = 30, xlab = "theta", main = "Posterior of theta")
mean(theta_sample); sd(theta_sample)
quantile(theta_sample, probs = c(0.025, 0.25, 0.5, 0.75, 0.095))

# Markov chains
plot(phi_sample, ylab = "phi", xlab = "Iteration", type = "l")
plot(theta_sample, ylab = "theta", xlab = "Iteration", type = "l")