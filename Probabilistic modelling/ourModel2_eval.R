rm(list = ls())
setwd("/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Semester 2/Probabilistic modellling/Implementation/")
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2); library(pROC)
library(LaplacesDemon)
library(magrittr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Definitions
K <- 1 # Assuming one task initially 
J <- 5
N <- 1000





# Generate a matrix of random numbers, and values for beta, nu and sigma

set.seed(42) # Set the random number generator seed so that we get the same parameters

# Simulate X
X <- matrix(N*J,N,J)
# Simulate Y
theta <- rcauchy(N, 0, 1)
#beta <- rcauchy(N, 0, 2.5)

m <- c()
for (n in 1:nrow(X)){
  m[n] <- ((rcauchy(1,0,1) + rcauchy(1,0,2.5) * X[n,] ))
}

y_sim <- c()
for (n in 1:nrow(X)){
  y_sim[n] <- rnorm(1,m[n],1)
  y_sim[n] <- round(invlogit(y_sim[n]))
  
}

# Secondary 
theta <- rnorm(N*K, 0,1)

y_sim <- invlogit(theta)
y_sim  <- as.vector(y_sim)
for (i in 1:length(y_sim)) {
  y_sim[i] <- rbern(1, y_sim[i])  
}




# Input data plots 
par(mfrow = c(3,1))
plot(density(y_sim))
plot(density(X))
plot(density(m))
par(op)
#Creating data for Stan
stan_data <- list(X=X, N=N, y = y_sim, J = J)


#Installing/loading libraries

library(ggplot2)
library(rstan)
#library(bayesplot) some error. keep this in mind

write("// Stan model for simple linear regression
      
      data {
      int <lower =1> N ; // number of patients 
      int <lower =1> K ; // number of outcomes
      int <lower =1> J ; // number of predictors
      int y[N] ;
      matrix[N ,J] X ; // Completed.
      }
      
      parameters {
      // real<lower=0> cov;
      // vector[J] r;
      vector[N] theta; // Completed.    

      }
      
      model {
      // Define the prior distributions
      theta ~ normal(0,1) ;

for (nn in 1:N){
      y[nn] ~ bernoulli_logit(theta[nn]);
}

      }
      
      generated quantities {
       vector[N] y_sim;
      //vector[N] theta_sim;
 for(i in 1:N){
    //theta_sim[i] <- normal_rng(0,1);
    y_sim[i] <- bernoulli_logit_rng(theta[i]);      
      }
      } // The posterior predictive distribution
      
      ",
      
      "stan_model1.stan")


stanc("stan_model1.stan")

stan_model1 <- "stan_model1.stan"

#Fitting the model


fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)




fit



#----------------------New Plotting----------------------------------



#--------------------------------------------------------------------
#library(shinystan)
#launch_shinystan(fit)
#Viewing the posterior


posterior <- rstan::extract(fit)
str(posterior)
par(mfrow = c(2,2))

plot(density(posterior$theta))
plot(density(theta))
plot(density(posterior$y_sim))
plot(density((y_sim)))

par(mfrow = c(1,1))
roc(sample(theta,1000), sample(posterior$theta_sim,1000), plot = TRUE)
roc(sample(y_sim,1000), sample(posterior$y_sim,1000), plot = TRUE)
#---------------------------------------AUC-----------------------------------------
category <- c(1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0)
prediction <- rev(seq_along(category))
prediction[9:10] <- mean(prediction[9:10])
roc_obj <- roc(category, prediction)
auc(roc_obj)
#----------------------------------------------------------------------------------
sample(posterior$beta, 100)
sample(beta, 100)
roc(sample(beta, 100),sample(posterior$beta, 100), plot = TRUE )
roc(sample(alpha, 100),sample(posterior$alpha, 100), plot = TRUE )

mat <- posterior$y_sim
hist(posterior$y_sim)
hist(y_sim)

r_sim <- c()
for (n in 1:nrow(X)){
  r_sim[n] <- rcauchy(1,0,1)
}


r_sim <- r_sim[r_sim>-0.5]
r_sim <- r_sim[r_sim<0.1]

r_posterior <- posterior$r[posterior$r>-1]
r_posterior <- r_posterior[r_posterior<0.1]

par(mfrow = c(1,2))
plot(density(r_sim))
plot(density(r_posterior))

dim(posterior$y_sim)
#Plotting the Posteriors
par(mfrow = c(2,1))
plot(density(y_sim))

plot(density(posterior$y_sim))

#Plotting the Priors
par(mfrow = c(2,1))
plot(density(posterior$cov))
plot(density(posterior$r))


plot(posterior$y_sim)
str(posterior$y_sim)



