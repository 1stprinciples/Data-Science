# Step 2 - Check notes 
#rm(list = ls())
setwd("/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Semester 2/Probabilistic modellling/Implementation/")
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
library(LaplacesDemon)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(devtools)
#devtools::install_github("rmcelreath/rethinking")
library(rethinking)
library(MASS); library(rockchalk)
#------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------
# Generate a matrix of random numbers, and values for beta, nu and sigma

set.seed(42) # Set the random number generator seed so that we get the same parameters
N <- 1000 # Number of observations
J <- 12 # Number of covariates
K <- 3

X <- matrix(rnorm(N*J), N, J) # generate an N*P covariate matrix of random data
set.seed(42)
alpha <- rcauchy(K, 0, 10)
alpha
abs(alpha)
#sigma <- 5 # And scale parameter # where is this used?

# to simulate beta
# I ------------------- r
# 1. Simulate tau
tau <- rcauchy(J,0,1)
# 2. Simulate psi
psi <- rcauchy(1,0,1)
# 3. Calculate r
r <- tau*psi

# II ------------------- Sigma
# 1. Simulate sigma 
sigma <- rcauchy(J*K, 0, 2.5)
sigma <- matrix(sigma, J,K) # converting into a matrix for easier computation. 

# 2. Simulate Omega
Omega <- array(dim = c(K,K,J))
for (j in 1:J){
  Omega[,,j] <- rlkjcorr(1,K,1)
}
Omega
# 3. Calculate Sigma j
Sigma <- array(dim = c(K,K,J))
# diag sigma %*% Omega j %*% diag sigma
for (j in 1:nrow(sigma)){
  Sigma[,,j] <- (diag(sigma[j,])%*%Omega[,,j]%*%diag(sigma[j,]))
}

Sigma
# 4. Calculate beta
# Calculate MVN_sigma 
mvn_sigma <- array(dim = c(K,K,J))
for (j in 1:J){
  mvn_sigma[,,j] <- r[j] * r[j] * Sigma[,,j]
}
mvn_sigma
beta <- array(dim = c(1,K,J))
for (j in 1:J){
  beta[1,,j] <- MASS::mvrnorm(n = 1, mu = c(0,0,0), Sigma= mvn_sigma[,,j])
}

beta
dim(beta)
#------------------------------------------------------------------------------------------------------
# Simulate theta 
theta <- array(dim = c(N,1,K))
for (k in 1:K){
  theta[,1,k] <- alpha[k] + X %*% beta[1,k,]
}

theta
#------------------------------------------------------------------------------------------------------
# Simulate y values => y_sim


y_sim1 <- invlogit(theta)
y_sim1  <- as.vector(y_sim1)
for (i in 1:length(y_sim1)) {
  y_sim1[i] <- rbern(1, y_sim1[i])  
}
y_sim1 <- matrix(y_sim1, nrow = N, ncol = K) 


#------------------------------------------------------------------------------------------------------
#                                          STAN
#------------------------------------------------------------------------------------------------------

zerovect <- as.vector(rep(0,K))
#Creating data for Stan
stan_data <- list(X=X, N=N, y = y_sim1, J = J,K = K, zerovect = zerovect)


#library(bayesplot) some error. keep this in mind

write("// Stan model for simple linear regression
      
      data {

      int <lower =0> N ; // number of patients 
      int <lower =0> K ; // number of outcomes
      int <lower =0> J ; // number of predictors
      vector[K] zerovect; 
      int y[N ,K ] ;
      matrix[N ,J] X ;
      }
      
      parameters {

      vector[K] alpha;
      vector[K] beta[J];
      vector<lower=0>[J] tau;
      real<lower=0> psi;
      corr_matrix[K] Omega[J];
      vector<lower=0>[K] sigma[J];
      
      
      }
      transformed parameters { 
          //vector[K] zerovect;
          cov_matrix[K] cov[J];
          for(j in 1:J){
            cov[j] = quad_form_diag(Omega[j], sigma[j]);
          }
          
      } 
      
      model {
      //Define the posterior: for priors and the data model
      // For the Priors - My prior

        real theta;
        real theta_inter;
        
        vector[J] r;

      // Creating r
          tau ~ cauchy(0,1);
          psi ~ cauchy(0,1)   ;
          r = tau*psi;

      for (ki in 1:K) {
      alpha[ki] ~ cauchy(0,10) ; 
      } 
      for (j in 1:J){
      sigma[j] ~ cauchy(0,2.5) ; 
      Omega[j]  ~ lkj_corr(1);
      beta[j] ~ multi_normal(zerovect,r[j]^2*cov[j]); 
      }
          

          for (nn in 1:N){
                  for(kk in 1:K) {
                        theta = alpha[kk];
                            for(jjj in 1:J){
                  
                  theta_inter = theta + X[nn,jjj] * beta[jjj,kk] ;
                  theta = theta_inter;
                  y[nn][kk] ~ bernoulli_logit(theta); 
                  }
                  
                  }
          
          }

                }
                
      generated quantities {
      int y_sim[N ,K ] ;
      real theta; 
      real theta_inter;
      for (nn in 1:N){
                  for(kk in 1:K) {
                  theta = alpha[kk];
                      for(jjj in 1:J){
                          theta_inter = theta + X[nn,jjj] * beta[jjj,kk] ;
                          theta = theta_inter;
                          y_sim[nn][kk] <- bernoulli_logit_rng(theta); 
      }
      
      }
      
      }
      } // The posterior predictive distribution
      
      ",
      "stan_model2.stan")


stanc("stan_model2.stan")

stan_model2 <- "stan_model2.stan"

#Fitting the model


fit2 <- stan(file = stan_model2, data = stan_data, warmup = 10, iter = 1000, chains = 2, cores = 4, thin = 1)




fit2

posterior2 <- rstan::extract(fit2)

#------------------------------------------------------------------------------------------------------
# Eval 1
#------------------------------------------------------------------------------------------------------

y_posterior2 <- posterior2$y_sim
length(y_posterior2[1,,3]) # 3 is k here
par(mfrow = c(1,1))
roc(y_sim1[,1],y_posterior2[500,,1], plot = TRUE)
roc(y_sim1[,2],y_posterior2[998,,2], plot = TRUE)
roc(y_sim1[,3],y_posterior2[345,,3], plot = TRUE)
roc(y_sim1[,4],y_posterior2[345,,4], plot = TRUE)

write.csv(y_sim1, file = "y_simu_actual.csv")
write.csv((y_posterior2)[1,,], file = "y_simu_predicted.csv")
# Plotting X 
par(mfrow=c(3,1))
plot.roc(roc(y_sim1[,1],y_posterior2[500,,1]),axes = TRUE, main = "Task 1")
plot.roc(roc(y_sim1[,2],y_posterior2[998,,2]),axes = TRUE, main = "Task 2")
plot.roc(roc(y_sim1[,3],y_posterior2[345,,3]),axes = TRUE, main = "Task 3")
par(mfrow=c(2,1))
# Col 1 in simulated data 
plot(density(X[,1]))
# Col 1 in real data 
plot(density(X_online[,1]))



par(mfrow=c(2,1))
# Col 1 in simulated data 
plot(density(X[,5]))
# Col 1 in real data 
plot(density(X_online[,5]))

#------------------------------------------------------------------------------------------------------
# Eval 2
#------------------------------------------------------------------------------------------------------
launch_shinystan(fit2)


#------------------------------------------------------------------------------------------------------
# Eval 3
#------------------------------------------------------------------------------------------------------


# Using Model to Predict 

logit.theta <- matrix(NA, nrow = N, ncol = K)

theta <- matrix(NA, nrow = N, ncol = K)
y_predicted2 <- matrix(0, nrow = N, ncol = K)

#alpha <- as.vector(sample(alpha_pos,K))
alpha <- as.vector(alpha_pos[3,])
for (j in 1:N) {
  for (k in 1:K) {
    logit.theta[j,k] <- alpha[k] + (beta[,k] %*% as.numeric(X[j,])) 
    #theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
    theta <- invlogit(logit.theta)
    y_sim_method2[j,k] <- rbern(1, theta[j,k])
  }
}




# plotting posterior beta
# To understand if the posteriors are similar
par(mfrow = c(2,2))
beta_pos_task1 <- beta_pos[1,,1]
beta_pos_task2 <- beta_pos[1,,2]
beta_pos_task3 <- beta_pos[1,,3]

plot(density(beta_pos_task1))
plot(density(beta_pos_task2))
plot(density(beta_pos_task3))
plot(density(beta_pos_task4))

#alpha <- as.vector(alpha_pos[3,])
# For task 1
y_predicted <- matrix(NA,nrow = N, ncol = K)
for (k in 1:K){
  logitTheta_predicted <- alpha_pos[1,k] + as.matrix(X) %*% as.vector(beta_pos[1,,k])  
  theta_predicted <- invlogit(logitTheta_predicted)
  
  #for (i in 1:N) {
  y_predicted[,k] <- rbern(1,theta_predicted)
  # }
}
str(y_predicted)
y_predicted <- as.numeric(y_predicted)
y_predicted <- as.data.frame(y_predicted)
y_predicted
y_sim1
par(mfrow = c(3,1))
roc(y_sim1[,1], y_predicted[1:1000,], plot = TRUE)
roc(y_sim1[,2], y_predicted[1001:2000,], plot = TRUE)
roc(y_sim1[,3], y_predicted[2001:3000,], plot = TRUE)


#------------------------------------------------------------------------------------------------------
# Shiny Stan
#------------------------------------------------------------------------------------------------------



launch_shinystan(fit)



ppc_dens_overlay(y_sim1[,1],y_posterior2[,,1])
