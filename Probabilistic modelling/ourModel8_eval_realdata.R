# Step 2 - Check notes 
#rm(list = ls())
setwd("/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Semester 2/Probabilistic modellling/Implementation/")
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
library(LaplacesDemon)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(devtools)
#devtools::install_github("rmcelreath/rethinking")
library(rethinking)
library(pROC)
library(MASS); library(rockchalk)
library(shinystan)
#------------------------------------------------------------------------------------------------------
onlineData <- read.csv("ProjectData.csv")
#Removing NAs in online data 
onlineData <- na.omit(onlineData)
X_online <- onlineData[,-(13:16)]

y_sim1 <- onlineData[,(13:16)]


  #------------------------------------------------------------------------------------------------------
# Generate a matrix of random numbers, and values for beta, nu and sigma

set.seed(42) # Set the random number generator seed so that we get the same parameters
#N <- 1000 # Number of observations
N <- nrow(X)
J <- ncol(X) # Number of covariates
K <- ncol(y_sim1)


#------------------------------------------------------------------------------------------------------
#                                          STAN
#------------------------------------------------------------------------------------------------------

zerovect <- as.vector(rep(0,K))
#Creating data for Stan
stan_data <- list(X=X_online, N=N, y = y_sim1, J = J,K = K, zerovect = zerovect)


#Installing/loading libraries

library(ggplot2)
library(rstan)
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
      int y_sim_real[N ,K ] ;
      real theta; 
      real theta_inter;
      for (nn in 1:N){
                  for(kk in 1:K) {
                  theta = alpha[kk];
                      for(jjj in 1:J){
                          theta_inter = theta + X[nn,jjj] * beta[jjj,kk] ;
                          theta = theta_inter;
                          y_sim_real[nn][kk] <- bernoulli_logit_rng(theta); 
      }
      
      }
      
      }
      } // The posterior predictive distribution
      
      ",
      "stan_model1.stan")


stanc("stan_model1.stan")

stan_model1 <- "stan_model1.stan"

#Fitting the model


fit <- stan(file = stan_model1, data = stan_data, warmup = 10, iter = 1000, chains = 2, cores = 4, thin = 1)




fit


posterior <- rstan::extract(fit)
str(posterior)

#------------------------------------------------------------------------------------------------------
# Eval 1
#------------------------------------------------------------------------------------------------------


y_posterior <- posterior$y_sim_real
dim(y_posterior)
length(y_posterior[1,,3]) # 3 is k here
par(mfrow = c(2,2))
roc(y_sim1[,1],y_posterior[1,,1], plot = TRUE)
roc(y_sim1[,2],y_posterior[1,,2], plot = TRUE)
roc(y_sim1[,3],y_posterior[1,,3], plot = TRUE)
roc(y_sim1[,4],y_posterior[1,,4], plot = TRUE)

write.csv(y_sim1, file = "y_online_actual.csv")
write.csv((y_posterior)[1,,], file = "y_online_predicted.csv")
#------------------------------------------------------------------------------------------------------
# Eval 2
#------------------------------------------------------------------------------------------------------
launch_shinystan(fit)


#ppc_dens_overlay(y_sim1[,4],y_posterior[,,4])
#------------------------------------------------------------------------------------------------------
# Eval 3
#------------------------------------------------------------------------------------------------------


plot(density(X[,6]))
