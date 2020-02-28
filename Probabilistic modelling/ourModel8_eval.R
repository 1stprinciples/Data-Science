# Step 2 - Check notes 
rm(list = ls())
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
J <- 5 # Number of covariates
K <- 3

X <- matrix(rnorm(N*J), N, J) # generate an N*P covariate matrix of random data
alpha <- rcauchy(K, 0, 10)

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
sigma <- matrix(sigma, 5,3) # converting into a matrix for easier computation. 

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
      "stan_model1.stan")


stanc("stan_model1.stan")

stan_model1 <- "stan_model1.stan"

#Fitting the model


fit <- stan(file = stan_model1, data = stan_data, warmup = 10, iter = 1000, chains = 2, cores = 4, thin = 1)




fit



#----------------------New Plotting----------------------------------
#--------------------------------------------------------------------



posterior <- rstan::extract(fit)
str(posterior)


par(mfrow = c(2,1))
plot(density(y_sim1))
plot(density(posterior$y_sim))
y_sim1_sampled <- sample(y_sim1, 1000)
y_sim_sampled <- sample(posterior$y_sim, 1000)

roc(y_sim1_sampled, y_sim_sampled,plot = T, xlab = "X" , ylab = "Y")


#--------------------------------------------------------------------
# ROC for one K at a time

par(mfrow = c(2,1))
plot(density(y_sim1[,3]))
plot(density((posterior$y_sim)[,,3]))
y_sim1_sampled <- sample(y_sim1[,3], 1000, replace = FALSE)
y_sim_sampled <- sample((posterior$y_sim)[,,3], 1000, replace = FALSE)

roc(y_sim1_sampled, y_sim_sampled,plot = T, xlab = "X" , ylab = "Y")
#--------------------------------------------------------------------

par(mfrow = c(2,1))
plot(density(y_sim1[,2]))
plot(density((posterior$y_sim)[,,2]))
y_sim1_sampled <- sample(y_sim1[,2], 10000, replace = FALSE)
y_sim_sampled <- sample((posterior$y_sim)[,,2], 10000, replace = FALSE)

roc(y_sim1_sampled, y_sim_sampled,plot = T, xlab = "X" , ylab = "Y")
#--------------------------------------------------------------------
par(mfrow = c(2,1))
plot(density(y_sim1[,1]))
plot(density((posterior$y_sim)[,,1]))
y_sim1_sampled <- sample(y_sim1[,1], 10000, replace = FALSE)
y_sim_sampled <- sample((posterior$y_sim)[,,1], 10000, replace = FALSE)

roc(y_sim1_sampled, y_sim_sampled,plot = T, xlab = "X" , ylab = "Y")
#--------------------------------------------------------------------
#--------------------------------------------------------------------
par(mfrow = c(2,1))
# THis is done first for a reason. And then the posterior for alpha

alpha_sampled <- rcauchy(100, 0, 10)
alpha_sampled <- alpha_sampled[alpha_sampled>-0&&alpha_sampled<30]
plot(density(alpha_sampled)) # since alpha is from cauchy
plot(density(sample(posterior$alpha,length(alpha_sampled))))




par(mfrow = c(2,1))
# THis is done first for a reason. And then the posterior for alpha

tau_sampled <- rcauchy(100, 0, 10)
tau_sampled <- tau_sampled[tau_sampled<5&tau_sampled>-5]
plot(density(tau_sampled)) # since alpha is from cauchy
plot(density(sample(posterior$tau,length(tau_sampled))))

#----------------------New Evaluation----------------------------------
#--------------------------------------------------------------------

library(shinystan)
launch_shinystan(fit)
#----------------------New Evaluation----------------------------------
#--------------------------------------------------------------------

library('caret')
library(lattice)
library(rSymPy)
library(rjson)
library(rJython)
library(rJava)
library(e1071)


str(y_sim_sampled)
str(y_sim1_sampled)
y_sim_sampled <- as.factor(y_sim_sampled)
y_sim1_sampled <- as.factor(y_sim1_sampled)
confusionMatrix(y_sim_sampled,y_sim1_sampled, positive = "1" )


#----------------------New Evaluation----------------------------------
#--------------------------------------------------------------------
library(bayesplot)

#ppc_dens_overlay(y_sim1[,1], as.matrix(y_sim_method2[,1]))  # actual, predicted


#--------------------------------------------------------------------Old 




# Probabilistic Modeling 




# Function for lkj distribution

rlkj <- function(dim, eta = 1){
  require(matrixcalc)
  if (dim < 3) { 
    stop("Dimension less than 3")} 
  
  # Initialize Beta 
  beta <- eta + (dim - 2)/2 
  
  u <- rbeta(1, beta, beta) 
  
  r12 <- 2*u - 1
  r <- matrix(c(1 , r12 , r12 , 1) , nrow =2 , ncol =2) 
  
  for (k in 2:(dim - 1)) {
    beta <- beta - 0.5 
    y <- rbeta(1, k/2, beta)
    u <- runif_sphere(1,k)
    w <- sqrt(y)*u
    A <- chol(r)
    z <- A %*% as.vector(w)
    r <- cbind(r,z)
    r <- rbind (r , c(z,1))
    
  } 
  
  if (!is.positive.definite(r)){
    warning("not positive definite") 
    r <- rlkj(dim, eta)
  } 
  return(r)
} 


runif_sphere <- function(n, dim){
  r <- matrix(NA, nrow = n, ncol = dim)
  
  for (i in 1:n) {
    q = 0
    r[i,] <- rnorm(dim)
    
    qsquare <- sum(r[i ,]^2) 
    q <- sqrt(qsquare) 
    r[i,] <- r[i,]/q
  }
  return(r)
}


# Standard Deviations 
# Each row represents a different sd vector 
sigma.sd <- matrix (rcauchy ( J * K , 2.5) , nrow = J , ncol = K ) # J x K matrix


# Correlation Matrix 
Omega <- list()
for (j in 1: J) {
  Omega [[ j ]] <- rlkj(K , 1) # for every j, we have a K x K matrix 
} 



# Covariance 
Sigma.cov <- list()
for (j in 1: J) {
  Sigma.cov[[j]] <- diag(sigma.sd[j,]) %*% Omega[[j]] %*% diag(sigma.sd[j,])
}


# Shrinkage Scalar 
psi <- rcauchy(1,1)
tau <- rcauchy(J,1)

r <- tau * psi 

#-------------------------------------Execute this Evaluation -----------------------------------------------

alpha_pos <- posterior$alpha
beta_pos <- posterior$beta
r_pos <- posterior$r
cov_pos <- posterior$cov  

# Beta 

beta <- matrix (NA , nrow= J , ncol= K)
for (j in 1: J) {
  for (k in 1:K){
    beta [j ,k] <- sample(beta_pos,1)  
  }
  

}  

# Creating Data 

X 
dim(X)

# Using Model to Predict 

logit.theta <- matrix(NA, nrow = N, ncol = K)
theta <- matrix(NA, nrow = N, ncol = K)
y_sim_method2 <- matrix(0, nrow = N, ncol = K)

alpha <- as.vector(sample(alpha_pos,K))

for (j in 1:N) {
  for (k in 1:K) {
    logit.theta[j,k] <- alpha[k] + (beta[,k] %*% as.numeric(X[j,])) 
    #theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
    theta <- invlogit(logit.theta)
    y_sim_method2[j,k] <- rbern(1, theta[j,k])
  }
}
y_sim_method2 <- as.data.frame(y_sim_method2)

library(pROC)
par(mfrow = c(2,2))
roc(y_sim1[,1], y_sim_method2[,1],plot = T, xlab = "False positive %" , ylab = "True positive %")
roc(y_sim1[,2], y_sim_method2[,2],plot = T, xlab = "False positive %" , ylab = "True positive %")
roc(y_sim1[,3], y_sim_method2[,3],plot = T, xlab = "False positive %" , ylab = "True positive %")
roc(y_sim1[,4], y_sim_method2[,4],plot = T, xlab = "False positive %" , ylab = "True positive %")
#Plotting the Priors
y
#Plotting Beta

plot_beta <- as.data.frame(beta)
par(mfrow= c(2,2))
plot(density(plot_beta[,1]), main = "Posterior - Task 1 Beta")
plot(density(plot_beta[,2]), main = "Posterior - Task 2 Beta")
plot(density(plot_beta[,3]), main = "Posterior - Task 3 Beta")
plot(density(plot_beta[,4]), main = "Posterior - Task 4 Beta")


par(mfrow = c(2,2))
plot(density(posterior$alpha), main = "Posterior - Alpha") # Cauchy
plot(density(posterior$sigma), main = "Posterior - Sigma") # half cauchy
plot(density(posterior$psi), main = "Posterior - Psi") # half cauchy
plot(density(posterior$tau), main = "Posterior - Tau") # half cauchy




#------------------------------------------------------------------------------------------------------
#                                          Stop
#------------------------------------------------------------------------------------------------------

testPlot <- (posterior$y_sim)
# Slicing only data for one iteration
myTest2 <- testPlot[1,,]
myTest2<-  as.data.frame(myTest2)

# Slicing y1, y2, y3
y1<- myTest2[,1]
y2<- myTest2[,2]
y3<- myTest2[,3]

# Posterior curves for y1, y2, y3
par(mfrow = c(3,1))
plot(density(y1))
plot(density(y2))
plot(density(y3))





plot(posterior$y_sim)
dim(posterior$y_sim)
mat <- posterior$y_sim

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
known_y <- data_frame(variable = paste0("y_sim[",1:N,"]"), real_y = y_sim)


# Extract params as a (draws * number of chains * number of params) array
plot_data <- extract(fit, permuted = F)
str(plot_data)
test1 <- as.data.frame(plot_data) 
str(test1)   
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Stack the chains on top of one another and drop the chains label
  melt() %>% 
  left_join(known_y, by = "variable") %>% # Join the known parameter table
  # Convert from wide form to long form (stack the columns on one another)
  # Write out the plot
  group_by(variable) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            actual = first(real_y)) 

plot_data %>%
  ggplot(aes(x = median)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(y = median)) +
  geom_point(aes(y = actual)) +
  ggtitle("Actual outcomes and 95% posterior predictive interval\n") # A title
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


library(shinystan)
launch_shinystan(fit)
#Viewing the posterior


# Plots

plot(y_sim1[,1] ~ X[], pch = 20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)


#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------

# Single task glm for the given task


myDataset <- cbind(X,y1)

myDataset <- as.data.frame(myDataset)

myLmodel <- glm(y1 ~., data = myDataset)

summary(myLmodel)
