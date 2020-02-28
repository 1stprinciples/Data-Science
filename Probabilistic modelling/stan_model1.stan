// Stan model for simple linear regression
      
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
      
      
