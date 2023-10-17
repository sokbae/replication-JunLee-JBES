
########################################################
####  Simulation Design for Case-Control Studies    ####
########################################################
sim_design_case_control = function(n=n,dx=dx,rho=rho,ap0=ap0,ap1=ap1,mu=mu){
  
   seqp = 0:(dx-1)  
  Sigma = toeplitz(rho^(seqp))         # construct a Toeplitz matrix for Sigma
      X = mvrnorm(n=n,mu,Sigma)        # n*dx dimensional matrix of X is generated from normal distribution
  
    phi = matrix(X,ncol=1)     
  ind_T = ap0 + X%*%matrix(ap1,ncol=1)
   pr_T = exp(ind_T)/( 1 + exp(ind_T))  
  Treat = (runif(n)<=pr_T)              # n dimensional vector of Treat is generated from logistic distribution
dataset = cbind(matrix(Treat,ncol=1),X)     
  
output_list = list("dataset"=dataset)    
 
return(output_list)
    
}    