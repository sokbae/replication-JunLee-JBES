### updated on 17 October 2023
### Table A.1
### Monte Carlo Experiments
rm(list=ls())

start_time = proc.time()

library("MASS")
library("xtable")
library("ciccr")

source("sim_design_case_control.R")

seed = 32356
set.seed(seed)

          nK = 5
           n = 1000
          dx = 5
         rho = 0.5

       a_tmp = rep(0,dx)
  a_tmp[1:5] = c(1, 1, 0, 0, 0)
    ap0_case = 0.5
    ap1_case = a_tmp
     mu_case = rep(1,dx)

       a_tmp = rep(0,dx)
   a_tmp[1:5] = c(0, 0, 1, 1, 0)
     ap0_ctrl = 0
     ap1_ctrl = a_tmp
      mu_ctrl = rep(0,dx)

   true_beta_case = (ap0_case-ap0_ctrl) + sum((mu_case)*(ap1_case-ap1_ctrl))
   true_beta_ctrl = (ap0_case-ap0_ctrl) + sum((mu_ctrl)*(ap1_case-ap1_ctrl))

##### Generating case-control data

       n_rep = 1000
     results = matrix(NA,nrow=n_rep,ncol=4)
  results_se = matrix(NA,nrow=n_rep,ncol=4)
  results_ci = matrix(NA,nrow=n_rep,ncol=4)

  cov_prob = 0.95
  cv = qnorm(cov_prob)

  for(rep in 1:n_rep){

# simulate datasets

  simdata_case = sim_design_case_control(n=n,dx=dx,rho=rho,ap0=ap0_case,ap1=ap1_case,mu=mu_case)
  simdata_ctrl = sim_design_case_control(n=n,dx=dx,rho=rho,ap0=ap0_ctrl,ap1=ap1_ctrl,mu=mu_ctrl)
    sample_all = rbind(simdata_case$dataset,simdata_ctrl$dataset)
    sample_all = cbind(as.matrix(c(rep(1,n),rep(0,n))),sample_all)

    sample_all = sample_all[sample.int(2*n,2*n),]  ### random permutation to shuffle the data
        Y_data = as.matrix(sample_all[,1])
        T_data = as.matrix(sample_all[,2])
        X_data = as.matrix(sample_all[,c(-1,-2)])


        # Construct quadratic and interaction terms

        logit_X_data = cbind(X_data,X_data^2)
        interaction_terms = {}
        for (i in 1:(ncol(X_data)-1)){

          for (j in (i+1):ncol(X_data)){
            interaction_terms = cbind(interaction_terms,X_data[,i]*X_data[,j])
          }
        }

        logit_X_data = cbind(logit_X_data,interaction_terms)


# Retrospective logit estimation

     para_case = avg_RR_logit(Y_data,T_data,X_data,'case')
 est_para_case = para_case$est
  se_para_case = para_case$se

     para_ctrl = avg_RR_logit(Y_data,T_data,X_data,'control')
 est_para_ctrl = para_ctrl$est
  se_para_ctrl = para_ctrl$se

    sieve_case = avg_RR_logit(Y_data,T_data,logit_X_data,'case')
est_sieve_case = sieve_case$est
 se_sieve_case = sieve_case$se

    sieve_ctrl = avg_RR_logit(Y_data,T_data,logit_X_data,'control')
est_sieve_ctrl = sieve_ctrl$est
 se_sieve_ctrl = sieve_ctrl$se

 results[rep,] = cbind(est_para_case,est_sieve_case,est_para_ctrl,est_sieve_ctrl)
 results_se[rep,] = cbind(se_para_case,se_sieve_case,se_para_ctrl,se_sieve_ctrl)

}

  ##### Estimation results

  true_beta = c(rep(true_beta_case,2),rep(true_beta_ctrl,2))
  mean_bias = apply(results,2,mean) - true_beta
  median_bias = apply(results,2,median) - true_beta

  difference = results-t(matrix(true_beta,nrow=length(true_beta),ncol=nrow(results)))
  root_MSE = apply(difference^2,2,mean)
  mean_AD = apply(abs(difference),2,mean)
  median_AD = apply(abs(difference),2,median)

  ##### Inference results

  results_ci = results + cv*results_se

  test = (true_beta <= results_ci)
  size = apply(test,2,mean)

  ##### Printing results

  results_table = rbind(mean_bias,median_bias,root_MSE,mean_AD,median_AD,size)

  colnames(results_table) = c("beta(1): Parametric","beta(1): Sieve","beta(0): Parametric","beta(0): Sieve")
  rownames(results_table) = c("Mean Bias","Median Bias","RMSE","Mean AD","Median AD","Cov. Prob.")

  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  results_xtable = xtable(results_table, digits=3)

  sink(file = "results/table_A1_monte_carlo.txt", append = FALSE)
  options(digits=3)

  print(results_xtable)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()


