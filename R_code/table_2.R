### updated on 17 October 2023
### Table 2
rm(list=ls(all=T))

start_time = proc.time()

library(ciccr)
library(xtable)

##################################################
###         Loading the Dataset                ###
##################################################

y = DZ_CC$case_sample
t = DZ_CC$private_school
x = cbind(DZ_CC$parent_ba, DZ_CC$parent_inc)

##################################################
###         Bounding Relative Risk             ###
##################################################

 cv_norm = qnorm(0.95)

case_res = avg_RR_logit(y, t, x, 'case')
case_est = case_res$est
 case_se = case_res$se
 case_up = case_est + cv_norm*case_se

ctrl_res = avg_RR_logit(y, t, x, 'control')
ctrl_est = ctrl_res$est
 ctrl_se = ctrl_res$se
 ctrl_up = ctrl_est + cv_norm*ctrl_se

### Summarizing the estimation results

results_table = matrix(NA,nrow=4,ncol=2)
results_table[1,] = c(case_est,ctrl_est)
results_table[2,] = c(case_up,ctrl_up)
results_table[3,] = exp(c(case_est,ctrl_est))
results_table[4,] = exp(c(case_up,ctrl_up))

colnames(results_table) = c("Case","Control")
rownames(results_table) = c("LogEstimate","LogCI","Estimate","CI")

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
results_xtable = xtable(results_table, digits=2)

##################################################
###     Printing the Estimation Results        ###
##################################################

sink(file = "results/table_2.txt", append = FALSE)

print("Table 2")
print(results_xtable)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()
