### updated on 17 October 2023
### Table 6
rm(list=ls(all=T))

start_time = proc.time()

set.seed(39583)

library(ciccr)
library(xtable)

results_table = matrix(NA,nrow=2,ncol=5)
cv_norm = qnorm(0.95)

sampling_sets = c('rs', 'cc', 'cp')

for (i_sets in sampling_sets){

##################################################
###         Loading the Dataset                ###
##################################################

if (i_sets == 'rs'){
  dat = FG 
} else if (i_sets == 'cc'){
  dat = FG_CC
} else if (i_sets == 'cp'){
  dat = FG_CP
}  
  
##################################################
###       Defining the Variables               ###
##################################################

y = dat$flag
if (i_sets == 'cp'){ # code missing y by 0 in the CP sample
  y = as.integer(is.na(y)==FALSE)
}  

t = dat$smallPractice
# cubic polynomial for experience in years
exp_bs = splines::bs(dat$experYear, df=3) 
x = cbind(dat$male,dat$isMD,exp_bs)

##################################################
###         Bounding Relative Risk             ###
##################################################

if (i_sets == 'rs'){
  all_res = avg_RR_logit(y, t, x, 'all')
  all_est = all_res$est
  all_se = all_res$se
  all_up = all_est + cv_norm*all_se 
  # save results #
  results_table[1,1] = exp(all_est)
  results_table[2,1] = exp(all_up)
  
} else {

  case_res = avg_RR_logit(y, t, x, 'case')
  case_est = case_res$est
  case_se = case_res$se
  case_up = case_est + cv_norm*case_se

  ctrl_res = avg_RR_logit(y, t, x, 'control')
  ctrl_est = ctrl_res$est
  ctrl_se = ctrl_res$se
  ctrl_up = ctrl_est + cv_norm*ctrl_se

  # save results #
  if (i_sets == 'cc'){
    results_table[1,2:3] = exp(c(case_est,ctrl_est))
    results_table[2,2:3] = exp(c(case_up,ctrl_up))
  } else if (i_sets == 'cp'){
    results_table[1,4:5] = exp(c(case_est,ctrl_est))
    results_table[2,4:5] = exp(c(case_up,ctrl_up))
  }  

}

}
##################################################
###         Printing the Table                 ###
##################################################

rownames(results_table) = c("Estimate","CI")
colnames(results_table) = c("RS","CC-Case","CC-Control","CP-Case","CP-Population")

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
results_xtable = xtable(results_table, digits=2)

sink(file = "results/table_6.txt", append = FALSE)
options(digits=3)

print("Table 6: Odds Ratio")
print(results_xtable)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()