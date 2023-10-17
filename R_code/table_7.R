### updated on 17 October 2023
### Table 7
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
###     Bounding Attributable Risk             ###
##################################################

AR = cicc_AR(y, t, x, sampling = i_sets, p_upper = 0.1, 
             cov_prob = 0.95, length = 3L,
             interaction = TRUE, no_boot = 1000L, eps=1e-8)
AR_est = AR$est
AR_ci = AR$ci
pseq = AR$pseq
rcode = AR$return_code

# save results #
if (i_sets == 'rs'){
  results_table[1,1] = AR_est[pseq==0] # pseq does not matter for 'rs'
  results_table[2,1] = AR_ci[pseq==0]  # pseq does not matter for 'rs'
} else if (i_sets == 'cc'){
  results_table[1,2:3] = c(AR_est[pseq==0.05],AR_est[pseq==0.10])
  results_table[2,2:3] = c(AR_ci[pseq==0.05],AR_ci[pseq==0.10])
} else if (i_sets == 'cp'){
  results_table[1,4:5] = c(AR_est[pseq==0.05],AR_est[pseq==0.10])
  results_table[2,4:5] = c(AR_ci[pseq==0.05],AR_ci[pseq==0.10])
} 

}

##################################################
###         Printing the Table                 ###
##################################################

rownames(results_table) = c("Estimate","CI")
colnames(results_table) = c("RS","CC-Case","CC-Control","CP-Case","CP-Population")

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
results_xtable = xtable(results_table, digits=3)

sink(file = "results/table_7.txt", append = FALSE)
options(digits=3)

print("Table 7: Bounds on Attributable Risk")
print(results_xtable)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()
