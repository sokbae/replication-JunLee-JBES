### updated on 17 October 2023
### Table 4
rm(list=ls(all=T))

start_time = proc.time()

set.seed(75341)

library(ciccr)
library(readstata13)
library(xtable)

##################################################
###         Loading the Dataset                ###
##################################################

# N.B.: The dataset used in Carvalho and Soares (2016) 
#       cannot be shared publicly for the privacy of 
#       individuals that participated in the study.
# Hence, "gangs_cleaned.dta" is not provided in this repo.

dat = read.dta13("data/gangs_cleaned.dta")

# column 1: age
# column 2: black
# column 3: noregligion
# column 4: illiterate
# column 5: owns_house
# column 6: case_sample
# column 7: control_sample
# column 6: noninschool

datuse = data.matrix(dat)
y = datuse[,6]
t = datuse[,8]
x = datuse[,c(1,2,3,4,5)]

##################################################
###         Bounding Relative Risk             ###
##################################################

  cv_norm = qnorm(0.95)

case_res = avg_RR_logit(y, t, x, 'case')
case_est = case_res$est
 case_up = case_est + cv_norm*case_res$se

ctrl_res = avg_RR_logit(y, t, x, 'control')
ctrl_est = ctrl_res$est
 ctrl_up = ctrl_est + cv_norm*ctrl_res$se

RR = cicc_RR(y, t, x, sampling = 'cp', cov_prob = 0.95)

### Summarizing the estimation results

results_table = matrix(NA,nrow=4,ncol=2)
results_table[1,] = c(case_est,ctrl_est)
results_table[2,] = c(case_up,ctrl_up)
results_table[3,] = exp(c(case_est,ctrl_est))
results_table[4,] = exp(c(case_up,ctrl_up))

colnames(results_table) = c("Case","Population")
rownames(results_table) = c("LogEstimate","LogCI","Estimate","CI")

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
results_xtable = xtable(results_table, digits=2)

##################################################
###     Bounding Attributable Risk             ###
##################################################

AR = cicc_AR(y, t, x, sampling = 'cp', p_upper = 0.15, 
             cov_prob = 0.95, length = 16L,
             interaction = TRUE, no_boot = 1000L, eps=1e-8)
AR_est = AR$est
AR_ci = AR$ci
pseq = AR$pseq
rcode = AR$return_code

AR_est_set = c(AR_est[pseq==0.05],AR_est[pseq==0.10],AR_est[pseq==0.15])
AR_ci_set = c(AR_ci[pseq==0.05],AR_ci[pseq==0.10],AR_ci[pseq==0.15])
AR_results = cbind(AR_est_set, AR_ci_set)
colnames(AR_results) = c("Est","C.I.")
rownames(AR_results) = c("p0=0.05","p0=0.10","p0=0.15")

##################################################
###     Printing the Estimation Results        ###
##################################################

sink(file = "results/table_4.txt", append = FALSE)
options(digits=2)

print("Bounding Relative Risk")
print(results_xtable)

print("Additional results reported in Section 6.2")
print("Bounding Attributable Risk")
print(AR_results)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()
