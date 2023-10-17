### updated on 17 October 2023
### Figure 1
rm(list=ls(all=T))

start_time = proc.time()

set.seed(39583)

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

RR = cicc_RR(y, t, x, sampling = 'cc', cov_prob = 0.95)

##################################################
###     Bounding Attributable Risk             ###
##################################################

AR = cicc_AR(y, t, x, sampling = 'cc', p_upper = 1L, 
                    cov_prob = 0.95, 
                    length = 21L, interaction = TRUE, 
                    no_boot = 10000L, eps=1e-8)
AR_est = AR$est
AR_ci = AR$ci
pseq = AR$pseq
rcode = AR$return_code

maxAR_est = max(AR_est)
maxAR_ci = AR_ci[which.max(AR_est)]
maxAR_est_p = pseq[which.max(AR_est)]

##################################################
###     Printing the Estimation Results        ###
##################################################

sink(file = "results/figure_1.txt", append = FALSE)
options(digits=3)

print("Maximum of the upper bound on AR over p")
print(maxAR_est)
print("End point of the corresponding confidence interval")
print(maxAR_ci)
print("Argmax of the upper bound on AR")
print(maxAR_est_p)
sink()

##################################################
###     Plotting the Estimation Results        ###
##################################################

cicc_plot(RR, parameter='RR', save_plots=TRUE, file_name="results/plot")
cicc_plot(AR, parameter='AR', save_plots=TRUE, file_name="results/plot")

time_taken = proc.time() - start_time
sink(file = "results/figure_1.txt", append = TRUE)
print("Time Taken (seconds)")
print(time_taken)
sink()
