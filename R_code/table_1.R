### updated on 17 October 2023
### Table 1
rm(list=ls(all=T))

library(ciccr)

##################################################
###         Loading the Dataset                ###
##################################################

y = DZ_CC$case_sample
t = DZ_CC$private_school

##################################################
###         Two-Way Table                      ###
##################################################

twt <- matrix(c(sum((1-y)*(1-t)),sum((1-y)*t),sum(1-y),
                sum(y*(1-t)),sum(y*t),sum(y),
                sum(1-t),sum(t),(sum(y)+sum(1-y))),
              ncol = 3, byrow = TRUE)
colnames(twt) <- c("Private School (T=0)", "Private School (T=1)","Total")
rownames(twt) <- c("Y=0 (SU)", "Y=1 (VSU)","Total")
twt <- as.table(twt)

### Printing the table

sink(file = "results/table_1.txt", append = FALSE)
print("Table 1: University entrance and private school attendance")
print(twt)
sink()
