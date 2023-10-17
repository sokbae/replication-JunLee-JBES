### updated on 17 October 2023
### Table 3
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
###         Summary Statistics                 ###
##################################################

sumstat1 <- cbind(mean(t[y==1]),mean(t[y==0]))
sumstat2 <- cbind(apply(x[y==1,],2,mean),apply(x[y==0,],2,mean))
samplesize <- cbind(sum(y),sum(1-y))
sumstat <- rbind(sumstat1, sumstat2)
colnames(sumstat) <- c("Case", "Population")
rownames(sumstat) <- c("Not in school", 
                       "Age",
                       "Black",
                       "No religion",
                       "Illiterate",
                       "Owns house")
sumstat <- as.table(sumstat)

colnames(samplesize) <- c("Case", "Population")
rownames(samplesize) <- c("Sample Size")
samplesize <- as.table(samplesize)

##################################################
###     Printing the Estimation Results        ###
##################################################

sink(file = "results/table_3.txt", append = FALSE)
options(digits=3)

print("Summary Statistics")
print(sumstat)

print("Sample Size")
print(samplesize)

time_taken = proc.time() - start_time
print("Time Taken (seconds)")
print(time_taken)
sink()
