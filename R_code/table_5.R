### updated on 17 October 2023
### Table 5
rm(list=ls(all=T))

library(ciccr)

##################################################
###         Loading the Dataset                ###
##################################################

y = FG$flag
t = FG$smallPractice

##################################################
###         Two-Way Table                      ###
##################################################

twt <- matrix(c(sum((1-y)*(1-t)),sum((1-y)*t),sum(1-y),
                sum(y*(1-t)),sum(y*t),sum(y),
                sum(1-t),sum(t),(sum(y)+sum(1-y))),
              ncol = 3, byrow = TRUE)
colnames(twt) <- c("Small Group Practice (T=0)","Small Group Practice (T=1)","Total")
rownames(twt) <- c("Y=0 (Never flagged)","Y=1 (Flagged in either year)","Total")
twt <- as.table(twt)

### Printing the table

sink(file = "results/table_5.txt", append = FALSE)
print("Table 5: Physician’s potential overbilling and size of the group practice")
print(twt)
sink()
