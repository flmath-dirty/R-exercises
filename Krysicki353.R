
library(stats)
Values = c(4.5,3.6,6.0,6.4,7.9,6.9,6.1,7.4,9.0,4.3,6.1,8.2,4.9,7.5,6.8)
Indexes = seq(1,length(Values))
InputData = data.frame(Indexes,Values)
CheckSum = sum(InputData["Values"])

Variance= var(as.numeric(unlist(InputData["Values"])))
#B = sd(Values)
#Page 89 Model1 

ChiStat = length(Values)*Variance/2

alpha=0.05
CriticalVal = qchisq(1-alpha,length(Values)-1)

IsRejected = CriticalVal < ChiStat