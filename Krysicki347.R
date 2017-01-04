Averages=c(mean(c(25,30)),mean(c(30,35)),mean(c(35,40)),
           mean(c(40,45)),mean(c(45,50)),mean(c(50,55)))
Quantities = c(20,40,95,25,15,5)
Indexes = seq(1,length(Quantities))
CheckSum = sum(Quantities)

InputData = data.frame(Indexes,Averages,Quantities)

WeightedAverage = (sum(InputData["Quantities"]*InputData["Averages"]))/CheckSum

Variance= (sum(InputData["Quantities"]*(InputData["Averages"]-WeightedAverage)^2))/(CheckSum-1)
StdDeviation = sqrt(Variance)

# page 85 Model 3
U=((WeightedAverage-35)/StdDeviation)*sqrt(CheckSum)

#K < 35
alpha = .05 
t.alpha = -qnorm(1-alpha)#left sided

IsRejected = U<t.alpha

