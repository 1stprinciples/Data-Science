##------------------------------------Assignment 3-------------------------------------------
library(dplyr)
library(magrittr)
library(ggplot2)


#-----------------------------------------Data 2 (new) -------------------------------------------------------
setwd("/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Work/Work1/Data/")
myData <- read.csv("20190121excelData.csv", sep = ';', header = TRUE)
str(myData)
myData <- as.data.frame(myData)


#-----------------------------------------------------------------------------------------------------------
myData1 <- myData %>% 
  select(alpha, beta, gamma,AGV.Alpha,AGV.Beta,Routing, Tardiness, Machine.Util) 

myData1$alpha = factor(myData1$alpha, ordered = TRUE)
myData1$beta = factor(myData1$beta, ordered = TRUE)
myData1$gamma = factor(myData1$gamma, ordered = TRUE)
myData1$AGV.Beta = factor(myData1$AGV.Beta, ordered = TRUE)
myData1$AGV.Alpha = factor(myData1$AGV.Alpha, ordered = TRUE)
myData1$Routing = factor(myData1$Routing, ordered = TRUE)

# Run this if you want to include Machine Util as a factor while running the ANOVA
# myData1$Machine.Util <- (cut(myData$Machine.Util, breaks = 5))
# myData1$Machine.Util = factor(myData$Machine.Util, ordered = TRUE)

str(myData1) 
# Sampling smaller data; R crashes during ANOVA during 
# the whole dataset ANOVA
myData1Sampled <- myData1[sample(nrow(myData1),10000),]

head(myData1Sampled)
str(myData1Sampled)
#------------------------------------ANOVA for sequencing var-------------------------------------------

#ANOVA
# without interaction effects
fm <- aov(Tardiness ~ alpha+beta+gamma+AGV.Alpha+AGV.Beta+Machine.Util, data = myData1)
summary(fm)
# with interaction effects
#fm <- aov(Tardiness ~ AGV.Alpha*AGV.Beta, data = myData1Sampled)
#summary(fm)


fm1 <- update(fm, .~alpha+beta+gamma)
summary(fm1)
#------------------------------------Plots-------------------------------------------
# Effect of individual factors on Tardiness
plot.design(Tardiness ~., data = myData1)

op <- par(mfrow = c(3, 1))
# Interaction plots
with(myData1$tardiness, {
  interaction.plot(myData1$alpha, myData1$beta, myData1$Tardiness)
  interaction.plot(myData1$beta, myData1$gamma, myData1$Tardiness)
  interaction.plot(myData1$alpha, myData1$gamma, myData1$Tardiness)
}
)
par(op)



myData$Machine.Util <- (cut(myData$Machine.Util, breaks = 5))
myData$Machine.Util
