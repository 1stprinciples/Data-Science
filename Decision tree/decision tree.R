library(magrittr)
library(dplyr)
library(rpart)
library(rpart.plot)
setwd('/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Work/Work1/Assign 2 /')
myData <- read.csv('2018.10.29 - ParamVar IAT380.csv', 
         header = TRUE, 
         sep = ';',
         dec = ',' )
str(myData)
myData <- as.data.frame(myData)

#----------------------------- Data 2 ---------------------------
setwd("/Users/suguthansekar/Library/Mobile Documents/com~apple~CloudDocs/Mine/MS/Course work/Work/Work1/Assign 2 /Dataset")
myData <- read.csv('20190111excelData.csv', 
                   header = TRUE, 
                   sep = ';',
                   dec = ',' )



# 0. Drop Flowtime and Run - DONE
myData$Flow.time <- NULL
myData$Run <- NULL
myData$Arrival.Rate <- NULL
myData$Amount.of.AGAV <- NULL
myData$DDF <- NULL
myData %<>% 
  filter(Delta == 1, Tardiness <= 30000, Machine.Util >= 0.81)
head(myData)
str(myData)
myData$Tardiness <- (cut(myData$Tardiness, breaks = 5))
myData$Tardiness
test <- myData%>%
  group_by(Tardiness) %>%
  summarise(n())
typeof(myData$Tardiness)

cols1 <- c(1,2,3,4,5,9,10)
myData[cols1] <- lapply(myData[cols1], factor)
head(myData)
str(myData)
tree <- rpart(formula = Tardiness~., data = myData)

rpart.plot(tree) # ,box.palette = "blue")

# 1. Convert 7 columns to factors - DONE
#Try 1

#myData$AGV.Alpha <- as.factor(myData$AGV.Alpha)
#myData %>% select(AGV.Alpha,AGV.Beta,alpha, beta, gamma, Routing, Delta) %>% as.factor() %>% str()

# try 2 - DONE
cols1 <- c(1,2,3,4,5,9,10)
myData[cols1] <- lapply(myData[cols1], factor)
head(myData)
str(myData)
# myData[,cols1]

# 2. Convert Machine Util and AGV Util and Tardiness into Categories
# First convert Tardiness, Routing, Delta as numeric
cols2 <- c(6,7,8)
myData[,cols2] <- (lapply(myData[,cols2], as.numeric))
str(myData)
myData$Tardiness <- (cut(myData$Tardiness, breaks = 5))

myData$AGV.Util <- (cut(myData$AGV.Util, breaks = 5, labels = c("0.934,0.946", "0.946,0.959", 
                                                                 "0.959,0.972", "0.972,0.984",
                                                                 "0.984,0.997")))

myData$Machine.Util <- (cut(myData$Machine.Util, breaks = 5, labels = c("0.933,0.946", "0.946,0.959",
                                                                        "0.959,0.972", "0.972,0.985",
                                                                        "0.985,0.998")))
# Finding out the boundaries of Tardiness breaks

#str(cut(myData$Tardiness, breaks = 5))

# Test

write.csv(myData, "DeciTree1.csv")

deciTreeData <- read.csv("DeciTree1.csv")
deciTreeData$X <- NULL
str(myData)
tree <- rpart(formula = Tardiness~., data = myData)

rpart.plot(tree)

str(deciTreeData)

# Things to do 
# 0. Drop Flowtime and Run - DONE
# 1. Convert 8 columns to factors
# 2. Convert Machine Util and AGV Util and Tardiness into Categories