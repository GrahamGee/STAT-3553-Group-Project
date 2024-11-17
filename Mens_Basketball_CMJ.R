#Men's Basketball Countermovement Jump
library(dplyr)
#Read data
datB<-read.csv('~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/MBBallCMJ.csv', header=TRUE)
datBC<-datB[datB$Tags=="" & datB$Type=="Countermovement Jump",]
datBC<-datBC[,c(-1:-6,-8:-9)]
datBC<-datBC[!apply(datBC == "N/A", 1, any),]

#Peak.Braking.Velocity for some reason is listed as a column of characters, this makes no sense, removing it.
datBC$Peak.Braking.Velocity<-as.numeric(datBC$Peak.Braking.Velocity)
#Columns 8,9,44 through 50 are also characters for some unknown reason, converting them to numeric
datBC[,c(4,8,9,44:50)] <- lapply(datBC[,c(4,8,9,44:50)], as.numeric)

#Will focus just on basketball data for the time being
datBCTest<-datBC[,c(4:50)]
testMulticollinearity<-round(cor(datBCTest),3)

#Identify values with high multicollinearity by iterating through lower triangular of the matrix, excluding the diagonal
indices <- which(lower.tri(testMulticollinearity, diag = FALSE), arr.ind = TRUE)
highMC<-c()

for (idx in 1:nrow(indices)) {
  i <- indices[idx, 1]
  j <- indices[idx, 2]
  if(testMulticollinearity[i,j] > 0.8){
    highMC<-cbind(highMC,testMulticollinearity[i,j])
  }
}

#Printing values with high MC value
print("The values with a high Value of Multicollinearity are:")
print(highMC)