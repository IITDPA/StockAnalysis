#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement KNN on Apple Dataset
#**************************************************************************************


#**************************************************************************************
# IMPORT LIBRARIES
#**************************************************************************************

library('readxl')
library('xlsx')
library('caret')
library('Amelia')
library('rminer')
library('class')

#**************************************************************************************
# READ FILE
#**************************************************************************************

AAPL <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset No Missing.xlsx'
AAPL <- read_excel(AAPL)

#**************************************************************************************
# CHECK SUMMARY FOR ANY MISSING VALUES
#**************************************************************************************

summary(AAPL)
sapply(AAPL, function(x) sum(is.na(x)))

#**************************************************************************************
# VISUALIZE ANY MISSING VALUES
#**************************************************************************************

missmap(AAPL, main = "Missing values vs observed")
head(AAPL)
n <- nrow(AAPL)
n
AAPL[,"Class"] <- 0

#**************************************************************************************
# UNCOMMENT THIS IN ORDER TO RUN WITHOUR ANY THRESHOLD
#**************************************************************************************

# for (i in 1:n)
# {
# 
#   if ((AAPL$Open [i] - AAPL$Close [i]) > 0)
#   {
#     AAPL$Class[i] <- 0
#   }
#   else
#   {
#     if ((AAPL$Open [i] - AAPL$Close [i]) == 0)
#     {
#       AAPL$Class[i] <- 0
#     }
#     else
#     {
#       AAPL$Class[i] <- 1
#     }
#   }
# }

#**************************************************************************************
# WITH THRESHOLD
# CREATE THE Y VARIABLE - CLASS OF 0 and 1
#**************************************************************************************
# 
for (i in 1:n)
{
  
  if ((AAPL$Open [i] - AAPL$Close [i]) > 0)
  {
    AAPL$Class[i] <- 0
  }
  else
  {
    if ((AAPL$Open [i] - AAPL$Close [i]) == 0)
    {
      AAPL$Class[i] <- 0
    }
    else
    {
      if ((AAPL$Open [i] - AAPL$Close [i]) < 0 && (AAPL$Open [i] - AAPL$Close [i]) > -0.5)
      {
        AAPL$Class[i] <- 0
      }
      else
      {
        AAPL$Class[i] <- 1
      }
      
    }
  }
}

#**************************************************************************************
# DROP CLOSE PRICE AND DATE(TO AVOID OVERFITTING)
#**************************************************************************************

AAPL$Class <- as.factor(AAPL$Class)
AAPL$Close <- NULL
AAPL$Date <- NULL
summary(AAPL)

#**************************************************************************************
# STRATIFIED SAMPLING SO SAFE CODING
#**************************************************************************************

set.seed(1000)
sampleIndex <- createDataPartition(y = AAPL$Class, p = 0.8, list = FALSE)
trainData_AAPL_KNN   <- AAPL[sampleIndex,]
testData_AAPL_KNN    <- AAPL[-sampleIndex,]
stopifnot(nrow(trainData_AAPL_KNN) + nrow(testData_AAPL_KNN) == nrow(AAPL))

#**************************************************************************************
# FIT NAIVE BAYES MODEL AND PREDICT USING TEST DATA
#**************************************************************************************

KNN <- knn(trainData_AAPL_KNN, testData_AAPL_KNN, trainData_AAPL_KNN$Class, k=1)

#**************************************************************************************
# EVALUATION METRIC - CONFUSION MATRIX WITH ACCURACY PRECISION RECALL
#**************************************************************************************

confusionMatrix(KNN,testData_AAPL_KNN$Class,mode = "prec_recall")
mean(KNN == testData_AAPL_KNN$Class)











