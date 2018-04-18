#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement KNN on Intel Dataset
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

INTC <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
INTC <- read_excel(INTC)

#**************************************************************************************
# CHECK SUMMARY FOR ANY MISSING VALUES
#**************************************************************************************

summary(INTC)
sapply(INTC, function(x) sum(is.na(x)))

#**************************************************************************************
# VISUALIZE ANY MISSING VALUES
#**************************************************************************************

missmap(INTC, main = "Missing values vs observed")
head(INTC)
n <- nrow(INTC)
n
INTC[,"Class"] <- 0

#**************************************************************************************
# UNCOMMENT THIS IN ORDER TO RUN WITHOUR ANY THRESHOLD
#**************************************************************************************

# for (i in 1:n)
# {
# 
#   if ((INTC$Open [i] - INTC$Close [i]) > 0)
#   {
#     INTC$Class[i] <- 0
#   }
#   else
#   {
#     if ((INTC$Open [i] - INTC$Close [i]) == 0)
#     {
#       INTC$Class[i] <- 0
#     }
#     else
#     {
#       INTC$Class[i] <- 1
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

  if ((INTC$Open [i] - INTC$Close [i]) > 0)
  {
    INTC$Class[i] <- 0
  }
  else
  {
    if ((INTC$Open [i] - INTC$Close [i]) == 0)
    {
      INTC$Class[i] <- 0
    }
    else
    {
      if ((INTC$Open [i] - INTC$Close [i]) < 0 && (INTC$Open [i] - INTC$Close [i]) > -0.5)
      {
        INTC$Class[i] <- 0
      }
      else
      {
        INTC$Class[i] <- 1
      }

    }
  }
}

#**************************************************************************************
# DROP CLOSE PRICE AND DATE(TO AVOID OVERFITTING)
#**************************************************************************************

INTC$Class <- as.factor(INTC$Class)
INTC$Close <- NULL
INTC$Date <- NULL
summary(INTC)

#**************************************************************************************
# STRATIFIED SAMPLING SO SAFE CODING
#**************************************************************************************

set.seed(1000)
sampleIndex <- createDataPartition(y = INTC$Class, p = 0.8, list = FALSE)
trainData_INTC_KNN   <- INTC[sampleIndex,]
testData_INTC_KNN    <- INTC[-sampleIndex,]
stopifnot(nrow(trainData_INTC_KNN) + nrow(testData_INTC_KNN) == nrow(INTC))

#**************************************************************************************
# FIT NAIVE BAYES MODEL AND PREDICT USING TEST DATA
#**************************************************************************************

KNN <- knn(trainData_INTC_KNN, testData_INTC_KNN, trainData_INTC_KNN$Class, k=1)

#**************************************************************************************
# EVALUATION METRIC - CONFUSION MATRIX WITH ACCURACY PRECISION RECALL
#**************************************************************************************

confusionMatrix(KNN,testData_INTC_KNN$Class,mode = "prec_recall")
mean(KNN == testData_INTC_KNN$Class)












