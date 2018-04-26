#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement Naive Bayes on Intel Dataset
#**************************************************************************************


#**************************************************************************************
# IMPORT LIBRARIES
#**************************************************************************************

library('readxl')
library('xlsx')
library('caret')
library('naivebayes')
library('Amelia')
library('rminer')

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
trainData_INTC_NB   <- INTC[sampleIndex,]
testData_INTC_NB    <- INTC[-sampleIndex,]
stopifnot(nrow(trainData_INTC_NB) + nrow(testData_INTC_NB) == nrow(INTC))

#**************************************************************************************
# FIT NAIVE BAYES MODEL AND PREDICT USING TEST DATA
#**************************************************************************************

nb <- naive_bayes(trainData_INTC_NB$Class ~ ., data = trainData_INTC_NB)
plot(nb)
pred <- predict(nb, testData_INTC_NB[,-32])

#**************************************************************************************
# EVALUATION METRIC - CONFUSION MATRIX WITH ACCURACY PRECISION RECALL
#**************************************************************************************

confusionMatrix(pred,testData_INTC_NB$Class,mode = "prec_recall")

# nb_kernel <- naive_bayes(x = trainData_INTC_NB[-32], y = trainData_INTC_NB$Class, usekernel = TRUE)
# pred_Kernel <- predict(nb_kernel, testData_INTC_NB[,-32])
# confusionMatrix(pred_Kernel,testData_INTC_NB$Class,mode = "prec_recall")















