#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement Naive Bayes on all sectors
#**************************************************************************************


#**************************************************************************************
# IMPORT LIBRARIES
#**************************************************************************************

library('readxl')
library('xlsx')
library('caret')
library('naivebayes')
library('Amelia')
library('ROCR')

#**************************************************************************************
# READ FILE
#**************************************************************************************

File_Name <- vector()
File_Name[1] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
File_Name[2] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
File_Name[3] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx'
File_Name[4] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/JPM.xlsx'
File_Name[5] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/C.xlsx'
File_Name[6] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/WFC.xlsx'
File_Name[7] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/BMY Final.xlsx'
File_Name[8] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/PFE Final.xlsx'
File_Name[9] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/ABBV Final.xlsx'
File_Name[10] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/COP Dataset.xlsx'
File_Name[11] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/MRO Dataset.xlsx'
File_Name[12] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/OXY Dataset.xlsx'

File_Name
for (j in 1:9)
{
  File <- File_Name[j]
  File <- read_excel(File)
  
  #**************************************************************************************
  # CHECK SUMMARY FOR ANY MISSING VALUES
  #**************************************************************************************
  
  print(summary(File))
  print(sapply(File, function(x) sum(is.na(x))))
  
  #**************************************************************************************
  # MISSING VALUES
  #**************************************************************************************
  
  missmap(File, main = "Missing values vs observed")
  print(head(File))
  n <- nrow(File)
  n
  # Missing values are already handled separately in another code. This removes very few missing records
  File      <- na.omit(File) 
                          
  #**************************************************************************************
  # UNCOMMENT THIS IN ORDER TO RUN WITHOUR ANY THRESHOLD
  #**************************************************************************************
  # Class <- vector()
  # for (i in 1:n)
  # {
  # 
  #   if ((File$Open [i] - File$Close [i]) > 0)
  #   {
  #     Class[i] <- 0
  #   }
  #   else
  #   {
  #     if ((File$Open [i] - File$Close [i]) == 0)
  #     {
  #       Class[i] <- 0
  #     }
  #     else
  #     {
  #       Class[i] <- 1
  #     }
  #   }
  # }
  
  #**************************************************************************************
  # WITH THRESHOLD
  # CREATE THE Y VARIABLE - CLASS OF 0 and 1
  #**************************************************************************************
  
  Class <- vector()
  for (i in 1:n)
  {
    
    if ((File$Open [i] - File$Close [i]) > 0)
    {
      Class[i] <- 0
    }
    else
    {
      if ((File$Open [i] - File$Close [i]) == 0)
      {
        Class[i] <- 0
      }
      else
      {
        if ((File$Open [i] - File$Close [i]) < 0 && (File$Open [i] - File$Close [i]) > -0.5)
        {
          Class[i] <- 0
        }
        else
        {
          Class[i] <- 1
        }
      }
    }
  }
  
  #**************************************************************************************
  # DROP CLOSE PRICE AND DATE(TO AVOID OVERFITTING) DO SCALING OF NUMERIC DATA
  #**************************************************************************************
  
  File$Close <- NULL
  File$Date <- NULL
  print(summary(File))
  File <- scale(data.matrix(File))
  print(head(File))
  File <- data.frame(File)
  File[,"Class"] <- 0
  File$Class <- as.factor(Class)
  print(head(File))
  colnum <- ncol(File)
  colnum
  
  #**************************************************************************************
  # STRATIFIED SAMPLING SO SAFE CODING
  #**************************************************************************************
  
  set.seed(1000)
  sampleIndex <- createDataPartition(y = File$Class, p = 0.8, list = FALSE)
  trainData_File_NB   <- File[sampleIndex,]
  testData_File_NB    <- File[-sampleIndex,]
  stopifnot(nrow(trainData_File_NB) + nrow(testData_File_NB) == nrow(File))
  
  #**************************************************************************************
  # FIT NAIVE BAYES MODEL, CROSS VALIDATION AND PREDICT USING TEST DATA
  #**************************************************************************************
  
  nb <- train(trainData_File_NB, trainData_File_NB$Class, method = "nb", tuneLength=20,
              trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
  plot(nb)
  print(nb)
  pred <- predict(nb, testData_File_NB[,-colnum],type = "raw")
  
  #**************************************************************************************
  # EVALUATION METRIC - CONFUSION MATRIX WITH ACCURACY PRECISION RECALL
  #**************************************************************************************
  
  print(confusionMatrix(pred,testData_File_NB$Class,mode = "prec_recall"))
  
  
  #**************************************************************************************
  # EVALUATION METRIC - ROC CURVE BETWEEN TRUE POSITIVE AND FALSE POSITIVE RATE
  #**************************************************************************************
  
  pr <- prediction(as.numeric(pred), as.numeric(testData_File_NB$Class))
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf, main = 'Naive Bayes ROC Curve',type = 'l')
  
  #**************************************************************************************
  # EVALUATION METRIC - AREA UNDER CURVE MEASURE
  #**************************************************************************************
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  print(auc)
  
}

