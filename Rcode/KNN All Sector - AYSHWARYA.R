#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement KNN on Apple Intel and HPQ Dataset
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
library('ROCR')

#**************************************************************************************
# READ FILE
#**************************************************************************************

File_Name <- vector()
File_Name[1] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
File_Name[2] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
File_Name[3] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx'
File_Name
Fname <- vector()
Fname[1] <- "Apple"
Fname[2] <- "Intel"
Fname[3] <- "HP"

for (j in 1:3)
{

  File <- File_Name[j]
  File <- read_excel(File)
  
  #**************************************************************************************
  # CHECK SUMMARY FOR ANY MISSING VALUES
  #**************************************************************************************
  
  print(paste("Summary for KNN model ",Fname[j]))
  print(summary(File))
  print(paste("Missing Values for KNN model ",Fname[j]))
  print(sapply(File, function(x) sum(is.na(x))))
  File      <- na.omit(File) # Missing values are already handled separately in a code. 
  
  #**************************************************************************************
  # VISUALIZE ANY MISSING VALUES
  #**************************************************************************************
  
  missmap(File, main = "Missing values vs observed")
  print(paste("Head values for KNN model ",Fname[j]))
  head(File)
  n <- nrow(File)
  
  #**************************************************************************************
  # UNCOMMENT THIS IN ORDER TO RUN WITHOUR ANY THRESHOLD
  #**************************************************************************************
  #  Class <- vector()
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
  # DROP CLOSE PRICE AND DATE(TO AVOID OVERFITTING) 
  #**************************************************************************************
  
  File$Close <- NULL
  File$Date <- NULL
  print(head(File))
  File <- scale(data.matrix(File))
  print(paste("Scaled head values for KNN model ",Fname[j]))
  print(head(File))
  File <- data.frame(File)
  File[,"Class"] <- 0
  File$Class <- as.factor(Class)
  #print(head(File))
  colnum <- ncol(File)
  #colnum
  
  #**************************************************************************************
  # STRATIFIED SAMPLING SO SAFE CODING
  #**************************************************************************************
  
  set.seed(1000)
  sampleIndex <- createDataPartition(y = File$Class, p = 0.8, list = FALSE)
  trainData_File_KNN   <- File[sampleIndex,]
  testData_File_KNN    <- File[-sampleIndex,]
  stopifnot(nrow(trainData_File_KNN) + nrow(testData_File_KNN) == nrow(File))
  
  #**************************************************************************************
  # FIT NAIVE BAYES MODEL, CROSS VALIDATION AND PREDICT USING TEST DATA
  #**************************************************************************************
  KNN <- knn(trainData_File_KNN[,-colnum], testData_File_KNN[,-colnum], trainData_File_KNN$Class, k=3)
  print(KNN)
  
  # uncomment to run with cross validation
  # KNN <- train(trainData_File_KNN[,-colnum], trainData_File_KNN$Class, method = "knn", tuneGrid   = expand.grid(k = 1:10),
  #              tuneLength = 10, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
  # plot(KNN)
  # print(paste("Cross validated KNN Model output ",Fname[j]))
  # print(KNN)
  # pred <- predict(KNN,newdata = testData_File_KNN,type = "raw")
  # head(pred)
  
  #**************************************************************************************
  # EVALUATION METRIC - CONFUSION MATRIX WITH ACCURACY PRECISION RECALL
  #**************************************************************************************
  
  print(paste("Confusion Matrix for KNN model ",Fname[j]))
  print(confusionMatrix(KNN,testData_File_KNN$Class,mode = "prec_recall"))
  
  # uncomment to run with cross validation
  #print(confusionMatrix(pred,testData_File_KNN$Class,mode = "prec_recall"))
  #print(mean(pred == testData_File_KNN$Class))
  
  #**************************************************************************************
  # EVALUATION METRIC - ROC CURVE BETWEEN TRUE POSITIVE AND FALSE POSITIVE RATE
  #**************************************************************************************
  
  # uncomment to run with cross validation
  # pr <- prediction(as.numeric(pred), as.numeric(testData_File_KNN$Class))
 
  pr <- prediction(as.numeric(KNN), as.numeric(testData_File_KNN$Class))
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  print(plot(prf))

  #**************************************************************************************
  # EVALUATION METRIC - AREA UNDER CURVE MEASURE
  #**************************************************************************************
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  print(paste("AUC for KNN model ",Fname[j]))
  print(auc)
  
  #**************************************************************************************
  # PERFORMANCE FOR VARIOUS K VALUES
  #**************************************************************************************
  
  accuracy <- rep(0, 20)
  k <- 1:20
  for(x in k)
  {
    prediction <- knn(trainData_File_KNN[,-colnum], testData_File_KNN[,-colnum], trainData_File_KNN$Class, k=x)
    accuracy[x] <- mean(prediction == testData_File_KNN$Class)
  }
  print(plot(k, accuracy, type = 'b'))
  
}

