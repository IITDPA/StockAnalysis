#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Analysis - Implement Decision Tree on All sectors
#**************************************************************************************


#**************************************************************************************
# IMPORT LIBRARIES
#**************************************************************************************

library('readxl')
library('xlsx')
library('caret')
library('rpart')
library('rpart.plot')
library('Metrics')
library('randomForest')
library('ggplot2')

#**************************************************************************************
# READ FILE
#**************************************************************************************

File_Name <- vector()
File_Name[1] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
File_Name[2] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
File_Name[3] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx'
File_Name[4] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/JPM Dataset.xlsx'
File_Name[5] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/WFC Dataset.xlsx'
File_Name[6] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/C Dataset.xlsx'
File_Name[7] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/ABBV Dataset.xlsx'
File_Name[8] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/BMY Dataset.xlsx'
File_Name[9] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/PFE Dataset.xlsx'
File_Name[10] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/OXY Dataset.xlsx'
File_Name[11] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/MRO Dataset.xlsx'
File_Name[12] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/COP Dataset.xlsx'

pdf("Decision Tree Visualizations.pdf")
for (j in 1:3)
{
  File <- File_Name[j]
  File <- read_excel(File)
  
  #**************************************************************************************
  # CHECK SUMMARY FOR ANY MISSING VALUES
  #**************************************************************************************
  
  print(summary(File))
  print(sapply(File, function(x) sum(is.na(x))))
  File      <- na.omit(File) # Missing values are already handled separately in a code.
  print(head(File))
  n <- nrow(File)
  n
  
  #**************************************************************************************
  # DROP DATE (TO AVOID OVERFITTING) DO SCALING OF NUMERIC DATA
  #**************************************************************************************

  File$Date <- NULL
  File <- scale(data.matrix(File))
  File<- data.frame(File)
  
  #**************************************************************************************
  # STRATIFIED SAMPLING DO SAFE CODING
  #**************************************************************************************
  
  set.seed(1000)
  sampleIndex <- createDataPartition(y = File$Close, p = 0.8, list = FALSE)
  trainData_File_DT   <- File[sampleIndex,]
  testData_File_DT    <- File[-sampleIndex,]
  stopifnot(nrow(trainData_File_DT) + nrow(testData_File_DT) == nrow(File))
  
  #**************************************************************************************
  # FIT DECISION TREE MODEL AND PREDICT USING TEST DATA
  #**************************************************************************************
  
  DT <- rpart(trainData_File_DT$Close ~ ., method="anova", data=trainData_File_DT)
  printcp(DT)
  plotcp(DT)
  summary(DT)
  rpart.plot(DT)
  pred <- predict(DT, newdata = testData_File_DT)
  rmse(testData_File_DT$Close, pred)
}
dev.off()

#**************************************************************************************
# GGPLOT BARPLOT IMPORTANT FEATURES CONSOLIDATION FOR IT SECTOR
#**************************************************************************************

print("Important Features")
print("Apple")
print(paste("Low", 17))
print(paste("High", 17))
print(paste("Open", 17))
print(paste("Market.Capital", 16))
print(paste("Book.Value.Per.Share", 15))
print(paste("GDP", 14))
print(paste("Debt.to.Equity.Ratio", 1))
print(paste("CPI", 1))
print(paste("Current.Ratio", 1))

VI_Apple = c("Low", "High", "Open", "Market.Capital","Book.Value.Per.Share","GDP","Debt.to.Equity.Ratio","CPI","Current.Ratio") 
Value_Apple <- c(17,17,17,16,15,14,1,1,1)

df = data.frame(VI_Apple, Value_Apple)

p<-ggplot(df, aes(x=reorder(df$VI_Apple, -df$Value_Apple) , y=df$Value_Apple, fill=VI_Apple, decreasing = TRUE)) +
  geom_bar(stat="identity")+theme_minimal()
p

print("Important Features")
print("Intel")
print(paste("Low", 21))
print(paste("High", 21))
print(paste("Open", 21))
print(paste("Market.Capital", 13))
print(paste("PS.Ratio", 5))
print(paste("GDP", 6))
print(paste("GNP", 6))
print(paste("EV_Revenue", 5))
print(paste("Price.to.book.value", 1))

VI_Intel = c("Low", "High", "Open", "Market.Capital","PS.Ratio","GDP","GNP","EV_Revenue","Price.to.book.value") 
Value_Intel <- c(21,21,21,13,5,6,6,5,1)

df = data.frame(VI_Intel, Value_Intel)

p<-ggplot(df, aes(x=reorder(df$VI_Intel, -df$Value_Intel) , y=df$Value_Intel, fill=VI_Intel, decreasing = TRUE)) +
  geom_bar(stat="identity")+theme_minimal()
p


print("Important Features")
print("HP")
print(paste("Low", 20))
print(paste("High", 20))
print(paste("Open", 20))
print(paste("Market.Capital", 10))
print(paste("Dividend.Yield", 14))
print(paste("Return.on.Assets", 10))
print(paste("OEY", 2))
print(paste("Return.on.Invested.Capital", 2))
print(paste("Return.on.Equity", 1))
print(paste("EV_Revenue", 1))

VI_HP = c("Low", "High", "Open", "Market.Capital","Dividend.Yield","Return.on.Assets","OEY","Return.on.Invested.Capital","Return.on.Equity","EV_Revenue") 
Value_HP <- c(20,20,20,10,14,10,2,2,1,1)

df = data.frame(VI_HP, Value_HP)

p<-ggplot(df, aes(x=reorder(df$VI_HP, -df$Value_HP) , y=df$Value_HP, fill=VI_HP, decreasing = TRUE)) +
  geom_bar(stat="identity")+theme_minimal()
p


print("Important Features for IT sector")
print(paste("Low", 20))
print(paste("High", 20))
print(paste("Open", 20))
print(paste("Market.Capital", 10))
print(paste("Dividend.Yield", 14))
print(paste("Return.on.Assets", 10))
print(paste("OEY", 2))
print(paste("Return.on.Invested.Capital", 2))
print(paste("Return.on.Equity", 1))
print(paste("EV_Revenue", 1))

VI_HP = c("Low", "High", "Open", "Market.Capital","Dividend.Yield","Return.on.Assets","OEY","Return.on.Invested.Capital","Return.on.Equity","EV_Revenue") 
Value_HP <- c(20,20,20,10,14,10,2,2,1,1)

df = data.frame(VI_HP, Value_HP)

p<-ggplot(df, aes(x=reorder(df$VI_HP, -df$Value_HP) , y=df$Value_HP, fill=VI_HP, decreasing = TRUE)) +
  geom_bar(stat="identity")+theme_minimal()
p
