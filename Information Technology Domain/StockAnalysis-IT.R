library('readxl')
library('xlsx')

# Join two datasets based on Date. Adding new column S&P index to existing dataset

filewotSP <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/DataChgPr+PEratio.xlsx'
fileSP <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/HistoricalPrices_S&P500.xlsx'
left <- read_excel(filewotSP)
right <- read_excel(fileSP)

head(left)
head(right)
filewtSP <- merge(x=left, y=right, by ="Date", all.x = TRUE)
head(filewtSP)
optFileName <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/AppleDataS&P500.xlsx'
write.xlsx(filewtSP, optFileName, showNA=FALSE)

# Join two datasets based on Date. Adding new column PEG Ratio to existing dataset

filewotPEG <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/project_data updated.xlsx'
filePEG <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/PEG ratio.xlsx'
left <- read_excel(filewotPEG)
right <- read_excel(filePEG)


head(left)
head(right)
filewtPEG <- merge(x=left, y=right, by ="Date", all.x = TRUE)
head(filewtPEG)
optFileName <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/project_data updated.xlsx'
write.xlsx(filewtPEG, optFileName, showNA=FALSE)

# Fill missing P/E ratio

fileMissPE <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/project_dataupdated.xlsx'
fileQEPS <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/EPS_Quarterlyvalues.xlsx'
fileMissPE <- read_excel(fileMissPE)
fileQEPS <- read_excel(fileQEPS)
head(fileMissPE)
head(fileQEPS)

sapply(fileMissPE, class)
fileMissPE$`PE ratio`    <- as.numeric(fileMissPE$`PE ratio`)
fileMissPE$Date <- as.Date(fileMissPE$Date,
                           format = "%Y-%m-%d")
sapply(fileMissPE, class)
sapply(fileMissPE, function(x) sum(is.na(x)))

# is.na(fileMissPE$`PE ratio`)
new_DF <- subset(fileMissPE, is.na(fileMissPE$`PE ratio`))
new_DF

nrow(new_DF)

# nrow(fileQEPS)
# head(fileQEPS)

fileQEPS$Date <- as.Date(fileQEPS$Date,
                       format = "%Y-%m-%d")
sapply(fileQEPS, class)

nrow(fileQEPS)

# new_DF and fileQEPS are the two files

for (i in 1:318)
{
  for (j in 1:84)
  {
    if (new_DF$Date[i] == fileQEPS$Date[j])
    {
      tempEPS <- abs(fileQEPS$EPS[j] + fileQEPS$EPS[j+1] + fileQEPS$EPS[j+2] + fileQEPS$EPS[j+3])
      print(new_DF$Date[i])
      print(tempEPS)
      new_DF$`PE ratio`[i] <- round(new_DF$Close[i]/tempEPS, digits = 2)
      print(new_DF$`PE ratio`[i])
      break
    }
    if (new_DF$Date[i] > fileQEPS$Date[j])
    {
      tempEPS <- abs(fileQEPS$EPS[j] + fileQEPS$EPS[j+1] + fileQEPS$EPS[j+2] + fileQEPS$EPS[j+3])
      print(new_DF$Date[i])
      print(tempEPS)
      new_DF$`PE ratio`[i] <- round(new_DF$Close[i]/tempEPS, digits = 2)
      print(new_DF$`PE ratio`[i])
      break
    }
  }
}

head(new_DF)
sapply(new_DF, function(x) sum(is.na(x)))

#left <- fileMissPE
#right <- new_DF

for (i in 1:318)
{
  for (j in 1:nrow(fileMissPE))
  {
    if (new_DF$Date[i] == fileMissPE$Date[j])
    {
      fileMissPE$`PE ratio`[j] <- new_DF$`PE ratio`[i]
      break
    }
  }
  
}

sapply(fileMissPE, function(x) sum(is.na(x)))

#temp <- merge(x=left, y=right, by ="Date", all.x = TRUE)
#head(temp)

optFileName <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/Data_Fill_PE_NA.xlsx'
write.xlsx(fileMissPE, optFileName, showNA=FALSE)

