
#**************************************************************************************
# TITLE  :- Data Pre Processing Converting monthly and quarterly data to daily data
#**************************************************************************************


library('readxl')
library('xlsx')

# MERGE ALL QUARTERLY VALUES WITH INTEL Inc DAILY STOCK OPEN CLOSE PRICES
# uncomment when running for INTEL
File1 <- 'C:/Users/ayshw/Documents/DPA MERGING/INTC Prices.xlsx'
File2 <- 'C:/Users/ayshw/Documents/DPA MERGING/INTC Quarterly.xlsx'
File1 <- read_excel(File1)
File2 <- read_excel(File2)

# MERGE ALL QUARTERLY VALUES WITH APPLE DAILY STOCK OPEN CLOSE PRICES
# uncomment when running for Apple
#File1 <- 'C:/Users/ayshw/Documents/DPA MERGING/APPLE Prices.xlsx'
#File2 <- 'C:/Users/ayshw/Documents/DPA MERGING/APPLE Quarterly.xlsx'

# MERGE ALL QUARTERLY VALUES WITH HP Inc DAILY STOCK OPEN CLOSE PRICES
# uncomment when running for HP
#File1 <- 'C:/Users/ayshw/Documents/DPA MERGING/HPQ Prices.xlsx'
#File2 <- 'C:/Users/ayshw/Documents/DPA MERGING/HPQ Quarterly.xlsx'

# CONVERT MONTHLY MICRO VARIABLES TO DAILY VALUES
# uncomment when running for INTEL
# File1 <- 'C:/Users/ayshw/Documents/DPA MERGING/Monthly Micro.xlsx'
# File2 <- 'C:/Users/ayshw/Documents/DPA MERGING/Macroeconomic Variables.xlsx'
# File1 <- read_excel(File1)
# File2 <- read_excel(File2)

File1$Open  <- round(File1$Open,digits = 2)
File1$High  <- round(File1$High,digits = 2)
File1$Low   <- round(File1$Low,digits = 2)
File1$Close <- round(File1$Close,digits = 2)
File1$`Adj Close` <- round(File1$`Adj Close`,digits = 2)

head(File1)
head(File2)

File1$Date <- as.Date(File1$Date,
                      format = "%Y-%m-%d")
File2$Date <- as.Date(File2$Date,
                      format = "%Y-%m-%d")

n1 <- nrow(File1)
n2 <- nrow(File2)
n1
n2
sapply(File1, class)
sapply(File2, class)

for (i in 1:n2) # Quarterly file 
{
  flag <- FALSE
  for (j in 1:n1)  # Daily Prices File 
  {
    if (i == n2)
    {
      if ((File1$Date[j] == File2$Date[i]) || (File1$Date[j] > File2$Date[i]))
      {
        print(j)
        File1$`Return on Assets`[j]             <- round(File2$`Return on Assets`[i],2)
        File1$`Receivables Turnover`[j]         <- round(File2$`Receivables Turnover`[i],2)
        File1$`Days Payable Outstanding`[j]     <- round(File2$`Days Payable Outstanding`[i],2)
        File1$`Days Sales Outstanding`[j]       <- round(File2$`Days Sales Outstanding`[i],2)
        File1$`Current Ratio`[j]                <- round(File2$`Current Ratio`[i],2)
        File1$`Book Value`[j]                   <- (File2$`Book Value`[i])
        File1$`Debt to Equity Ratio`[j]         <- round(File2$`Debt to Equity Ratio`[i],2)
        File1$`Return on Equity`[j]             <- round(File2$`Return on Equity`[i],2)
        File1$`Return on Invested Capital`[j]   <- round(File2$`Return on Invested Capital`[i],2)
        File1$`Gross Profit Margin`[j]          <- round(File2$`Gross Profit Margin`[i],2)
        File1$`Payout Ratio`[j]                 <- round(File2$`Payout Ratio`[i],2)
        File1$`Profit Margin`[j]                <- round(File2$`Profit Margin`[i],2)
        File1$`Cash Dividend Payout Ratio`[j]   <- round(File2$`Cash Dividend Payout Ratio`[i],2)
        File1$GDP[j]                            <- (File2$GDP[i])
        File1$GNP[j]                            <- (File2$GNP[i])
        File1$`Book Value Per Share`[j]         <- round(File2$`Book Value Per Share`[i],2)
        #File1$IPI[j]                           <- round(File2$IPI[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        #File1$LT[j]                            <- round(File2$LT[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        #File1$CPI[j]                           <- round(File2$CPI[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        
      }
    }
    else
    {
      if (((File1$Date[j] == File2$Date[i]) || (File1$Date[j] > File2$Date[i])) && (File1$Date[j] < File2$Date[i+1]))
      {
        print(j)
        File1$`Return on Assets`[j]             <- round(File2$`Return on Assets`[i],2)
        File1$`Receivables Turnover`[j]         <- round(File2$`Receivables Turnover`[i],2)
        File1$`Days Payable Outstanding`[j]     <- round(File2$`Days Payable Outstanding`[i],2)
        File1$`Days Sales Outstanding`[j]       <- round(File2$`Days Sales Outstanding`[i],2)
        File1$`Current Ratio`[j]                <- round(File2$`Current Ratio`[i],2)
        File1$`Book Value`[j]                   <- (File2$`Book Value`[i])
        File1$`Debt to Equity Ratio`[j]         <- round(File2$`Debt to Equity Ratio`[i],2)
        File1$`Return on Equity`[j]             <- round(File2$`Return on Equity`[i],2)
        File1$`Return on Invested Capital`[j]   <- round(File2$`Return on Invested Capital`[i],2)
        File1$`Gross Profit Margin`[j]          <- round(File2$`Gross Profit Margin`[i],2)
        File1$`Payout Ratio`[j]                 <- round(File2$`Payout Ratio`[i],2)
        File1$`Profit Margin`[j]                <- round(File2$`Profit Margin`[i],2)
        File1$`Cash Dividend Payout Ratio`[j]   <- round(File2$`Cash Dividend Payout Ratio`[i],2)
        File1$GDP[j]                            <- (File2$GDP[i])
        File1$GNP[j]                            <- (File2$GNP[i])
        File1$`Book Value Per Share`[j]         <- round(File2$`Book Value Per Share`[i],2)
        #File1$IPI[j]                           <- round(File2$IPI[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        #File1$LT[j]                            <- round(File2$LT[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        #File1$CPI[j]                           <- round(File2$CPI[i],2) USE ONLY WHILE CONVERTING MONTHLY DATASETS
        
      }
      if ((File1$Date[j] > File2$Date[i+1]) || (File1$Date[j] == File2$Date[i+1]))
      {
        flag <- TRUE
      }
    }
    if (flag == TRUE)
      break
  }
}

head(File1)

#optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/AAPLwithQly.xlsx'
#optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/HPQwithQly.xlsx'
optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/INTCwithQly.xlsx'
write.xlsx(File1, optFileName, showNA=FALSE)

