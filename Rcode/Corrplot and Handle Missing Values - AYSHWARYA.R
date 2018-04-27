#**************************************************************************************
# AUTHOR :- AYSHWARYA SAMBASIVAN A20411226
# TITLE  :- Data Preparation EDA - Handle missing values for Apple Intel and HP (IT Sector)
#**************************************************************************************
library('readxl')
library('xlsx')
library('ggplot2')
library ('psych')
library('corrplot')

File_Name[1] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
File_Name[2] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
File_Name[3] <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx'

for (j in 1:3)
{
  File <- File_Name[j]
  File <- read_excel(File)
  pairs.panels(File[  ,2:10])
  pairs.panels(File[  ,11:19])
  pairs.panels(File[  ,20:29])
  corrplot(cor(File),type="upper", method = "number")
}


#**************************************************************************************
# Fill missing P/E ratio - Apple and HP
#**************************************************************************************

# HPQ    <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx' uncomment when running for HP
AAPL      <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
AAPL      <- read_excel(AAPL)

AAPL$`PE ratio`    <- as.numeric(AAPL$`PE ratio`)
AAPL$Date <- as.Date(AAPL$Date,
                     format = "%Y-%m-%d")

sapply(AAPL, function(x) sum(is.na(x)))

new_DF <- subset(AAPL, is.na(AAPL$`PE ratio`))
head(new_DF)
miss <- nrow(new_DF)
# HPQEPS <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ_EPS.xlsx' uncomment when running for HP
AAPLEPS   <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL_EPS.xlsx'
AAPLEPS   <- read_excel(AAPLEPS)
AAPLEPS$Date <- as.Date(AAPLEPS$Date,
                        format = "%Y-%m-%d")
A_EPS <- nrow(AAPLEPS)

for (i in 1:miss)
{
  for (j in 1:A_EPS)
  {
    if (new_DF$Date[i] == AAPLEPS$Date[j])
    {
      tempEPS <- abs(AAPLEPS$EPS[j] + AAPLEPS$EPS[j+1] + AAPLEPS$EPS[j+2] + AAPLEPS$EPS[j+3])
      print(new_DF$Date[i])
      print(tempEPS)
      new_DF$`PE ratio`[i] <- round(new_DF$Close[i]/tempEPS, digits = 2)
      print(new_DF$`PE ratio`[i])
      break
    }
    if (new_DF$Date[i] > AAPLEPS$Date[j])
    {
      tempEPS <- abs(AAPLEPS$EPS[j] + AAPLEPS$EPS[j+1] + AAPLEPS$EPS[j+2] + AAPLEPS$EPS[j+3])
      print(new_DF$Date[i])
      print(tempEPS)
      new_DF$`PE ratio`[i] <- round(new_DF$Close[i]/tempEPS, digits = 2)
      print(new_DF$`PE ratio`[i])
      break
    }
  }
}

sapply(new_DF, function(x) sum(is.na(x)))

for (i in 1:miss)
{
  for (j in 1:nrow(AAPL))
  {
    if (new_DF$Date[i] == AAPL$Date[j])
    {
      AAPL$`PE ratio`[j] <- new_DF$`PE ratio`[i]
      break
    }
  }
}

sapply(AAPL, function(x) sum(is.na(x)))
optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
write.xlsx(fileMissPE, optFileName, showNA=FALSE)
# optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx' uncomment when running for HP

#**************************************************************************************
# SAMPLE OUTPUT OF HANDLING MISSING DEBT TO EQUITY RATIO FOR APPLE
# Formula = Long Term Debt / Equity where Equity = BS Total - Total Liabilities
# ALL CORRECT VALUES TAKEN FROM QUARTERLY REPORT
#**************************************************************************************

# Example calculation
Date <- 6/1/1998
Total_Current_liabilities <- 1389
LTD <- 953
Deferred_tax_Liabilities <- 213
Def_Rev_NC <- 0
Non_Current_Liabilities <- 0
Total_Liabilities <- Total_Current_liabilities + LTD + Deferred_tax_Liabilities + Def_Rev_NC + Non_Current_Liabilities
BS_Total <- 4041
Equity <- BS_Total - Total_Liabilities
Equity
Debt_Equity_Ratio <- LTD/Equity
Debt_Equity_Ratio

#**************************************************************************************
# REMAINING DEBT EQUITY RATIO CALCULATION IS DONE HERE
#**************************************************************************************

APPL_Q <- 'C:/Users/ayshw/Documents/DPA MERGING/APPLE Quarterly.xlsx'
APPL_Q <- read_excel(APPL_Q)
Cons_Q_A <- 'C:/Users/ayshw/Documents/DPA MERGING/Consolidated Quartery Data Apple.xlsx'
Cons_Q_A <- read_excel(Cons_Q_A)

n1 <- nrow(APPL_Q)
n2 <- nrow(Cons_Q_A)
for ( i in 1 : n1)
{
  for ( j in 1 : n2)
  {
    if ((APPL_Q$Date[i] == Cons_Q_A$Date[j])) 
    {
      if(is.na(APPL_Q$`Debt to Equity Ratio`[i]))
      {
        Date <- Cons_Q_A$Date[j]
        Total_Current_liabilities <- Cons_Q_A$Total_Current_liabilities[j]
        LTD <- Cons_Q_A$LTD[j]
        Deferred_tax_Liabilities <- Cons_Q_A$Deferred_tax_Liabilities[j]
        Def_Rev_NC <- Cons_Q_A$Def_Rev_NC[j]
        Non_Current_Liabilities <- Cons_Q_A$Non_Current_Liabilities[j]
        Total_Liabilities <- Total_Current_liabilities[j] + LTD[j] + Deferred_tax_Liabilities[j] + Def_Rev_NC[j] + Non_Current_Liabilities[j]
        BS_Total <- Cons_Q_A$BS_Total[j]
        Equity <- BS_Total[j] - Total_Liabilities
        Equity
        APPL_Q$Debt_Equity_Ratio[i] <- LTD[j]/Equity
        APPL_Q$Debt_Equity_Ratio[i]
      }
      else
      {
        break
      }
    }
  }
}

optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/APPLE Quarterly.xlsx'
write.xlsx(APPL_Q, optFileName, showNA=FALSE)

#**************************************************************************************
# SAMPLE OUTPUT OF HANDLING MISSING "RETURN ON INVESTED CAPITAL" FOR HPQ
# Formula = (Net Income - Dividend) / Capital
# ALL CORRECT VALUES TAKEN FROM QUARTERLY REPORT
#**************************************************************************************

# Example calculation
Date <- 12/31/2004
Return_on_Equity <- 10.16
Net_Income <- 295
Dividend <- 0
Capital <- 5790
Net_Income_Sub_Dividend <- Net_Income - Dividend
ROIC <- (Net_Income_Sub_Dividend/Capital) * 100
ROIC


#**************************************************************************************
# REMAINING RETURN ON INVESTED CAPITAL CALCULATION IS DONE HERE
#**************************************************************************************


HPQ_Q <- 'C:/Users/ayshw/Documents/DPA MERGING/HPQ Quarterly.xlsx'
HPQ_Q <- read_excel(HPQ_Q)
Cons_Q_H <- 'C:/Users/ayshw/Documents/DPA MERGING/Consolidated Quartery Data HPQ.xlsx'
Cons_Q_H <- read_excel(Cons_Q_H)

n1 <- nrow(HPQ_Q)
n2 <- nrow(Cons_Q_H)
for ( i in 1 : n1)
{
  for ( j in 1 : n2)
  {
    if ((HPQ_Q$Date[i] == Cons_Q_H$Date[j])) 
    {
      if(is.na(HPQ_Q$`Return On Invested Capital`[i]))
      {
        Date <- Cons_Q_H$Date[j]
        Return_on_Equity <- Cons_Q_H$Return_on_Equity[j]
        Net_Income <- Cons_Q_H$Net_Income[j]
        Dividend <- Cons_Q_H$Dividend[j]
        Capital <- Cons_Q_H$Capital[j]
        Net_Income_Sub_Dividend <- Net_Income - Dividend
        ROIC <- (Net_Income_Sub_Dividend/Capital) * 100
        HPQ_Q$`Return On Invested Capital`[i] <- ROIC
      }
      else
      {
        break
      }
    }
  }
}

optFileName <- 'C:/Users/ayshw/Documents/DPA MERGING/HPQ Quarterly.xlsx'
write.xlsx(HPQ_Q, optFileName, showNA=FALSE)

#**************************************************************************************
# REMOVE EV_EBIT MISSING VALUES AFTER FILLING MAJOR MISSING VALUES ABOVE
#**************************************************************************************
# 
# AAPL      <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/AAPL Dataset.xlsx'
# AAPL      <- read_excel(AAPL)
# sapply(AAPL, function(x) sum(is.na(x)))
# AAPL      <- na.omit(AAPL)

# HPQ       <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/HPQ Dataset.xlsx'
# HPQ       <- read_excel(HPQ)
# sapply(HPQ, function(x) sum(is.na(x)))
# HPQ       <- na.omit(HPQ)
# 
# INTC      <- 'C:/Users/ayshw/Documents/DPA MERGING/Final Datasets/INTC Dataset.xlsx'
# INTC      <- read_excel(INTC)
# sapply(INTC, function(x) sum(is.na(x)))
# INTC      <- na.omit(INTC)
