library('readxl')
library('xlsx')

# FILL ALL QUARTERLY VALUES FOR APPLE

File1 <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/DataChgPr+PEratio.xlsx'
File2 <- 'C:/Users/ayshw/Documents/Aysh MS/Fall 2017/2nd Semester/DPA/Project/DataChgPr+PEratio.xlsx'
File1 <- read_excel(File1)
File2 <- read_excel(File2)
head(File1)
head(File2)

File1$Date <- as.Date(File1$Date,
                           format = "%Y-%m-%d")
File2$Date <- as.Date(File2$Date,
                      format = "%Y-%m-%d")
File2$Qvl  <- as.numeric(File2$Qvl)

n1 <- nrow(File1)
n2 <- nrow(File2)


for (i in n2)
{
  for (j in n1)
  {
    if (File1$Date[j] == File2$Date[i])
    {
      File1$ReturnOnAssets[j] <- File2$Qvl[i]
      j = j+1
      while ((File1$Date[j] > File2$Date[i]) & (File1$Date[j] < File2$Date[i+1]))
      {
        File1$ReturnOnAssets[j] <- File2$Qvl[i]
        j = j+1
      }
      break
    }
    
  }
}
    