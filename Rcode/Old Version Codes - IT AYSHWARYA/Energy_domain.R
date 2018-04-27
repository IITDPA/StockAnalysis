#=================================================================Reading Daily Data for OXY==========================================================================
OXY_volume<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_volume.csv",header = FALSE,col.names=c("date","volume"),sep=",")
OXY_low_price<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_low_price.csv",header = FALSE,col.names=c("date","low_price"),sep=",")
OXY_high_price<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_high_price.csv",header = FALSE,col.names=c("date","high_price"),sep=",")
OXY_market_cap<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_volume.csv",header = FALSE,col.names=c("date","volume"),sep=",")
OXY_earning_yield<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_earning_yield.csv",header = FALSE,col.names=c("date","earning_yield"),sep=",")
OXY_operating_earning_yield<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_operating_earning_yield.csv",header = FALSE,col.names=c("date","operating_earning_yield"),sep=",")
OXY_ps_ratio<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_ps_ratio.csv",header = FALSE,col.names=c("date","ps_ratio"),sep=",")
OXY_dividend_yield<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_dividend_yield.csv",header = FALSE,col.names=c("date","dividend_yield"),sep=",")
OXY_ev_revenues<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_ev_revenues.csv",header = FALSE,col.names=c("date","ev_revenues"),sep=",")
OXY_price_to_book_value<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_price_to_book_value.csv",header = FALSE,col.names=c("date","price_to_book_value"),sep=",")
OXY_ev_ebit<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_ev_ebit.csv",header = FALSE,col.names=c("date","ev_ebit"),sep=",")
OXY_pe_ratio<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_pe_ratio.csv",header = FALSE,col.names=c("date","pe_ratio"),sep=",")
OXY_peg_ratio<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_peg_ratio.csv",header = FALSE,col.names=c("date","peg_ratio"),sep=",")

OXY_data<-merge(OXY_volume,OXY_low_price,by="date")
temp<-list(OXY_high_price,OXY_market_cap,OXY_earning_yield,OXY_operating_earning_yield,OXY_ps_ratio,OXY_dividend_yield,OXY_ev_revenues,OXY_price_to_book_value,OXY_ev_ebit,OXY_pe_ratio,OXY_peg_ratio)
for( x in temp){
  OXY_data<-merge(OXY_data,x,by="date",all=TRUE)
}
View(OXY_data)

a <- as.Date(OXY_data$date,format="%B %d %Y") 
b <- as.Date(OXY_data$date,format="%b. %d %Y") 
a[is.na(a)] <- b[!is.na(b)] 
OXY_data$date <- a 
View(OXY_data)

write.csv(OXY_data,file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_data.csv")

#=================================================================Reading Daily Data OXY End======================================================================


#=================================================================Merging Daily and Quaterly OXY Data=============================================================

setwd("/Users/ishwaryachemarthi/Desktop/OXY/quarterly")
path="/Users/ishwaryachemarthi/Desktop/OXY/quarterly/"
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
list.filenames
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  a <- as.Date(element$date,format="%B %d %Y") 
  b <- as.Date(element$date,format="%b. %d %Y") 
  a[is.na(a)] <- b[!is.na(b)]
  element$date <- a
  write.csv(element,file=file_path)
  i=i+1
}
intermediate_merge<-OXY_data
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  intermediate_merge<-merge(intermediate_merge,element,by="date",all=TRUE)
  print(nrow(intermediate_merge))
  i=i+1
}
View(intermediate_merge)

for(i in 15:30){
  st<-1
  ed<-0
  for(index in 1:nrow(intermediate_merge)){
    if(!is.na(intermediate_merge[index,i])){
      ed<-index
      dt<-intermediate_merge[index,i]
      for(j in st:ed){
        intermediate_merge[j,i]<-dt
      }
      st<-index+1
    }
    if(index==nrow(intermediate_merge))
    {
      intermediate_merge[,i][is.na(intermediate_merge[,i])] <- dt
    }
  }
}

View(intermediate_merge)
View(unique(intermediate_merge))
write.csv(unique(intermediate_merge),file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_Quaterly_Daily_combined_data.csv")
#=================================================================Merging Daily and Quaterly OXY Data=============================================================


#=================================================================Merging Micro OXY Data==========================================================================
macro_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/macroeconomicfactors/Macro_Data.csv",sep = ",",col.names = c("date","GDP","GNP","IPI","LT","CPI"))
oxy_data_quarterly<-read.csv(file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_Quaterly_Daily_combined_data.csv",sep=",")


macro_data$date<- as.Date(macro_data$date,format="%m/%d/%y")
oxy_data_quarterly$date<- as.Date(oxy_data_quarterly$date,format="%Y-%m-%d")
oxy_data_quarterly$X<-NULL
oxy_full_data<-merge(oxy_data_quarterly,macro_data,by="date",all=TRUE)
write.csv(oxy_full_data,file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_fulldata.csv")
View(oxy_full_data)

#=================================================================Merging Micro OXY Data End======================================================================

#=================================================================Removing Duplicate OXY Data======================================================================

oxy_data_dup<-read.csv("/Users/ishwaryachemarthi/Desktop/OXY/OXY_fulldata.csv",sep = ",")
oxy_data_dup<-oxy_data_dup[rowSums(is.na(oxy_data_dup[ , 4:10])) == 0, ]
View(oxy_data_dup)
write.csv(oxy_data_dup,file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_FINAL_DATA.csv")

#=================================================================Removing Duplicate OXY Data End======================================================================






#=================================================================Reading Daily COP Data==========================================================================
setwd("/Users/ishwaryachemarthi/Desktop/COP/")
path="/Users/ishwaryachemarthi/Desktop/COP/"
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
list.filenames
print(list.filenames)
COP<-list()
i=1
while(i<14)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,header = FALSE,col.names=c("date",colm),sep=",")
  COP[[length(COP)+1]]<-element
  i=i+1
}

COP_data<-merge(COP[1],COP[2],by="date",all=TRUE)
x=3
while(x<=length(COP)){
  COP_data<-merge(COP_data,COP[x],by="date",all=TRUE)
  x=x+1
}

a <- as.Date(COP_data$date,format="%B %d %Y") 
b <- as.Date(COP_data$date,format="%b. %d %Y") 
a[is.na(a)] <- b[!is.na(b)] 
COP_data$date <- a 
View(COP_data)

write.csv(COP_data,file="/Users/ishwaryachemarthi/Desktop/COP/COP_data.csv")
#=================================================================Reading Daily COP Data end==========================================================================


#=================================================================Merging Daily and Quaterly COP Data=============================================================

setwd("/Users/ishwaryachemarthi/Desktop/COP/quarterly")
path="/Users/ishwaryachemarthi/Desktop/COP/quarterly/"
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
list.filenames
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  a <- as.Date(element$date,format="%B %d %Y") 
  b <- as.Date(element$date,format="%b. %d %Y") 
  a[is.na(a)] <- b[!is.na(b)]
  element$date <- a
  write.csv(element,file=file_path)
  i=i+1
}
intermediate_merge<-COP_data
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  intermediate_merge<-merge(intermediate_merge,element,by="date",all=TRUE)
  print(nrow(intermediate_merge))
  i=i+1
}
View(intermediate_merge)

for(i in 15:30){
  st<-1
  ed<-0
  for(index in 1:nrow(intermediate_merge)){
    if(!is.na(intermediate_merge[index,i])){
      ed<-index
      dt<-intermediate_merge[index,i]
      for(j in st:ed){
        intermediate_merge[j,i]<-dt
      }
      st<-index+1
    }
    if(index==nrow(intermediate_merge))
    {
      intermediate_merge[,i][is.na(intermediate_merge[,i])] <- dt
    }
  }
}

View(intermediate_merge)
View(unique(intermediate_merge))
write.csv(unique(intermediate_merge),file="/Users/ishwaryachemarthi/Desktop/COP/COP_Quaterly_Daily_combined_data.csv")

#=================================================================Merging Daily and Quaterly COP Data END=============================================================


#=================================================================Merging Micro COP Data==========================================================================
macro_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/macroeconomicfactors/Macro_Data.csv",sep = ",",col.names = c("date","GDP","GNP","IPI","LT","CPI"))
cop_data_quarterly<-read.csv(file="/Users/ishwaryachemarthi/Desktop/COP/COP_Quaterly_Daily_combined_data.csv",sep=",")


macro_data$date<- as.Date(macro_data$date,format="%m/%d/%y")
cop_data_quarterly$date<- as.Date(cop_data_quarterly$date,format="%Y-%m-%d")
cop_data_quarterly$X<-NULL
cop_full_data<-merge(cop_data_quarterly,macro_data,by="date",all=TRUE)
write.csv(cop_full_data,file="/Users/ishwaryachemarthi/Desktop/COP/COP_fulldata.csv")
View(cop_full_data)

#=================================================================Merging Micro COP Data End======================================================================

#=================================================================Removing Duplicate COP Data======================================================================

cop_data_dup<-read.csv("/Users/ishwaryachemarthi/Desktop/COP/COP_fulldata.csv",sep = ",")
cop_data_dup<-cop_data_dup[rowSums(is.na(cop_data_dup[ , 4:10])) == 0, ]
View(cop_data_dup)
write.csv(cop_data_dup,file="/Users/ishwaryachemarthi/Desktop/COP/COP_FINAL_DATA.csv")

#=================================================================Removing Duplicate COP Data End======================================================================




#=================================================================Reading Daily MRO Data ==========================================================================

setwd("/Users/ishwaryachemarthi/Desktop/MRO/")
path="/Users/ishwaryachemarthi/Desktop/MRO/"
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
list.filenames
print(list.filenames)
MRO<-list()
i=1
while(i<14)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,header = FALSE,col.names=c("date",colm),sep=",")
  MRO[[length(MRO)+1]]<-element
  i=i+1
}

MRO_data<-merge(MRO[1],MRO[2],by="date",all=TRUE)
x=3
while(x<=length(MRO)){
  MRO_data<-merge(MRO_data,MRO[x],by="date",all=TRUE)
  x=x+1
}
View(MRO_data)


a <- as.Date(MRO_data$date,format="%B %d %Y") 
b <- as.Date(MRO_data$date,format="%b. %d %Y") 
a[is.na(a)] <- b[!is.na(b)] 
MRO_data$date <- a 
View(MRO_data)
write.csv(MRO_data,file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_data.csv")

#=================================================================Reading Daily MRO Data end==========================================================================


#=================================================================Merging Daily and Quaterly MRO Data=============================================================

setwd("/Users/ishwaryachemarthi/Desktop/MRO/quarterly")
path="/Users/ishwaryachemarthi/Desktop/MRO/quarterly/"
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
list.filenames
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  a <- as.Date(element$date,format="%B %d %Y") 
  b <- as.Date(element$date,format="%b. %d %Y") 
  a[is.na(a)] <- b[!is.na(b)]
  element$date <- a
  write.csv(element,file=file_path)
  i=i+1
}
intermediate_merge<-MRO_data
i=1
while(i<17)
{
  fl<-list.filenames[i]
  colm<-substr(fl,5,nchar(fl)-4)
  file_path<-paste(path,fl, sep="")
  element<-read.csv(file_path,col.names=c("date",colm),sep=",")
  intermediate_merge<-merge(intermediate_merge,element,by="date",all=TRUE)
  print(nrow(intermediate_merge))
  i=i+1
}
View(intermediate_merge)

for(i in 14:29){
  st<-1
  ed<-0
  for(index in 1:nrow(intermediate_merge)){
    if(!is.na(intermediate_merge[index,i])){
      ed<-index
      dt<-intermediate_merge[index,i]
      for(j in st:ed){
        intermediate_merge[j,i]<-dt
      }
      st<-index+1
    }
    if(index==nrow(intermediate_merge))
    {
      intermediate_merge[,i][is.na(intermediate_merge[,i])] <- dt
    }
  }
}

View(intermediate_merge)
View(unique(intermediate_merge))
write.csv(unique(intermediate_merge),file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_Quaterly_Daily_combined_data.csv")

#=================================================================Merging Daily and Quaterly MRO Data end=============================================================


#=================================================================Merging Micro COP Data==========================================================================
macro_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/macroeconomicfactors/Macro_Data.csv",sep = ",",col.names = c("date","GDP","GNP","IPI","LT","CPI"))
mro_data_quarterly<-read.csv(file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_Quaterly_Daily_combined_data.csv",sep=",")


macro_data$date<- as.Date(macro_data$date,format="%m/%d/%y")
mro_data_quarterly$date<- as.Date(mro_data_quarterly$date,format="%Y-%m-%d")
mro_data_quarterly$X<-NULL
mro_full_data<-merge(mro_data_quarterly,macro_data,by="date",all=TRUE)
write.csv(mro_full_data,file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_fulldata.csv")
View(mro_full_data)

#=================================================================Merging Micro COP Data End======================================================================

#=================================================================Removing Duplicate COP Data======================================================================

mro_data_dup<-read.csv("/Users/ishwaryachemarthi/Desktop/MRO/MRO_fulldata.csv",sep = ",")
mro_data_dup<-mro_data_dup[rowSums(is.na(mro_data_dup[ , 4:10])) == 0, ]
View(mro_data_dup)
write.csv(mro_data_dup,file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_FINAL_DATA.csv")

#=================================================================Removing Duplicate COP Data End======================================================================



#=================================================================Dealing Missing Values OXY ======================================================================

OXY_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/ENERGY/OXY/OXY_FINAL_DATA.csv")
View(OXY_final_data)
#OXY_final_data$date<-as.Date(OXY_final_data$date,format="%m/%d/%y")
OXY_final_data$X<-NULL


colnames(OXY_final_data)
colSums(is.na(OXY_final_data1))

install.packages("mice")
library(mice)
#MICE (Mulitple Imputation by Chained Equations)

OXY_final_data1<-OXY_final_data
#OXY_final_data1$date<-NULL

#imp1 <- mice(OXY_final_data1, m =1,method ="pmm",seed = 500)

missingrows<-which(is.na(OXY_final_data1$ev_ebit), arr.ind=TRUE)
missingrows1<-which(is.na(OXY_final_data1$pe_ratio), arr.ind=TRUE)

OXY_final_data1<-OXY_final_data1[-c(missingrows[1:377]),]
OXY_final_data1<-OXY_final_data1[-c(missingrows1[1:486]),]

OXY_final_data<-OXY_final_data1
write.csv(OXY_final_data,file="/Users/ishwaryachemarthi/Desktop/OXY/OXY_FINAL_DATA.csv")

#=================================================================Dealing Missing Values MRO ======================================================================


MRO_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/TINA/MRO_FINAL_DATA.csv")
COP_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/TINA/COP_FINAL_DATA.csv")


pairs(MRO,x=2:25,y=2:25)

View(MRO_final_data)
MRO_final_data$date<-as.Date(MRO_final_data$date,format="%m/%d/%y")
MRO_final_data$X<-NULL
MRO_final_data$X.1<-NULL

colSums(is.na(MRO_final_data))
MRO_final_data$peg_ratio<-NULL
missingrows2<-which(is.na(MRO_final_data$pe_ratio), arr.ind=TRUE)
MRO_final_data<-MRO_final_data[-c(missingrows[1:191]),]


write.csv(MRO_final_data,file="/Users/ishwaryachemarthi/Desktop/MRO/MRO_FINAL_DATA.csv")


#=================================================================Dealing Missing Values COP ======================================================================
COP_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/ENERGY/COP/COP_FINAL_DATA.csv")
View(COP_final_data)
COP_final_data$date<-as.Date(COP_final_data$date,format="%m/%d/%y")
COP_final_data$X<-NULL
COP_final_data$X.1<-NULL

colSums(is.na(COP_final_data))
COP_final_data$peg_ratio<-NULL
COP_final_data$volume<-NULL
missingrows3<-which(is.na(COP_final_data$pe_ratio), arr.ind=TRUE)
COP_final_data<-COP_final_data[-c(missingrows[1:126]),]

write.csv(COP_final_data,file="/Users/ishwaryachemarthi/Desktop/COP/COP_FINAL_DATA.csv")

#=======================================================================================================================================


#=======================================================================================================================================

#normalisation of numeric variables in OXY data MRO and COP data
onlynumeric<-names(OXY_final_data)[sapply(OXY_final_data, class) == "numeric" ]
integervariables<-names(OXY_final_data)[sapply(OXY_final_data, class) == "integer"]
numericvariables<-c(onlynumeric,integervariables)
for( i in numeric){
  normalize(OXY_final_data[,i])
}

clo

numeric1<-names(MRO_final_data)[sapply(MRO_final_data, class) == "numeric" ]
for( i in numeric1){
  normalize(MRO_final_data[,i])
}

View(MRO_final_data)

numeric2<-names(COP_final_data)[sapply(COP_final_data, class) == "numeric" ]
for( i in numeric2){
  normalize(COP_final_data[,i])
}

View(COP_final_data)


#====================================================================================================================
#corelation 


corrplot(cor(best_lr_model[sapply(best_lr_model, is.numeric)]))
corrplot(cor(COP_final_data[sapply(COP_final_data, is.numeric)]))

#====================================================================================================================
#spliting data to train and test 
View(OXY_final_data)

library(caret)
install.packages('e1071', dependencies=TRUE)

set.seed(123)
in1 <- createDataPartition(y = OXY_final_data[['close']], list = FALSE, p = .8)
oxy_train <- OXY_final_data[in1,]
oxy_test <- OXY_final_data[-in1,]


set.seed(123)
in2 <- createDataPartition(y = MRO_final_data[['close']], list = FALSE, p = .8)
mro_train <- MRO_final_data[in2,]
mro_test <- MRO_final_data[-in2,]

set.seed(123)
in3 <- createDataPartition(y = COP_final_data[['close']], list = FALSE, p = .8)
cop_train <- COP_final_data[in3,]
cop_test <- COP_final_data[-in3,]


#====================================================================================================================
#fitting the Logistic regression model
oxy_logreg<-read.csv(file="/Users/ishwaryachemarthi/Desktop/ENERGY/OXY/oxy_logreg.csv")
OXY_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/tina/OXY_FINAL_DATA.csv")
oxy_logreg<-OXY_final_data
View(oxy_logreg)


oxy_logreg$close_lr


for(i in 1:nrow(OXY_final_data)){
     if((OXY_final_data[i,'open']- OXY_final_data[i,'close'])>=0){
       
       oxy_logreg[i,'close_lr']<- 0
     }
     else
       oxy_logreg[i,'close_lr']<- 1
}

View(oxy_logreg)

#deleting since these variables are directly related to response variable
#to avoid over fitting
oxy_logreg$close<-NULL
oxy_logreg$high<-NULL
oxy_logreg$low<-NULL
oxy_logreg$X<-NULL
oxy_logreg$date<-NULL

View(oxy_logreg)

set.seed(1000)
sampleind <- createDataPartition(y =oxy_logreg$close_lr, list = FALSE,p=0.8)
oxy_lr_train <- oxy_logreg[sampleind,]
oxy_lr_test <- oxy_logreg[-sampleind,]
nrow(oxy_lr_test)
nrow(oxy_lr_train)
nrow(oxy_logreg)
stopifnot(nrow(oxy_lr_train) + nrow(oxy_lr_test) == nrow(oxy_logreg))

colnames(oxy_logreg)

best_lr_model<-NULL

best_lr_model <-oxy_lr_train[, c("ps_ratio","ev_revenues","price_to_book_value","ev_ebit","pe_ratio"
                                  ,"current_ratio","days_payables_outstanding","days_sales_outstanding",
                                  "debt_equity_ratio_annual","fulmer_h_score","quick_ratio",
                                  "receivables_turnover_ttm","GDP","GNP","IPI","LT","CPI","open","volume",
                                 "operating_earning_yield","earning_yield","dividend_yield",
                                 "cash_dividend_payout_ratio_annual","eps_est_long_term_growth",
                                 "gross_profit_margin_ttm","payout_ratio","profit_margin_ttm",
                                 "return_on_assets","return_on_equity","return_on_invested_capital",
                                 "book_value_of_equity","close_lr")]


best_lr_model_test <-oxy_lr_test[,c("ps_ratio","ev_revenues","price_to_book_value","ev_ebit","pe_ratio"
                                    ,"current_ratio","days_payables_outstanding","days_sales_outstanding",
                                    "debt_equity_ratio_annual","fulmer_h_score","quick_ratio",
                                    "receivables_turnover_ttm","GDP","GNP","IPI","LT","CPI","open","volume",
                                    "operating_earning_yield","earning_yield","dividend_yield",
                                    "cash_dividend_payout_ratio_annual","eps_est_long_term_growth",
                                    "gross_profit_margin_ttm","payout_ratio","profit_margin_ttm",
                                    "return_on_assets","return_on_equity","return_on_invested_capital",
                                    "book_value_of_equity","close_lr")]



## Perform
mylogit <- glm(best_lr_model$close_lr ~ ., data = best_lr_model, family = binomial)

summary(mylogit)

print(ncol(best_lr_model_test))

pred <- predict(mylogit,best_lr_model_test,type='response')
results <- ifelse(pred > 0.52,1,0)
table(results,oxy_lr_test$close_lr)


plot(mylogit)
step(mylogit,direction="both")


best_logistic_model <-bestglm(Xy = best_lr_model,
          family = gaussian,          # binomial family for logit
          IC = "AIC",                 # AIC chosen to select models
          TopModels = 5,
          method = "exhaustive",
          CVArgs = "default")

summary(best_logistic_model)

install.packages("pROC")
library(pROC)

plot(roc(best_lr_model$close_lr,best_lr_model$close_lr, direction="<"),
     col="yellow", lwd=5, main="ROC CURVE LOGISTIC REGRESSION")

Model_sig_var<-glm(formula=best_lr_model$close_lr ~ ps_ratio + ev_revenues + price_to_book_value + 
  ev_ebit + pe_ratio + current_ratio + days_payables_outstanding + 
  days_sales_outstanding + debt_equity_ratio_annual + fulmer_h_score + 
  quick_ratio + receivables_turnover_ttm + GDP + GNP + IPI + 
  LT + CPI + open + volume + operating_earning_yield + earning_yield + 
  dividend_yield + cash_dividend_payout_ratio_annual + eps_est_long_term_growth + 
  gross_profit_margin_ttm + payout_ratio + profit_margin_ttm + 
  return_on_assets + return_on_equity + return_on_invested_capital,data=best_lr_model,family = binomial)

summary(Model_sig_var)

predicted <- predict.glm(Model_sig_var,newdata=best_lr_model_test,type = 'response')

#set threshold as .75 probability
results1 <- ifelse(predicted > 0.50,1,0)
table(results1,oxy_lr_test$close_lr)

#=============================================================================================================

#Shiny R visualtisation and deployment of logistic regression algorithm output
ui <- fluidPage(
  titlePanel("Stock Analysis"),
  sidebarLayout(
    sidebarPanel(
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "domain",
                  label = "Choose an Domain:",
                  choices = c("IT","Finance","Energy","Pharmaceutical","Algorithm")),
      uiOutput("company")
      
    ),
    
    mainPanel(
      h1("output results"),
      uiOutput("tb")
    )
  )
)





server <- function(input, output) {
  output$company<-renderUI({
    if(input$domain=="Energy")
    {
      var<-c("COP","OXY","MRO")
      selectInput("companyName","Select the company:",choices = var)
    }
  })
  
  output$tb<-renderUI({
    if(input$companyName=="COP")
    {
      HTML('<img src="Rplot.png", height="400px"    
           style="float:right"/>','<p style="color:black"></p>')
    }
    })
  
  output$volume<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      textInput("volume_input", h3("Volume"), value = "Enter a value") 
    }
  })
  output$open<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      textInput("open_input", h3("open"), value = "Enter a value") 
    }
  })
  output$debt_equity<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      textInput("debt_equity_input", h3("debt_equity"), value = "Enter a value") 
    }
  })
  output$current_ratio<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      textInput("current_ratio_input", h3("current_ratio"), value = "Enter a value") 
    }
  })
  output$price_to_book_value<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      textInput("price_to_book_value_input", h3("price_to_book_value"), value = "Enter a value") 
    }
  })
  
  
  
  output$lg_results<-renderUI({
    if(input$algorithm=="Logistic Regression")
    {
      paste("predicted value")
      observeEvent(input$action,{
        vars<-c(2.7760,
                2.756,
                as.numeric(input$price_to_book_value_input),
                6.013,
                10.200,
                as.numeric(input$current_ratio_input),
                158.60,
                91.62,
                2.021,
                11.73,
                1.0330,
                4.611,
                9.00e+12,
                1.15e+13,
                84.05,
                5.46,
                163.2,
                as.numeric(input$open_input),
                as.numeric(input$volume_input),
                14.64,
                9.81,
                1.61,
                -38.93,
                5.01,
                70.48,
                11.25,29.03,20.37,32.57,30.25,3.6e+08,0)
        
        best_lr_model_test[nrow(best_lr_model_test)+1,]<-vars
        pred <- predict(mylogit,best_lr_model_test[nrow(best_lr_model_test),],type='response')
        View(pred)
        var_res<-0
        if(pred>=0.5)
        {
          var_res<-1      
        }  
        paste("predicted value")
      })
      OXY_final_data<-read.csv(file="/Users/ishwaryachemarthi/Desktop/tina/OXY_FINAL_DATA.csv")
      oxy_logreg<-OXY_final_data
      oxy_logreg$close_lr
      for(i in 1:nrow(OXY_final_data)){
        if((OXY_final_data[i,'open']- OXY_final_data[i,'close'])>=0){
          
          oxy_logreg[i,'close_lr']<- 0
        }
        else
          oxy_logreg[i,'close_lr']<- 1
      }
      set.seed(1000)
      sampleind <- createDataPartition(y =oxy_logreg$close_lr, list = FALSE,p=0.8)
      oxy_lr_train <- oxy_logreg[sampleind,]
      oxy_lr_test <- oxy_logreg[-sampleind,]
      stopifnot(nrow(oxy_lr_train) + nrow(oxy_lr_test) == nrow(oxy_logreg))
      
      best_lr_model<-NULL
      best_lr_model <-oxy_lr_train[, c("ps_ratio","ev_revenues","price_to_book_value","ev_ebit","pe_ratio"
                                       ,"current_ratio","days_payables_outstanding","days_sales_outstanding",
                                       "debt_equity_ratio_annual","fulmer_h_score","quick_ratio",
                                       "receivables_turnover_ttm","GDP","GNP","IPI","LT","CPI","open","volume",
                                       "operating_earning_yield","earning_yield","dividend_yield",
                                       "cash_dividend_payout_ratio_annual","eps_est_long_term_growth",
                                       "gross_profit_margin_ttm","payout_ratio","profit_margin_ttm",
                                       "return_on_assets","return_on_equity","return_on_invested_capital",
                                       "book_value_of_equity","close_lr")]
      
      
      best_lr_model_test <-oxy_lr_test[,c("ps_ratio","ev_revenues","price_to_book_value","ev_ebit","pe_ratio"
                                          ,"current_ratio","days_payables_outstanding","days_sales_outstanding",
                                          "debt_equity_ratio_annual","fulmer_h_score","quick_ratio",
                                          "receivables_turnover_ttm","GDP","GNP","IPI","LT","CPI","open","volume",
                                          "operating_earning_yield","earning_yield","dividend_yield",
                                          "cash_dividend_payout_ratio_annual","eps_est_long_term_growth",
                                          "gross_profit_margin_ttm","payout_ratio","profit_margin_ttm",
                                          "return_on_assets","return_on_equity","return_on_invested_capital",
                                          "book_value_of_equity","close_lr")]
      
      mylogit <- glm(best_lr_model$close_lr ~ ., data = best_lr_model, family = binomial)
      
      
      
      
    }
    
    output$company<-renderUI({
      if(input$domain=="Algorithm")
      {
        var<-c("TimeSeries","NaiveBayes","LogisticRegression","LinearRegression","DecisionTrees")
        selectInput("companyName","Select the company:",choices = var)
      }
    })
    
    
    
    output$tb<-renderUI({
      if(input$domain=="Algorithm")
      {
        if(input$companyName=="LogisticRegression")
        {
          HTML(
            
            '<img src="pvalue.png", height="600px",width="500px",padding="40px",title="IMPORTANT FEATURES" 
            style="float:center"/>','<p style="color:black"></p>',
            
            '<img src="confusion.png", height="200px",width="400px",padding="100px"    
            style="float:center"/>','<p style="color:black"></p>',
            
            
            
            '<img src="accuracy.png", height="100px",width="300px",padding="100px" , 
            style="float:center"/>','<p style="color:black"></p>',
            
            
            
            '<img src="AIC.png", height="800px",width="500px",padding="40px", 
            style="float:center"/>','<p style="color:black"></p>',
            
            
            '<img src="Rplot.png", height="400px",width="500px",padding="40px",title="IMPORTANT FEATURES" 
            style="float:center"/>','<p style="color:black"></p>',
            
            '<img src="Rplot01.png", height="400px",width="500px",padding="40px"    
            style="float:center"/>','<p style="color:black"></p>'
            
          )
          #COP IMAGES
        }
        else
          if(input$companyName=="LinearRegression")
          {
            HTML(
              '<img src="fin1.png", height="400px",width="300px",padding="40px",title="IMPORTANT FEATURES" 
              style="float:center"/>','<p style="color:black"></p>',
              
              '<img src="fin2.png", height="400px",width="300px",padding="40px"    
              style="float:center"/>','<p style="color:black"></p>',
              
              
              
              '<img src="fin3.png", height="400px",width="300px",padding="40px" , 
              style="float:center"/>','<p style="color:black"></p>',
              
              
              
              '<img src="fin4.png", height="400px",width="300px",padding="40px", 
              style="float:center"/>','<p style="color:black"></p>',
              
              '<img src="fin5.png", height="400px",width="300px",padding="40px",title="IMPORTANT FEATURES" 
              style="float:center"/>','<p style="color:black"></p>'
              
              
              
              
            )
            
          }
        else
          if(input$companyName=="TimeSeries")
          {
            HTML(
              '<img src="time1.png", height="400px",width="300px",padding="40px",title="IMPORTANT FEATURES" 
              style="float:center"/>','<p style="color:black"></p>',
              
              '<img src="time2.png", height="400px",width="300px",padding="40px"    
              style="float:center"/>','<p style="color:black"></p>',
              
              
              
              '<img src="time3.png", height="400px",width="300px",padding="40px" , 
              style="float:center"/>','<p style="color:black"></p>',
              
              
              
              '<img src="time4.png", height="400px",width="300px",padding="40px", 
              style="float:center"/>','<p style="color:black"></p>'
              
              
              
              
            )
            
          }
        else
          if(input$companyName=="NaiveBayes")
          {
            
            HTML(
              
              '<img src="roc1.png", height="400px"    
              style="float:right"/>','<p style="color:black"></p>',
              
              
              '<img src="roc2.png", height="400px"    
              style="float:right"/>','<p style="color:black"></p>'
              
              
            )
          }
        else
          if(input$companyName=="DecisionTrees")
          {
            HTML('<img src="dt1.png", height="400px",width="300px",padding="40px"    
                 style="float:center"/>','<p style="color:black"></p>',
                 
                 
                 
                 '<img src="dt2.png", height="400px",width="300px",padding="40px" , 
                 style="float:center"/>','<p style="color:black"></p>'
                 
            )
          }
      }
      else
        if(input$domain=="IT")
          
        {
          HTML(
            '<img src="VIAPPLE.png", height="400px",width="300px",padding="40px"    
            style="float:center"/>','<p style="color:black"></p>',
            
            
            
            '<img src="VIHP.png", height="400px",width="300px",padding="40px" , 
            style="float:center"/>','<p style="color:black"></p>',
            
            
            
            '<img src="VIINTEL.png", height="400px",width="300px",padding="40px", 
            style="float:center"/>','<p style="color:black"></p>'
            
            
          )
        }
      else
        if(input$domain=="Finance")
          
        {
          HTML('<img src="finance1.png", height="400px",width="400px",padding="40px"    
               style="float:center"/>','<p style="color:black"></p>',

               '<img src="finance2.png", height="400px",width="400px",padding="40px" , 
               style="float:center"/>','<p style="color:black"></p>',
           
               '<img src="finance3.png", height="400px",width="400px",padding="40px", 
               style="float:center"/>','<p style="color:black"></p>',
               
               '<img src="basicstat.png", height="400px",width="400px",padding="40px",title="IMPORTANT FEATURES" 
               style="float:center"/>','<p style="color:black"></p>'
               
               
          )
        }
      else
        if(input$domain=="Pharmaceutical")
          
        {
          HTML('<img src="oxy_imp.png", height="100px",width="400px",padding="40px",title="IMPORTANT FEATURES" 
               style="float:center"/>','<p style="color:black"></p>',
                          '<img src="basicstat.png", height="400px",width="400px",padding="40px"    
               style="float:center"/>','<p style="color:black"></p>',
                          '<img src="pairs.png", height="400px",width="400px",padding="40px" , 
               style="float:center"/>','<p style="color:black"></p>',
         '<img src="OXY_Corelation.png", height="400px",width="400px",padding="40px", 
               style="float:center"/>','<p style="color:black"></p>'
               
               
               
          )
        }
      else
        if(input$domain=="Energy")
          
        {
          HTML(
            
            '<img src="oxy_imp.png", height="100px",width="400px",padding="40px",title="IMPORTANT FEATURES" 
            style="float:center"/>','<p style="color:black"></p>',
                '<img src="basicstat.png", height="400px",width="400px",padding="40px"    
            style="float:center"/>','<p style="color:black"></p>',
            '<img src="pairs.png", height="400px",width="400px",padding="40px" , 
            style="float:center"/>','<p style="color:black"></p>',
              '<img src="OXY_Corelation.png", height="400px",width="400px",padding="40px", 
            style="float:center"/>','<p style="color:black"></p>'
            
            
            
            
            
          )
        }
      
      
      })
    
    
        }
    shinyApp(ui=ui, server=server)
#==============================================================================================    

