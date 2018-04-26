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



