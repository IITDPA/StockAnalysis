#reading data of OXY company 

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




#reading data from COP and MRO companies in a better way 



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


#This below code deals different date formats present in a single column
a <- as.Date(COP_data$date,format="%B %d %Y") 
b <- as.Date(COP_data$date,format="%b. %d %Y") 
a[is.na(a)] <- b[!is.na(b)] 
COP_data$date <- a 
View(COP_data)

write.csv(COP_data,file="/Users/ishwaryachemarthi/Desktop/COP/COP_data.csv")



#----------------------------------------------------------


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




#---------------------------------------------------------------------------------------------------------


