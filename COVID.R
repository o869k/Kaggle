library(data.table)
library(rworldmap)
library(tidyquant)
library(wppExplorer)
# pass SP500 ticker ^GSPC to tq_get function
one_ticker = tq_get("^GSPC", from = "1999-01-01")
one_ticker = data.frame(one_ticker)
one_ticker = merge(one_ticker,expand.grid(date=seq.Date(from = min(one_ticker$date),
                                                        to = max(one_ticker$date),
                                                        by = "days")),all=T)
one_ticker = na.locf(one_ticker)
#one_ticker$diff = one_ticker$close
#In 2008 financial crisis the sp500 plumbbed from max 1500 to min 700 in 1.5 years - thats 55% drop 
#In 2020 coronavirus event the sp500 plumbbed so far from 3300 to 2500 - thats 25% drop.

data(countryExData)
Popualtion <- read.csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")
Popualtion <- Popualtion[!duplicated(Popualtion$Country.Name,fromLast = T),] #save last data ppoint per country
Confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = T,stringsAsFactors = F)
Deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",header = T,stringsAsFactors = F)
Recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",header = T,stringsAsFactors = F)
Active <- Confirmed[,-c(1:4)]-Recovered[,-c(1:4)]-Deaths[,-c(1:4)]
Active <- cbind(Confirmed[,c(1:4)],Active)
Active[is.na(Active)] = 0
Active[Active==Inf] = 1
Deaths_Rate <- round(100*Deaths[,-c(1:4)]/Confirmed[,-c(1:4)],3)
Deaths_Rate <- cbind(Confirmed[,c(1:4)],Deaths_Rate)
Deaths_Rate[is.na(Deaths_Rate)] = 0
Deaths_Rate[Deaths_Rate==Inf] = 1
Recovered_Rate <- round(100*Recovered[,-c(1:4)]/Confirmed[,-c(1:4)],3)
Recovered_Rate <- cbind(Confirmed[,c(1:4)],Recovered_Rate)
Recovered_Rate[is.na(Recovered_Rate)] = 0
Recovered_Rate[Recovered_Rate==Inf] = 1
New <- t(diff(t(as.matrix(Confirmed[,-c(1:4)]))))
New <- cbind(rep(0,nrow(New)),New)
New <- cbind(Confirmed[,c(1:4)],New)
Confirmed_norm <- merge(Confirmed,Popualtion[,c("Country.Name","Value")],by.x="Country.Region",by.y="Country.Name",all.x=T,sort=F)
Confirmed_norm[,-c(1:4,ncol(Confirmed_norm))] <- round(100*Confirmed_norm[,-c(1:4,ncol(Confirmed_norm))]/Confirmed_norm$Value,6)
Confirmed_norm$Value = NULL
Confirmed_norm = Confirmed_norm[order(Confirmed_norm[,ncol(Confirmed_norm)],decreasing = T),]

Plot_amounts_per_day <- function(data_used=Confirmed,country="") {
    if (country[1]!="") {
        datasest <- colSums(data_used[data_used$Country.Region %in% country,-c(1:4)],na.rm=T)
        if (length(country)>1) country="Multiple Countries"
    } else {
        datasest <- colSums(data_used[,-c(1:4)],na.rm=T)
        country = "all world"
    }
    if (identical(data_used,Confirmed)) type="Confirmed"
    if (identical(data_used,Deaths)) type="Deaths"
    if (identical(data_used,Recovered)) type="Recovered"
    if (identical(data_used,Active)) type="Active"
    if (identical(data_used,Deaths_Rate)) type="Deaths_Rate"
    if (identical(data_used,Recovered_Rate)) type="Recovered_Rate"
    if (identical(data_used,New)) type="New Cases"
    if (identical(data_used,Confirmed_norm)) type="Confirmed Nrom"
    par(mfrow=c(2,1))
    
    #Total Amounts
    lo <- loess(as.numeric(datasest)~c(1:length(datasest)))
    plot(as.Date(gsub("X","",names(datasest)),format = "%m.%d.%y"),as.numeric(datasest),type="b",xlab="Time",ylab = NA,main=paste0("Total ",type," - ",country),lwd=2, xaxt = "n", yaxt = "n")
    axis(4)
    lines(as.Date(gsub("X","",names(datasest)),format = "%m.%d.%y"),predict(lo), col='red', lwd=1,lty=2)
    axis(1, as.Date(gsub("X","",names(datasest)),format = "%m.%d.%y"), format(as.Date(gsub("X","",names(datasest)),format = "%m.%d.%y"), "%b %d"), cex.axis = .7)
    abline(h=0)
    
    #Rate of new Cases
    rate_dataset = round(100*(tail(datasest,-1)-head(datasest,-1))/head(datasest,-1),2)
    rate_dataset[is.na(rate_dataset)] = 0
    rate_dataset[rate_dataset==Inf] = 0
    lo <- loess(as.numeric(rate_dataset)~c(1:length(rate_dataset)))
    plot(as.Date(gsub("X","",names(rate_dataset)),format = "%m.%d.%y"),as.numeric(rate_dataset),type="l",ylab = NA,xlab="Time",main=paste0(type," Rate - ",country),lwd=2, xaxt = "n",ylim=c(-25,100), yaxt = "n")
    lines(as.Date(gsub("X","",names(rate_dataset)),format = "%m.%d.%y"),predict(lo), col='red', lwd=1,lty=2)
    axis(4)
    axis(1, as.Date(gsub("X","",names(rate_dataset)),format = "%m.%d.%y"), format(as.Date(gsub("X","",names(rate_dataset)),format = "%m.%d.%y"), "%b %d"), cex.axis = .7)
    abline(h=0)
    
    par(mfrow=c(1,1))
    
}
Plot_same_figure <- function(country="",show_stock=F) {
    if (country[1]!="") {
        Confirmed <- colSums(Confirmed[Confirmed$Country.Region %in% country,-c(1:4)],na.rm=T)
        Deaths <- colSums(Deaths[Deaths$Country.Region %in% country,-c(1:4)],na.rm=T)
        Recovered <- colSums(Recovered[Recovered$Country.Region %in% country,-c(1:4)],na.rm=T)
        Active <- colSums(Active[Active$Country.Region %in% country,-c(1:4)],na.rm=T)
        New <- colSums(New[New$Country.Region %in% country,-c(1:4)],na.rm=T)
        if (length(country)>1) country="Multiple Countries"
    } else {
        Confirmed <- colSums(Confirmed[,-c(1:4)],na.rm=T)
        Deaths <- colSums(Deaths[,-c(1:4)],na.rm=T)
        Recovered <- colSums(Recovered[,-c(1:4)],na.rm=T)
        Active <- colSums(Active[,-c(1:4)],na.rm=T)
        New <- colSums(New[,-c(1:4)],na.rm=T)
        country = "all world"
    }

    par(mar=c(5, 4, 4, 6) + 0.1)
    
    #Total Amounts
    plot(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(Confirmed),type="b",pch=19,xlab="Time",ylab="Total",main=paste0("Total - ",country),lwd=2, xaxt = "n")
    #text(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),1.1*as.numeric(Confirmed),as.numeric(Confirmed),cex=0.8)
    
    lines(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(Deaths), col='red', lwd=2,type="b",pch=19)
    #text(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),1.1*as.numeric(Deaths),as.numeric(Deaths),cex=0.8)
    
    lines(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(Recovered), col='darkgreen', lwd=2,type="b",pch=19)
    #text(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),1.1*as.numeric(Recovered),as.numeric(Recovered),cex=0.8)
    
    lines(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(Active), col='blue', lwd=2,type="b",pch=19)
    #text(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),1.1*as.numeric(Active),as.numeric(Active),cex=0.8)
    
    lines(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(New), col='orange', lwd=2,type="b",pch=19)
    #text(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),1.1*as.numeric(New),as.numeric(New),cex=0.8)
    
    
    axis(1, as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"), format(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"), "%b %d"), cex.axis = .7)
    abline(h=0)
    
    box()
    
    if (show_stock) {
        par(new=TRUE)
        
        one_ticker = one_ticker[one_ticker$date>=as.Date("2020-01-22"),]
        plot(as.Date(gsub("X","",names(Confirmed)),format = "%m.%d.%y"),as.numeric(one_ticker$close), col='purple', lwd=2,type="b",pch=19,axes=FALSE,  xlab="", ylab="")
        axis(4, ylim=c(min(as.numeric(one_ticker$close)),max(as.numeric(one_ticker$close))), col="purple",col.axis="purple",las=1)
        mtext("sp500",side=4,col="purple",line=4) 
    }
    legend("topleft",legend=c("Confirmed","Deaths","Recovered","Active","New","SP500"),col=c("black","red","darkgreen","blue","orange","purple"),lty=1,cex=0.8, lwd=2)

}

Plot_same_figure()
Plot_same_figure(country="US")
Plot_same_figure(country="Korea, South")
Plot_same_figure(country="Israel")
Plot_same_figure(country="China")
Plot_same_figure(country="Italy")
Plot_same_figure(country="Spain")
Plot_same_figure(country="United Kingdom")
Plot_same_figure(country=countryExData$Country[countryExData$EPI_regions=="Europe"])

Plot_amounts_per_day(data_used = Confirmed)
Plot_amounts_per_day(data_used = Deaths)
Plot_amounts_per_day(data_used = Recovered)
Plot_amounts_per_day(data_used = Active)
Plot_amounts_per_day(data_used = Deaths_Rate)
Plot_amounts_per_day(data_used = Recovered_Rate)
Plot_amounts_per_day(data_used = New)

Plot_amounts_per_day(data_used = Confirmed,country="Italy")
Plot_amounts_per_day(data_used = Deaths,country="Italy")
Plot_amounts_per_day(data_used = Recovered,country="Italy")
Plot_amounts_per_day(data_used = Active,country="Italy")
Plot_amounts_per_day(data_used = Deaths_Rate,country="Italy")
Plot_amounts_per_day(data_used = Recovered_Rate,country="Italy")
Plot_amounts_per_day(data_used = New,country="Italy")

Plot_amounts_per_day(data_used = Confirmed,country="China")
Plot_amounts_per_day(data_used = Deaths,country="China")
Plot_amounts_per_day(data_used = Recovered,country="China")
Plot_amounts_per_day(data_used = Active,country="China")
Plot_amounts_per_day(data_used = Deaths_Rate,country="China")
Plot_amounts_per_day(data_used = Recovered_Rate,country="China")
Plot_amounts_per_day(data_used = New,country="China")

Plot_amounts_per_day(data_used = Confirmed,country="Israel")
Plot_amounts_per_day(data_used = Deaths,country="Israel")
Plot_amounts_per_day(data_used = Recovered,country="Israel")
Plot_amounts_per_day(data_used = Active,country="Israel")
Plot_amounts_per_day(data_used = Deaths_Rate,country="Israel")
Plot_amounts_per_day(data_used = Recovered_Rate,country="Israel")
Plot_amounts_per_day(data_used = New,country="Israel")

Plot_amounts_per_day(data_used = Confirmed,country="US")
Plot_amounts_per_day(data_used = Deaths,country="US")
Plot_amounts_per_day(data_used = Recovered,country="US")
Plot_amounts_per_day(data_used = Active,country="US")
Plot_amounts_per_day(data_used = Deaths_Rate,country="US")
Plot_amounts_per_day(data_used = Recovered_Rate,country="US")
Plot_amounts_per_day(data_used = New,country="US")

Plot_amounts_per_day(data_used = Confirmed,country="United Kingdom")
Plot_amounts_per_day(data_used = Deaths,country="United Kingdom")
Plot_amounts_per_day(data_used = Recovered,country="United Kingdom")
Plot_amounts_per_day(data_used = Active,country="United Kingdom")
Plot_amounts_per_day(data_used = Deaths_Rate,country="United Kingdom")
Plot_amounts_per_day(data_used = Recovered_Rate,country="United Kingdom")
Plot_amounts_per_day(data_used = New,country="United Kingdom")

Plot_amounts_per_day(data_used = Confirmed,country="Korea, South")
Plot_amounts_per_day(data_used = Deaths,country="Korea, South")
Plot_amounts_per_day(data_used = Recovered,country="Korea, South")
Plot_amounts_per_day(data_used = Active,country="Korea, South")
Plot_amounts_per_day(data_used = Deaths_Rate,country="Korea, South")
Plot_amounts_per_day(data_used = Recovered_Rate,country="Korea, South")
Plot_amounts_per_day(data_used = New,country="Korea, South")

Plot_amounts_per_day(data_used = Confirmed,country=countryExData$Country[countryExData$EPI_regions=="Europe"])
Plot_amounts_per_day(data_used = Deaths,country=countryExData$Country[countryExData$EPI_regions=="Europe"])
Plot_amounts_per_day(data_used = Recovered,country=countryExData$Country[countryExData$EPI_regions=="Europe"])
Plot_amounts_per_day(data_used = Active,country=countryExData$Country[countryExData$EPI_regions=="Europe"])
Plot_amounts_per_day(data_used = Deaths_Rate,country=countryExData$Country[countryExData$EPI_regions=="Europe"])
Plot_amounts_per_day(data_used = Recovered_Rate,country=countryExData$Country[countryExData$EPI_regions=="Europe"])

countryExData$Country[countryExData$EPI_regions=="Central and Eastern Europ"]
countryExData$Country[countryExData$EPI_regions=="Europe"]
