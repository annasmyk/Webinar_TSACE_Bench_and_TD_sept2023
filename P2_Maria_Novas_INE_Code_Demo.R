library(rjd3bench)
library(data.table)
library(xlsx)
library(rjd3toolkit)
######################
#Read data
######################

PATH <- "C:/WebinarSeptiembre2023/Datos"
SPPI<- data.table(read.xlsx(file = paste0(PATH,"/SPPI_CPI.xlsx"), sheetName = 'SPPI_AirTransport'))
CPI<- data.table(read.xlsx(file = paste0(PATH,"/SPPI_CPI.xlsx"), sheetName = 'CPI_AirTransport'))
y <- ts(SPPI[,-1], start=c(year(SPPI[1,1][[1]]),quarter(SPPI[1,1][[1]])),end=c(2018,1), frequency = 4)
x <- ts(CPI[,-1], start=c(year(CPI[1,1][[1]]),month(CPI[1,1][[1]])),end=c(2018,6), frequency = 12)

################################
#Temporal Disaggregation methods
################################
output<-rjd3bench::temporaldisaggregation(series=y, indicators=x, model="Rw",freq = 12,
                                          conversion="Average",diffuse.algorithm="Diffuse")

typeof(output).
#################
#rho estimation
################

output$estimation$parameter

#####################
#Disaggregated series
####################

output$estimation$disagg

################
#Extrapolation
################

trimestralizar <- function(mensual){
    mensual_aux <- copy(mensual)
    setnames(mensual_aux,1,'period')
    mensual_aux[, trimestre:=quarter(mensual_aux[, period])][, anyo:= year(mensual_aux[, period])]
    trim <- mensual_aux[,lapply(.SD, mean), by = c('trimestre', 'anyo')]
    trim[, c('trimestre', 'anyo'):=NULL]
    return(trim[])
}

selecdatos<-function(tablfech,model){

    y <- ts(SPPI[,-1], start=c(year(SPPI[1,1][[1]]),quarter(SPPI[1,1][[1]])),end=c(tablfech[1],tablfech[2]), frequency = 4)
    if (tablfech[2]*3+3>12) {
        yend<-tablfech[1]+1
        mend<-3
    } else {
        yend<-tablfech[1]
        mend<-tablfech[2]*3+3
    }
    x <- ts(CPI[,-1], start=c(year(CPI[1,1][[1]]),month(CPI[1,1][[1]])),end=c(yend,mend), frequency = 12)
    output<-rjd3bench::temporaldisaggregation(y,indicators=x, model=model,freq = 12,conversion="Average",diffuse.algorithm="Diffuse",diffuse.regressors = T)

    DT<-data.table()
    DT<-DT[,period:=seq(as.Date(paste0(end(output$estimation$disagg)[[1]],"/",end(output$estimation$disagg)[[2]]-2,"/1")), by="month", length.out=3)]
    s<-length(output$estimation$disagg)-2
    e<-length(output$estimation$disagg)
    DT<-DT[,(model):=output$estimation$disagg[s:e]]
    return(DT)
}
fechlist<-cbind(year(SPPI[,1][[1]]),quarter(SPPI[,1][[1]]))
model<-list("Ar1","Rw", "RwAr1")

aux1<-lapply(model,function(y) apply(fechlist[25:33,],1,function(x) selecdatos(x,y)))
aux2<-lapply(aux1,function(z) Reduce(function(x,y) rbind(x,y),z))
output<-Reduce(function(x,y) merge(x,y, by="period"),aux2)
outputtrim<-trimestralizar(output)
outputtrim<-outputtrim[, quarter:=paste0(year(period),quarter(period))]
SPPI<-SPPI[, quarter:=paste0(year(period),quarter(period))]
outputtrim<-merge(SPPI,outputtrim,by="quarter")

Ar1<-sqrt(sum(outputtrim[,res:=(Ar1-s51)**2][,res])/nrow(outputtrim))

Rw<-sqrt(sum(outputtrim[,res:=(Rw-s51)**2][,res])/nrow(outputtrim))

RwAr1<-sqrt(sum(outputtrim[,res:=(RwAr1-s51)**2][,res])/nrow(outputtrim))

