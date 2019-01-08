#cluster analysis packages
library(vegan)
library(cluster)
library(ape)
#color pallette
library(RColorBrewer)
library(colorspace)
#classification tree packages
library(rpart)
library(rpart.plot)
# scatter plot matrix packages
library(car)
# Create a scatter plot matrix
library(ggplot2)
#calculate percentiles
library(plyr)

Biomeclimate <- readRDS("data/BiomeClimate3.RDS") #random point data from worldclim.org grids
DaysMonth <- readRDS("data/DaysMonth.RDS")
biomesummary <- readRDS("data/biomesummary.RDS") #link between biome number and name
Norms2010 <- readRDS("data/Norms2010.RDS") #US Climate Norms 1981-2010
USH_station <- readRDS("data/USH_station.RDS") #US historic climate
ecolink <- readRDS('data/ecolink.RDS') #US historic climate
USH_stationjoin <- readRDS("data/USH_stationjoin.RDS") #WWF Ecoregion link to US historic climate
USH_station <- USH_station[,c('lat', 'lon', 'elev', 'state', 'station_Name', 'StationID')]
USH_station <- merge(USH_station, USH_stationjoin, by.x = 'StationID', by.y = 'StationID')
globhistclim <- readRDS("data/globhistclim.RDS")#Global historic climate
ecolink2 <- readRDS("data/ecolink2.RDS") #WWF Ecoregion link to Global historic climate
globstations <- merge(globhistclim, ecolink2[,c('ID','ECO_ID', 'ECO_NAME', 'BIOME' )], by='ID')
adjprecipitation <- readRDS("data/adjprecipitation.RDS") #Global historic climate
rawprecipitation <- readRDS("data/rawprecipitation.RDS") #Global historic climate
GHC_ELEMENTS <- readRDS("data/GHC_ELEMENTS.RDS") #Global historic climate
#clmchg <- readRDS("data/clmchg.RDS") #random point data sampled from worldclim.org grids of climate models 
#write.csv(clmchg, 'C:/a/Ecological_Sites/GIS/Climate/worldclim/clmchg.csv')
#clmchg2 <- read.csv('C:/a/Ecological_Sites/GIS/Climate/worldclim/Export_Output_5.txt')
#clmchg2 <- unique(clmchg2[,3:184])
#saveRDS(clmchg2, 'data/clmchg2')
clmchgagg <- readRDS("data/clmchgagg2.RDS") #random point data sampled from worldclim.org grids of climate models 
#clmchg fields: cclgmpr1 | cclgmtn1 | cclgmtx1 = last glacial maximum; cc45pr501 | cc45tn501 | cc45tx501 = future climate 2070 conservative; prec_1 | tmin_1 | tmax_1= present climate 1990; tn or tmin is daily minimum, pr or prec is precipitation; tx or tmax is daily maximum. Numbering 1-12 is month.

#Global historic climate---- 
ghc_prec <- rbind(adjprecipitation, rawprecipitation)
ghc_prec <- unique(ghc_prec[,c(1,3:15)])

for (j in 1:12){
  ghc_prec[,j+2] <- ifelse(ghc_prec[,j+2] == -8888, 0.2,ghc_prec[,j+2]/10)
}

ghc_tmean <- unique(GHC_ELEMENTS[GHC_ELEMENTS$ELEMENT %in% 'TAVG' & 
                                   GHC_ELEMENTS$YEAR >=1961 &
                                   GHC_ELEMENTS$YEAR <=2010,
                                 c('ID', 'YEAR', 'V01', 'V02', 'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10', 'V11', 'V12')])
colnames(ghc_tmean) <- c('ID', 'YEAR', 't01', 't02', 't03', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')
ghc_tlow <- unique(GHC_ELEMENTS[GHC_ELEMENTS$ELEMENT %in% 'TMIN' & 
                                  GHC_ELEMENTS$YEAR >=1961 &
                                  GHC_ELEMENTS$YEAR <=2010,
                                c('ID', 'YEAR', 'V01', 'V02', 'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10', 'V11', 'V12')])
colnames(ghc_tlow) <- c('ID', 'YEAR', 'tl01', 'tl02', 'tl03', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')

ghc_thigh <- unique(GHC_ELEMENTS[GHC_ELEMENTS$ELEMENT %in% 'TMAX' & 
                                  GHC_ELEMENTS$YEAR >=1961 &
                                  GHC_ELEMENTS$YEAR <=2010,
                                c('ID', 'YEAR', 'V01', 'V02', 'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10', 'V11', 'V12')])
colnames(ghc_thigh) <- c('ID', 'YEAR', 'th01', 'th02', 'th03', 'th04', 'th05', 'th06', 'th07', 'th08', 'th09', 'th10', 'th11', 'th12')
ghc_t <- merge(ghc_tmean,ghc_tlow, by=c('ID','YEAR'), all.x=TRUE, all.y=TRUE)
ghc_t <- merge(ghc_t,ghc_thigh, by=c('ID','YEAR'), all.x=TRUE, all.y=TRUE)
ghc_t[,3:38] <- ghc_t[,3:38] / 100
ghc <- merge(ghc_t,ghc_prec, by.x=c('ID','YEAR'),by.y=c('ID','year'))
rm(ghc_thigh,ghc_tlow,ghc_tmean )
ghc$check <- (ghc$th01 + ghc$tl01 + ghc$th07 + ghc$th07 + ghc$p01 + ghc$p07)*0+1
ghc1990 <- ghc[ghc$YEAR >= 1961  & ghc$YEAR <= 1990,  ]
ghc1990checkyears <- aggregate(ghc1990[,'check'], by=list(ghc1990$ID), FUN= 'sum', na.rm=TRUE)
colnames(ghc1990checkyears) <- c('ID', 'ct1990')
ghc2010 <- ghc[ghc$YEAR >= 1981  & ghc$YEAR <= 2010,  ]
ghc2010checkyears <- aggregate(ghc2010[,'check'], by=list(ghc2010$ID), FUN= 'sum', na.rm=TRUE)
colnames(ghc2010checkyears) <- c('ID', 'ct2010')
ghccheckyears <- merge(ghc2010checkyears, ghc1990checkyears, by='ID')
rm(ghc2010checkyears, ghc1990checkyears)
selectID <- ghccheckyears[ghccheckyears$ct2010 >=20 & ghccheckyears$ct1990 >=20,'ID']
ghc <- ghc[ghc$ID %in% selectID,]
for (j in 1:12){
ghc[,which(colnames(ghc)=='t01')+j-1]<- ifelse(is.na(ghc[,which(colnames(ghc)=='t01')+j-1]),
(ghc[,which(colnames(ghc)=='tl01')+j-1] + ghc[,which(colnames(ghc)=='th01')+j-1])/2,
ghc[,which(colnames(ghc)=='t01')+j-1])
}
ghcpre <-  ghc[ghc$YEAR >= 1961  & ghc$YEAR <= 1990,]
ghc1990norm <- aggregate(ghcpre[,c(
  't01', 't02', 't03', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12',
  'p01', 'p02', 'p03', 'p04', 'p05', 'p06', 'p07', 'p08', 'p09', 'p10', 'p11', 'p12',
  'tl01', 'tl02', 'tl03', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')],
  by=list(ghcpre$ID), FUN = 'mean', na.rm=TRUE
)
ghcpre <-  ghc[ghc$YEAR >= 1981  & ghc$YEAR <= 2010,]
ghc2010norm <- aggregate(ghcpre[,c(
  't01', 't02', 't03', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12',
  'p01', 'p02', 'p03', 'p04', 'p05', 'p06', 'p07', 'p08', 'p09', 'p10', 'p11', 'p12',
  'tl01', 'tl02', 'tl03', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')],
  by=list(ghcpre$ID), FUN = 'mean', na.rm=TRUE
)
ghc2010norm$Norm <- 2010
ghc1990norm$Norm <- 1990
ghc2010norm <- rbind(ghc2010norm, ghc1990norm)
colnames(ghc2010norm)[1] <- 'ID'
globstations <- merge(globstations,ghc2010norm, by='ID' )
rm(GHC_ELEMENTS, adjprecipitation, rawprecipitation, globhistclim, globhistoricecoregions, ghc, ghcpre, ghc_prec, ghc_thigh, ghc_t,
   ghc_tlow, ghc1990norm, ghc2010norm, ghc1990, ghc2010, ghc2010checkyears, ghc1990checkyears, ghccheckyears)

globstations$Source <- "GHCN"
globstations$State <- ""
colnames(globstations)[which(colnames(globstations)=='ID')] <- "Station_ID"
colnames(globstations)[which(colnames(globstations)=='stnname')] <- "Station_Name"
colnames(globstations)[which(colnames(globstations)=='latitude')] <- "Latitude"
colnames(globstations)[which(colnames(globstations)=='longitude')] <- "Longitude"
colnames(globstations)[which(colnames(globstations)=='elev')] <- "Elevation"


#US Normals 1981-2010 ----  
  
Norms2010$p01 <- Norms2010$pp01*10
Norms2010$p02 <- Norms2010$pp02*10
Norms2010$p03 <- Norms2010$pp03*10
Norms2010$p04 <- Norms2010$pp04*10
Norms2010$p05 <- Norms2010$pp05*10
Norms2010$p06 <- Norms2010$pp06*10
Norms2010$p07 <- Norms2010$pp07*10
Norms2010$p08 <- Norms2010$pp08*10
Norms2010$p09 <- Norms2010$pp09*10
Norms2010$p10 <- Norms2010$pp10*10
Norms2010$p11 <- Norms2010$pp11*10
Norms2010$p12 <- Norms2010$pp12*10
Norms2010 <- subset(Norms2010, select= -c(Latitude, Longitude)) #found these fields truncated by an export, will replace below
Norms2010 <- merge(ecolink[,c("Station_ID","Latitude","Longitude","ECO_NAME","REALM","BIOME","ECO_ID" )], Norms2010, by='Station_ID')
Norms2010$Norm <- 2010
Norms2010$Source <- "2010 Normals"
Norms2010pre <-Norms2010[!is.na(Norms2010$t01)&!is.na(Norms2010$t07)&!is.na(Norms2010$tl01)&!is.na(Norms2010$tl07)&!is.na(Norms2010$p01)&!is.na(Norms2010$p07),c("ECO_ID","ECO_NAME","BIOME","Station_ID","Station_Name","State","Norm","Source","Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12")]

Biomeclimate$Station_ID <- ""
Biomeclimate$Station_Name <- ""
Biomeclimate$State <- ""
Biomeclimate$Norm <- 1990
Biomeclimate$Source <- "WorldClim.org"
Biomeclimatepre <-Biomeclimate[!is.na(Biomeclimate$t01)&!is.na(Biomeclimate$t07)&!is.na(Biomeclimate$tl01)&!is.na(Biomeclimate$tl07)&!is.na(Biomeclimate$p01)&!is.na(Biomeclimate$p07),c("ECO_ID","ECO_NAME","BIOME","Station_ID","Station_Name","State","Norm","Source","Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12")]

Biomeclimate <- rbind(Biomeclimatepre,Norms2010pre)
globstationspre <-globstations[!is.na(globstations$t01)&!is.na(globstations$t07)&!is.na(globstations$tl01)&!is.na(globstations$tl07)&!is.na(globstations$p01)&!is.na(globstations$p07),c("ECO_ID","ECO_NAME","BIOME","Station_ID","Station_Name","State","Norm","Source","Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12")]
Biomeclimate <- rbind(Biomeclimate,globstationspre)
Biomeclimate <- merge(biomesummary, Biomeclimate, by='BIOME')
Biomeclimate$wts<- ifelse(Biomeclimate$Source %in% 'GHCN', 100,1)
#generate Uniq ID
Biomeclimate$ID <- seq.int(nrow(Biomeclimate))
rm(ecolink, Norms2010, Norms2010pre, biomesummary, Biomeclimatepre, globstationspre)
Biomeclimate<-Biomeclimate[,c("ID","ECO_ID","ECO_NAME","BIOME","biomname","Station_ID","Station_Name","State","Norm", "Source","wts", "Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12")]
N1990 <- Biomeclimate[Biomeclimate$Norm == 1990,]
N2010 <- Biomeclimate[Biomeclimate$Norm == 2010,]
NGHCN <- Biomeclimate[Biomeclimate$Source == 'GHCN',]

wwfregions1 <- aggregate(N1990[,c('ID')], by=list(N1990$ECO_ID,N1990$ECO_NAME), FUN='length')
colnames(wwfregions1) <- c('ECO_ID', 'ECO_NAME', 'N1990')
wwfregions2 <- aggregate(N2010[,c('ID')], by=list(N2010$ECO_ID,N2010$ECO_NAME), FUN='length')
colnames(wwfregions2) <- c('ECO_ID', 'ECO_NAME', 'N2010')
wwfregions3 <- aggregate(NGHCN[,c('ID')], by=list(NGHCN$ECO_ID,NGHCN$ECO_NAME), FUN='length')
colnames(wwfregions3) <- c('ECO_ID', 'ECO_NAME', 'NGHCN')
wwfregions<- merge(wwfregions2, wwfregions1[,c('ECO_ID', 'N1990')], by='ECO_ID', all.x = TRUE)
wwfregions<- merge(wwfregions, wwfregions3[,c('ECO_ID', 'NGHCN')], by='ECO_ID', all.x = TRUE)
wwfregions$NGHCN <- ifelse(is.na(wwfregions$NGHCN),0,wwfregions$NGHCN)
selectb <- Biomeclimate[Biomeclimate$ECO_ID %in% c('70701', '70106', '70701', '70702', '70202'),]
wwfregions$cht01 <- 0
wwfregions$cht02 <- 0
wwfregions$cht03 <- 0
wwfregions$cht04 <- 0
wwfregions$cht05 <- 0
wwfregions$cht06 <- 0
wwfregions$cht07 <- 0
wwfregions$cht08 <- 0
wwfregions$cht09 <- 0
wwfregions$cht10 <- 0
wwfregions$cht11 <- 0
wwfregions$cht12 <- 0
wwfregions$chtl01 <- 0
wwfregions$chtl02 <- 0
wwfregions$chtl03 <- 0
wwfregions$chtl04 <- 0
wwfregions$chtl05 <- 0
wwfregions$chtl06 <- 0
wwfregions$chtl07 <- 0
wwfregions$chtl08 <- 0
wwfregions$chtl09 <- 0
wwfregions$chtl10 <- 0
wwfregions$chtl11 <- 0
wwfregions$chtl12 <- 0
wwfregions$chp01 <- 0
wwfregions$chp02 <- 0
wwfregions$chp03 <- 0
wwfregions$chp04 <- 0
wwfregions$chp05 <- 0
wwfregions$chp06 <- 0
wwfregions$chp07 <- 0
wwfregions$chp08 <- 0
wwfregions$chp09 <- 0
wwfregions$chp10 <- 0
wwfregions$chp11 <- 0
wwfregions$chp12 <- 0

for (i in 1:nrow(wwfregions)){ #create a matrix that shows difference between the 1990 and 2010 normals by ecoregion.
  
  
  selectbioclim <- Biomeclimate[Biomeclimate$ECO_ID %in% wwfregions[i,'ECO_ID'],    c("ID","ECO_ID","ECO_NAME","BIOME","Station_ID","Station_Name","State","Norm","wts","Latitude","Longitude","Elevation",
                                                                                      "t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12",
                                                                                      "tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12",
                                                                                      "p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12") ]
  selectbioclim$v1 <- 0
  for (j in 1:12){
    selectbioclim$v1 <- selectbioclim[,which(colnames(selectbioclim)=='t01')+j-1]
    model1 = lm(v1 ~ Latitude + Longitude + Elevation + Norm, data=selectbioclim, weights = selectbioclim$wts)      
    wwfregions[i,which(colnames(wwfregions)=='cht01')+j-1] <- 
      as.numeric(model1$coef[5])
  }
  for (j in 1:12){  
    
    selectbioclim$v1 <- log(selectbioclim[,which(colnames(selectbioclim)=='p01')+j-1] +1 )
    model1 = lm(v1 ~ Latitude + Longitude + Elevation + Norm, data=selectbioclim, weights = selectbioclim$wts)      
    wwfregions[i,which(colnames(wwfregions)=='chp01')+j-1] <- 
      as.numeric(model1$coef[5])
  }  
  
  for (j in 1:12){    
    selectbioclim$v1 <- log(selectbioclim[,which(colnames(selectbioclim)=='t01')+j-1] - selectbioclim[,which(colnames(selectbioclim)=='tl01')+j-1] + 1)
    model1 = lm(v1 ~ Latitude + Longitude + Elevation + Norm, data=selectbioclim, weights = selectbioclim$wts)       
    wwfregions[i,which(colnames(wwfregions)=='chtl01')+j-1] <- 
      as.numeric(model1$coef[5])
  }  
  
}

bioclimatechange <- merge(Biomeclimate[Biomeclimate$Norm == 2010,], wwfregions, by=c('ECO_ID', 'ECO_NAME'))

bioclimatechange$newt01 <- 0
bioclimatechange$newt02 <- 0
bioclimatechange$newt03 <- 0
bioclimatechange$newt04 <- 0
bioclimatechange$newt05 <- 0
bioclimatechange$newt06 <- 0
bioclimatechange$newt07 <- 0
bioclimatechange$newt08 <- 0
bioclimatechange$newt09 <- 0
bioclimatechange$newt10 <- 0
bioclimatechange$newt11 <- 0
bioclimatechange$newt12 <- 0
bioclimatechange$newtl01 <- 0
bioclimatechange$newtl02 <- 0
bioclimatechange$newtl03 <- 0
bioclimatechange$newtl04 <- 0
bioclimatechange$newtl05 <- 0
bioclimatechange$newtl06 <- 0
bioclimatechange$newtl07 <- 0
bioclimatechange$newtl08 <- 0
bioclimatechange$newtl09 <- 0
bioclimatechange$newtl10 <- 0
bioclimatechange$newtl11 <- 0
bioclimatechange$newtl12 <- 0
bioclimatechange$newp01 <- 0
bioclimatechange$newp02 <- 0
bioclimatechange$newp03 <- 0
bioclimatechange$newp04 <- 0
bioclimatechange$newp05 <- 0
bioclimatechange$newp06 <- 0
bioclimatechange$newp07 <- 0
bioclimatechange$newp08 <- 0
bioclimatechange$newp09 <- 0
bioclimatechange$newp10 <- 0
bioclimatechange$newp11 <- 0
bioclimatechange$newp12 <- 0

for (j in 1:12){  
  bioclimatechange[,which(colnames(bioclimatechange)=='newt01')+j-1] <- bioclimatechange[,which(colnames(bioclimatechange)=='t01')+j-1] - bioclimatechange[,which(colnames(bioclimatechange)=='cht01')+j-1]*20

  bioclimatechange[,which(colnames(bioclimatechange)=='newtl01')+j-1] <- bioclimatechange[,which(colnames(bioclimatechange)=='newt01')+j-1] - pmax(exp(log(bioclimatechange[,which(colnames(bioclimatechange)=='t01')+j-1] - bioclimatechange[,which(colnames(bioclimatechange)=='tl01')+j-1] +1) - bioclimatechange[,which(colnames(bioclimatechange)=='chtl01')+j-1]*20) -1, 0) #predict low, ensure that low is never higher than mean.

  bioclimatechange[,which(colnames(bioclimatechange)=='newp01')+j-1] <- pmax(exp(log(bioclimatechange[,which(colnames(bioclimatechange)=='p01')+j-1] + 1) - bioclimatechange[,which(colnames(bioclimatechange)=='chp01')+j-1]*20) - 1, 0) #predict precip, ensure that precip is never negative.
}

bioclimatechange <- bioclimatechange[bioclimatechange$Source %in%'2010 Normals',]
for (j in 1:12){  
  bioclimatechange[,which(colnames(bioclimatechange)=='t01')+j-1] <- bioclimatechange[,which(colnames(bioclimatechange)=='newt01')+j-1] 
  
  bioclimatechange[,which(colnames(bioclimatechange)=='tl01')+j-1] <-   bioclimatechange[,which(colnames(bioclimatechange)=='newtl01')+j-1]
  
  bioclimatechange[,which(colnames(bioclimatechange)=='p01')+j-1] <-   bioclimatechange[,which(colnames(bioclimatechange)=='newp01')+j-1]
}
bioclimatechange$Norm<- 1990  
bioclimatechange <- bioclimatechange[bioclimatechange$N2010 >2 |bioclimatechange$NGHCN > 0, ]#Need to identify bad extrapolations... 
bioclimatechange<- bioclimatechange[,colnames(Biomeclimate)]
Biomeclimate <- rbind(Biomeclimate, bioclimatechange)
Biomeclimate <- Biomeclimate[Biomeclimate$Source %in% c('WorldClim.org', '2010 Normals')& Biomeclimate$Norm == 1990, ]

#work in the climate change ----
rm(bioclimatechange, ecolink2, globstations, model1, N1990, N2010, NGHCN, USH_station, USH_stationjoin, wwfregions, wwfregions3, wwfregions2, wwfregions1)
#remove rows with no data...
clmchgagg <- subset(clmchgagg, ccmidpr1!= -9999)
Biomeclimate$chglink <- paste(round(Biomeclimate$Latitude,1), round(Biomeclimate$Longitude,1))
#clmchg2$chglink <- paste(round(clmchg2$Latitude,1), round(clmchg2$Longitude,1))
#clmchgagg <- aggregate(clmchg2[,3:182], by=list(clmchg2$chglink), FUN='mean', na.rm=TRUE)
#colnames(clmchgagg)[1] <- 'chglink'
#saveRDS(clmchgagg, 'data/clmchgagg2.RDS')

#2070 45
ch <- merge(Biomeclimate[Biomeclimate$Norm == 1990,], clmchgagg, by=c('chglink'), all.x = FALSE)
for (j in 1:12){  
  ch$newTx <- ch[,which(colnames(ch)=='cc45tx501')+j-1]/10 #cclgmtx1
  ch$newTn <- ch[,which(colnames(ch)=='cc45tn501')+j-1]/10 #cclgmtn1
  ch$newP <- ch[,which(colnames(ch)=='cc45pr501')+j-1] #cclgmpr1
  
  ch$refTx <- ch[,which(colnames(ch)=='tmax_1')+j-1]/10
  ch$refTn <- ch[,which(colnames(ch)=='tmin_1')+j-1]/10
  ch$refP <- ch[,which(colnames(ch)=='prec_1')+j-1]
  
  ch$oldT <- ch[,which(colnames(ch)=='t01')+j-1]
  ch$oldTn <- ch[,which(colnames(ch)=='tl01')+j-1]
  ch$oldP <- ch[,which(colnames(ch)=='p01')+j-1]  
  
  ch[,which(colnames(ch)=='t01')+j-1] <- (ch$newTx + ch$newTn)/2 - (ch$refTx + ch$refTn)/2 + ch$oldT
  
  ch[,which(colnames(ch)=='tl01')+j-1] <-   ch$newTn - ch$refTn + ch$oldTn
  
  ch[,which(colnames(ch)=='p01')+j-1] <-   pmax(exp(log(ch$newP + 1) - log(ch$refP + 1) + log(ch$oldP + 1))-1,0)
}
ch$Norm <- 2070
ch$Source <- 'Moderate Global Warming'
ch <- ch[,colnames(Biomeclimate)]
Biomeclimate <- rbind(Biomeclimate, ch)

#2070 45
ch <- merge(Biomeclimate[Biomeclimate$Norm == 1990,], clmchgagg, by=c('chglink'), all.x = FALSE)
for (j in 1:12){  
  ch$newTx <- ch[,which(colnames(ch)=='cc85tx501')+j-1]/10 #cclgmtx1
  ch$newTn <- ch[,which(colnames(ch)=='cc85tn501')+j-1]/10 #cclgmtn1
  ch$newP <- ch[,which(colnames(ch)=='cc85pr501')+j-1] #cclgmpr1
  
  ch$refTx <- ch[,which(colnames(ch)=='tmax_1')+j-1]/10
  ch$refTn <- ch[,which(colnames(ch)=='tmin_1')+j-1]/10
  ch$refP <- ch[,which(colnames(ch)=='prec_1')+j-1]
  
  ch$oldT <- ch[,which(colnames(ch)=='t01')+j-1]
  ch$oldTn <- ch[,which(colnames(ch)=='tl01')+j-1]
  ch$oldP <- ch[,which(colnames(ch)=='p01')+j-1]  
  
  ch[,which(colnames(ch)=='t01')+j-1] <- (ch$newTx + ch$newTn)/2 - (ch$refTx + ch$refTn)/2 + ch$oldT
  
  ch[,which(colnames(ch)=='tl01')+j-1] <-   ch$newTn - ch$refTn + ch$oldTn
  
  ch[,which(colnames(ch)=='p01')+j-1] <-   pmax(exp(log(ch$newP + 1) - log(ch$refP + 1) + log(ch$oldP + 1))-1,0)
}
ch$Norm <- 2071
ch$Source <- 'Severe Global Warming'
ch <- ch[,colnames(Biomeclimate)]
Biomeclimate <- rbind(Biomeclimate, ch)

#LGM
ch <- merge(Biomeclimate[Biomeclimate$Norm == 1990,], clmchgagg, by=c('chglink'), all.x = FALSE)
for (j in 1:12){  
  ch$newTx <- ch[,which(colnames(ch)=='cclgmtx1')+j-1]/10
  ch$newTn <- ch[,which(colnames(ch)=='cclgmtn1')+j-1]/10
  ch$newP <- ch[,which(colnames(ch)=='cclgmpr1')+j-1] 
  
  ch$refTx <- ch[,which(colnames(ch)=='tmax_1')+j-1]/10
  ch$refTn <- ch[,which(colnames(ch)=='tmin_1')+j-1]/10
  ch$refP <- ch[,which(colnames(ch)=='prec_1')+j-1]
  
  ch$oldT <- ch[,which(colnames(ch)=='t01')+j-1]
  ch$oldTn <- ch[,which(colnames(ch)=='tl01')+j-1]
  ch$oldP <- ch[,which(colnames(ch)=='p01')+j-1]  
  
  ch[,which(colnames(ch)=='t01')+j-1] <- (ch$newTx + ch$newTn)/2 - (ch$refTx + ch$refTn)/2 + ch$oldT
  
  ch[,which(colnames(ch)=='tl01')+j-1] <-   ch$newTn - ch$refTn + ch$oldTn
  
  ch[,which(colnames(ch)=='p01')+j-1] <-   pmax(exp(log(ch$newP + 1) - log(ch$refP + 1) + log(ch$oldP + 1))-1,0)
}
ch$Norm <- -25000
ch$Source <- 'LGM'
ch <- ch[,colnames(Biomeclimate)]
Biomeclimate <- rbind(Biomeclimate, ch)

#MHO
ch <- merge(Biomeclimate[Biomeclimate$Norm == 1990,], clmchgagg, by=c('chglink'), all.x = FALSE)
for (j in 1:12){  
  ch$newTx <- ch[,which(colnames(ch)=='ccmidtx1')+j-1]/10
  ch$newTn <- ch[,which(colnames(ch)=='ccmidtn1')+j-1]/10
  ch$newP <- ch[,which(colnames(ch)=='ccmidpr1')+j-1] 
  
  ch$refTx <- ch[,which(colnames(ch)=='tmax_1')+j-1]/10
  ch$refTn <- ch[,which(colnames(ch)=='tmin_1')+j-1]/10
  ch$refP <- ch[,which(colnames(ch)=='prec_1')+j-1]
  
  ch$oldT <- ch[,which(colnames(ch)=='t01')+j-1]
  ch$oldTn <- ch[,which(colnames(ch)=='tl01')+j-1]
  ch$oldP <- ch[,which(colnames(ch)=='p01')+j-1]  
  
  ch[,which(colnames(ch)=='t01')+j-1] <- (ch$newTx + ch$newTn)/2 - (ch$refTx + ch$refTn)/2 + ch$oldT
  
  ch[,which(colnames(ch)=='tl01')+j-1] <-   ch$newTn - ch$refTn + ch$oldTn
  
  ch[,which(colnames(ch)=='p01')+j-1] <-   pmax(exp(log(ch$newP + 1) - log(ch$refP + 1) + log(ch$oldP + 1))-1,0)
}
ch$Norm <- -4000
ch$Source <- 'MHO'
ch <- ch[,colnames(Biomeclimate)]
Biomeclimate <- rbind(Biomeclimate, ch)
#finish climate change
Biomeclimate <-subset(Biomeclimate, select = -c(chglink, wts))
#saveRDS(Biomeclimate,'data/preBiomeclimate.rds')
#Biomeclimate <- readRDS('data/preBiomeclimate.rds')
#reduce data size----
Biomeclimate$climblock <- paste(floor(Biomeclimate$Latitude),floor(Biomeclimate$Latitude/2.5),floor(Biomeclimate$Elevation/500) )

climblocks <- aggregate(Biomeclimate[,c("Latitude","Longitude","Elevation",
                                        "t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12",
                                        "tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12",
                                        "p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12") ],
                        by=list(Biomeclimate$ECO_ID,Biomeclimate$ECO_NAME,Biomeclimate$BIOME, Biomeclimate$biomname, Biomeclimate$Norm, Biomeclimate$climblock), FUN='mean')

colnames(climblocks)[1:6] <- c('ECO_ID', 'ECO_NAME', 'BIOME', 'biomname', 'Norm','climblock')

climblocks <- subset(climblocks, select = -c(climblock))
Biomeclimate <- climblocks
rm(clmchgagg, selectb, selectbioclim, ch)
#---- Begin summary
Biomeclimate$b01 <- 0
Biomeclimate$b02 <- 0
Biomeclimate$b03 <- 0
Biomeclimate$b04 <- 0
Biomeclimate$b05 <- 0
Biomeclimate$b06 <- 0
Biomeclimate$b07 <- 0
Biomeclimate$b08 <- 0
Biomeclimate$b09 <- 0
Biomeclimate$b10 <- 0
Biomeclimate$b11 <- 0
Biomeclimate$b12 <- 0
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='b01')+i]  <- Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i]*
    (Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i]>0)*1
}




Biomeclimate$th01 <- Biomeclimate$t01*2-Biomeclimate$tl01
Biomeclimate$th02 <- Biomeclimate$t02*2-Biomeclimate$tl02
Biomeclimate$th03 <- Biomeclimate$t03*2-Biomeclimate$tl03
Biomeclimate$th04 <- Biomeclimate$t04*2-Biomeclimate$tl04
Biomeclimate$th05 <- Biomeclimate$t05*2-Biomeclimate$tl05
Biomeclimate$th06 <- Biomeclimate$t06*2-Biomeclimate$tl06
Biomeclimate$th07 <- Biomeclimate$t07*2-Biomeclimate$tl07
Biomeclimate$th08 <- Biomeclimate$t08*2-Biomeclimate$tl08
Biomeclimate$th09 <- Biomeclimate$t09*2-Biomeclimate$tl09
Biomeclimate$th10 <- Biomeclimate$t10*2-Biomeclimate$tl10
Biomeclimate$th11 <- Biomeclimate$t11*2-Biomeclimate$tl11
Biomeclimate$th12 <- Biomeclimate$t12*2-Biomeclimate$tl12

#calculate extreme winter low----
Biomeclimate$pacificsouth <- 1/((((Biomeclimate$Latitude - -22.7)/13)^2 + ((Biomeclimate$Longitude - -82.3)/14)^2)^2+1)
Biomeclimate$amazon2 <- 1/((((Biomeclimate$Latitude - -10.2)/5)^2 + ((Biomeclimate$Longitude - -59.9)/10)^2)^2+1)
Biomeclimate$amazon1 <- 1/((((Biomeclimate$Latitude - -2.8)/14)^2 + ((Biomeclimate$Longitude - -61.3)/19)^2)^2+1)
Biomeclimate$pacificcent <- 1/((((Biomeclimate$Latitude - 4.1)/21)^2 + ((Biomeclimate$Longitude - -122.4)/41)^2)^2+1)
Biomeclimate$mexico <- 1/((((Biomeclimate$Latitude - 26)/6)^2 + ((Biomeclimate$Longitude - -98.4)/12)^2)^2+1)
Biomeclimate$florida <- 1/((((Biomeclimate$Latitude - 27.5)/4)^2 + ((Biomeclimate$Longitude - -81.1)/8)^2)^2+1)
Biomeclimate$pacificnorth <- 1/((((Biomeclimate$Latitude - 32.9)/26)^2 + ((Biomeclimate$Longitude - -145)/27)^2)^2+1)
Biomeclimate$oklahoma <- 1/((((Biomeclimate$Latitude - 33.6)/4)^2 + ((Biomeclimate$Longitude - -98.4)/8)^2)^2+1)
Biomeclimate$arizona <- 1/((((Biomeclimate$Latitude - 34)/12)^2 + ((Biomeclimate$Longitude - -113.1)/8)^2)^2+1)
Biomeclimate$atlantic <- 1/((((Biomeclimate$Latitude - 34)/15)^2 + ((Biomeclimate$Longitude - -60.7)/19)^2)^2+1)
Biomeclimate$himalayas <- 1/((((Biomeclimate$Latitude - 35.3)/6)^2 + ((Biomeclimate$Longitude - 91.3)/13)^2)^2+1)
Biomeclimate$kentucky <- 1/((((Biomeclimate$Latitude - 38.5)/3)^2 + ((Biomeclimate$Longitude - -87.6)/9)^2)^2+1)
Biomeclimate$detroit <- 1/((((Biomeclimate$Latitude - 41.8)/3)^2 + ((Biomeclimate$Longitude - -82.6)/4)^2)^2+1)
Biomeclimate$ontario <- 1/((((Biomeclimate$Latitude - 44.6)/2)^2 + ((Biomeclimate$Longitude - -79.2)/6)^2)^2+1)
Biomeclimate$montana <- 1/((((Biomeclimate$Latitude - 45.4)/5)^2 + ((Biomeclimate$Longitude - -111.8)/10)^2)^2+1)
Biomeclimate$minn <- 1/((((Biomeclimate$Latitude - 47.6)/6)^2 + ((Biomeclimate$Longitude - -92.6)/12)^2)^2+1)
Biomeclimate$hudson <- 1/((((Biomeclimate$Latitude - 60)/7)^2 + ((Biomeclimate$Longitude - -87)/34)^2)^2+1)
Biomeclimate$siberia <- 1/((((Biomeclimate$Latitude - 61.2)/20)^2 + ((Biomeclimate$Longitude - 105.7)/39)^2)^2+1)
Biomeclimate$california <- 1/((((Biomeclimate$Latitude - 34.8)/9)^2 + ((Biomeclimate$Longitude - -128.2)/9)^2)^2+1)
Biomeclimate$washington <- 1/((((Biomeclimate$Latitude - 46)/5)^2 + ((Biomeclimate$Longitude - -126.6)/5)^2)^2+1)
Biomeclimate$colorado <- 1/((((Biomeclimate$Latitude - 38.3)/2)^2 + ((Biomeclimate$Longitude - -108.8)/3)^2)^2+1)
Biomeclimate$hawaii <- 1/((((Biomeclimate$Latitude - 21.3)/7)^2 + ((Biomeclimate$Longitude - -157.5)/11)^2)^2+1)
Biomeclimate$chess <- 1/((((Biomeclimate$Latitude - 37)/3)^2 + ((Biomeclimate$Longitude - -74)/3)^2)^2+1)

Biomeclimate$Tg <- pmax(apply(Biomeclimate[,c('b05','b06','b07','b08','b09','b10')], 1, FUN = mean), apply(Biomeclimate[,c('b11','b12','b01','b02','b03','b04')], 1, FUN = mean))/1
Biomeclimate$Tc <- apply(Biomeclimate[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = min)
Biomeclimate$Tcl <- apply(Biomeclimate[,c('tl01', 'tl02', 'tl04', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')], 1, FUN = min)
Biomeclimate$Tw <- apply(Biomeclimate[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = max)
Biomeclimate$Twh <- apply(Biomeclimate[,c('th01', 'th02', 'th04', 'th04', 'th05', 'th06', 'th07', 'th08', 'th09', 'th10', 'th11', 'th12')], 1, FUN = max)


Biomeclimate$Tclx<-	-9.171	+
  Biomeclimate$Tcl *	1.202	+
  Biomeclimate$Latitude *	-0.04149	+
  Biomeclimate$Elevation *	0.0008691	+
  Biomeclimate$Latitude * Biomeclimate$Elevation *	-0.00002455	+
  Biomeclimate$pacificsouth *	-1.792	+
  Biomeclimate$amazon2 *	2.573	+
  Biomeclimate$amazon1 *	-1.014	+
  Biomeclimate$pacificcent *	-0.749	+
  Biomeclimate$mexico *	-0.8227	+
  Biomeclimate$florida *	-3.557	+
  Biomeclimate$pacificnorth *	-1.246	+
  Biomeclimate$oklahoma *	0.1758	+
  Biomeclimate$arizona *	2.605	+
  Biomeclimate$chess *	0.8347	+
  Biomeclimate$atlantic *	0.2967	+
  Biomeclimate$himalayas *	-1.814	+
  Biomeclimate$kentucky *	-2.644	+
  Biomeclimate$detroit *	0	+
  Biomeclimate$ontario *	-2.314	+
  Biomeclimate$montana *	-4.415	+
  Biomeclimate$minn *	1.136	+
  Biomeclimate$hudson *	-5.154	+
  Biomeclimate$siberia *	-3.797	+
  Biomeclimate$california *	4.48	+
  Biomeclimate$washington *	3.597	+
  Biomeclimate$colorado *	1.458	+
  Biomeclimate$hawaii *	6.673	

Biomeclimate <- Biomeclimate[!is.na(Biomeclimate$Tclx),]
Biomeclimate <- subset(Biomeclimate, select = -c(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida,                                               pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, 
                                                   detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii))

#daylength

DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)

Daylength <- merge(unique(Biomeclimate[,c("Latitude","Longitude")]), DaysMonth)
Daylength$Daylength <- ifelse(Daylength$Latitude + Daylength$declination*360/2/3.141592 > 89.16924, 24, ifelse(Daylength$Latitude - Daylength$declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))

Biomeclimate$Dn01 <- 31
Biomeclimate$Dn02 <- 28.25
Biomeclimate$Dn03 <- 31
Biomeclimate$Dn04 <- 30
Biomeclimate$Dn05 <- 31
Biomeclimate$Dn06 <- 30
Biomeclimate$Dn07 <- 31
Biomeclimate$Dn08 <- 31
Biomeclimate$Dn09 <- 30
Biomeclimate$Dn10 <- 31
Biomeclimate$Dn11 <- 30
Biomeclimate$Dn12 <- 31


LatDaylen <- unique(Daylength[Daylength$Month_ == 1,c('Latitude', 'Daylength')])
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl01'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 2,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl02'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 3,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl03'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 4,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl04'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 5,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl05'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 6,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl06'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 7,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl07'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 8,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl08'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 9,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl09'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 10,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl10'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 11,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl11'
LatDaylen <- merge(LatDaylen,unique(Daylength[Daylength$Month_ == 12,c('Latitude', 'Daylength')]), by= 'Latitude')
colnames(LatDaylen)[colnames(LatDaylen) == 'Daylength']<- 'Dl12'
Biomeclimate <- merge(Biomeclimate, LatDaylen, by= 'Latitude')

Biomeclimate$e01 <- 0
Biomeclimate$e02 <- 0
Biomeclimate$e03 <- 0
Biomeclimate$e04 <- 0
Biomeclimate$e05 <- 0
Biomeclimate$e06 <- 0
Biomeclimate$e07 <- 0
Biomeclimate$e08 <- 0
Biomeclimate$e09 <- 0
Biomeclimate$e10 <- 0
Biomeclimate$e11 <- 0
Biomeclimate$e12 <- 0

Biomeclimate$I <- 0
for (i in 0:11){
  Biomeclimate$I <- Biomeclimate$I +
    (Biomeclimate[,which(colnames(Biomeclimate)=='b01')+i]/5+0.000001)^1.514
}
Biomeclimate$alpha <- (6.75e-7)*Biomeclimate$I^3 - (7.71e-5)*Biomeclimate$I^2 +
  (1.792e-2)*Biomeclimate$I + 0.49239
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i] <-
    16*(Biomeclimate[,which(colnames(Biomeclimate)=='Dl01')+i]/12)*
    (Biomeclimate[,which(colnames(Biomeclimate)=='Dn01')+i]/30)*
    (10*Biomeclimate[,which(colnames(Biomeclimate)=='b01')+i]/Biomeclimate$I)^Biomeclimate$alpha + 0.001
  
}

Biomeclimate <- subset(Biomeclimate, 
                       select = -c(Dn01, Dn02, Dn03, Dn04, Dn05, Dn06, Dn07, Dn08, Dn09, Dn10, Dn11, Dn12, 
                                   Dl01, Dl02, Dl03, Dl04, Dl05, Dl06, Dl07, Dl08, Dl09, Dl10, Dl11, Dl12))


Biomeclimate$aet01 <- 0
Biomeclimate$aet02 <- 0
Biomeclimate$aet03 <- 0
Biomeclimate$aet04 <- 0
Biomeclimate$aet05 <- 0
Biomeclimate$aet06 <- 0
Biomeclimate$aet07 <- 0
Biomeclimate$aet08 <- 0
Biomeclimate$aet09 <- 0
Biomeclimate$aet10 <- 0
Biomeclimate$aet11 <- 0
Biomeclimate$aet12 <- 0
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='aet01')+i] <- 
    pmin(Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i], Biomeclimate[,which(colnames(Biomeclimate)=='p01')+i])
}

Biomeclimate$d01 <- 0
Biomeclimate$d02 <- 0
Biomeclimate$d03 <- 0
Biomeclimate$d04 <- 0
Biomeclimate$d05 <- 0
Biomeclimate$d06 <- 0
Biomeclimate$d07 <- 0
Biomeclimate$d08 <- 0
Biomeclimate$d09 <- 0
Biomeclimate$d10 <- 0
Biomeclimate$d11 <- 0
Biomeclimate$d12 <- 0
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='d01')+i] <- 
    pmax(Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i] - Biomeclimate[,which(colnames(Biomeclimate)=='p01')+i], 
         0)
}
Biomeclimate$s01 <- 0
Biomeclimate$s02 <- 0
Biomeclimate$s03 <- 0
Biomeclimate$s04 <- 0
Biomeclimate$s05 <- 0
Biomeclimate$s06 <- 0
Biomeclimate$s07 <- 0
Biomeclimate$s08 <- 0
Biomeclimate$s09 <- 0
Biomeclimate$s10 <- 0
Biomeclimate$s11 <- 0
Biomeclimate$s12 <- 0
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='s01')+i] <- 
    pmax(Biomeclimate[,which(colnames(Biomeclimate)=='p01')+i] - Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i], 
         0)
}
Biomeclimate$pAET <- pmax(Biomeclimate$aet01,
                          Biomeclimate$aet02,
                          Biomeclimate$aet03,
                          Biomeclimate$aet04,
                          Biomeclimate$aet05,
                          Biomeclimate$aet06,
                          Biomeclimate$aet07,
                          Biomeclimate$aet08,
                          Biomeclimate$aet09,
                          Biomeclimate$aet10,
                          Biomeclimate$aet11,
                          Biomeclimate$aet12)
Biomeclimate$Deficit <- (Biomeclimate$d01+
                          Biomeclimate$d02+
                          Biomeclimate$d03+
                          Biomeclimate$d04+
                          Biomeclimate$d05+
                          Biomeclimate$d06+
                          Biomeclimate$d07+
                          Biomeclimate$d08+
                          Biomeclimate$d09+
                          Biomeclimate$d10+
                          Biomeclimate$d11+
                          Biomeclimate$d12)
Biomeclimate$Surplus <- (Biomeclimate$s01+
                          Biomeclimate$s02+
                          Biomeclimate$s03+
                          Biomeclimate$s04+
                          Biomeclimate$s05+
                          Biomeclimate$s06+
                          Biomeclimate$s07+
                          Biomeclimate$s08+
                          Biomeclimate$s09+
                          Biomeclimate$s10+
                          Biomeclimate$s11+
                          Biomeclimate$s12)
Biomeclimate <- subset(Biomeclimate, select= -c(b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,s01,s02,s03,s04,s05,s06,s07,s08,s09,s10,s11,s12,d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,aet01,aet02,aet03,aet04,aet05,aet06,aet07,aet08,aet09,aet10,aet11,aet12))
Biomeclimate <- 
  Biomeclimate[,c("ECO_ID","ECO_NAME","BIOME","biomname","Norm",
                  "Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07",
                  "t08","t09","t10","t11","t12","p01","p02","p03","p04","p05",
                  "p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03",
                  "tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12","th01",
                  "th02","th03","th04","th05","th06","th07","th08","th09","th10","th11",
                  "th12","Tg","Tc","Tcl","Tw","Twh","Tclx","e01","e02","e03",
                  "e04","e05","e06","e07","e08","e09","e10","e11","e12","pAET",
                  "Deficit","Surplus")]
saveRDS(Biomeclimate, file='data/ThornBiomeclimate.RDS')

#_Biomeclimate <- readRDS(file='C:/workspace2/BiomeClimate/data/Biomeclimate.RDS')
