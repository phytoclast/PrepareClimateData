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
Biomeclimate <- read.delim("data/BiomeClimate3.txt")
DaysMonth <- read.delim("data/DaysMonth.txt")
biomesummary<-read.delim("data/biomesummary.txt")
Biomeclimate <- merge(biomesummary, Biomeclimate, by='BIOME')
#generate Uniq ID
Biomeclimate$ID <- seq.int(nrow(Biomeclimate))
Biomeclimate<-Biomeclimate[,c("ID","ECO_ID","ECO_NAME","BIOME", "biomname","Latitude","Longitude","Elevation","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12","tl01","tl02","tl03","tl04","tl05","tl06","tl07","tl08","tl09","tl10","tl11","tl12")]


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

Daylength <- merge(unique(Biomeclimate[,1:7]), DaysMonth)
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

for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i] <- 216.7*6.108*exp(17.26939*
   Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i]/(Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i]+237.3))/
    (Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i]+273.3)*0.1651*(Biomeclimate[,which(colnames(Biomeclimate)=='Dl01')+i]
          /12)*Biomeclimate[,which(colnames(Biomeclimate)=='Dn01')+i]*0.2606*abs((Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i] - Biomeclimate[,which(colnames(Biomeclimate)=='tl01')+i])*2)^0.5 + 0.001
  
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

saveRDS(Biomeclimate, file='data/Biomeclimate.RDS')

#_Biomeclimate <- readRDS(file='C:/workspace2/BiomeClimate/data/Biomeclimate.RDS')
