# create a yearly wide shape dataset-----
library(spacetime)
m = stConstruct(TRMMENSO, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
yearaverage <- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)

library(reshape2)
TRMMwide=acast(yearaverage, Year~lon+lat, value.var="rainanom")

# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998.06.01']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)




#rasterbrick and mann kendall in r
library(raster)
library(EnvStats)
library(Kendall)
fun_kendall=function(x){ return(unlist(MannKendall(x)))}
kendall_result=calc(RR,fun_kendall)
## Plot the first layer of the RasterBrick
## three rasters (sen slope, tau-b and p-value)
plot(kendall_result, 1) # Tau
plot(kendall_result, 2) #two-sided p-value
data=as.data.frame(kendall_result)
## Plot the first layer of the RasterBrick
## three rasters (sen slope, tau-b and p-value)
plot(m, 1) # Tau
plot(m, 2)
plot(m, 3)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "yearly precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))





#Mapping EOF using ggplot2 
test_spdf <- as(gimms_trends, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
fix(test_df)
library(ggplot2)
EOF1<- ggplot(test_df, aes(x, y, fill = sl)) + geom_raster() + 
  scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                       breaks=c(0,0.5,1),labels=c("Minimum",0.001,"Maximum"),
                       limits=c(0,1))
EOF1






# create a monthly wide shape dataset------
library(reshape2)
TRMMwide=acast(TRMMENSO, date~lon+lat, value.var="rainanom")

# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,229:230]
data <- TRMM[,1:228]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998.06.01']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-12-01'), by='month')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 1)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "monthly precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))





#Mapping EOF using ggplot2 
test_spdf <- as(gimms_trends, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
fix(test_df)
library(ggplot2)
EOF1<- ggplot(test_df, aes(x, y, fill = sl)) + geom_raster() + 
  scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                       breaks=c(0,0.5,1),labels=c("Minimum",0.001,"Maximum"),
                       limits=c(0,1))
EOF1

# create kirmet wide shape dataset-----
kirmet <- subset(TRMMENSO, Month > 5 & Month < 10)
library(spacetime)
m = stConstruct(kirmet, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
kirmetaverage <- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)

library(reshape2)
TRMMwide=acast(kirmetaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998.06.01']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "Kirmet precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))





#Mapping EOF using ggplot2 
test_spdf <- as(gimms_trends, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
fix(test_df)
library(ggplot2)
EOF1<- ggplot(test_df, aes(x, y, fill = sl)) + geom_raster() + 
  scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                       breaks=c(0,0.5,1),labels=c("Minimum",0.001,"Maximum"),
                       limits=c(0,1))
EOF1


# create Belg wide shape dataset-----
belg=subset(TRMMENSO, Month > 2 & Month < 6)
library(spacetime)
m = stConstruct(belg, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
belgaverage <- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)

library(reshape2)
TRMMwide=acast(belgaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "Belg precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))


# create Bega wide shape dataset-----
bega=subset(TRMMENSO, Month =c(1, 2, 10, 11, 12))
library(spacetime)
m = stConstruct(bega, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
begaaverage <- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)

library(reshape2)
TRMMwide=acast(begaaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "Bega precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create January wide shape dataset-----
Jan=subset(TRMMENSO, Month ==1)
library(spacetime)
m = stConstruct(Jan, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Janaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Janaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)



## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "January precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create Feb wide shape dataset-----
Feb=subset(TRMMENSO, Month ==2)
library(spacetime)
m = stConstruct(Feb, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Febaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Febaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)



## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "February precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create March wide shape dataset-----
March=subset(TRMMENSO, Month ==3)
library(spacetime)
m = stConstruct(March, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Marchaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Marchaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "March precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))
# create April wide shape dataset-----
April=subset(TRMMENSO, Month ==4)
library(spacetime)
m = stConstruct(April, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Aprilaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Aprilaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "April precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))
# create May wide shape dataset-----
May=subset(TRMMENSO, Month ==5)
library(spacetime)
m = stConstruct(May, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Mayaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Mayaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "May precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))
# create June wide shape dataset-----
June=subset(TRMMENSO, Month ==6)
library(spacetime)
m = stConstruct(June, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Juneaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Juneaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "June precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))
# create July wide shape dataset-----
July=subset(TRMMENSO, Month ==7)
library(spacetime)
m = stConstruct(July, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Julyaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Julyaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "July precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create August wide shape dataset-----
August=subset(TRMMENSO, Month ==8)
library(spacetime)
m = stConstruct(August, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Augustaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Augustaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "August precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create September wide shape dataset-----
September=subset(TRMMENSO, Month ==9)
library(spacetime)
m = stConstruct(September, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Septemberaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Septemberaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "September precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create October wide shape dataset-----
October=subset(TRMMENSO, Month ==10)
library(spacetime)
m = stConstruct(October, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Octoberaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Octoberaverage, Year~lon+lat, value.var="rainanom")


# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "October precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))


# create November wide shape dataset-----
November=subset(TRMMENSO, Month ==11)
library(spacetime)
m = stConstruct(November, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Novemberaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Novemberaverage, Year~lon+lat, value.var="rainanom")

# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "November precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))

# create December wide shape dataset-----
December=subset(TRMMENSO, Month ==12)
library(spacetime)
m = stConstruct(December, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
library(lubridate)
Decemberaverage<- aggregate(rainanom ~Year+lon+lat, data=STFDF,FUN=mean)
library(reshape2)
TRMMwide=acast(Decemberaverage, Year~lon+lat, value.var="rainanom")

# transpose all but the first column (name)
TRMM <- as.data.frame(t(TRMMwide[,]))
TRMM$loc=row.names(TRMM)
TRMM$myfactor <- factor(row.names(TRMM))
str(TRMMwide) # Check the column types

str(TRMM) # Check the column types

n <- 1
for(i in strsplit(as.character(TRMM$myfactor),'_')){
  TRMM[n, 'lon'] <- i[[1]]
  TRMM[n, 'lat'] <- i[[2]]
  n <- n + 1
}
TRMM$lon=as.numeric(TRMM$lon)
summary(TRMM$lon)
TRMM$lat=as.numeric(TRMM$lat)
summary(TRMM$lat)
names(TRMM)
TRMM$loc=NULL
TRMM$myfactor=NULL
TRMM=save(TRMM,file = "TRMM.Rdata")
write.csv(TRMM, file ="TRMM.csv")

# coerce to SpatialPixelsDataFrame
library(raster)
library(sp)
proj <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m")
coords <- TRMM[,20:21]
data <- TRMM[,1:19]
nasaSP <- SpatialPixelsDataFrame(points=coords, data=data, proj4string=proj)
library(raster)
rbck <- brick(nasaSP)
rbck
#select one layer
solX1998.06.01 <- rbck[['X1998']]
# plot
plot(solX1998.06.01)


## You were using library(base) but I am sure you really want raster :-)
library(raster)
library(zoo)
## Data example: substitute it with your brick and time index
index <- seq(as.Date('1998-01-01'), as.Date('2016-01-01'), by='years')
RR <- brick(lapply(seq_along(index), function(x) setValues(r, rnorm(ncell(r)))))
RR <- setZ(rbck,index)
summary(RR)
plot(RR, 19)

## Significant tau : rasterbrick and mann kendall in r
library(gimms)
gimms_trends <- significantTau(RR, p = 0.05,df=TRUE, prewhitening = TRUE)
## create figure
library(RColorBrewer)
library(latticeExtra)
library(rworldmap)
data(countriesLow)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))
spplot(gimms_trends, col.regions = cols(100), scales = list(draw = TRUE),
       main = expression("Significant Kendall's" ~ tau ~ "December precip. (1998-2016)")) +
  layer(sp.polygons(countriesLow))
