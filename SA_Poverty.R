library(maptools)
library(car)
library(lmtest)
library(RColorBrewer)
library(classInt)
library(zoo)
library(spdep)
library(aod)
library(spgwr)
library(DCluster)
library(shapefiles)
library(rgdal)

dat<-readShapePoly("C:/Users/Matt/Dropbox/Graduate School/Maps/Shape Files/Bexar County/Bexar Tracts (2000)/Bexar_Tracts_2000.shp"
                   ,proj4string=CRS("+proj=utm +zone=14"))
names(city2)
attribute<-dat@data
merge1<-merge(x=attribute,y=city2, by.x="GISJOIN", by.y="GISJOIN", all.x=F, sort=F)
merge1$GISJOIN<-dat$GISJOIN
dat@data<-merge1

#Needed for cluster analysis later
dat$x<-coordinates(dat)[,1]
dat$y<-coordinates(dat)[,2]
occData<-data.frame(x=c(dat$x), y=c(dat$y))
##

dat70<-subset(dat, subset=complete.cases(dat$ppov70))
dat80<-subset(dat, subset=complete.cases(dat$ppov80))
dat90<-subset(dat, subset=complete.cases(dat$ppov90))
dat00<-subset(dat, subset=complete.cases(dat$ppov00))
dat00$x<-coordinates(dat00)[,1]
dat00$y<-coordinates(dat00)[,2]
dat70c<-subset(dat, subset=complete.cases(dat$ppov70, dat$ppubinc70, dat$pemp70, dat$pfhh70, dat$pdrop70, dat$phis70))
dat80c<-subset(dat, subset=complete.cases(dat$ppov80, dat$ppubinc80, dat$pemp80, dat$pfhh80, dat$pdrop80, dat$phis80))
dat90c<-subset(dat, subset=complete.cases(dat$ppov90, dat$ppubinc90, dat$pemp90, dat$pfhh90, dat$pdrop90, dat$phis90))
dat00c<-subset(dat, subset=complete.cases(dat$ppov00, dat$ppubinc00, dat$pemp00, dat$pfhh00, dat$pdrop00, dat$phis00))

#Moran's I
  #1970
  sanb70<-poly2nb(dat70, queen=T)
  salw70<-nb2listw(sanb70, style="W")
  wmat70<-nb2mat(sanb70, style="W")
  moran.test(dat70$ppov70, listw=salw70)

  #1980
  sanb80<-poly2nb(dat80, queen=T)
  salw80<-nb2listw(sanb80, style="W")
  wmat80<-nb2mat(sanb80, style="W")
  moran.test(dat80$ppov80, listw=salw80)

  #1990
  sanb90<-poly2nb(dat90, queen=T)
  salw90<-nb2listw(sanb90, style="W")
  wmat90<-nb2mat(sanb90, style="W")
  moran.test(dat90$ppov90, listw=salw90)

  #2000
  sanb00<-poly2nb(dat00, queen=T)
  salw00<-nb2listw(sanb00, style="W")
  wmat00<-nb2mat(sanb00, style="W")
  moran.test(dat00$ppov00, listw=salw00)


########## Adaptive Bandwidth - 1970 ##############
gwr.b.a<-gwr.sel(ppov70~ppubinc70 + pemp70 + pfhh70 + pdrop70 + phis70 +pchild70, dat70c, adapt=T)
gwr.a70<-gwr(ppov70~ppubinc70 + pemp70 + pfhh70 + pdrop70 + phis70 + pchild70, dat70c, adapt=gwr.b.a,
             se.fit=T, hatmatrix=T)
gwr.a70
gwr.dat70<-gwr.a70$SDF
att70<-gwr.dat70@data
att70$test<-att70$pdrop70_se*1.96
att70$Lower<-(att70$pdrop70 - att70$test)
att70$Upper<-(att70$pdrop70 + att70$test)
att70$Sig<-(att70$Lower<0) + (0<att70$Upper)
gwr.dat70@data<-att70

coordinates70<-c('dat70$x','dat70$y')
writeOGR(gwr.dat70, dsn='C:/Users/Matt/Dropbox/Graduate School/Maps/SA Maps', 
         layer='SA_1970_GWR', driver="ESRI Shapefile",coordinates70)

########## Adaptive Bandwidth - 1980 ##############
gwr.b.a<-gwr.sel(ppov80~ppubinc80 + pemp80 + pfhh80 + pdrop80 + phis80 + pchild80, dat80c, adapt=T)
gwr.a80<-gwr(ppov80~ppubinc80 + pemp80 + pfhh80 + pdrop80 + phis80 + pchild80, dat80c, adapt=gwr.b.a,
             se.fit=T, hatmatrix=T)
gwr.a80
gwr.dat80<-gwr.a80$SDF
att80<-gwr.dat80@data
att80$test<-att80$pdrop80_se*1.96
att80$Lower<-(att80$pdrop80 - att80$test)
att80$Upper<-(att80$pdrop80 + att80$test)
att80$Sig<-(att80$Lower<0) + (0<att80$Upper)
gwr.dat80@data<-att80

coordinates80<-c('dat80$x','dat80$y')
writeOGR(gwr.dat80, dsn='C:/Users/Matt/Dropbox/Graduate School/Maps/SA Maps', 
         layer='SA_1980_GWR', driver="ESRI Shapefile",coordinates80)

########## Adaptive Bandwidth - 1990 ##############
gwr.b.a<-gwr.sel(ppov90~ppubinc90 + pemp90 + pfhh90 + pdrop90 + phis90 + pchild90, dat90c, adapt=T)
gwr.a90<-gwr(ppov90~ppubinc90 + pemp90 + pfhh90 + pdrop90 + phis90 + pchild90, dat90c, adapt=gwr.b.a,
             se.fit=T, hatmatrix=T)
gwr.a90
gwr.dat90<-gwr.a90$SDF
att90<-gwr.dat90@data
att90$test<-att90$pdrop90_se*1.96
att90$Lower<-(att90$pdrop90 - att90$test)
att90$Upper<-(att90$pdrop90 + att90$test)
att90$Sig<-(att90$Lower<0) + (0<att90$Upper)
gwr.dat90@data<-att90

coordinates90<-c('dat90$x','dat90$y')
writeOGR(gwr.dat90, dsn='C:/Users/Matt/Dropbox/Graduate School/Maps/SA Maps', 
         layer='SA_1990_GWR', driver="ESRI Shapefile",coordinates90)

########## Adaptive Bandwidth - 2000 ##############
gwr.b.a<-gwr.sel(ppov00~ppubinc00 + pemp00 + pfhh00 + pdrop00 + phis00 + pchild00, dat00c, adapt=T)
gwr.a00<-gwr(ppov00~ppubinc00 + pemp00 + pfhh00 + pdrop00 + phis00 + pchild00, dat00c, adapt=gwr.b.a,
             se.fit=T, hatmatrix=T)
gwr.a00
gwr.dat00<-gwr.a00$SDF
att00<-gwr.dat00@data
att00$test<-att00$pdrop00_se*1.96
att00$Lower<-(att00$pdrop00 - att00$test)
att00$Upper<-(att00$pdrop00 + att00$test)
att00$Sig<-(att00$Lower<0) + (0<att00$Upper)
att00$Use<-rep(NA, length(att00$Sig))
att00$Use[att00$Sig==1]<-att00$pdrop00

gwr.dat00@data<-att00
coordinates00<-c('dat00$x','dat00$y')
writeOGR(gwr.dat00, dsn='C:/Users/Matt/Dropbox/Graduate School/Maps/SA Maps', 
         layer='SA_2000_GWR', driver="ESRI Shapefile",coordinates00)

############## Cluster Analysis - 1970 #############
#Take care of zero values for calculations
dat70$pov70n<-(dat70$POVRAT7N +.00000000001)
dat70$pov70d<-(dat70$POVRAT7D +.00000000001)
dat70$povrt<-(dat70$pov70n/dat70$pov70d)

#Openshaw's GAM
dat70a<-data.frame(Observed=dat70$pov70n)
dat70a<-cbind(dat70a, Expected=dat70$pov70d*sum(dat70$pov70n)/sum(dat70$pov70d))
dat70a<-cbind(dat70a, x=dat$x, y=dat$y)
dat70a$Observed<-as.numeric(dat70a$Observed)
head(dat70a)
#Standardized Rate
dat70$spovr<-dat70$pov70n/dat70$pov70d

#Scan Statistic - #Kuldorff-Nagarwalla analysis
mle<-calculate.mle(dat70a, model="poisson")
knres<-opgam(data=dat70a, thegrid=dat70a[,c("x","y")], alpha=.05, R=999, iscluster=kn.iscluster, fractpop=.05,
             model="poisson", mle=mle, log.v=T)

clusters<-get.knclusters(dat70a, knres)
plot(dat70)
points(dat70a$x[clusters[[1]]], dat70a$y[clusters[[1]]], col="green", pch=19, cex=2, lwd=2)
points(dat70a$x[clusters[[2]]], dat70a$y[clusters[[2]]], col="blue", pch=5, cex=2,lwd=2)
points(dat70a$x[clusters[[3]]], dat70a$y[clusters[[3]]], col="black", pch=25, lwd=2)

############## Cluster Analysis - 1980 #############
#Take care of zero values for calculations
dat80$pov80n<-(dat80$POVRAT8N +.00000000001)
dat80$pov80d<-(dat80$POVRAT8D +.00000000001)
dat80$povrt<-(dat80$pov80n/dat80$pov80d)

#Openshaw's GAM
dat80a<-data.frame(Observed=dat80$pov80n)
dat80a<-cbind(dat80a, Expected=dat80$pov80d*sum(dat80$pov80n)/sum(dat80$pov80d))
dat80a<-cbind(dat80a, x=dat$x, y=dat$y)
dat80a$Observed<-as.numeric(dat80a$Observed)
head(dat80a)
#Standardized Rate
dat80$spovr<-dat80$pov80n/dat80$pov80d

#Scan Statistic - #Kuldorff-Nagarwalla analysis
mle<-calculate.mle(dat80a, model="poisson")
knres<-opgam(data=dat80a, thegrid=dat80a[,c("x","y")], alpha=.05, R=999, iscluster=kn.iscluster, fractpop=.05,
             model="poisson", mle=mle, log.v=T)

clusters<-get.knclusters(dat80a, knres)
plot(dat80)
points(dat80a$x[clusters[[1]]], dat80a$y[clusters[[1]]], col="green", pch=19, cex=2, lwd=2)
points(dat80a$x[clusters[[2]]], dat80a$y[clusters[[2]]], col="blue", pch=5, cex=2,lwd=2)
points(dat80a$x[clusters[[3]]], dat80a$y[clusters[[3]]], col="black", pch=25, lwd=2)

############## Cluster Analysis - 1990 #############
#Take care of zero values for calculations
dat90$pov90n<-(dat90$POVRAT9N +.00000000001)
dat90$pov90d<-(dat90$POVRAT9D +.00000000001)
dat90$povrt<-(dat90$pov90n/dat90$pov90d)

#Openshaw's GAM
dat90a<-data.frame(Observed=dat90$pov90n)
dat90a<-cbind(dat90a, Expected=dat90$pov90d*sum(dat90$pov90n)/sum(dat90$pov90d))
dat90a<-cbind(dat90a, x=dat$x, y=dat$y)
dat90a$Observed<-as.numeric(dat90a$Observed)
head(dat90a)
#Standardized Rate
dat90$spovr<-dat90$pov90n/dat90$pov90d

#Scan Statistic - #Kuldorff-Nagarwalla analysis
mle<-calculate.mle(dat90a, model="poisson")
knres<-opgam(data=dat90a, thegrid=dat90a[,c("x","y")], alpha=.05, R=999, iscluster=kn.iscluster, fractpop=.05,
             model="poisson", mle=mle, log.v=T)

clusters<-get.knclusters(dat90a, knres)
plot(dat90)
points(dat90a$x[clusters[[1]]], dat90a$y[clusters[[1]]], col="green", pch=19, cex=2, lwd=2)
points(dat90a$x[clusters[[2]]], dat90a$y[clusters[[2]]], col="blue", pch=5, cex=2,lwd=2)
points(dat90a$x[clusters[[3]]], dat90a$y[clusters[[3]]], col="black", pch=25, lwd=2)

############## Cluster Analysis - 2000 #############
#Take care of zero values for calculations
dat00$pov00n<-(dat00$GN6001 +.00000000001)
dat00$pov00d<-(dat00$GN5001 +.00000000001)
dat00$povrt<-(dat00$pov00n/dat00$pov00d)

#Openshaw's GAM
dat00a<-data.frame(Observed=dat00$pov00n)
dat00a<-cbind(dat00a, Expected=dat00$pov00d*sum(dat00$pov00n)/sum(dat00$pov00d))
dat00a<-cbind(dat00a, x=dat00$x, y=dat00$y)
dat00a$Observed<-as.numeric(dat00a$Observed)
head(dat00a)
#Standardized Rate
dat00$spovr<-dat00$pov00n/dat00$pov00d

#Scan Statistic - #Kuldorff-Nagarwalla analysis
mle<-calculate.mle(dat00a, model="poisson")
knres<-opgam(data=dat00a, thegrid=dat00a[,c("x","y")], alpha=.05, R=999, iscluster=kn.iscluster, fractpop=.05,
             model="poisson", mle=mle, log.v=T)

clusters<-get.knclusters(dat00a, knres)
plot(dat00)
points(dat00a$x[clusters[[1]]], dat00a$y[clusters[[1]]], col="green", pch=19, cex=2, lwd=2)
points(dat00a$x[clusters[[2]]], dat00a$y[clusters[[2]]], col="blue", pch=5, cex=2,lwd=2)
points(dat00a$x[clusters[[3]]], dat00a$y[clusters[[3]]], col="black", pch=25, lwd=2)


######### R Plots ########
spplot(gwr.dat, "ppubinc00", at=quantile(gwr.a$SDF$ppubinc00), col.regions=cols, main="Proportion Public Income Effect")
spplot(gwr.dat, "pemp00", at=quantile(gwr.a$SDF$pemp00), col.regions=cols, main="Proportion Employed Effect")
spplot(gwr.dat, "pfhh00", at=quantile(gwr.a$SDF$pfhh00), col.regions=cols, main="Proportion Female Headed Household Effect on 2000 Poverty")
spplot(gwr.dat, "phis00", at=quantile(gwr.a$SDF$phis00), col.regions=cols, main="Proportion Hispanic Effect on 2000 Poverty")

spplot(gwr.dat70, "pdrop70", at=quantile(gwr.a70$SDF$pdrop70), main="Proportion Dropout Rate Effect on 1970 Poverty")
spplot(gwr.dat80, "pdrop80", at=quantile(gwr.a80$SDF$pdrop80), main="Proportion Dropout Rate Effect on 1980 Poverty")
spplot(gwr.dat90, "pdrop90", at=quantile(gwr.a90$SDF$pdrop90), main="Proportion Dropout Rate Effect on 1990 Poverty")
spplot(gwr.dat00, "pdrop00", at=quantile(gwr.a00$SDF$pdrop00), main="Proportion Dropout Rate Effect on 2000 Poverty")

