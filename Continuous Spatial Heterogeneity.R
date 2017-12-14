library(foreign)
balt<- read.dbf("C:/Users/mmarti24/Downloads/baltim.dbf")
summary(balt)

#Regular Regression
reg1<- lm(PRICE~LOTSZ+PATIO, data=balt)
summary(reg1)

#Linear Trend Expansion - Never do this for real just for exploratory analyses
#Var1:Var2 - Interaction Effects
  reg2<-lm(PRICE~LOTSZ + PATIO + X + Y +
             LOTSZ:X + PATIO:X + LOTSZ:Y + PATIO:Y, data=balt)
  summary(reg2)
  
  reg2$coefficients

  #Compute Spatially Varying Coefficients
  v1<-  balt$X * reg2$coefficients["X"] + balt$Y * reg2$coefficients["Y"] + reg2$coefficients["(Intercept)"]
  v2 <- balt$X * reg2$coefficients["LOTSZ:X"] + balt$Y * reg2$coefficients["LOTSZ:Y"] + reg2$coefficients["LOTSZ"]
  v3 <- balt$X * reg2$coefficients["PATIO:X"] + balt$Y * reg2$coefficients["PATIO:Y"] + reg2$coefficients["PATIO"]
  summary(cbind(v1,v2,v3))
  
  hist(v3)
  
  #Map Spatially Varying Coefficients
  library(sp)
  library(maptools)
  coordinates(balt)<- ~X + Y
  
  plot(balt, asp=1, cex=3*abs(v1)/max(v1), pch=1, col="red")
  plot(balt, asp=1, cex=3*abs(v2)/max(v2), pch=1, col="blue")
  
  #Map v3 so that negatives are taken into account
  v3neg<- v3<0
  v3neg1<- v3neg * 1 + 1
  mycols = c("red", "blue")
  plot(balt, asp=1, cex=3*abs(v3)/max(v3), pch=1, col=mycols[v3neg1])
  
#GWR
  library(spgwr)
  form<- PRICE~ LOTSZ + PATIO
  greg1<- gwr(form, data=balt, adapt=0.1)
  greg1
  summary(greg1$SDF)
  
  gwLOT<- greg1$SDF$LOTSZ
  plot(balt, asp=1, cex=3*abs(gwLOT)/max(gwLOT), pch=1, col="blue")
  
  #bandwidth Selection from GWR greg1
  m01<- max(greg1$bandwidth)
  m01
  bw1<- gwr.sel(form, data=balt)
  bw1
  
  #With optimal bandwidth
  greg2<- gwr(form, data=balt, bandwidth=bw1)
  summary(greg2$SDF)
  
  #For max bandwidth
  greg3<- gwr(form, data=balt, bandwidth=m01)
  summary(greg3$SDF)
  #Plot all bandwidths including optimal (greg2) and max(greg3)
  boxplot(cbind(greg1$SDF$LOTSZ, greg2$SDF$LOTSZ, greg3$SDF$LOTSZ))
  
  #KERNAL FUNCTION SELECTION - Defualt is Guassian
  #Bisquare Kernel
  greg4<-gwr(form, data=balt, gweight=gwr.bisquare, adapt=0.1)
  summary(greg4$SDF)
  #Tricube Kernel
  greg5<- gwr(form, data=balt, gweight=gwr.tricube, adapt=0.1)
  summary(greg5$SDF)
  
  boxplot(cbind(greg1$SDF$LOTSZ, greg4$SDF$LOTSZ, greg5$SDF$LOTSZ))
  