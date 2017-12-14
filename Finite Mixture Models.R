library(flexmix)
library(foreign)
library(lattice)

summary(balt)
bb<- split(balt, balt$CITCOU)
summary(bb[["0"]])
summary(bb[["1"]])

reg1<- lm(PRICE~AC+AGE+FIREPL+GAR+LOTSZ+
            NBATH+NROOM+PATIO+SQFT, data=balt)
summary(reg1)

#By Regime
reg3<- lm(PRICE~AC+AGE+FIREPL+GAR+LOTSZ+
            NBATH+NROOM+PATIO+SQFT, data=bb[["0"]])
summary(reg3)

reg4<- lm(PRICE~AC+AGE+FIREPL+GAR+LOTSZ+
            NBATH+NROOM+PATIO+SQFT, data=bb[["1"]])
summary(reg4)

#Finite Mixture Regression
set.seed(123456789)

reg5<-flexmix(PRICE~AC+AGE+FIREPL+GAR+LOTSZ+NBATH+
                NROOM+PATIO+SQFT, data=balt, k=2)
reg5
summary(reg5)
parameters(reg5)
plot(reg5)

#Refit with final estimates with standard errors
freg5<-refit(reg5)
summary(freg5)

c5<-reg5@cluster

#You can change the number of clusters via k= argument
reg6<- flexmix(PRICE~AC+AGE+FIREPL+GAR+LOTSZ+NBATH+
                 NROOM+PATIO+SQFT, data=balt, k=2)
reg6
summary(reg6)

#Plotting Clusters
library(sp)
coordinates(balt)<-~X+Y
class(balt)

baltcat1<-balt$CITCOU +1
mycols2<- c("blue", "red")
plot(balt, pch=1, col=mycols2[baltcat1])
