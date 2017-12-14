library(sp)
library(Matrix)
library(spdep)

ncounty<- read.gal("C:/Users/mmarti24/Dropbox/Classes/Spatial Analyses (Special Cases)/sids_q.gal")
summary(ncounty)

#Set Seed
n<-100
set.seed(022222223)
#Style W is the default.  B=Binary
#Weights as a neighbor object
ncWB<-nb2listw(ncounty, style="B")
summary(ncWB)

#Weights as an array
ncWBmat<-nb2mat(ncounty,style="B")
dim(ncWBmat)

#Setting up matrices for linear simulations

#rnorm is random number generator from normal distribution
x1<-rnorm(n,mean=5.0, sd=2.0)
Xb<- x1+1.0
X<- cbind(1,x1)
summary(Xb)

#Function to do OLS
simpols<- function(y,x){
  xx<- crossprod(x)
  xxi<- solve(xx)
  xy<- crossprod(x,y)
  b<- xxi %*% xy
  return(b)
}

#Eigenvalues for weights matrix
eigw<- eigen(ncWBmat, symmetric=TRUE, only.values=TRUE)
wmm<- cbind(eigw$values[1],eigw$values[n])
wmminv<- 1.0/wmm
wmminv

#Create vector of random error terms
uu<- rnorm(n)
yy<- Xb + uu
b0<- simpols(yy,X)
b0

#SAR-Lag
rho<- 0.1
bin1<- invIrW(ncWB,rho)
y<- bin1 %*% (Xb + uu)
b1<- simpols(y,X)
b1

#SAR-Error
yyy<- Xb + bin1 %*% uu
b2<- simpols(yyy,X)
b2

#SMA-Error
wu<- lag.listw(ncWB,uu)
uma<- rho*wu+uu
y3<- Xb + uma
b3<- simpols(y3,X)
b3

#CAR-Error
cholirw<- chol(bin1)
ucar<- t(cholirw) %*% uu
y4<- Xb + ucar
b4<- simpols(y4,X)
b4
