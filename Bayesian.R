n <-100
mu <-5
set.seed(1234567)
v <- rpois(n,mu)
barplot(table(v))
summary(v)

mv<-mean(v)
vv<-var(v)
print(c(mv,vv))

vp<-dpois(5,mu)
vp
barplot(dpois(0:15,mu))

n<- 100
#Rate is beta
g<- rgamma(n,shape=0.25, rate=0.05)
summary(g)
#Variance
var(g)
hist(g)

n<-100
v<-list()
mu=5
var=100
beta=mu/var
alpha=beta*mu
for (i in 1:n){
  gg<-rgamma(1, shape=alpha, rate=beta)
  pp<- rpois(1,gg)
  v<- c(v,pp)
}
#Close list and turn back into vector in order to plot
vv<-unlist(v)
barplot(table(vv))
summary(vv)

library(mvtnorm)
#Use sigma to set correlation matrix
sigma<- matrix(c(4,2,2,3),ncol=2)
x<-rmvnorm(n=100, mean=c(1,2), sigma=sigma, method="chol")
plot(x)
abline(lm(x[,2]~x[,1]),col="red")
colMeans(x)

#########Gibbs sampler for a bivariate normal distribution
n<-1000
mu1 <- 1
mu2 <- 2
var1 <- 4
var2 <- 3
#When rho is 1, you get one dot as variability is zero.
rho <- 0.64
sdd <- sqrt(1.0 - rho*rho)
xs1 <- rnorm(1,mu1,sqrt(var1)) # first draw to get started
x1 <- list()
x2 <- list()
for (i in 1:n){
  cmx2 <- mu2 + rho *( xs1 - mu1)
  xs2 <- rnorm(1,cmx2,sdd) # conditional for x2 given x1
  cmx1 <- mu1 + rho * (xs2 - mu2)
  xs1 <- rnorm(1,cmx1,sdd) # conditional for x1 given x2
  x1 <- c(x1,xs1)
  x2 <- c(x2,xs2)
}
xx1 <- unlist(x1)
xx2 <- unlist(x2)
summary(lm(xx2~xx1))
plot(xx2~xx1)
abline(lm(xx2~xx1),col="red")

#Bring in Weight Matrix and convert it to matrix
library(spdep)
library(foreign)
glasprop <- read.dbf("C:/Users/mmarti24/Downloads/glasgow2.dbf")
summary(glasprop)

glasq1 <- read.gal("C:/Users/mmarti24/Downloads/glasgow2_q.gal")
summary(glasq1)
glasmat <- nb2mat(glasq1,style="B")
dim(glasmat)

form <- price~ crime + rooms + sales + driveshop + FLAT
reg1 <- lm(formula=form,data=glasprop)
summary(reg1)

#Run Regression
library(CARBayes)
reg2 <- S.CARleroux(formula=form,data=glasprop,family="gaussian",
                    W=glasmat,burnin=20000,n.sample=120000,verbose=FALSE,thin=10)
print(reg2)
