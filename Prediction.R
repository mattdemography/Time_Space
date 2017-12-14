library(foreign)
library(sp)
library(gstat)

balt<- read.dbf("C:/Users/mmarti24/Downloads/baltim.dbf")
summary(balt)

coordinates(balt)<- ~X+Y
class(balt)

plot(balt,asp=1,cex=3 * balt$PRICE/max(balt$PRICE),pch=1,col="blue")

#Takes bounding box of points and figure out 
#vertical and horizontal range
pt2grid <- function(ptframe,n){
  bb <- bbox(ptframe)
  xrange <- abs(bb[1,1] - bb[1,2])
  yrange <- abs(bb[2,1] - bb[2,2])
  #Offset of x and y direction
  cs <- c(xrange/n,yrange/n)
  #Get first offset set + cell center offset
  cc <- bb[,1] + (cs/2)
  #n= dimension of grid (number of cells is equal in each dimension,
  #but the shape of the cell is not square)
  dc <- c(n,n)
  x1 <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=dc)
  x2 <- SpatialGrid(grid=x1)
  #Returns a Spatial Grid object
  return(x2)
}

#Using ten to keep the grid simple
#Computes 100 squares (n,n)
#Here you are predicting the points in the center of each block
#Could estimate each block - called block kerging
baltg<- pt2grid(balt,n=10.0)
class(baltg)

plot(balt,asp=1,cex= 3* balt$PRICE/max(balt$PRICE),pch=1,col="blue")
#Adds grid on top of the former plot
plot(baltg,add=T)

v<- variogram(PRICE~ X+Y+I(X*Y), balt, cutoff=35)
plot(v)

vsph <- vgm(psill=300,model="Sph",range=20,nugget=100)
plot(v,model=vsph)
#Now use parameters from vsph as starting values in model fitting process
vsphfit <- fit.variogram(v,vsph)
  #Plotting v (dots) and then the model
  plot(v,model=vsphfit)
  #Extract best fit parameters
  print(vsphfit)
    #First value is the nugget the lower value is the partial sill
  str(vsphfit)
  #Sum of squared errors
  attr(vsphfit,"SSErr")

#Now do the same for the exponential variogram
vexp <- vgm(psill=400,model="Exp",range=25,nugget=150)
plot(v,model=vexp)
vexpfit <- fit.variogram(v,vexp)
  plot(v,model=vexpfit)
  print(vexpfit)
  attr(vexpfit,"SSErr")

#######Predicted Value and Error Map#####

#Set up Grid
  baltg100<- pt2grid(balt,n=100.0)
  class(baltg100)
  plot(baltg100)
  
#Kriging
#Spherical Model
  krsp1<- krige(PRICE~X+Y+I(X*Y),locations=balt,
                newdata=baltg100, model=vsphfit)
  summary(krsp1)
  str(krsp1)
  #predicted values
  hist(krsp1$var1.pred)
  hist(krsp1$var1.var)
  
  #Predicted Value Map
  pts.s<-list("sp.points", balt, col="White",
              pch=1, cev=3*balt$PRICE/max(balt$PRICE))
  spplot(krsp1,"var1.pred", asp=1, sp.layout=list(pts.s))
  #Prediction Error Map
  pts.s <- list("sp.points",balt,col="black",
                pch=20)
  spplot(krsp1,"var1.var",asp=1,col.regions=cm.colors(64),sp.layout=list(pts.s))
  
  #Cross-Validation - krige.cv
  #nfold - how many to leave out
  #nmax - X number are non-zero, everything else is 0, cuts off covariance matrix at x
  cv1 <- krige.cv(PRICE~ X+Y+I(X*Y),balt,vsphfit,nmax=40, nfold=5)
  summary(cv1)
  #Correlation between observed and predicted
  cor(cv1$observed, cv1$var1.pred)
  #For Mean and Residual, the smaller the number the better the fit
  mean(cv1$residual)
  mean(cv1$residual^2)
  
#Exponential
  krex1 <- krige(PRICE~X+Y+I(X*Y),locations=balt,newdata=baltg100,model=vexpfit)
  summary(krex1)
  #Cross-Validation
  cv2 <- krige.cv(PRICE~ X+Y+I(X*Y),balt,vexpfit,nmax=40)
  summary(cv2)
  cor(cv2$observed, cv2$var1.pred)
  