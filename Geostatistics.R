library(sp)
library(gstat)

#Exercise 6.8, Groundwater tables, High Plains, Kansas
#Load datasets
setwd('~/Documents/ADS Master/Spatial Statistics/Lab Sessions/Spatial interpolation 2')
aqgrid = read.csv('aqgrid.csv')
aquifer = read.csv('Aquifer.csv')

aquifer$X=NULL
aquifer$X.1=NULL
summary(aqgrid)
coordinates(aquifer) = ~x+y
coordinates(aqgrid) = ~x+y
gridded(aqgrid)=TRUE
sp.theme(TRUE)
spplot(aquifer, "wt", key.space="right")

#Trend surface analysis
#Fit 1st 2nd and 3rd order trends through groundwater data
aqgrid$tr1 = krige(log(wt) ~1, aquifer, aqgrid, degree=1)$var1.pred
aqgrid$tr2 = krige(log(wt) ~1, aquifer, aqgrid, degree=2)$var1.pred
aqgrid$tr3 = krige(log(wt) ~1, aquifer, aqgrid, degree=3)$var1.pred
spplot(aqgrid, c("tr1", "tr2", "tr3"), main="water table height, trend surface", key.space="right", scales=list(draw=TRUE))

#Get the residuals from 1st and 2nd order trend
aquifer$res1 = residuals(lm(wt ~ x + y, aquifer))
aquifer$res2 = residuals(lm(wt ~ x + y + I(x^2) + I(y^2) + I(x*y), aquifer))

mean(aquifer$res1)
mean(aquifer$res2)
summary(aquifer)

#Create plots of the residuals and calculate variograms
spplot(aquifer, "res1", key.space="right", scales=list(draw=TRUE))
spplot(aquifer, "res2", key.space="right", scales=list(draw=TRUE))

aq.vgm1 = variogram(res1 ~ 1, aquifer)
aq.vgm2 = variogram(res2 ~ 1, aquifer)
plot(aq.vgm1)
plot(aq.vgm2)

aq.mod1 = fit.variogram(aq.vgm1, vgm(0, 'Sph', 40000, 100))
aq.mod2 = fit.variogram(aq.vgm2, vgm(0, 'Sph', 40000, 100))

plot(aq.vgm1, aq.mod1)
plot(aq.vgm2, aq.mod2)

#Kriging on the residuals with 1st order trend
aq.res1 = krige(res1 ~ 1, aquifer, aqgrid, aq.mod1)
spplot(aq.res1)
summary(aq.res1)

#Kriging on the residuals with 2nd order trend
aq.res2 = krige(res2 ~ 1, aquifer, aqgrid, aq.mod2)
spplot(aq.res2)
summary(aq.res2)

#Water table prediction with 1st order trend



aqgrid_1 = aqgrid[, 1:2]
aqgrid_1.sp = SpatialPolygons(list(Polygons(list(Polygon(aqgrid_1)), "aqgrid")))
aqgrid_1.lt = list("sp.polygons", aqgrid_1.sp, fill="grey")
spplot(aquifer, "wt", key.space="right", sp.layout = aqgrid_1.lt)
aqgrid_1 = as.data.frame(aqgrid_1)

#Grids
data(aqgrid_1)
class(aqgrid_1)
coordinates(aqgrid_1)=c("x",  "y")
aqgrid_1 <- SpatialPointsDataFrame(aqgrid_1, data.frame(ID=1:length(aqgrid_1)))
class(aqgrid_1)
gridded(aqgrid_1)=TRUE
class(aqgrid_1)
summary(aqgrid_1)
aquifer.lt = list(pts=list("sp.points", aquifer, pch=3, cex=0.5, col="black"))
spplot(aqgrid_1, sp.layout=aquifer.lt, key.space="right")

data(aqgrid_1)
coordinates(aqgrid_1)=~x+y
gridded(aqgrid_1)=TRUE









#Kriging on residuals on 1st order trend
aq.res1 = krige(res1 ~ 1, aquifer, aqgrid_1, aq.mod1)
spplot(aq.res1)
summary(aq.res1)

#Kriging on residuals on 2nd order trend
aq.res2 = krige(res2 ~ 1, aquifer, aqgrid_1, aq.mod2)
spplot(aq.res2)
summary(aq.res2)

#Water table prediction with 1st order trend
aq.res1$tr = aqgrid_1$tr1
summary(aq.res1$tr)

#Water table prediction with 2nd order trend
aq.res2$tr = aqgrid_2$tr2

#Local trends in a neighborhood may also be fitted
m = krige(log(wt) ~ x+y, aquifer, aqgrid_1, nmax=10)
spplot(m, "var1.pred", sp.layout=aquifer.lt, main="local 1st order trend")

#Inverse distance interpolation
lzn.tp = idw(log(wt) ~ 1, aquifer, aqgrid_1)
spplot(lzn.tp, "var1.pred", sp.layout=aquifer.lt, main="log(water), inverse distance inter polation")
aqgrid_1$idp0.5 = idw(log(wt) ~1, aquifer, aqgrid_1, idp=0.5)$var1.pred
aqgrid_1$idp02 = idw(log(wt) ~1, aquifer, aqgrid_1, idp=2)$var1.pred
aqgrid_1$idp05 = idw(log(wt) ~1, aquifer, aqgrid_1, idp=5)$var1.pred
aqgrid_1$idp10 = idw(log(wt) ~1, aquifer, aqgrid_1, idp=10)$var1.pred
spplot(aqgrid_1,c("idp0.5", "idp02", "idp05", "idp10"), sp.layout=aquifer.lt,
       main="log(water), inverse distance interpolation")

#Variogram cloud and point pair plots
water.vgm = variogram(log(wt) ~ 1, aquifer, cloud=TRUE)
pp.water = plot(water.vgm, identify=TRUE)
plot(pp.water, aquifer)

pp.sel = plot(lzn.vgm, digitize=TRUE)
plot(pp.sel, meuse)

#Outliers
aquifer.outl = aquifer
aquifer.outl$water[1] = 1e+05
lzn2.vgm = variogram(log(wt) ~ 1, aquifer.outl, cloud=TRUE)
pp.sel = plot(lzn2.vgm, identify=TRUE)
plot(pp.sel, aquifer.outl)

#Sample variogram and variogram model
lzn3.vgm = variogram(log(wt) ~ 1, aquifer)
lzn3.vgm 
plot(lzn3.vgm)

lzn4.vgm = variogram(log(wt) ~ 1, aquifer, cutoff = 40000, width=50)
lzn4.vgm
plot(lzn4.vgm)

#Kriging
lzn5.vgm = variogram(log(wt)~ 1, aquifer, cutoff = 40000, width = 50)
plot(gamma ~ dist, lzn5.vgm)
lines(c(0, lzn5.vgm$dist), c(0, lzn5.vgm$gamma))

#Show valid parametic models
show.vgms()

#Try a spherical model
lzn6.vgm = variogram(log(wt) ~ 1, aquifer)
plot(lzn6.vgm)
lzn6.mod = vgm(0.6, 'Hol', 10000, 0.05)
plot(lzn6.vgm, lzn6.mod)

#Fit the three parameters automatically
lzn7.vgm = variogram(log(zinc)~ 1, meuse)
lzn7.mod = fit.variogram(lzn7.vgm, vgm(0.0005, "Hol", 10000, 0.0001))
lzn7.mod
plot(lzn7.vgm, lzn7.mod)
