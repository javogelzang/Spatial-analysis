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
aq.res1$tr = aqgrid$tr1
aq.res1$wt = aq.res1$var1.pred + aq.res1$tr
spplot(aq.res1, 'wt', main='Ordinary kriging of water table', key.space='right',
       scales=list(draw=TRUE))

aq.res1$se = sqrt(aq.res1$var1.var)
spplot(aq.res1, 'se', main='Kriging standard error', key.space='right', 
       scales=list(draw=TRUE))

#Water table prediction with 2nd order trend
aq.res2$tr = aqgrid$tr2
aq.res2$wt = aq.res2$var1.pred + aq.res2$tr
spplot(aq.res2, 'wt', main='Ordinary kriging of water table', key.space='right',
       scales=list(draw=TRUE))

aq.res2$se = sqrt(aq.res2$var1.var)
spplot(aq.res2, 'se', main='Kriging standard error', key.space='right', 
       scales=list(draw=TRUE))

#Universal kriging with 1st order drift
aq.res1$ukr1 = krige(wt ~ x+y, aquifer, aqgrid, aq.mod1)$var1.pred
aq.res1$uk1var = krige(wt ~ x+y, aquifer, aqgrid, aq.mod1)$var1.var
aq.res1$uk1se = sqrt(aq.res1$uk1var)
spplot(aq.res1, 'ukr1', main='Universal kriging of water table', key.space='right',
       scales=list(draw=TRUE))
spplot(aq.res1, 'uk1se', main='Universal kriging standard error', key.space='right',
       scales=list(draw=TRUE))

#Universal kriging with 2nd order drift
aq.res2$ukr2 = krige(wt ~ x+y, aquifer, aqgrid, aq.mod2)$var1.pred
aq.res2$uk2var = krige(wt ~ x+y, aquifer, aqgrid, aq.mod1)$var1.var
aq.res2$uk2se = sqrt(aq.res2$uk2var)
spplot(aq.res2, 'ukr2', main='Universal kriging of water table', key.space='right',
       scales=list(draw=TRUE))
spplot(aq.res2, 'uk2se', main='Universal kriging standard error', key.space='right', 
       scales=list(draw=TRUE))

