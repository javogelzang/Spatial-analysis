library(sp)
library(gstat)
library(meuse)
class(meuse)
summary(meuse)
coordinates(meuse) =c("x", "y")
class(meuse)
summary(meuse)

sp.theme(TRUE)
spplot(meuse, "zinc", key.space="right")

data(meuse)
coordinates(meuse)=~x+y
coordinates(meuse)[1:10,]
spplot(meuse, "zinc", key.space="right")
spplot(meuse, "zinc", key.space="right", scales=list(draw=TRUE))

data(meuse.riv)
dim(meuse.riv)
meuse.riv[1:10,]
meuse.sp=SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")))
meuse.lt = list("sp.polygons", meuse.sp, fill="grey")
spplot(meuse, "zinc", key.space = "right", sp.layout = meuse.lt)

#Grids
data(meuse.grid)
class(meuse.grid)
coordinates(meuse.grid)=c("x",  "y")
class(meuse.grid)
gridded(meuse.grid)=TRUE
class(meuse.grid)
summary(meuse.grid)
meuse.lt = list(riv=list("sp.polygons", meuse.sp, fill="grey"),
                pts=list("sp.points", meuse, pch=3, cex=0.5, col="black"))
spplot(meuse.grid, sp.layout=meuse.lt, key.space="right")

#Exploratory data analysis
library(sp)
sp.theme(TRUE)
data(meuse)
coordinates(meuse)=c("x", "y")
sel = spplot(meuse, "zinc", identify=TRUE)
sel
meuse$zinc[as.numeric(sel)]

#Simple interpolation algorithms
#Trend surface analysis
library(gstat)
data(meuse.grid)
coordinates(meuse.grid)=~x+y
gridded(meuse.grid)=TRUE
meuse.grid$tr1 = krige(log(zinc) ~1, meuse, meuse.grid, degree=1)$var1.pred
meuse.grid$tr2 = krige(log(zinc) ~1, meuse, meuse.grid, degree=2)$var1.pred
meuse.grid$tr3 = krige(log(zinc) ~1, meuse, meuse.grid, degree=3)$var1.pred
spplot(meuse.grid, c("tr1", "tr2", "tr3"), sp.layout=meuse.lt,
       main="log(zinc),trend surface interpolation")

#Local trends in a neighbourhood may also be fitted
m = krige(log(zinc) ~ x+y, meuse, meuse.grid, nmax=10)
spplot(m, "var1.pred", sp.layout=meuse.lt, main="local 1st order trend")

#Inverse distance interpolation
lzn.tp = idw(log(zinc) ~ 1, meuse, meuse.grid)
spplot(lzn.tp, "var1.pred", sp.layout=meuse.lt, main="log(zinc), inverse distance inter polation")
meuse.grid$idp0.5 = idw(log(zinc) ~1, meuse, meuse.grid, idp=0.5)$var1.pred
meuse.grid$idp02 = idw(log(zinc) ~1, meuse, meuse.grid, idp=2)$var1.pred
meuse.grid$idp05 = idw(log(zinc) ~1, meuse, meuse.grid, idp=5)$var1.pred
meuse.grid$idp10 = idw(log(zinc) ~1, meuse, meuse.grid, idp=10)$var1.pred
spplot(meuse.grid,c("idp0.5", "idp02", "idp05", "idp10"), sp.layout=meuse.lt,
       main="log(zinc), inverse distance interpolation")

#Variogram cloud and point pair plots
lzn.vgm = variogram(log(zinc) ~ 1, meuse, cloud=TRUE)
pp.sel = plot(lzn.vgm, identify=TRUE)
plot(pp.sel, meuse)

pp.sel = plot(lzn.vgm, digitize=TRUE)
plot(pp.sel, meuse)

#Outliers
meuse.outl = meuse
meuse.outl$zinc[1] = 1e+05
lzn2.vgm = variogram(log(zinc) ~ 1, meuse.outl, cloud=TRUE)
pp.sel = plot(lzn2.vgm, identify=TRUE)
plot(pp.sel, meuse.outl)

#Sample variogram and variogram model
lzn3.vgm = variogram(log(zinc)~ 1, meuse)
lzn3.vgm 
plot(lzn3.vgm)

lzn4.vgm = variogram(log(zinc) ~ 1, meuse, cutoff = 1000, width=50)
lzn4.vgm
plot(lzn4.vgm)

#Kriging
lzn5.vgm = variogram(log(zinc)~ 1, meuse, cutoff = 1000, width = 50)
plot(gamma ~ dist, lzn5.vgm)
lines(c(0, lzn5.vgm$dist), c(0, lzn5.vgm$gamma))

#Show valid parametic models
show.vgms()

#Try a spherical model
lzn6.vgm = variogram(log(zinc) ~ 1, meuse)
plot(lzn6.vgm)
lzn6.mod = vgm(0.6, 'Sph', 1000, 0.05)
plot(lzn6.vgm, lzn6.mod)

#Fit the three parameters automatically
lzn7.vgm = variogram(log(zinc)~ 1, meuse)
lzn7.mod = fit.variogram(lzn7.vgm, vgm(0.6, "Sph", 1000, 0.05))
lzn7.mod
plot(lzn7.vgm, lzn7.mod)

#The initial parameters have to be somewhat precise, see example
lzn7.misfit = fit.variogram(lzn7.vgm, vgm(0.5, "Sph", 600, 0.05))
plot(lzn7.vgm, lzn7.misfit)

#Apply ordinary kriging
lzn.ok = krige(log(zinc)~ 1, meuse, meuse.grid, lzn7.mod)
lzn = lzn.ok
lzn$ok = lzn.ok$var1.pred
spplot(lzn, "ok", main="log(zinc), ordinary kriging")

#Apply simple kriging
lzn.sk = krige(log(zinc)~ 1, meuse, meuse.grid, lzn7.mod, beta = 5.9)
lzn$sk = lzn.sk$var1.pred
spplot(lzn, c("ok", "sk"), main = "log(zinc), ordinary and simple kriging")

#Apply universal kriging, defining nmax to specify the neighbourhood size
lzn.uk = krige(log(zinc)~ x+y, meuse, meuse.grid, lzn7.mod, nmax=16)
lzn$uk = lzn.uk$var1.pred
meuse.lt$pts$col = "black"
spplot(lzn, c("ok", "sk", "uk"), sp.layout = meuse.lt, main = "log(zinc), ordinary, simple, universal kriging")

#Compare the kriging standard errors
lzn$ok.se = sqrt(lzn.ok$var1.var)
lzn$sk.se = sqrt(lzn.sk$var1.var)
lzn$uk.se = sqrt(lzn.uk$var1.var)
meuse.lt$pts$col = "green"
spplot(lzn, c("ok.se", "sk.se", "uk.se"), sp.layout = meuse.lt, main="log(zinc), ordinairy, simple and universal kriging standard errors")

#Local kriging
lzn.lok = krige(log(zinc)~ 1, meuse, meuse.grid, lzn7.mod, nmax=5)
lzn$lok = lzn.lok$var1.pred
spplot(lzn, c("lok", "ok"), main="lok: local(n=5); ok: global")

#Block kriging
lzn.ok = krige(log(zinc)~ 1, meuse, meuse.grid, lzn7.mod)
lzn.blok = krige(log(zinc)~ 1, meuse, meuse.grid, lzn7.mod, block = c(40, 40))
lzn.ok$points = lzn.ok$var1.pred
lzn.ok$block = lzn.blok$var1.pred
spplot(lzn.ok, c("points", "block"), sp.layout = meuse.lt, names.attr = c("point kriging", "block kriging, 40m x 40m blocks"))

gridparameters(meuse.grid)

#Prediction standard errors
lzn.ok$points.se = sqrt(lzn.ok$var1.var)
lzn.ok$block.se = sqrt(lzn.blok$var1.var)
spplot(lzn.ok, c("points.se", "block"), sp.layout = meuse.lt, names.attr = c("SE point kriging", "SE block kriging, 40m x 40m blocks"))

#Calculate block averages for 400m x 400m grids
lzn.blok400 = krige(log(zinc)~1, meuse, meuse.grid, lzn7.mod, block=c(400, 400))
lzn.ok$block400 = lzn.blok400$var1.pred
spplot(lzn.ok, c("points", "block400"), scales = list(draw=TRUE), names.attr = c("point kriging", "block kriging, 400m x 400m blocks"),
       sp.layout = meuse.lt)
spplot(lzn.ok, c("block", "block400"), scales = list(draw=TRUE), names.attr = c("block kriging 40m", "block kriging, 400m"),
       sp.layout = meuse.lt)

#Load datasets
aqgrid = read.csv("~/Documents/ADS Master/Spatial Statistics/Lab Sessions/Spatial interpolation 2/aqgrid.csv")
aquifer = read.csv("~/Documents/ADS Master/Spatial Statistics/Lab Sessions/Spatial interpolation 2/Aquifer.csv")


