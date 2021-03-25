#Loading Packages 
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(geosphere)

#Reading the dataset containing the rental properties
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Project/')
utrecht.sf = read_sf("rental_central.shp", stringsAsFactors = T)

#Reading the dataset containing the venues
venues.sf = read_sf("utrecht_venues.shp", stringsAsFactors = T)

#Check column names
names(utrecht.sf)
names(venues.sf)

#Plotting the coordinates
plot(utrecht.sf$geometry)

#Plotting the the rent data together with maps 
tmap_mode("view")
utrecht.sf$stand.rent = as.numeric(scale(utrecht.sf$Rent)) #<-plotting different prices in different colors
tm_shape(utrecht.sf) + tm_dots(col = "stand.rent", size = 0.05)

#Plotting venues together with maps
tmap_mode("view")
tm_shape(venues.sf) + tm_dots(size = 0.05)

#Plotting libraries in Utrecht
tmap_mode("view")
train.sf = subset(venues.sf, Venue.Cate == 'Train Station')
tm_shape(library.sf) + tm_dots(size=0.05)

library.coords = st_coordinates(library.sf)

#Check the distribution of the rental prices
ggplot(utrecht.sf, aes(x=Rent))+
  geom_histogram()

#Compute logarithms of rent price
utrecht.sf$lnRent= log(utrecht.sf$Rent)

#Check the distribution of log(rent)
ggplot(utrecht.sf, aes(x=lnRent))+
  geom_histogram()

#Plotting the log(rent) data together with maps 
tmap_mode("view")
utrecht.sf$stand.rent = as.numeric(scale(utrecht.sf$lnRent)) #<-plotting different prices in different colors
tm_shape(utrecht.sf) + tm_dots(col = "stand.rent", size = 0.05)

#extrating the coordinates from the dataste
utrecht.coords = st_coordinates(utrecht.sf)

#Coords are in meters
utrecht.coords

#Measuring distance between rent property and nearby train station

#Defining adjacency W
#Location is near if less than 500 meters
utrecht.nb = dnearneigh(utrecht.coords, 0, 500, longlat = FALSE) #defining as adjacent all obs closer than 500

#Zero policy at true so that points could have zero near points. 
utrecht.Wadj = nb2listw(utrecht.nb, style = "W", zero.policy = TRUE) #computing W; style W row normalized; B non-normalized; zero.policy = 1 means that some obs may have no adjacent obs

#Zero policy must be indicated everytime
summary(utrecht.Wadj)
utrecht.Wadj
summary(utrecht.Wadj, zero.policy=TRUE)

set.ZeroPolicyOption(TRUE) # unless we set it.
summary(utrecht.Wadj)#not it works without indication zero policy
utrecht.Wadj

#Printing the weights for the first 5 obs.
weights(utrecht.Wadj)[1:5]

#Defining distance-based W: 1/d

#Note that we only calculate for the adjacent obs (but it can be changed)
dist = nbdists(utrecht.nb, utrecht.coords, longlat = TRUE)
ids = lapply(dist, function(x) 1/(x))
ids[1:5]

utrecht.Wids = nb2listw(utrecht.nb, glist = ids, style = "W", zero.policy = TRUE)

#Now the weights are based on their distance 
weights(utrecht.Wids)[1:5]

#definig distance-based W: 1/d^2
ids2 = lapply(dist, function(x) 1/(x^2))
ids2[1:5]

utrecht.Wids2 = nb2listw(utrecht.nb, glist = ids2, style = "W", zero.policy = TRUE)
weights(utrecht.Wids2)[1:5]

#Defining distance-based W: exp(-x)
expdis <- lapply(dist, function(x) {
  tryCatch(exp(-x/10000), error = function(e) return(NULL))
})
expdis[1:5]

utrecht.Wexpdis = nb2listw(utrecht.nb, glist = expdis, style = "W", zero.policy = TRUE)
weights(utrecht.Wexpdis)[1:5]

#Estimating LR without correlation (OLS)
price.ols = lm(lnRent ~ Size + Furnished + Rooms, data = utrecht.sf)
summary(price.ols)

#Estimating Spatial Error Model with adjacency W
price.err_adj = errorsarlm(lnRent ~ Size + Furnished + Rooms, data = utrecht.sf, listw= utrecht.Wadj,zero.policy = TRUE)
summary(price.err_adj)

#Estimating Spatial Error Model with distance-based W 1/d
price.err_ids = errorsarlm(lnPrice ~ Type, data = bristol.sf, listw= bristol.Wids,zero.policy = TRUE)
summary(price.err_ids)

#Estimating Spatial Error Model with distance-based W 1/d^2
price.err_ids2 = errorsarlm(lnPrice ~ Type, data = bristol.sf, listw= bristol.Wids2,zero.policy = TRUE)
summary(price.err_ids2)

#Estimating Spatial Error Model with distance-based W exp(-x)
price.err_expdis = errorsarlm(lnPrice ~ Type, data = bristol.sf, listw= bristol.Wexpdis,zero.policy = TRUE)
summary(price.err_expdis)

#Estimating Spatial Error Model with adjacency W
price.err_adj = errorsarlm(lnPrice ~ Type + Outdoors + Access, data = bristol.sf, listw= bristol.Wadj,zero.policy = TRUE)
summary(price.err_adj)

