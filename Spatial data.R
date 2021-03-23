#Packages to read and work with shapes
install.packages("sf")
install.packages("spdep")
install.packages("tmap")

#Loading Packages 
library("sf")
library("spdep")
library("tmap")

#Reading the dataset
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Lab Sessions/GAM and Spatial Regression/')
bristol.sf = read_sf("dataset-bristol-sale-2004.shp", stringsAsFactors = T)

#strings as factors produces an automatic categorization.


#we check the variables
names(bristol.sf)

#Type D: detached house, T: terraced house, S: semi-detached house
#Outdoor: continuous variable, indicating quality of outdoor environment in a local area taken from the English IMD. Higher scores indicate lower levels of air pollution and fewer road traffic accidents.
#Access: continuous variable, indicating the level of access to common amenities and services (as measured by travel time) taken from the English IMD. Higher scores indicate better access to common amenities and services.


#plotting the coordinates
plot(bristol.sf$geometry)

#we can also work with a subset of the data contained in the shape
bristol.sf1=bristol.sf[1:100,]
plot(bristol.sf1$geometry)

#plotting the the pricing data together with maps 
tmap_mode("view")
bristol.sf$stand.price = as.numeric(scale(bristol.sf$Price)) #<-plotting different prices in differnt colors
tm_shape(bristol.sf) + tm_dots(col = "stand.price", size = 0.05)

#Check distribution of price
ggplot(bristol.sf, aes(x=Price))+
  geom_histogram()

#Might be better to work with logarithmic scale
ggplot(bristol.sf, aes(x=lnPrice))+
  geom_histogram()

#plotting the the logartihmic scale price together with maps 
tmap_mode("view")
bristol.sf$stand.price = as.numeric(scale(bristol.sf$lnPrice)) #<-plotting different prices in differnt colors
tm_shape(bristol.sf) + tm_dots(col = "stand.price", size = 0.05)

#extrating the coordinates from the dataste
bristol.coords = st_coordinates(bristol.sf)

#Coords are in meters
bristol.coords

#Defining adjacency W
#Location is near if less than 500 meters
bristol.nb = dnearneigh(bristol.coords, 0, 500, longlat = FALSE) #defining as adjacent all obs closer than 500

#Zero policy at true so that points could have zero near points. 
bristol.Wadj = nb2listw(bristol.nb, style = "W", zero.policy = TRUE) #computing W; style W row normalized; B non-normalized; zero.policy = 1 means that some obs may have no adjacent obs

#Zero policy must be indicated everytime
summary(bristol.Wadj)
bristol.Wadj
summary(bristol.Wadj, zero.policy=TRUE)

set.ZeroPolicyOption(TRUE) # unless we set it.
summary(bristol.Wadj)#not it works without indication zero policy
bristol.Wadj

#Printing the weights for the first 5 obs.
weights(bristol.Wadj)[1:5]

#Defining distance-based W: 1/d

#Note that we only calculate for the adjacent obs (but it can be changed)
dist = nbdists(bristol.nb,bristol.coords, longlat = TRUE)
ids = lapply(dist, function(x) 1/(x))
ids[1:5]

bristol.Wids = nb2listw(bristol.nb, glist = ids, style = "W", zero.policy = TRUE)

#Now the weights are based on their distance 
weights(bristol.Wids)[1:5]

#definig distance-based W: 1/d^2
ids2 = lapply(dist, function(x) 1/(x^2))
ids2[1:5]

bristol.Wids2 = nb2listw(bristol.nb, glist = ids2, style = "W", zero.policy = TRUE)
weights(bristol.Wids2)[1:5]

#Defining distance-based W: exp(-x)
expdis <- lapply(dist, function(x) {
  tryCatch(exp(-x/10000), error = function(e) return(NULL))
})
expdis[1:5]

bristol.Wexpdis = nb2listw(bristol.nb, glist = expdis, style = "W", zero.policy = TRUE)
weights(bristol.Wexpdis)[1:5]

#Estimating LR without correlation (OLS)
price.ols = lm(lnPrice ~ Type, data = bristol.sf)
summary(price.ols)

#Estimating Spatial Error Model with adjacency W
price.err_adj = errorsarlm(lnPrice ~ Type, data = bristol.sf, listw= bristol.Wadj,zero.policy = TRUE)
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
