#Loading Packages 
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(geosphere)
library(caret)
library(quantreg)
library(FNN)

#Reading the dataset containing the rental properties and venues
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Project/Data/')
utrecht.sf = read_sf("rental_central.shp", stringsAsFactors = T)
venues.sf = read_sf("utrecht_venues.shp", stringsAsFactors = T)
complete.sf = read_sf("rentals_complete.shp", stringsAsFactors = T)
complete.sf$woz_waarde <- as.numeric(complete.sf$woz_waarde)

#Deal with missing values
complete.sf = na.omit(complete.sf)
complete.sf$geometry = NULL
complete.sf$Zipcode = NULL
complete.sf$Neighboorh = NULL
complete.sf$District = NULL
complete.sf$woz_waarde = NULL
complete.sf$criminal = NULL
complete.sf$longitude = NULL
complete.sf$Latitude = NULL
complete.sf$bus_dist = NULL
complete.sf$park_dist = NULL
complete.sf$schools_di = NULL
complete.sf$center = NULL
complete.sf$Train_dist = NULL
complete.sf$Rent = NULL
complete.sf$migration_ = NULL

#Run a basic logistic regression model
summary(lm(Rent ~., data=complete.sf))
summary(lm(rentsqm ~., data=complete.sf))
plot(lm(rentsqm ~., data=complete.sf))

#Add row with rent per sqm
complete.sf$rentsqm = complete.sf$Rent/complete.sf$Size
summary(lm(rentsqm ~., data=complete.sf))

#Plot the distribution of rentsqm
hist(complete.sf$rentsqm)
hist(log(complete.sf$rentsqm))

#Categorizing the rooms variable
summary(lm(Rent ~ Rooms + Size + center, data=complete))
complete$one_room <- (complete$Rooms==1)+0 
complete$two_rooms <- (complete$Rooms>1)+0 

summary(lm(Rent ~ two_rooms + Size + center + Furnished_furnished + Furnished_upholstered +
             restaurants_dist + Train_dist + woz_waarde +
             criminal + collected, data=complete))

#Normalizing
preproc1 <- preProcess(complete[,c(10:16)], method=c('center', 'scale'))
norm1 <- predict(preproc1, complete[,c(10:16)])

normalized = as.data.frame(norm1)
normalized['Rent'] = complete['Rent']
normalized['Rooms'] = complete['Rooms']
normalized['Furnished_furnished'] = complete['Furnished_furnished']
normalized['Furnished_upholstered'] = complete['Furnished_upholstered']
normalized['Size'] = complete['Size']
normalized['center'] = complete['center']
normalized['collected'] = complete['collected']

#Split on new and old dataset
old = complete[complete$collected == '0',]
new = complete[complete$collected == '1',]

#Dealing with outliers
boxplot(complete$Rent)
Q <- quantile(complete$Rent, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(complete$Rent)
up <- Q[2]+1.5*iqr
low <- Q[1]-1.5*iqr
complete<- subset(complete, complete$Rent > (Q[1] - 2*iqr) & complete$Rent < (Q[2]+2*iqr))

#Compute logarithms of rent price
complete.sf$lnRent= log(complete.sf$Rent)
complete.sf$lnRentSqm = log(complete.sf$rentsqm)

#Plotting the coordinates
plot(complete.sf$geometry)

#Plot the different rentals by collection moment
tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "collected", size = 0.05, alpha=0.7, palette='plasma', n=2)

#Plotting the rent data together with maps 
tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "rentsqm", size = 0.05, alpha=0.8, style='pretty', n=3, palette='inferno', title='Rent per sqm (€)')

#Plotting the log(rent) data together with maps 
tmap_mode("view")
complete.sf$stand.rent = as.numeric(scale(complete.sf$lnRent)) #<-plotting different prices in different colors
tm_shape(complete.sf) + tm_dots(col = "stand.rent", size = 0.05)

#Plotting venues together with maps
tmap_mode("view")
tm_shape(venues.sf) + tm_dots(size = 0.05)

#Plotting train stations in Utrechtzz``
tmap_mode("view")
train.sf = subset(venues.sf, Venue.Cate == 'Train Station')
tm_shape(train.sf) + tm_dots(size=0.05)

train.coords = st_coordinates(train.sf)

#Check the distribution of the rental prices
ggplot(complete.sf, aes(x=Rent))+
  geom_histogram()

#Check the distribution of log(rent)
ggplot(complete.sf, aes(x=lnRent))+
  geom_histogram()

#Check the distribution of the rentalsqm
ggplot(complete.sf, aes(x=rentsqm))+
  geom_histogram()

#Check the distribution of the log(rentalsqm)
ggplot(complete.sf, aes(x=lnRentSqm))+
  geom_histogram()

#extrating the coordinates from the dataste
utrecht.coords = st_coordinates(complete.sf)

#Coords are in meters
utrecht.coords

#Defining adjacency W
#Location is near if less than 500 meters
utrecht.nb = dnearneigh(utrecht.coords, 0, 250, longlat = FALSE) #defining as adjacent all obs closer than 500

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
price.ols = lm(lnRent ~ Size + restaurant + Furnished_ +
                 Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                 collected, data = complete.sf)
summary(price.ols)

#Estimating Spatial Error Model with adjacency W
price.err_adj = errorsarlm(lnRent ~ Size + restaurant + Furnished_ +
                             Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                             collected, data = complete.sf, listw= utrecht.Wadj, zero.policy = TRUE)
summary(price.err_adj)

#Estimating Spatial Error Model with distance-based W 1/d
price.err_ids = errorsarlm(lnRent ~ Size + restaurant + Furnished_ +
                             Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                             collected, data = complete.sf, listw= utrecht.Wids, zero.policy = TRUE)
summary(price.err_ids)

#Estimating Spatial Error Model with distance-based W 1/d^2
price.err_ids2 = errorsarlm(lnRent ~ Size + restaurant + Furnished_ +
                              Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                              collected, data = complete.sf, listw= utrecht.Wids2,zero.policy = TRUE)
summary(price.err_ids2)

#Estimating Spatial Error Model with distance-based W exp(-x)
price.err_expdis = errorsarlm(lnRent ~ Size + restaurant + Furnished_ +
                                Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                                collected, data = complete.sf, listw= utrecht.Wexpdis,zero.policy = TRUE)
summary(price.err_expdis)

#Model with all variables
price = errorsarlm(lnRent ~ ., data = complete.sf,
           listw= utrecht.Wadj,zero.policy = TRUE)
summary(price)

price.err_adj = errorsarlm(Rent ~ Size + restaurants_dist + Furnished_furnished +
                             Furnished_shell ,data = rentals, listw= utrecht.Wadj,zero.policy = TRUE)
summary(price.err_adj)

#Linear model with all variables
price.ols = lm(Rent ~ Size + Rooms + Furnished_furnished + Furnished_upholstered +
                 center + schools_dist + park_dist + restaurants_dist +  bus_dist + Train_dist + woz_waarde +
                 criminal + collected, data = complete)
sm <- summary(price.ols)


#Linear model with only significant variables and high R-squared
price.ols = lm(lnRent ~ Size + restaurant + Furnished_ +
                 Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
                 collected, data = complete.sf,)
sm <- summary(price.ols)
sm

#Plot the residuals on a map
tmap_mode("view")
complete.sf$LR_residuals = price.ols$residuals
complete.sf$stand.LR_residuals = as.numeric(scale(complete.sf$LR_residuals)) #<-plotting different prices in different colors
tm_shape(complete.sf) + tm_dots(col = "stand.LR_residuals", size = 0.05)


#Linear model with only significant variables and high R-squared on log(rent)
price.ols = lm(rentsqm ~ Size + Rooms + Furnished_ + Furnishe_1 +
                 restaurant + Train_dist + woz_waarde +
                 criminal + collected, data = complete.sf)
sm <- summary(price.ols)
sm
res=resid(price.ols)
pre=predict(price.ols)
plot(res~pre)

#Locating outlier with cooks
cookd=cooks.distance(price.ols)
which(cookd>1)

#Performing Random Forest Regression
trctrl <- trainControl(method = "none")

#Train random forest model with variables selected by linear regression
rentals = na.omit(complete.sf)
rfregFit <- train(rentsqm ~ ., data = complete.sf, 
                  method = "ranger",
                  trControl=trctrl,
                    # calculate importance
                  importance="permutation", 
                  tuneGrid = data.frame(mtry=8, min.node.size = 5, splitrule="variance")
)

#Train random forest model with full variables and log(Rent)
rfregFit <- train(lnRent ~ Size + Rooms + Furnished_furnished + Furnished_upholstered +
                    center + schools_dist + park_dist + restaurants_dist + bus_dist + 
                    Train_dist + woz_waarde + criminal, 
                  data = rentals, 
                  method = "ranger",
                  trControl=trctrl,
                  # calculate importance
                  importance="permutation", 
                  tuneGrid = data.frame(mtry=12, min.node.size = 5, splitrule="variance")
)
#Plot the Observed vs OOB predicted values from the model
plot(complete.sf$rentsqm,rfregFit$finalModel$predictions,
     pch=19,xlab="observed Rent",
     asp=1,
     ylab="OOB predicted Rent")
mtext(paste("R-squared",
            format(rfregFit$finalModel$r.squared,digits=2)))
abline(0,1)

#Plot the residuals
plot(complete.sf$lnRent,(rfregFit$finalModel$predictions-complete.sf$lnRent),
     pch=18,ylab="residuals (predicted-observed)",
     xlab="observed Rent",col="blue4",
     asp=1)
abline(h=0,col="red4",lty=2)

#R-squared of final model
rfregFit$finalModel$r.squared

res=resid(rfregFit)
pre=predict(rfregFit)
plot(res~pre)

#Plot the residuals on a map
tmap_mode("view")
complete.sf$RF_residuals = rfregFit$finalModel$predictions-complete.sf$rentsqm
complete.sf$stand.RF_residuals = as.numeric(scale(complete.sf$RF_residuals)) #<-plotting different prices in different colors
tm_shape(complete.sf) + tm_dots(col = "stand.RF_residuals", size = 0.05)
