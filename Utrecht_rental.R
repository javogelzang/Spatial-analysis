#Loading Packages 
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(geosphere)
library(caret)
library(quantreg)
library(FNN)
library(leaps)
library(ISLR)
library(glmnet)
library(spatialreg)

#Reading the dataset containing the rental properties and venues
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Project/Data/')
utrecht.sf = read_sf("rental_central.shp", stringsAsFactors = T)
venues.sf = read_sf("utrecht_venues.shp", stringsAsFactors = T)
complete.sf = read_sf("rentals_complete.shp", stringsAsFactors = T)

#Deal with missing values
complete.sf = na.omit(complete.sf)
complete.sf$geometry = NULL
complete.sf$Zipcode = NULL
complete.sf$Neighboorh = NULL
complete.sf$District = NULL

#Calculate the rent per sqm
rentsqm = complete.sf$rent/complete.sf$size
lnrentsqm = log(rentsqm)
complete.sf$rent = NULL

#Standardizing independent variables
complete.sf = scale(complete.sf)
complete.sf = cbind(lnrentsqm, complete.sf)
complete.sf = as.data.frame(complete.sf)

#Performing Lasso for variable selection
x=model.matrix(lnrentsqm~.,complete.sf)[,-1] 
y=complete.sf$lnrentsqm
grid=10^seq(10,-2,length=100)
set.seed (1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed (1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])

#MSE on lasso
mean((lasso.pred-y.test)^2)

#Calculate coefficients 
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:21,]
lasso.coef

#Check for multicollinearity
correlation = cor(complete.sf)
View(correlation)
which(correlation>0.5 & correlation<1)

#Formula based on lasso regression selected variables
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2 + Latitude
LR_model = lm(formula, data=complete.sf)
sm = summary(LR_model)
mean(sm$residuals^2)
sm$r.squared

plot(complete.sf$lnrentsqm,LR_model$fitted.values,
     pch=19,xlab="observed log(Rent) per sqm",
     asp=1,
     pty='s',
     cex=0.3,
     ylab="predicted log(Rent) per sqm",
     xlim=c(1,4.5),
     ylim=c(2,4.5))
mtext(paste("R-squared",
            format(sm$r.squared,digits=2)))
abline(0,1)

#Plot the residuals
coor.sf = read_sf("rentals_complete.shp", stringsAsFactors = T)
residuals = as.numeric(sm$residuals)
coor.sf$residuals = residuals
tmap_mode("view")
tm_shape(coor.sf) + tm_dots(col = "residuals", size = 0.05, alpha=1, palette='PuOr', n=7) + 
  tm_scale_bar() + tm_compass()

#Plot the residuals
venues_plot.sf = read_sf("venues_plot.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(venues_plot.sf) + tm_dots(col = "Venue_1", size = 0.05, alpha=1, title='Venue type') + tm_scale_bar()

venues_plot.sf = read_sf("venues_all.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(venues_plot.sf) + tm_dots(col = "Venue2", size = 0.03, alpha=1, title='Venue type', palette='Dark2') + tm_scale_bar()

residual_plot.sf = read_sf("residual_plot.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(venues_plot.sf) + tm_dots(col = "RandomFore", size = 0.05, alpha=1, palette='PuOr', n=7) + tm_scale_bar()

residual_plot.sf = read_sf("residual_plot.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(venues_plot.sf) + tm_dots(col = "LinearRegr", size = 0.05, alpha=1, title='Venue type') + tm_scale_bar()
# formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2 + Latitude
# LR_model = glm(formula, data=complete.sf)
# sm = summary(LR_model)
# mean(sm$deviance.resid^2)
# sm$aic
# LR_model$fitted.values


# #Dealing with outliers
# boxplot(complete.sf$lnrentsqm)
# Q <- quantile(complete.sf$lnrentsqm, probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(complete.sf$lnrentsqm)
# up <- Q[2]+1.5*iqr
# low <- Q[1]-1.5*iqr
# outliers.sf<- subset(complete.sf, complete.sf$lnrentsqm > (Q[1] - 2*iqr) & complete.sf$lnrentsqm < (Q[2]+2*iqr))

# #Lasso regression selected variables without outliers
# formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2 + Latitude
# LR_model = lm(formula, lamdbda=grid, data=outliers.sf)
# sm = summary(LR_model)
# mean(sm$residuals^2)


#Plotting the coordinates
plot(complete.sf$geometry)

#Plot the different rentals by collection moment
tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "collected", size = 0.05, alpha=0.7, palette='plasma', n=2, title='Date of collection', labels=c('2019', '2021'))

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

#Plotting train stations in Utrecht
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

#Extrating the coordinates from the dataste
coordinates.sf = read_sf("rentals_complete.shp", stringsAsFactors = T)
utrecht.coords = st_coordinates(coordinates.sf)

#Coords are in meters
utrecht.coords

#Defining adjacency W
#Location is near if less than 500 meters
utrecht.nb = dnearneigh(utrecht.coords, 0, 50, longlat = FALSE) #defining as adjacent all obs closer than 500

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

#Estimating Spatial Error Model with adjacency W
# price.err_adj = errorsarlm(lnRent ~ Size + restaurant + Furnished_ +
#                              Furnishe_1 + restaurant + Train_dist + woz + criminal_v +
#                              collected, data = complete.sf, listw= utrecht.Wadj, zero.policy = TRUE)
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2
price.err_adj = errorsarlm(formula = formula, listw=utrecht.Wadj, zero.policy = TRUE, data=complete.sf)
sm = summary(price.err_adj)
mean(sm$residuals^2)
sm$AIC_lm.model

#Estimating Spatial Error Model with distance-based W 1/d
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2
price.err_adj = errorsarlm(formula = formula, listw=utrecht.Wids, zero.policy = TRUE, data=complete.sf)
sm = summary(price.err_adj)
mean(sm$residuals^2)
sm$AIC_lm.model

#Estimating Spatial Error Model with distance-based W 1/d^2
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2
price.err_adj = errorsarlm(formula = formula, listw=utrecht.Wids2, zero.policy = TRUE, data=complete.sf)
sm = summary(price.err_adj)
mean(sm$residuals^2)
sm$AIC_lm.model
#Estimating Spatial Error Model with distance-based W exp(-x)
summary(price.err_expdis)
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2
price.err_adj = errorsarlm(formula = formula, listw=utrecht.Wexpdis, zero.policy = TRUE, data=complete.sf)
sm = summary(price.err_adj)
mean(sm$residuals^2)
sm$AIC_lm.model

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

#####RandomForest####

#Performing Random Forest Regression
trctrl <- trainControl(method = "none")

#Train random forest model with variables selected by linear regression
rentals = na.omit(complete.sf)
rfregFit <- train(lnrentsqm ~ ., data = complete.sf, 
                  method = "ranger",
                  trControl=trctrl,
                    # calculate importance
                  importance="permutation", 
                  tuneGrid = data.frame(mtry=8, min.node.size = 5, splitrule="variance")
)

#Train random forest model with full variables and log(Rent)
rfregFit <- train(lnrentsqm ~ ., 
                  data = complete.sf, 
                  method = "ranger",
                  trControl=trctrl,
                  # calculate importance
                  importance="permutation", 
                  tuneGrid = data.frame(mtry=12, min.node.size = 5, splitrule="variance")
)
rfregFit$finalModel$r.squared
#Plot the Observed vs OOB predicted values from the model
plot(complete.sf$lnrentsqm,rfregFit$finalModel$predictions,
     pch=19,xlab="observed log(Rent) per sqm",
     asp=1,
     cex=0.3,
     xlim=c(1,4.5),
     ylim=c(2,4.5),
     ylab="OOB predicted log(Rent) per sqm")
mtext(paste("R-squared",
            format(rfregFit$finalModel$r.squared,digits=2)))
abline(0,1)

#Plot the residuals
plot(complete.sf$lnRent,(rfregFit$finalModel$predictions-complete.sf$lnRent),
     pch=18,ylab="residuals (predicted-observed)",
     xlab="observed Rent",col="blue4",
     asp=1,
     )
abline(h=0,col="red4",lty=2)

#R-squared of final model
rfregFit$finalModel$r.squared

res=resid(rfregFit)
pre=predict(rfregFit)
plot(res~pre)

#Plot the residuals on a map
data = as.data.frame(as.numeric(rfregFit$finalModel$predictions-complete.sf$lnrentsqm))
data$residuals = as.numeric(sm$residuals)
complete.sf$stand.RF_residuals = as.numeric(scale(complete.sf$RF_residuals)) #<-plotting different prices in different colors
tm_shape(complete.sf) + tm_dots(col = "stand.RF_residuals", size = 0.05)

Boston = complete.sf
set.seed(1)
train = sample (1:nrow(Boston), nrow(Boston)/2)

#########################################################bagging########################################
bag.boston= randomForest(lnrentsqm~.,data=Boston , subset=train ,
                         mtry=13,importance =TRUE) ##### m = p=Baging since mtry=13
bag.boston

###############test set perfomace basedon the model above
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
boston.test = Boston[-train,"lnrentsqm"]

boston.test = as.data.frame(boston.test[,1])
boston.test = boston.test[,1]
plot(yhat.bag , boston.test)
abline (0,1)

mean((yhat.bag-boston.test)^2)###calculate MSE

###change the number of trees grown by randomForest()
bag.boston= randomForest(lnrentsqm~.,data=Boston , subset=train ,
                         mtry=13,ntree=25)#ntree=25
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
mean((yhat.bag -boston.test)^2)

##############################################Random Forest###############################################
##By default, randomForest() uses p/3 variables for regression trees & ???p variables for classification trees

set.seed(1)
rf.boston= randomForest(lnrentsqm~.,data=Boston , subset=train ,
                        mtry=8, importance =TRUE)###mytry=6
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)

###########important variables#######
importance (rf.boston) #### %IncMSE: mean decrease of accuracy in predictions on the out of bag samples
#                                    when a given variable is excluded from the model
#                          IncNodePurity: total decrease in node impurity that results from splits over that
#                                          variable, averaged over all tree (in regresion treesbased on training RSS)

varImpPlot(rf.boston)##plot
plot(rf.boston$mtry)
yhat.rf

tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "residuals", size = 0.05, alpha=1, palette='PuOr', n=10)
##########################################Boostig#############################################################################3

library (gbm)

set.seed(1)
boost.boston1=gbm( lnrentsqm~.,data=Boston[train ,], distribution="gaussian",
                   n.trees=5000, interaction.depth=4)###distribution="gaussian" since this is a regression problem
summary(boost.boston1)

#####partial dependence plots based on marginal effect of the selected variables on the response after
#                                                                integrating out the other variables
par(mfrow=c(1,2))
plot(rf.boston, i='liveabilit')
plot(boost.boston1,i="liveabilit")
plot(boost.boston1,i="migratio_2")

######use the boosted model to predict medv on the test set
yhat.boost1=predict (boost.boston1 ,newdata =Boston[-train ,],## default shrinkage parameter ??= 0.001 
                     n.trees=5000)
mean((yhat.boost1 - boston.test)^2)

####change ??
boost.boston2=gbm( lnrentsqm~.,data=Boston[train ,], distribution=
                     "gaussian",n.trees =1000, interaction.depth =4, shrinkage =0.1,
                   verbose=F)
yhat.boost2=predict (boost.boston2 ,newdata =Boston[-train ,],
                     n.trees=1000)
mean((yhat.boost2 - boston.test)^2)
summary(boost.boston2)


