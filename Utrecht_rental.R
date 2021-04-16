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
library (gbm)

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

#Check for multicollinearity
correlation = cor(complete.sf)
View(correlation)
which(correlation>0.5 & correlation<1)

#####LinearRegression####
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

#Formula based on lasso regression selected variables
formula = lnrentsqm ~ size + furnished + collected + park_dist + woz_waarde + traffic + migratio_1 + migratio_2 + Latitude
LR_model = lm(formula, data=complete.sf)
sm = summary(LR_model)
mean(sm$residuals^2)
sm$r.squared

#Plot observed vs. predicted
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
data = as.numeric(complete.sf$lnrentsqm - rfregFit$finalModel$predictions)
complete.sf$rf = data

#Plot the venues
venues_plot.sf = read_sf("venues_all.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(venues_plot.sf) + tm_dots(col = "Venue2", size = 0.05, alpha=1, title='Venue type', palette='Dark2') + tm_scale_bar()

#Plot the different rentals by collection moment
tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "collected", size = 0.05, alpha=0.7, palette='plasma', n=2, title='Date of collection', labels=c('2019', '2021'))

#Plotting the rent data together with maps 
tmap_mode("view")
tm_shape(complete.sf) + tm_dots(col = "rentsqm", size = 0.05, alpha=0.8, style='pretty', n=3, palette='inferno', title='Rent per sqm (€)')

#Plot the residuals
residual_plot.sf = read_sf("residual_rf.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(residual_plot.sf) + tm_dots(col = "rf", size = 0.05, alpha=1, title='residuals', palette='RdYlBu') + tm_scale_bar()

residual_plot.sf = read_sf("residual_plot.shp", stringsAsFactors = T)
tmap_mode("view")
tm_shape(residual_plot.sf) + tm_dots(col = "LinearRegr", size = 0.05, alpha=1, title='residuals', palette='RdYlBu') + tm_scale_bar()
