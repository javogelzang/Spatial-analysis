#Load libraries
library(Hmisc)
library(xlsx)
library(dplyr)

#Load data set
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Assignment Linear Regression/')
income <- read.xlsx('income.xlsx',sheetName = 'Dataset')

#Explore data set
ggplot(data=income, aes(Income))+
  geom_boxplot()+
  geom_density()

#Setup of basic model
model = lm(Income ~ ., data=income)
summary(model)
plot(model)
#Rename variables
income$mortgage <- income$Dwelling_type^1
income$outright <- income$Dwelling_type^2
income$rental <- income$Dwelling_type^3
income$free <- income$Dwelling_type^4
income$rental <- income$Dwelling_type^5
income$illegal <- income$Dwelling_type^6
income$other <- income$Dwelling_type^7

summary_stats <- income %>% 
  group_by(Cars) %>% 
  summarise(mean_by_group = mean(Income))
summary_stats

ggplot(summary_stats, aes(Cars, log(mean_by_group))) +
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
