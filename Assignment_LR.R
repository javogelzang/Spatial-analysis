#Load libraries
library(Hmisc)
library(xlsx)

#Load data set
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Assignment Linear Regression/')
income <- read.xlsx('income.xlsx',sheetName = 'Dataset')

#Explore data set
Hmisc::describe(income)
