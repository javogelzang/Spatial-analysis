#Load libraries
library(Hmisc)
library(xlsx)
library(dplyr)

#Load data set
setwd('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Assignment Linear Regression/')
income <- read.xlsx('income.xlsx',sheetName = 'Dataset')
names(income)

#Explore data set
#Income
min(income$Income)
max(income$Income)
ggplot(data=income, aes(Income))+
  geom_boxplot()+
  geom_density()

#Persons
table(income$Persons) #Class size decreases after 4

#Dwelling type
table(income$Dwelling_type) #Categorical variable

#Cars
table(income$Cars) #Class size decreases after 3

#Bicycles 
table(income$Bicycles) #Outliers with numbers of bicycles of 98 and 99 
table(income$Children.Bicycles) #Outliers with numbers of bicycles of 98 and 99 

#Males and females
table(income$Males)
table(income$Females)

#ED
table(income$ED_Primary)
table(income$ED_Middle)
table(income$ED_Tech_Voc)
table(income$ED_University)

sum(table(income$ED_Primary))
sum(table(income$ED_Middle))
sum(table(income$ED_Tech_Voc))
sum(table(income$ED_University))

#ACT
table(income$ACT_Working)
table(income$ACT_Students)
table(income$ACT_Pensioner)
table(income$ACT_House)
table(income$ACT_Jobless)
table(income$ACT_Other)

sum(table(income$ACT_Working))
sum(table(income$ACT_Students))
sum(table(income$ACT_Pensioner))
sum(table(income$ACT_House))
sum(table(income$ACT_Jobless))
sum(table(income$ACT_Other))

#SCHDL
table(income$SCHDL_Fulltime)
table(income$SCHDL_Parttime)
table(income$SCHDL_Occasional)
table(income$SCHDL_WE)

sum(table(income$SCHDL_Fulltime))
sum(table(income$SCHDL_Parttime))
sum(table(income$SCHDL_Occasional))
sum(table(income$SCHDL_WE))

#Setup of basic model
model = lm(Income ~ ., data=income)
summary(model)
plot(model)

res=resid(model)
pre=predict(model)
plot(res~pre)

income2 = subset(income, Income != 0)
model = lm(LogIncome ~ ., data=income2)
summary(model)
plot(model)

#Check the distribution of the variables

table(income$Dwelling_type) # Categorical variable

hist(income$Income) #No gausian distribution
income$LogIncome = log(income$Income)

#Renaming dwelling type to dummie variables
income$own_mortgage <- (income$Dwelling_type==1)+0 
income$own_outright <- (income$Dwelling_type==2)+0 
income$rental <- (income$Dwelling_type==3)+0 
income$free_rent <- (income$Dwelling_type==4)+0 
income$rental_salary <- (income$Dwelling_type==5)+0 
income$illegal <- (income$Dwelling_type==6)+0 
income$other <- (income$Dwelling_type==7)+0 

summary(lm(Income ~  own_outright + rental + free_rent + rental_salary + illegal + other, data = income))

summary_stats <- income %>% 
  group_by(Cars) %>% 
  summarise(mean_by_group = mean(Income))
summary_stats

ggplot(summary_stats, aes(Cars, log(mean_by_group))) +
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
