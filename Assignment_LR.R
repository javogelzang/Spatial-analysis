#Load libraries
library(Hmisc)
library(xlsx)
library(dplyr)
library(car)

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
#Removal of outliers
income_wi = subset(income, Bicycles != 98)
income_wi = subset(income_wi, Bicycles != 99)
income_wi = subset(income_wi, Children.Bicycles != 98)
income_wi = subset(income_wi, Children.Bicycles != 99)
nrow(income_wi)
summary(lm(Income ~ Bicycles, data=income))
summary(lm(Income ~ Bicycles, data=income_wi))

summary(lm(Income ~ Children.Bicycles, data=income))
summary(lm(Income ~ Children.Bicycles, data=income_wi))
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
income2 = subset(income, Income != 0)
income2$Income = log(income2$Income)
model = lm(Income ~ ., data=income2)
summary(model)
plot(model)

res=resid(model)
pre=predict(model)
plot(res~pre)

hist(res)
x <- -4:4
lines(x, 99*0.7*dnorm(x,0,sd(res)),col=2)

cookd=cooks.distance(model)
which(cookd>1)
 
income2 = subset(income, Income != 0)
model = lm(Income ~ ., data=income2)
summary(model)
plot(model)

#Check if explanatory variables are highly correlated with variance inflation factor
VIF1=VIF(lm(Dwelling_type ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF2=vif(lm(Cars ~ Dwelling_type + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF3=vif(lm(Bicycles ~ Cars + Dwelling_type + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF4=vif(lm(Children.Bicycles ~ Cars + Bicycles + Dwelling_type + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF5=vif(lm(Males ~ Cars + Bicycles + Children.Bicycles + Dwelling_type + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF6=vif(lm(Females ~ Cars + Bicycles + Children.Bicycles + Males + Dwelling_type +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF8=vif(lm(ED_Middle ~ Cars + Bicycles + Children.Bicycles + Males + Females + 
              Dwelling_type + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF9=vif(lm(ED_Tech_Voc ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Middle + Dwelling_type + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF10=vif(lm(ED_University ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Middle + ED_Tech_Voc + Dwelling_type + ACT_Working  + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF12=vif(lm(ACT_Students ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + Dwelling_type +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF13=vif(lm(ACT_Pensioner ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Students +
              Dwelling_type + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF14=vif(lm(ACT_House ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Students +
              ACT_Pensioner + Dwelling_type + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF15=vif(lm(ACT_Jobless ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Students +
              ACT_Pensioner + ACT_House + Dwelling_type + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF16=vif(lm(ACT_Other ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + Dwelling_type + SCHDL_Parttime +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF18=vif(lm(SCHDL_Parttime ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + Dwelling_type +
              SCHDL_Occasional + SCHDL_WE,data=income))
VIF19=vif(lm(SCHDL_Occasional ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              Dwelling_type + SCHDL_WE,data=income))
VIF20=vif(lm(SCHDL_WE ~ Cars + Bicycles + Children.Bicycles + Males + Females +
              ED_Primary + ED_Middle + ED_Tech_Voc + ED_University + ACT_Working + ACT_Students +
              ACT_Pensioner + ACT_House + ACT_Jobless + ACT_Other + SCHDL_Parttime +
              SCHDL_Occasional + Dwelling_type,data=income))
VIF1
VIF2
VIF3
VIF4
VIF5
VIF6
VIF7
VIF8
VIF9
VIF10
VIF12
VIF13
VIF14
VIF15
VIF16
VIF18
VIF19
VIF20

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

table(income_wi$Cars)
summary(lm(Income ~ Cars, data=income))
income$no_car = (income$Cars==0)+0
income$one_car = (income$Cars=1)+1

