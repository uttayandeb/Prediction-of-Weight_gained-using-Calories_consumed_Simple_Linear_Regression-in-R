###understanding and reading the data

calories_consumed_wg<-read.csv(file.choose())
View(calories_consumed_wg)# 14 Observations of 2 variables
names(calories_consumed_wg)# "Weight.gained..grams." "Calories.Consumed"

# Scatter Diagram ie Plot(x,y) continuous variables
plot(calories_consumed_wg$Calories.Consumed,calories_consumed_wg$Weight.gained..grams.)

attach(calories_consumed_wg)
?attach#will add ur dataset to the R search path

# Other Exploratory data analysis and Plots

boxplot(calories_consumed_wg)

hist(calories_consumed_wg$Calories.Consumed)#histogram calories consumed
hist(calories_consumed_wg$Weight.gained..grams.)#histrogram of weightgained


summary(calories_consumed_wg)


## Correlation coefficient value for Calories Consumes and Weight Gained ##
cc<- calories_consumed_wg$Calories.Consumed
View(cc)
wg <- calories_consumed_wg$Weight.gained..grams.
View(wg)
#cor(x,y)
cor(cc,wg)#### [1] 0.946991 is the correlation value as nearly equal to 1 is a good correlation

### as the correlation is strong we dont need any transformation of i/p features
##so by approachinf by a simple regression formula we can proceed

# Simple model without using any transformation
reg<-lm(cc~wg)
?lm #lm is used to fit linear models. It can be used to carry out regression
summary(reg)

pred<-predict(reg)
View(pred)
reg$residuals
mean(reg$residuals)#[1] -1.320075e-14

sqrt(sum(reg$residuals^2)/nrow(calories_consumed_wg))  # root mean square eoor(RMSE)
#[1] 232.8335
sqrt(mean(reg$residuals^2))
#[1] 232.8335


### Probability value should be less than 0.05(4.54e-05) and p value is 2.856e-07 which is less overall
## The multiple R-Squared Value is 0.8968 which is greater 0.8 so it is fine

# confidence interval
confint(reg,level = 0.95)# with confidence level  0.95
# 2.5 % is the lowwer range and 97.5 %is the upper range
#lower cc= 1358.141455+ 1.678994*(wg)
# upper CC= 1796.259949+2.589852(wg)
# The above will give 2 equations one lower and one upper
# 1 to caliculate the lower range and other for the upper range

# and another equatons for the true value
# cc=1577.201+2.134(wg)


# Function to Predict the above model 
predict(reg,interval="predict")#prediction interval
#predicted value(fit value )-actual value(cc)=error

#for getting the confidence interval value 
predict(reg,iinterval = "confident")



# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = calories_consumed_wg, aes(x =Weight.gained..grams. , y = Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =calories_consumed_wg, aes(x=wg, y=pred))


# A simple ggplot code for directly showing the line

ggplot(calories_consumed_wg,aes(Weight.gained..grams.,Calories.Consumed))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')



#now the R square value we get id 0.8968,which is good but for better R square value,
#we can do transformation of variables

###*********** Logrithamic Model(transform) ****************###

#x = log(Weight.gained..grams.); y = Calories.Consumed
#or x=log(wg);y=cc

plot(log(Weight.gained..grams.), Calories.Consumed)
#plot(log(wg),cc)#*******both will give the same plot or visulization

cor(log(wg), cc)#[1] 0.9368037,the R squared value is increased now 

reg_log <- lm(cc ~ log(wg))   # lm(Y ~ X)
View(reg_log)

summary(reg_log)
predict(reg_log)


reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calories_consumed_wg))### RMSE(root mean square error)
# [1] 253.558
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
predict(reg_log,interval = "predict")



###******** Exponential Model or Transformation ************###

plot(wg,log(cc))

cor(wg,log(cc))#[1] 0.8987253

reg_exp<-lm(log(cc)~wg)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))#[1] 0.1336239

pred_2 <- predict(reg_exp)
View(pred_2)
cc_1 <- exp(pred_2)
View(cc_1)

#error = calories_consumed_wg$cc - cc_1
#error

sqrt(sum(error^2)/nrow(calories_consumed_wg))  #####RMSE####
#[1] 0

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")



######**** Polynomial model with 2 degree (quadratic model)*******#####

plot(wg,cc)
plot(wg*wg, cc)

cor(wg*wg, cc)#[1] 0.8962742

plot(wg*wg, log(cc))#[1] 0.8987253

cor(wg, log(cc))#[1] 0.8987253
cor(wg*wg, log(cc))## [1] 0.8249636


# lm(Y ~ X + I(X*X) +...+ I(X*X*X...v(w))

reg2degree <- lm(log(cc) ~ wg + I(wg*wg))
summary(reg2degree)

logpol <- predict(reg2degree)
View(logpol)
expy <- exp(logpol)
?exp

err = calories_consumed_wg$cc- expy

sqrt(sum(err^2)/nrow(calories_consumed_wg))#### RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")


# visualization
ggplot(data = calories_consumed_wg, aes(x = wg+ I(wg^2), y = log(cc))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed_wg, aes(x=wg+I(wg^2), y=logpol))



#####****  Polynomial model with 3 degree ****####
####***** or cubic model****#####

#reg3degree<-lm(log(cc)~wg + I(wg*wg) + I(wg*wg*wg))

#summary(reg3degree)
#logpol3 <- predict(reg3degree)
#expy3 <- exp(logpol3)
#View(expy3)


reg3degree <- lm(wg~cc+I(cc^2)+I(cc^3),data=calories_consumed_wg)
View(reg3degree)
summary(reg3degree) # 0.9811

logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
View(expy3)

confint(reg3degree,level=0.95)
predict(reg3degree,interval="confidence")

###################### visualization ##########
ggplot(data = calories_consumed_wg, aes(x = wg + I(wg^2) + I(wg^3), y = cc)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed_wg, aes(x=wg+I(wg^2)+I(wg^3), y=expy3))


plot(reg3degree)

hist(residuals(reg3degree)) # close to normal distribution
### therefore cubic model gives the best R squared value  0.9811


