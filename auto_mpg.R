
#Calling labrary
install.packages("xlsx")
library(xlsx)
library(readr)
library(stats)

#Importing data
auto_mpg <- read_csv("auto_mpg.csv")
View(auto_mpg) 

summary(auto_mpg)

#Remove NA
auto_mpg1<-subset(auto_mpg, !is.na(auto_mpg$mpg))
auto_mpg1

auto_mpg3<-subset(auto_mpg1,!is.na(auto_mpg1$horsepower))
auto_mpg3

#Summary of auto_mpg3

summary(auto_mpg3)

#mpg          cylinders      displacement     horsepower        weight      acceleration  
#Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0   Min.   :1613   Min.   : 8.00  
#1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0   1st Qu.:2225   1st Qu.:13.78  
#Median :22.75   Median :4.000   Median :151.0   Median : 93.5   Median :2804   Median :15.50  
#Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5   Mean   :2978   Mean   :15.54  
#3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0   3rd Qu.:3615   3rd Qu.:17.02  
#Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0   Max.   :5140   Max.   :24.80  

#model_year        orogin          name          
#Min.   :70.00   Min.   :1.000   Length:392        
#1st Qu.:73.00   1st Qu.:1.000   Class :character  
#Median :76.00   Median :1.000   Mode  :character  
#Mean   :75.98   Mean   :1.577                     
#3rd Qu.:79.00   3rd Qu.:2.000                     
#Max.   :82.00   Max.   :3.000  


#Looking at varaible distributions in german credit:
auto_mpg3 <- as.data.frame(auto_mpg3)
for (i in 1:ncol(auto_mpg3)){
  (as.numeric(auto_mpg3[,i]))
}

#Graphic
scatter.smooth(x=auto_mpg3$mpg, y=auto_mpg3$weight, main="MPG ~ Weight")  # scatterplot

#BoxPlot – Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(auto_mpg3$mpg, main="MPG", sub=paste("Outlier rows: ", boxplot.stats(auto_mpg3$mpg)$out))  # box plot for 'mpg'
boxplot(auto_mpg3$weight, main="Weight", sub=paste("Outlier rows: ", boxplot.stats(auto_mpg3$weight)$out))  # box plot for 'weight'
#There are no outliers

#Density plot
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(auto_mpg3$mpg), main="Density Plot: MPG", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_mpg3$mpg), 2)))  # density plot for 'mpg'
polygon(density(auto_mpg3$mpg), col="red")
plot(density(auto_mpg3$weight), main="Density Plot: Weight", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(auto_mpg3$weight), 2)))  # density plot for 'weight'
polygon(density(auto_mpg3$weight), col="red")

#Correlation
cor(auto_mpg3$mpg, auto_mpg3$weight)  # calculate correlation between MPG and weight
#-0.8322442 --> Strong correlation, inverse 

#Linear Regression 
auto_mod<- lm(mpg~weight, data=auto_mpg3)
summary(auto_mod)

#Call:
#lm(formula = mpg ~ weight, data = auto_mpg3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.9736  -2.7556  -0.3358   2.1379  16.5194 

#Coefficients: EQUATION: WEIGHT = 46.22 - 0.007*MPG
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 46.216524   0.798673   57.87   <2e-16 ***
#  weight      -0.007647   0.000258  -29.64   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.333 on 390 degrees of freedom
#Multiple R-squared:  0.6926,	Adjusted R-squared:  0.6918 
#F-statistic: 878.8 on 1 and 390 DF,  p-value: < 2.2e-16

#######       Why not glm???    ###################

#equation: WEIGHT = 46.22 - 0.007*MPG

#Calculate t-value and p-value
modelSummary <- summary(auto_mod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["mpg", "Estimate"]  # get beta estimate for MPG***
std.error <- modelCoeffs["mpg", "Std. Error"]  # get std.error for MPG**
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(auto)-ncol(auto))  # calc p Value
f_statistic <- auto_mod$fstatistic[1]  # fstatistic
f <- summary(auto_mod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)


####################################################################

#Multiple Regression
auto_mod1<- lm(mpg~weight+cylinders+displacement+horsepower+acceleration, data=auto_mpg3)
summary(auto_mod1)

#Call:
#  lm(formula = mpg ~ weight + cylinders + displacement + horsepower + 
#       acceleration, data = auto_mpg3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.5816  -2.8618  -0.3404   2.2438  16.3416 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.626e+01  2.669e+00  17.331   <2e-16 ***
#  weight       -5.187e-03  8.167e-04  -6.351    6e-10 ***
#  cylinders    -3.979e-01  4.105e-01  -0.969   0.3330    
#displacement -8.313e-05  9.072e-03  -0.009   0.9927    
#horsepower   -4.526e-02  1.666e-02  -2.716   0.0069 ** 
# acceleration -2.910e-02  1.258e-01  -0.231   0.8171    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.247 on 386 degrees of freedom
#Multiple R-squared:  0.7077,	Adjusted R-squared:  0.7039 
#F-statistic: 186.9 on 5 and 386 DF,  p-value: < 2.2e-16

#Creating fuel colunm
auto_mpg3$fuel<-c()
auto_mpg3$fuel<-c(auto_mpg3$displacement/auto_mpg3$mpg)

#New regression considering Weight,Cylinders,Horsepower and Fuel.
auto_mod2<- lm(mpg~weight+cylinders+horsepower+fuel, data=auto_mpg3)
summary(auto_mod2)

#Call:
#  lm(formula = mpg ~ weight + cylinders + horsepower + fuel, data = auto_mpg3)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-11.070  -2.712  -0.640   2.203  16.311 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 39.6019233  1.6814486  23.552  < 2e-16 ***
# weight      -0.0041252  0.0006884  -5.993 4.73e-09 ***
# cylinders    0.1826952  0.3240933   0.564    0.573    
# horsepower  -0.0107576  0.0137816  -0.781    0.436    
# fuel        -0.3470550  0.0842648  -4.119 4.66e-05 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.152 on 387 degrees of freedom
#Multiple R-squared:  0.7199,	Adjusted R-squared:  0.717 
#F-statistic: 248.7 on 4 and 387 DF,  p-value: < 2.2e-16

#New regression considering Weight and Fuel.
auto_mod3<- lm(mpg~weight+fuel, data=auto_mpg3)
summary(auto_mod3)

#Call:
#  lm(formula = mpg ~ weight + fuel, data = auto_mpg3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.4194  -2.7063  -0.5772   2.2861  16.3806 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 39.4413915  1.3523349  29.165  < 2e-16 ***
#  weight      -0.0040525  0.0006414  -6.318 7.24e-10 ***
#  fuel        -0.3637405  0.0598965  -6.073 2.99e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.146 on 389 degrees of freedom
#Multiple R-squared:  0.7192,	Adjusted R-squared:  0.7178 
#F-statistic: 498.3 on 2 and 389 DF,  p-value: < 2.2e-16

#New regression considering all variables. **** Understand this!!!!
auto_mod4<- lm(mpg~weight+cylinders+horsepower+fuel+displacement+model_year+name+orogin, data=auto_mpg3)
summary(auto_mod4)

