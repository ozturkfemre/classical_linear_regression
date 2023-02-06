###################################
# Fatih Emre Öztürk               #                     
# Classical Linear Regression     #
###################################

#################
### Libraries ###
#################

library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(tidyverse)
library(moments)
library(bestNormalize)
library(magrittr)


###########
# Dataset #
###########

df <- Wage

dim(df)

# There are 3000 observations 11 variables in the dataset.


str(df)

# There are 2 integer, 7 factor, 2 numeric variables.


# What variables stand for:

# Year:	Year that wage information was recorded
# Age:	Age of worker
# Maritl: A factor with levels 1. Never Married 2. Married 3. Widowed 4. Divorced and 5. Separated indicating marital status
# Race: A factor with levels 1. White 2. Black 3. Asian and 4. Other indicating race
# Education: A factor with levels 1. < HS Grad 2. HS Grad 3. Some College 4. College Grad and 5. Advanced Degree indicating education level
# Region: Region of the country (mid-atlantic only)
# Jobclass: A factor with levels 1. Industrial and 2. Information indicating type of job
# Health: A factor with levels 1. <=Good and 2. >=Very Good indicating health level of worker
# Health_ins: A factor with levels 1. Yes and 2. No indicating whether worker has health insurance
# Logwage: Log of workers wage
# Wage: Workers raw wage


# Data was manually assembled by Steve Miller, of Inquidia Consulting (formerly Open BI). From the March 2011 Supplement to Current Population Survey data.

# https://www.re3data.org/repository/r3d100011860

###############################
### Test - Train Separation ###
################################

smp_size <- floor(0.75 * nrow(df)) 
set.seed(2021900444) 
train_ind <- sample(nrow(df), size = smp_size, replace = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]
test <- test[-6]
train <- train[-6]
##########################
# Descriptive Statistics #
##########################

summary(df)

# When the descriptive statistics of numerical variables are analyzed:
  
# The range of the "Year" variable is quite low (6), which is noticeable.
# The mean and median of the "Age" variable are quite close to each other.
# It is noticed in "logwage" varible that the mean and median values are quite close to each other and the skewness is so low that it can be considered as zero.
# The mean of the "Wage" variable is higher than the median. It is therefore considered that it might be skewed to the right. Additionally, considering the difference between the first quartile (85) and the minimum value (20), there may be outliers. A more accurate interpretation can be made with a boxplot analysis.

# When the descriptive statistics of categorical variables are analyzed:
  
# It is observed that there are many levels in the factor variable that shows the marital status. After the graphic analysis, a decision will be made on whether to reduce these levels.
# When the variable that contains racial information is examined, while the majority of the White race is noticeable, it is understood when looking at the variable that contains the regional information.
# When the graph that contains education information is analyzed, it is thought that "Some College" level may have been unclear or left half-done. After the graphic analysis, it is thought that changes can also be made in the levels of this variable.
# The fact that there is no information other than Middle Atlantic in the "Region" variable is noticed. The fact that there is no other level suggests that this variable will not create any differences. Therefore, it has no meaning to include this variable in the model.
# As a result of the analysis, the estimate will only be for workers living in the Middle Atlantic region. This is why this variable has been removed from the data set.
# It is noticed that the "Jobclas" variable has two levels and the observation values belonging to these levels are quite close to each other.
# In the "Health" variable, it is noticed that the level "Very Good" has more observations.
# When the "Health Insurance" variable is analyzed, it is noticed that a large majority of the workers have insurance.


### Graphical Analysis

par(mfrow = c(1,4))

boxplot(df$year)
boxplot(df$age)
boxplot(df$logwage)
boxplot(df$wage)


# When the box plots of numerical values were examined, it was noticed that there was one outlier in the Age variable and several outliers in the logwage and wage variables. 
# To make comments about the distributions of the two options, logwage and wage variables, that are available as dependent variables, histogram charts and normality tests will be conducted for these two variables.

## Logwage

dev.off()
fun <- dnorm(df$logwage, mean = mean(test$logwage), sd = sd(df$logwage))

hist(df$logwage, main = "Log(Wage)", ylab = "Density", xlab = "Log(Wage)",col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun) + 1))

lines(density(df$logwage), col = 9, lwd = 2)

skewness(df$logwage)

# The Logwage variable is shown to have two peaks in the histogram graph, and a skewness to the left is also observed. 
# Normality tests are intended to make a more informed interpretation. 

### Normality Tests

# All the tests have the same hypothesis, so the hypotheses are shown only once.

# Ho: Logwage variable is normally distributed.
# Ha: Logwage variable is not normally distributed.


### Anderson - Darling normallik testi

ad.test(df$logwage)
# The Anderson-Darling normality test has a P-value less than 0.05, so the hypothesis that the variable is normally distributed can be rejected. 



### Shapiro - Wilk normallik testi

shapiro.test(df$logwage)
# The Shapiro-Wilk normality test also has a P-value less than 0.05, so the hypothesis that the variable is normally distributed can be rejected.

# The hypothesis tests also confirm the outcome of the histogram. The Logwage variable is not normally distributed.


## Wage

dev.off()

fun <- dnorm(df$wage, mean = mean(df$wage), sd = sd(df$wage))

hist(df$wage, main = "Wage", ylab = "Density", xlab = "Wage",col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun))+0.001)

lines(density(df$wage), col = 9, lwd = 2)

# The histogram of the wage variable shows that it has two peaks and a right skewed. It may be normalized by removing the values over 250, which can be considered as outliers. 
# However, normality tests have been performed to make a more reliable interpretation.

### Normality Tests ###

# The hypothesis tests have the same hypotheses:
  
# Ho: The wage variable is normally distributed.
# Ha: The wage variable is not normally distributed.

### Anderson - Darling 

ad.test(df$wage)
# The Anderson-Darling normality test was performed using the ad.test function, and the result rejected the hypothesis of normality because the p-value is less than 0.05. 

### Shapiro - Wilk

shapiro.test(df$wage)
# The Shapiro-Wilk normality test was also performed, and the result rejected the hypothesis of normality because the p-value is less than 0.05.


# These hypothesis tests confirmed the result from the histogram, which showed that the wage variable is not normally distributed. 
# However, it is believed that normalization of the wage variable will be easier compared to logwage. 
# Therefore, the decision was made to continue with the wage variable. 
# The non-normality of the variable may prevent the errors from being normally distributed in later parts of the analysis. 
# The histogram shows that the reason for the non-normality is outliers, but whether they will be removed or not will be decided in the later sections, based on the analysis of standardized residuals and outliers.

# Graphical Analysis of Categorical Variables

indexes = sapply(df, is.factor)
indexes["wage"] = TRUE
indexes["region"] = FALSE
df[,indexes]%>%
  gather(-wage, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = wage, color = value)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")


# The relationship between categorical variables and the wage variable was observed in a graph and it was observed that as the level of education increases, there is an increase in salary.
# It is thought that it could be a good variable for the model. 
# The impact of Health, Health Insurance, Job Class and Race variables was observed to be very small. 
# The decision of whether to continue with these variables will be made in the variable selection section. 
# When examining the graph showing the marital status, it was noticed that the "Never Married," "Divorced," and "Separated" variables were not very different. 
# In fact, combining these three levels that mean "single" into one roof and reducing the levels of the variable to three "Single, Divorced, and Married" may be logical. This possibility will be considered in later sections.

pairs(df[c(1,2,9,10,11)])

# The scatterplot graphs, which reveal the relationship between the variables, were examined and no linear relationship was found. 
# Some variables were found to behave categorically despite being numerical.


# Correlation Analysis

cor(df[c(1,2,9,10)])

# In the correlation analysis, no correlation was found between the dependent variable wage and the year variable (0.05) even though the year variable is numerical. 
# The year variable stands out as a variable that acts categorically despite being numerical. 
# Using this variable categorically may cause a change. This possibility will be considered in later sections. 
# A positive correlation of 0.25 was found between the dependent variable wage and the independent variable age.

##########################
### First Linear Model ###
##########################

model1 <- lm(wage ~ year + age + maritl + race + education + jobclass + health + health_ins, data = train)

summary(model1)

# Model Validity F Hypothesis Test

# Ho: There is no linear relationship between the dependent and independent variables.
# Ha: There is a linear relationship between the dependent variable and at least one independent variable (Bi != 0)

# According to variance analysis, the F statistic of the model created was determined to be 70.22.
# Because the p-value of the F statistic is less than 0.05 level of significance, Ho is rejected.
# At least one coefficient is significant for the model.

# Coefficient Analysis

# Hypothesis Test for the Significance of Regression Coefficients

# Ho: The slope coefficient is not significantly different from zero.
# Ha: The slope coefficient is significantly different from zero.

# The hypothesis that the slope coefficient is not significantly different from zero cannot be rejected because the p-value of the following variables is less than 0.05; therefore, the variable(s) are significant:

# Year, age, maritl2. Married, all levels of education factor, jobclass, health and health insurance

# We have enough evidence to reject the hypothesis that the slope coefficient is not significantly different from zero because the p-value of the following variables is greater than 0.05; therefore, the variable(s) are not significant:

# Maritl3, maritl4, maritl5, race2, race3, race4

# It was expected that such a situation regarding marital status would be encountered during the graphical examination of categorical variables. These improvements will be made in the part of the model development."

###########
### VIF ###
###########

vif(model1)

# When the VIF values of the model are analyzed, no independent variable has a VIF value greater than 5, so there is no connection problem in the model.

### Variable Selection ###

##########################
### All Steps Possible ###
##########################

all.steps <- ols_step_all_possible(model1)
plot(all.steps)

# Instead of R-square, the adjusted R-square values have been analyzed due to the presence of multiple parameters and the comparison of models. 
# The models with the best results based on all metrics (R-square, Adj. R-square, Cp, AIC, SBIC, and SBC) are 255 (full model), 248, 247, 219, 163, 93, 37, and 1. 
# When compared with each other, there is no noticeable difference in adjusted R-square values. 
# It was realized that it is impossible to distinguish between the values when R-square, AIC, SBIC, and SBC criteria are examined. 

# To make a distinction, it was realized that the best option was to look at the Cp values. 
# When Cp values are examined, observations that have a Cp value smaller than the number of parameters (to prevent errors) were selected. 
# The model options were reduced to 255 (full model), 248, 247, and 219. 
# When the Cp values of these model options are thoroughly analyzed, model 219, which has a Cp value closest to the number of parameters, is selected as the best model. 
# Another reason for choosing this model option is the number of parameters. 
# Although the number of parameters in the other proposed model options is greater than that of model 219, there is no significant increase in Adj. R-square and R-square values. 
# This indicates that the addition of those independent variables does not have a positive effect on the explanatory power of the model. 
# The independent variables of the all steps possible selection model decided after possible selection are: 

all.steps$predictors[219]

# However, the 247th model was selected for comparison on the test set. 
# The independent variables of these two models are as follows for 247:

all.steps$predictors[247]
all.steps$predictors[248]


#######################################################################
### Determination of the best models based on performance criterion ###
#######################################################################

model219 <- lm(wage ~year+ age + maritl + education + health + health_ins, data=train)
summary(model219)


model247 <- lm(wage ~ year+age+ maritl+ education +jobclass+ health + health_ins, data=train)
summary(model247)

model248 <- lm(wage ~ year+ age + maritl + education + jobclass + health + health_ins, data=train)
summary(model248)


predictions1 <- predict(model219,test)
predictions2 <- predict(model247,test)
predictions3 <- predict(model248,test)

RMSE1 <- RMSE(predictions1, test$wage)
RMSE2 <- RMSE(predictions2, test$wage)
RMSE3 <- RMSE(predictions3, test$wage)

cbind(RMSE1,RMSE2, RMSE3)

# When the Root Mean Square Error values of the predictions made on the test data sets of models 219, 247 and 248 are analyzed, it is found that there is no significant difference between them. Model 248 was determined to have the lowest error.

mae1 <- mae(predictions1, test$wage)
mae2 <- mae(predictions2, test$wage)
mae3 <- mae(predictions3, test$wage)

cbind(mae1,mae2, mae3)

# When the Mean Absolute Error values of the predictions made on the test data sets of models 219, 247 and 248 are analyzed, it is found that there is no significant difference between them. Model 219 was determined to have the lowest error.

# The fact that the difference between the errors is not very significant, and the fact that there is a significant change in the errors and Rsquare and Adj.Rsquare values with the number of variables, model 219 was chosen as the best model.
# The final decision was to use the following model:

modelbest <-  lm(wage ~year+ age + maritl + education + health + health_ins, data=train)

#########################
### Assumptions Check ###
#########################

##############################
### Normality of Residuals ###
##############################

fun <- dnorm(modelbest$residuals, mean = mean(modelbest$residuals), sd = sd(modelbest$residuals))

hist(modelbest$residuals, ylab = "Density", xlab = "Residuals", col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun) + 0.01))


lines(density(modelbest$residuals), col = 9, lwd = 2)

# Upon examining the histogram graph of residuals, a clear right skewness is noticed. 
# This skewness was previously anticipated. The reason for this could be the non-normal distribution of the dependent variable. If the normal distribution of residuals is not achieved after removing outliers, leverage and cook's distance observations from the data set, changes will be made to the dependent variable.

# Hypothesis Testing

# Shapiro-Wilk Normality Test Hypotheses:

# Ho: Errors are normally distributed.
# Ha: Errors are not normally distributed.



shapiro.test(modelbest$residuals)

# Since the p-value is less than 0.05, there is sufficient evidence to reject the hypothesis that the variable is normally distributed with 95% confidence."

########################
### Homoscedasticity ###
########################

# BP Test Hypotheses:
  
# Ho: Errors have constant variance.
# Ha: Errors do not have constant variance.

bptest(wage ~year+ age + maritl + education + health + health_ins, data=train)

# Since the p-value is less than 0.05, the hypothesis that the errors have constant variance can be confidently rejected with 95% confidence.

par(mfrow = c(2,2))
plot(modelbest)

# When the graph between Residuals and Fitted Values is examined, no constriction is observed. It is seen that the errors do not have constant variance.
# Additionally, it is noteworthy that outliers are affecting the model. Removing outliers from the data set could be a logical move."

########################
### Outlier Analysis ###
########################

standardized.residuals<-
  modelbest$residuals/sqrt(var(modelbest$residuals))

dev.off()
plot(modelbest$fitted.values,standardized.residuals, xlab = "Predictions", ylab = "Standardized Residuals")
abline(h=0)
abline(h=-3)
abline(h=3)

# When the graph of the predicted values and standardized residuals is examined;



# 62 residuals with a standardized residual value less than -3 or greater than 3 have been detected.
# These 62 values are interpreted as outliers.
# The index numbers of the observations detected as outliers are as follows.
which(abs(standardized.residuals)>3)
length(which(abs(standardized.residuals)>3))

#########################
### Leverage Analysis ###
#########################

st.res <- modelbest$residuals/sd(modelbest$residuals) #modelin hatasını bulmak gerekir, degrees of freedom hatası yüzünden
plot(hatvalues(modelbest),st.res)
abline(h=c(-2,2),v=2*mean(hatvalues(modelbest)))

# When the graph between the hat values and standardized residuals is examined, there are no bad leverage points observed. 
which(hatvalues(modelbest)>2*mean(hatvalues(modelbest)))
length(which(hatvalues(modelbest)>2*mean(hatvalues(modelbest))))

# The number of good leverage points has been detected as 86.

######################
### Cooks Distance ###
######################

plot(train$wage,cooks.distance(modelbest))
abline(h=4/(length(train$wage)-6))

# When analyzing the graph of the Cook's Distance, it was determined that there were 101 effective observations. The observations that were marked as outliers in the graph were found to be effective observations. This means that the outliers have a significant effect on the slope of the model. It may be logical to make changes in this regard in the model improvement.

length(which(cooks.distance(modelbest) > 4/(length(train$wage)-6)))
# The index numbers of the observations that are considered effective according to Cook's Distance are as follows:
which(cooks.distance(modelbest) > 4/(length(train$wage)-6))

########################
## Model Development ###
########################

#**Maritl Variable**
  
#The analysis of the established models has shown that only the married level of the factor indicating marital status is significant. Therefore, the model development method will continue by reducing the levels of this factor to two, i.e. married 1 - 0.

train %<>%  mutate(maritl = ifelse(train$maritl == "2. Married", 1, 0)) 
train$maritl <- as.factor(train$maritl)
str(train)


# None of the changes made so far have helped the model meet its assumptions. The dependent variable not being normally distributed may be the reason for this. Therefore, trying to make the dependent variable have a normal distribution may be the first step in improving the model. The bestNormalize package will be used to try to ensure that Y has a normal distribution.


bestNormalize(test$wage)

# The "bestNormalize" package suggested the following transformations for normalizing the dependent variable: boxcox, yeo-johnson, arcsinh. In addition, the logarithmic transformation of the wage variable was observed to be skewed to the left in the descriptive statistics section. For this reason, in addition to the recommendations of the bestNormalize, the square root of the logarithmic wage will also be taken.

# **Box Cox**
  

boxwage <- boxcox(train$wage)
ad.test(boxwage$x.t)
shapiro.test(boxwage$x.t)

# After the Box Cox transformation, the normal distribution of Y could not be achieved.

# **Yeo-Johnson**
  

yeowage <- yeojohnson(train$wage)
ad.test(yeowage$x.t)
shapiro.test(yeowage$x.t)

# After the Yeo-Johnson transformation, the normal distribution of Y could not be achieved.

# **arcsinh**
  

arcwage <- arcsinh_x(train$wage)
ad.test(arcwage$x.t)
shapiro.test(arcwage$x.t)

# After the arcsinh transformation, the normal distribution of Y could not be achieved.

# **sqrt(log(x))**
  

donusuwage <- sqrt(log(train$wage))
ad.test(donusuwage)
shapiro.test(donusuwage)


# After the sqrt(log(x)) transformation, the normal distribution of Y could not be achieved.

# None of the recommended transformations managed to normalize Y. During the analysis of descriptive statistics, it was observed that the wage variable had a double peaked shape and had many outliers. Therefore, it is believed that removing outliers caused by the second peak will normalize by removing extreme values from the dependent variable using the IQR method.

# **Outlier Removal \| IQR**
  

quartiles <- quantile(train$wage, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(train$wage)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

train_no_outlier <- subset(train, train$wage > Lower & train$wage < Upper)

# After removing the outliers, normalization was still not achieved, but it has been approached somewhat. The histogram graph also supported this information.


ad.test(train_no_outlier$wage)
shapiro.test(train_no_outlier$wage)

# After removing the outliers, normalization was still not achieved, but it has been approached somewhat. The histogram graph also supported this information.



fun <- dnorm(train_no_outlier$wage, mean = mean(train_no_outlier$wage), sd = sd(train_no_outlier$wage))

hist(train_no_outlier$wage, col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun)))

lines(density(train_no_outlier$wage), col = 9, lwd = 2)

skewness(train_no_outlier$wage)

# Since skewness is higher than 0, it is right skewed.


bestNormalize(train_no_outlier$wage)

# The bestNormalize package has determined that the best method for normalizing the dependent variable after removing the outliers is the orderNorm normalization transformation. The Ordered Quantile (ORQ) normalization transformation is a ranking-based procedure where the values of a vector are matched to the percentile ranks and then matched to the same percentile ranks of a normal distribution. This essentially guarantees the transformation to lead to a smooth distribution without the presence of ties.


orderwage <- orderNorm(train_no_outlier$wage)

ad.test(orderwage$x.t)
shapiro.test(orderwage$x.t)


# The normality of the dependent variable has been established according to the results of the Anderson-Darling and Shapiro-Wilk normality tests. Therefore, it is desired to re-build the model with the dependent variable that has undergone this transformation.

trainnormal <- train_no_outlier
trainnormal$wage <- orderwage$x.t

modelout <- lm(wage ~ year + age + maritl + education + health + health_ins, data = trainnormal)
summary(modelout)

# After removing outliers and reducing the level of marital status factor and ensuring the normality of the dependent variable, upon examining the output of the newly created model, the new model was valid and all independent variables were seen to be significant.

# In trials outside the report, the square and then cube of the Age variable, which is a single numerical variable, were taken. It was observed that both of these modifications improved the model. Therefore, these changes will also be added to the model.


train_deneme <- trainnormal
train_deneme[11] <- (train_no_outlier$age)^2
train_deneme[12] <- (train_no_outlier$age)^3

modeldeneme <- lm(wage ~ year + age + V11 + V12 + maritl + education + health + health_ins, data= train_deneme)
summary(modeldeneme)

# After all these operations, all added variables are significant. In addition, a 7% increase in explanatory power was observed in the values of R-square and Adj. R-square. Further improvement in the model may be achieved after new outliers (if any) can be detected during new assumption checks.

### Assumption Check

fun <- dnorm(modelout$residuals, mean = mean(modelout$residuals), sd = sd(modelout$residuals))

hist(modelout$residuals,col="brown1",
     border="brown4", ylab = "Density", xlab = "Residuals", probability = T, ylim = c(0, max(fun)))


lines(density(modelout$residuals), col = 1, lwd = 2)

# When examining the histogram of the residuals, a graph that is close to normal is observed. Since the tails on both sides are long, checking the skewness value will provide a more reliable interpretation.

skewness(modeldeneme$residuals)

# The skewness value of the errors is -0.1. It has been observed that it is quite close to being normal. However, a definite conclusion is desired through hypothesis testing.

shapiro.test(modeldeneme$residuals)
ad.test(modeldeneme$residuals)
lillie.test(modeldeneme$residuals)

# According to the Lilliefors normality test, the p-value is greater than 0.05, so the hypothesis that the variable is normally distributed cannot be rejected with 95% confidence.

### Homoscedasticity


bptest(wage ~ year + age + V11 + V12 + maritl + education + health + health_ins, data= train_deneme)

# With a p-value less than 0.05, it can be rejected with 0.05 significance level that the residuals have constant variance.


par(mfrow = c(2,2))
plot(modelout)

# Upon examining the four-part model graph, the graph between residuals and estimated values shows that the residuals do not have constant variance. Also, the outliers in the residuals are seen to make a compression on the left side of the graph. It is thought that removing these outliers from the data set could improve the model and (maybe) meet the assumptions. Therefore, these procedures will be carried out.


standardized.residuals<-
  modelout$residuals/sqrt(var(modelout$residuals))
dev.off()
plot(modelout$fitted.values,standardized.residuals, xlab = "Predictions", ylab = "Standardized Residuals")
abline(h=0)
abline(h=-3)
abline(h=3)

# There are 12 observation that is outlier is observed. these observations will be removed from the dataset.

train.out.out <- train_deneme[-c(which(abs(standardized.residuals)>3)),]

modeloutout <- lm(wage ~year+ age + maritl + V11 + V12 + education + health + health_ins, data= train.out.out)

summary(modeloutout)

# After examining the output of the new model created by removing the observations identified as outliers based on the residuals, it has been determined that the model and all the independent variables in the model are significant. An increase of 0.03 in the Rsquare and Adj.Rsquare values has been observed. Tests to determine if the assumptions have been met are desired.

# **Normality**
  

shapiro.test(modeloutout$residuals)
ad.test(modeloutout$residuals)
lillie.test(modeloutout$residuals)

# The hypotheses for all normality tests are as follows:
  
# $H_o$: Residuals are normally distributed. 

# $H_a$: Residuals are not normally distributed.

# All normality tests except the Shapiro-Wilk have a p-value greater than 0.05, so the hypothesis that the variable is normally distributed cannot be rejected.

# **Homoscedasticity**
  
bptest(wage ~year+ age + V11 + V12 + maritl + education + health + health_ins, data= train.out.out)

# p-value is smaller than 0.05, so the hypothesis that errors have unrestricted variance can be rejected.

## Conclusion

# As a result of all these analyses:
  
#  -   The assurance of normality of the dependent variable in the model increased the values of Rsquare and Adj.Rsquare, but it was still not possible to ensure the assumptions.

# -   To ensure these assumptions, the observations that were detected as outliers based on standardized errors were removed from the data set and a new model was established. When hypothesis tests were applied in this model, it was noticed that the assumptions were still not met despite the approximation.

# -   It was also noticed in the descriptive statistics that there was no linear relationship between the variables. The only relationship we could say was linear was between the dependent variable and education. Therefore, it may be more logical to try this data set with non-linear regression models.

# -   Adding new independent variables that could have a linear relationship with the wage variable might also be logical.


