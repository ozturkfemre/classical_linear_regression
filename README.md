# Classical Linear Regression Project

#### OLS

##### Fatih Emre Ozturk

## Aim of the Project

In this study, I tried to estimate the wages of workers in the Mid-Atlantic region using a classical linear regression model. The data set is not very suitable for linear regression. Since most of the regression analysis studies on the internet are done with data sets that are quite suitable for regression, I hope that this study can give an idea about how to act when faced with a difficult data set to work with. All coding is done in R. Both [Turkish](https://github.com/ozturkfemre/classical_linear_regression/blob/main/TR/TR_report.Rmd) and [English](https://github.com/ozturkfemre/classical_linear_regression/blob/main/ENG/ENG_report.Rmd) .Rmd and .R files are available..

## Dataset Information

The [dataset](https://www.re3data.org/repository/r3d100011860) is taken from the `ISLR` package. The dataset contains information about wage and other information for a group of 3000 male workers in the Mid-Atlantic Region. There are 11 variables in the dataset. While 9 of the 11 variables are independent variables, there are 2 dependent variables. One is the salary of workers and the other is log(wage). The information of all variables can be found in the table below.

| Variable   | Meaning                                                                                                                                  |
|-------------|-----------------------------------------------------------|
| year       | Year that wage information was recorded                                                                                                  |
| age        | Age of worker                                                                                                                            |
| maritl     | A factor with levels `1. Never Married` `2. Married` `3. Widowed` `4. Divorced` and `5. Separated` indicating marital status             |
| race       | A factor with levels `1. White` `2. Black` `3. Asian` and `4. Other` indicating race                                                     |
| education  | A factor with levels `1. < HS Grad` `2. HS Grad` `3. Some College` `4. College Grad` and `5. Advanced Degree` indicating education level |
| region     | Region of the country (mid-atlantic only)                                                                                                |
| jobclass   | A factor with levels `1. Industrial` and `2. Information` indicating type of job                                                         |
| health     | A factor with levels `1. <=Good` and `2. >=Very Good` indicating health level of worker                                                  |
| health_ins | A factor with levels `1. Yes` and `2. No` indicating whether worker has health insurance                                                 |
| logwage    | Log of workers wage                                                                                                                      |
| wage       | Workers raw wage                                                                                                                         |

## Study

The study section consists of eight parts:

1.  Descriptive Statistics

2.  The First Model(contains all independent variables)

3.  Variable Selection

4.  Assumption Check

5.  Regression Diagnosis

6.  Model Development

7.  Assumption Check

8.  Conclusion

### Descriptive Statistics

In the descriptive statistics section, descriptive statistics of each variable were analysed. These analyses were made separately for categorical and numerical variables.

#### Data Visualization

In this section, boxplots of numerical variables are analysed first. Plots are interpreted in detail. Then, the boxplots of the dependent variables according to the levels of the categorical variables are also drawn. In this way, it was examined whethere there is a linear relationship between ordinal variables and dependent variables. Then, pairplot was drawn in order to have an idea about the relationship between numerical variables and the dependent variable. In addition, correlation analysis was also performed. Finally, the distributions of the two variables that are likely to be the dependent variable were analysed and it was decided which variable to choose.

### The First Model

In this section, the full model was created with all variables in the data set. Model Validity was checked with F hypothesis test. Finally, Coefficient analysis was performed.

### Variable Selection

In this section, the best subset selection method is used to decide which variables are included in the best three models. Best subset selection involves fitting all possible subsets of the predictor variables and selecting the best model based on some criteria, such as the Akaike information criterion (AIC), the Bayesian information criterion (BIC) or CP. Then, the best of the three model options was selected with performance metrics such as RMSE and MAE.

### Assumption Check

The following assumptions were checked in this section:

1.  **Multicollinearity** which can be stated as the independent variables should not be highly correlated with each other. This checked by computing VIF.

2.  **Normality** which can be stated as the errors should be normally distributed. This checked by examining a histogram of the residuals, a normal probability plot of the residuals, and hypothesis tests.

3.  **Homoscedasticity** which can be stated as the variance of the errors should be constant across all levels of the independent variable(s). This checked by plotting the residuals against the predicted values and BP hypothesis test.

### Regression Diagnosis

In this section, the performance of the model was checked. Some improvements to be made in the model development section were decided by applying the following procedures:

1.  **Outliers** which are the observation that are far from the other observations in the dataset. They identified by examining scatterplot of the standardized residuals against the fitted values.

2.  **Leverage** which is a measure of how far an observation is from the mean of the independent variables. Since an observation with high leverage may have a large influence on the model, they identified by examining scatterplot of the standardized residuals against the fitted values.

3.  **Cook's Distance** which is a measure of the influence of each observation on the regression model. Since large values of Cook's distance indicate that the corresponding observation has a large influence on the model, it needs to be examined.

### Model Development

In this section, efforts were made to ensure that the model meets the assumptions by applying the following steps:

1.  **Data manipulation:** not all levels of some categorical variables were significant. Therefore, changes were made to the data to exclude the insignificant variables from the model.

2.  **Normalization:** non-normal distribution of the dependent variable may cause errors to be non-normally distributed. For this reason, some transformations were applied to the dependent variable and normal distribution was attempted.

3.  **Outliers:** Outliers were removed from the data set and the model was improved.

### Assumpiton Check

In this section, it is checked whether the model reconstructed after the changes made in the data set meets the assumptions. Although normality and multicolinearity assumptions were met, homoscedasticity assumption was not met.

### Conclusion

As a result of all these analyses:

-   The assurance of normality of the dependent variable in the model increased the values of Rsquare and Adj.Rsquare, but it was still not possible to ensure the assumptions.

-   To ensure these assumptions, the observations that were detected as outliers based on standardized errors were removed from the data set and a new model was established. When hypothesis tests were applied in this model, it was noticed that the assumptions were still not met despite the approximation.

-   It was also noticed in the descriptive statistics that there was no linear relationship between the variables. The only relationship we could say was linear was between the dependent variable and education. Therefore, it may be more logical to try this data set with non-linear regression models.

-   Adding new independent variables that could have a linear relationship with the wage variable might also be logical.
