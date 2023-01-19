# install
install.packages("mice")

# load the package
library("mice")

#### minimal example ####

# show the missing data pattern
md.pattern(nhanes)

# multiple impute the missing values
imp <- mice(nhanes, maxit = 2, m = 2, seed = 1)
#> 
#>  iter imp variable
#>   1   1  bmi  hyp  chl
#>   1   2  bmi  hyp  chl
#>   2   1  bmi  hyp  chl
#>   2   2  bmi  hyp  chl

# inspect quality of imputations
stripplot(imp, chl, pch = 19, xlab = "Imputation number")

# In general, we would like the imputations to be plausible, i.e., values that 
# could have been observed if they had not been missing.

# fit complete-data model
fit <- with(imp, lm(chl ~ age + bmi))

# pool and summarize the results
summary(pool(fit))
#>          term estimate std.error statistic    df p.value
#> 1 (Intercept)     9.08     73.09     0.124  4.50  0.9065
#> 2         age    35.23     17.46     2.017  1.36  0.2377
#> 3         bmi     4.69      1.94     2.417 15.25  0.0286
# The complete-data is fit to each imputed dataset, and the results are 
# combined to arrive at estimates that properly account for the missing data.


