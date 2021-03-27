# GDAtools 1.6

## Bug fixes

* `assoc.twocat()` : bug fix with warning
* `ggcloud_variables()` : bug fix when `prop` not NULL.
* `pem()` : bug fix with NA values
* `translate.logit()` : results for multinomial models were instable

## New functions

* `phi.table()` : computes phi coefficient for every cells of a contingency table
* `assoc.twocont()` : measures the association between two continuous variables with Pearson, Spearman and Kendall correlations and a permutation test.
* `assoc.yx()` : computes bivariate association measures between a response and predictor variables
* `desc_as_reg()` : computes bivariate association measures between a response and predictor variables, producing a summary looking like a regression one.
  
## Changes in existing functions:

* `wtable()` : can now compute percentages (`prop.wtable()` is removed)
* `assoc.twocat()` : Cramer's V instead of V-squared, permutation p-values, Pearson residuals, percentage of maximum deviation from independence, summary data frame
* `assoc.twocat()` : better handling of NAs
* `assoc.twocat()` : faster computation
* `assoc.catcont()` : permutation p-values
* `ggcloud_variables()` : improved color management
* `pem()` : one can choose to sort rows and columns or not
* weights are now allowed in functions `phi.table()`, `pem()`, `assoc.twocat()`, `assoc.twocont()`, `assoc.catcont()` and `assoc.yx()`



# GDAtools 1.5

## New functions

* `assoc.twocat()`: measures the association between two categorical variables
* `assoc.catcont()`: measures the association between a categorical variable and a continuous variable
* `catdesc()`: measures the association between a categorical variable and some continuous and/or categorical variables
* `condesc()`: measures the association between a continuous variable and some continuous and/or categorical variables}
* `ggcloud_indiv()`: cloud of individuals using ggplot
* `ggcloud_variables()`: cloud of variables using ggplot
* `ggadd_supvar()`: adds a supplementary variable to a cloud of variables using ggplot
* `ggadd_interaction()`: adds the interaction between two variables to a cloud of variables using ggplot
* `ggadd_ellipses()`: adds concentration ellipses to a cloud of individuals using ggplot

## Changes in existing functions

* `conc.ellipses()`: additional options
 


# GDAtools 1.4

## New functions
  - `translate.logit()`: translates logit models coefficients into percentages
  - `tabcontrib()`: displays the categories contributing most to MCA dimensions

## Changes in existing functions
* `varsup()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `textvarsup()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `conc.ellipse()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `plot.multiMCA()`: `threshold` argument, aimed at selecting the categories most associated to axes
* `plot.stMCA()`: `threshold` argument, aimed at selecting the categories most associated to axes



# GDAtools 1.3

## Changes in existing functions
  
* `dimdesc.MCA()`: now uses weights

## Bug fixes

* `dimdesc.MCA()`: problem of compatibility next to a FactoMineR update



# GDAtools 1.2

## New functions

* `dimvtest()`: computes test-values for supplementary variables

## Changes in existing functions

* `dimeta2()`: now allows `stMCA` objects



# GDAtools 1.1

## New functions

* `wtable()`: works as `table()` but allows weights and shows NAs as default
* `prop.wtable()`: works as `prop.table()` but allows weights and shows NAs as default

## Changes in existing functions

* `multiMCA()`: RV computation is now an option, with FALSE as default, which makes the function execute faster

## Bug fixes

* `textvarsup()`: there was an error with the supplementary variable labels when `resmca` was of class `csMCA`.

## Error fixes

* `textvarsup()`: plots supplementary variables on the cloud of categories (and not the cloud of individuals as it was mentioned in help).