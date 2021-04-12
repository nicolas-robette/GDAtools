# GDAtools 1.7

## New functions

* `ggadd_density()`: adds a density layer to the cloud of individuals for a category of a supplementary variable
* `ggadd_corr()`: adds a heatmap of under/over-representation of a supplementary variable to a cloud of individuals
* `ggassoc_crosstab()` : plots counts and associations of a crosstabulation, using ggplot
* `ggassoc_phiplot()` : bar plot of phi measures of association of a crosstabulation, using ggplot
* `ggassoc_boxplot()` : displays of boxplot and combines it with a violin plot, using ggplot
* `ggassoc_scatter()` : scatter plot with a smoothing line, using ggplot
* `dimdescr()` : replaces `dimdesc.MCA()`. It works with `condesc()` instead of FactoMineR::condes().

## Changes in existing functions

* `ggcloud_indiv()` : the density of points can be represented as an additional layer through contours or hexagon bins
* `catdesc()` and `condesc()` : new nperm and distrib options
* `assoc.twocont()`, `assoc.twocat()` and `assoc.catcont()` : nperm option is set to NULL by default
* `darma()` : nperm is set to 100 by default
* `ggcloud_variables()` and `ggcloud_indiv()` : a few changes in the theme (grids are removed, etc.)
* `ggcloud_indiv()` and `ggadd_ellipses()` : new size option

## Bug fixes

* `csMCA()`, `speMCA()` and `translate.logit()` : now work with tibbles
* `ggcloud_variables()` : now works when shapes=TRUE and there are many variables



# GDAtools 1.6

## New functions

* `phi.table()` : computes phi coefficient for every cells of a contingency table
* `assoc.twocont()` : measures the association between two continuous variables with Pearson, Spearman and Kendall correlations and a permutation test.
* `assoc.yx()` : computes bivariate association measures between a response and predictor variables
* `darma()` : computes bivariate association measures between a response and predictor variables, displaying results in a form looking like the summary of a regression model analysis.

## Bug fixes

* `assoc.twocat()` : bug fix with warning
* `ggcloud_variables()` : bug fix when `prop` not NULL.
* `pem()` : bug fix with NA values
* `translate.logit()` : results for multinomial models were instable
  
## Changes in existing functions

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
* `ggcloud_indiv()`: cloud of individuals, using ggplot
* `ggcloud_variables()`: cloud of variables, using ggplot
* `ggadd_supvar()`: adds a supplementary variable to a cloud of variables, using ggplot
* `ggadd_interaction()`: adds the interaction between two variables to a cloud of variables, using ggplot
* `ggadd_ellipses()`: adds concentration ellipses to a cloud of individuals, using ggplot

## Changes in existing functions

* `conc.ellipses()`: additional options
 


# GDAtools 1.4

## New functions

* `translate.logit()`: translates logit models coefficients into percentages
* `tabcontrib()`: displays the categories contributing most to MCA dimensions

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