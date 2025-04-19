# GDAtools 2.2

## New function

* `scaled.dev()` : scaled deviations between categories of a supplementary categorical variable
* `reshape_between()` : reshapes objects created with `bcMCA()` so that they can be used with other functions from the package
* `ggadd_partial()` : main and partial effects of a supplementary variable

## Changes in existing functions

* `ggaxis_variables()` : new "min.ctr" (to filter categories), "force" and "max.overlaps" arguments (suggestion from Gregoire Eveillard)
* `supvar()`, `supvars()`, `supind()`, `ggcloud_variables()`, `ggcloud_indiv()`, `contrib()`, `tabcontrib()`, `planecontrib()`, `scaled.dev()`, `homog.test()`, `ggaxis_variables()`, `ggadd_supvar()`, `ggadd_supvars()`, `ggadd_supind()`, `ggadd_ellipses()`, `ggadd_kellipses()`, `ggadd_interaction()`, `ggadd_density()`, `ggadd_corr()`, `ggadd_attractions()`, `ggeta2_variables()`, `ggsmoothed_supvar()`, `flip.mca()`, `barplot_contrib()`, `dimcontrib()`, `dimdescr()`, `dimtypicality()`, `dimeta2()`, `bootvalid_variables()`, `ggbootvalid_variables()`, `bootvalid_supvars()`, `ggbootvalid_supvars()` and `quadrant()` functions are now compatible with objects created with `bcMCA()` and `wcMCA()`
* `ggadd_interaction()` : new "cloud" (to choose between the cloud of variables or the cloud of individuals), "lines" and "dashes" (to choose whether to connect categories), "force" and "max.overlaps" (text repelling) arguments
* `tabcontrib()` : new "limit" argument (to filter categories)

## Bug fixes

* `ggadd_supind()` : bug fix when only one supplementary individual (thanks to @MathieuFerry) + typo in documentation (thanks to @419kfj)
* `supvar()` : bug fix when coordinates are exactly 0
* `ggadd_interaction()` : bug fix when there are empty cells in the crosstabulation of v1 and v2 (thanks to @419kfj)
* `dimtypicality()` : bug fix with dim argument (thanks to @blaisemouton)
* `ggadd_kellipses()` : bug fix with objects of class `csMCA` when some categories of `var` are note in the subcloud 
* `ggadd_supvars()` : bug fix when categories with no observations in the subcloud for objects of class `csMCA` 



# GDAtools 2.1 [CRAN]

## New function

* `barplot_contrib()` : bar plot for contributions
* `dichotomixed()` : dichotomizes the factor variables in a mixed format data frame

## Changes in existing functions

* `ggcloud_variables()` and `ggadd_supvars()` : new options ("force" and "max.overlaps") to adjust how text labels are repelled.

## Tiny fixes

* `dimdescr()` : fixed column names in the results + junk categories are not displayed for speMCA results
* `ggadd_density()` : fixed deprecated ggplot2 arguments
* `bcMCA()` : bug fix when there are junk categories 



# GDAtools 2.0.1 [CRAN]

## Changes in existing functions

* `tabcontrib()` : new shortlabs option, to display short column labels (as suggested by @janhovden)
* `planecontrib()` : the elements of the resulting lists have been renamed. This fixes a bug in `ggcloud_variables()` and `ggcloud_indiv()` when points = "best" and axes are not c(1,2) (thanks to Amal Damien Tawfik)
* new argument points = "besthv" for `ggcloud_variables()`, `ggcloud_indiv()`, `plot.speMCA()` and `plot.csMCA()`.
* `ggadd_supvars()` : new option "excl", to exclude some supplementary categories from the plot (as suggested by @janhovden)
* `dimdescr()` : new shortlabs option, to display short column labels
* `ggaxis_variables()` : new vlab argument, to choose whether to use variable names as prefixes (as suggested by @janhovden)
* `ggadd_supvars()` : vname argument has been renamed to vlab (for consistency with other functions)
* `ggadd_supvars()` : new arguments (points and min.cos2) to filter categories according to the squared cosine (as suggested by @janhovden)

## Bug fixes

* bug fix in `ggbootvalid_variables()` and `ggaxis_variables()` when factor levels have special characters (thanks to Amal Damien Tawfik)
* bug fix in `ggadd_supvars()` when factors have two levels (thanks to Amal Damien Tawfik)
* correction of computation error in `homog.test()` (thanks to @Yusuke-Ono)
* bug fix in `ggaxis_variables()` when var argument has two or more variable names (thanks to @janhovden)



# GDAtools 2.0 [CRAN]

Please note that the 1.8 version of GDAtools was not published on CRAN. So, compared to the last version on CRAN, 2.0 version inherits the changes of 1.8 version.

## Major change

* The package now focuses exclusively on Geometric Data Analysis, which makes it more coherent, lighter and with less dependencies. This implies that many functions have moved to the new `descriptio` package (available on CRAN or github) : `wtable()`, `pem()`, `phi.table()`, `oddsratio.table()`, `catdesc()`, `condesc()`, `assoc.twocat()`, `assoc.twocont()`, `assoc.catcont()`, `assoc.yx()`, `darma()`, `ggassoc_chiasmogram()`, `ggassoc_assocplot()`, `ggassoc_bertin()`, `ggassoc_phiplot()`, `ggassoc_boxplot()`, `ggassoc_crosstab()`, `ggassoc_scatter()`. Lastly, `translate.logit()` has moved to the (also new) `translate.logit` package (available on CRAN).
* The vignettes have been extensively rewritten.

## New functions

* `gPCA()` : Generalized Principal Component Analysis
* `bcMCA()` : Between-class Multiple Correspondence Analysis
* `bcPCA()` : Between-class Principal Component Analysis
* `wcMCA()` : Within-class Multiple Correspondence Analysis
* `wcPCA()` : Within-class Principal Component Analysis
* `PCAiv()` : Principal Component Analysis with Instrumental Variables
* `MCAiv()` : Multiple Correspondence Analysis with Instrumental Variables
* `PCAoiv()` : Principal Component Analysis with Orthogonal Instrumental Variables
* `MCAoiv()` : Multiple Correspondence Analysis with Orthogonal Instrumental Variables
* `coiPCA()` : Coinertia analysis between two groups of numerical variables
* `coiMCA()` : Coinertia analysis between two groups of categorical variables
* `DA()` : Descriptive Discriminant Analysis
* `DAQ()` : Descriptive Discriminant Analysis with Qualitative Variables (aka disqual)
* `rvcoef()` : RV coefficient between two groups of variables
* `planecontrib()` : For a given plane of a MCA, computes contributions et squared cosines of the active variables and categoriesand of the individuals
* `ggeta2_variables()` : Plots the eta-squared of the active variables of a MCA
* `quasindep()` : Transforms a symmetrical contingency table so that it can be used for quasi-correspondence analysis, also called correspondence analysis of incomplete contingency table
* `ggsmoothed_supvar()` : Plots the density a supplementary variable in a MCA space
* `bootvalid_variables()` : Bootstrap validation for active variables of a MCA
* `bootvalid_supvars()` : Bootstrap validation for supplementary variables of a MCA
* `ggbootvalid_variables()` : Ellipses for bootstrap validation of active variables of a MCA
* `ggbootvalid_supvars()` : Ellipses for bootstrap validation of supplementary variables of a MCA
* `supind()` : replaces `indsup()`, which is softly deprecated
* `supvar()` : replaces `varsup()`, which is softly deprecated
* `supvars()` : replaces `varsups()`, which is softly deprecated
* `nsCA()` : Nonsymmetric Correspondence Analysis

## Changes in existing functions

* `tabcontrib()` : the function has been rewritten to include contributions of deviations (thanks to @419kfj) and quality of representation.
* The handling of colors is simplified in `ggcloud_indiv()`, `ggcloud_variables()`, `ggadd_chulls()`, `ggadd_ellipses()`, `ggadd_kellipses()` and `ggadd_interaction()`.
* `ggcloud_variables()`, `ggcloud_indiv()` and `plot.speMCA()` can use contributions to the plane to select categories of individuals.
* `speMCA()` : new items are computed (squared cosines and total distances for individuals, total distances for categories)

## Other changes

* The package has much fewer dependencies : some packages are no longer needed, other have been moved to Suggests.



# GDAtools 1.8

## New functions

* `ijunk()`: Shiny app to select interactively the junk categories before a specific MCA.
* `quadrant()`: Computes the quadrant of active individuals in a given space of a MCA.
* `oddsratio.table()`: Computes the odds ratio for every cell in a contingency table.
* `ggassoc_chiasmogram()`: Plots the chiasmogram of a crosstabulation, using ggplot2.
* `ggassoc_assocplot()`: Association plot of a crosstabulation, using ggplot2.
* `ggassoc_bertin()`: Bertin plot of a crosstabulation, using ggplot2.
* `ahc.plots()`: Various plots of Ascending Hierarchical Clustering.
* `dist.chi2()`: Computes chi-squared distance.
* `ggaxis_variables()`: Plots variables on a single axis of a MCA.
* `varsups()`: Computes statistics for categorical supplementary variables. 
* `ggadd_supvars()`: Adds categorical supplementary variables to a cloud of variables. 

## Bug fixes

* bug fix in `speMCA()`, `csMCA()` and `getindexcat()` when empty levels or non-factor vectors in the data
* bug fix in `indsup()` : supdata can now be a tibble
* bug fix in `assoc.yx()` : integers are now allowed for y; empty levels are dropped in x
* bug fix in `wtable()` : empty cells are replaced by 0.

## Changes in existing functions

* `speMCA()` and `csMCA()` : junk categories can now be specified as a character vector
* `csMCA()` : results can now be used with `explor` package
* `tabcontrib()` : new "best" option (thanks to @419kfj)
* `assoc.twocat()` : standardized (i.e. Pearson) residuals, adjusted standardized residuals, odds ratios, PEM and Goodman-Kruskal tau are computed. The object is reorganized into several sublists. "gather" data frame has columns for margins frequencies and percentages. 
* `ggassoc_crosstab()` : rewriting with several new options (size, measure, limit, palette and direction) and no more dependency to GGally package
* `ggassoc_phiplot()`, `ggassoc_assocplot()` and `ggassoc_crosstab()` : now allow faceting. The measure of local association can be any one computed by `assoc.twocat()`
* `ggadd_interaction()` : geom_line replaced by geom_path (thanks to @419kfj)
* `ggadd_chulls()` : new "prop" option to allow peeling of the hull

## Other

* Removed dependency to GGally package



# GDAtools 1.7.2 [CRAN]

## Bug fixes

* bug fix with in `medoids()`



# GDAtools 1.7.1

## New functions

* `angles.csa()`: Computes the cosines similarities and angles between the dimensions of a CSA and those of a MCA.

## Bug fixes

* bug fix with vignettes
* bug fix with NA values in `dichotom()` (thanks to @juba)
* bug fix with dim option in `dimdescr()`

## Changes in existing functions

* `assoc.twocat()` : PEM are no longer computed.
* `ggadd_supvar()` : for shapes, a value of 0 is mapped to a size of 0 and new shapesize option (as suggested by @osturnus)



# GDAtools 1.7 [CRAN]

## New functions

* `ggadd_density()`: adds a density layer to the cloud of individuals for a category of a supplementary variable
* `ggadd_corr()`: adds a heatmap of under/over-representation of a supplementary variable to a cloud of individuals
* `ggadd_kellipses()` : adds concentration ellipses to a cloud of individuals, using ggplot
* `ggadd_chulls()` : adds convex hulls to a cloud of individuals, using ggplot
* `ggassoc_crosstab()` : plots counts and associations of a crosstabulation, using ggplot
* `ggassoc_phiplot()` : bar plot of phi measures of association of a crosstabulation, using ggplot
* `ggassoc_boxplot()` : displays of boxplot and combines it with a violin plot, using ggplot
* `ggassoc_scatter()` : scatter plot with a smoothing line, using ggplot
* `dimdescr()` : works with `condesc()` instead of `FactoMineR::condes()` and takes row weights into account.
* `dimtypicality()` : computes typicality tests for supplementary variables
* `ggadd_attractions()` : adds attractions between categories (via segments) to a cloud of variables
* `ggadd_supind()` : adds supplementary individuals to a cloud of individuals, using ggplot
* `flip.mca()` : flips the coordinates of the individuals and the categories on one or more dimensions of a MCA

## Removed functions :

* `dimdesc.MCA()` : replaced by `dimdescr()`
* `dimvtest()` : use `dimtypicality()` instead

## Changes in existing functions

* `ggcloud_indiv()` : the density of points can be represented as an additional layer through contours or hexagon bins
* `catdesc()` and `condesc()` : allow weights
* `catdesc()` and `condesc()` : new nperm and distrib options
* `catdesc()` and `condesc()` : new robust option
* `assoc.twocont()`, `assoc.twocat()` and `assoc.catcont()` : nperm option is set to NULL by default
* `darma()` : nperm is set to 100 by default
* `ggcloud_variables()` and `ggcloud_indiv()` : a few changes in the theme (grids are removed, etc.)
* `ggcloud_indiv()` and `ggadd_ellipses()` : new size option
* `ggcloud_variables()` : new min.ctr option to filter categories according to their contribution (for objects of class MCA, speMCA and csMCA)
* `ggcloud_variables()` : new max.pval option to filter categories according to the p-value derived from their test-value (for objects of class stMCA and multiMCA)
* `ggcloud_variables()` : prop argument can take values "vtest1" and "vtest2"
* `ggcloud_variables()` : for shapes and colors, variables are used in their order of appearance in the data instead of alphabetical order
* `ggcloud_variables()` : new face argument to use font face to identify the most contributing categories
* `homog.test()` : gives the p-values in addition to the test statistics
* `dimeta2()` : l argument renamed to vars and n argument removed
* `varsup()` : also computes typicality tests and correlation coefficients
* `conc.ellipse()` : several kinds of inertia ellipses can be plotted thanks to the kappa option
* `ggadd_ellipses()` : level is set to 0.05 by default, which corresponds to conventional confidence ellipses. Option 'points' to choose to color the points or not.
* `modif.rate()` : computes raw and modified rates
* `homog.test()` : new dim argument
* `modif.rate()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggcloud_variables()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggcloud_indiv()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_supvar()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_interaction()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `dimeta2()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `dimcontrib()` : compatibility with objects of class MCA, speMCA and csMCA
* `tabcontrib()` : compatibility with objects of class MCA, speMCA and csMCA
* `homog.test()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `varsup()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_chulls()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_corr()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_density()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_ellipses()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA
* `ggadd_kellipses()` : compatibility with objects of class MCA, speMCA, csMCA, stMCA and multiMCA

## Bug fixes

* `csMCA()`, `speMCA()` and `translate.logit()` : now work with tibbles
* `ggcloud_variables()` : now works when shapes=TRUE and there are many variables
* `assoc.twocat()` : bug fix for empty cells
* `multiMCA()` : bug fix with eigen values



# GDAtools 1.6 [CRAN]

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

* `wtable()` : can compute percentages (`prop.wtable()` is removed)
* `assoc.twocat()` : Cramer's V instead of V-squared, permutation p-values, Pearson residuals, percentage of maximum deviation from independence, summary data frame
* `assoc.twocat()` : better handling of NAs
* `assoc.twocat()` : faster computation
* `assoc.catcont()` : permutation p-values
* `ggcloud_variables()` : improved color management
* `pem()` : one can choose to sort rows and columns or not
* weights are allowed in functions `phi.table()`, `pem()`, `assoc.twocat()`, `assoc.twocont()`, `assoc.catcont()` and `assoc.yx()`



# GDAtools 1.5 [CRAN]

## New functions

* `assoc.twocat()`: measures the association between two categorical variables
* `assoc.catcont()`: measures the association between a categorical variable and a continuous variable
* `catdesc()`: measures the association between a categorical variable and some continuous and/or categorical variables
* `condesc()`: measures the association between a continuous variable and some continuous and/or categorical variables
* `ggcloud_indiv()`: cloud of individuals, using ggplot
* `ggcloud_variables()`: cloud of variables, using ggplot
* `ggadd_supvar()`: adds a supplementary variable to a cloud of variables, using ggplot
* `ggadd_interaction()`: adds the interaction between two variables to a cloud of variables, using ggplot
* `ggadd_ellipses()`: adds confidence ellipses to a cloud of individuals, using ggplot

## Changes in existing functions

* `conc.ellipses()`: additional options



# GDAtools 1.4 [CRAN]

## New functions

* `translate.logit()`: translates logit models coefficients into percentages
* `tabcontrib()`: displays the categories contributing most to MCA dimensions

## Changes in existing functions

* `varsup()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `textvarsup()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `conc.ellipse()`: with csMCA, the length of variable argument can be equal to the size of the cloud or the subcloud
* `plot.multiMCA()`: `threshold` argument, aimed at selecting the categories most associated to axes
* `plot.stMCA()`: `threshold` argument, aimed at selecting the categories most associated to axes



# GDAtools 1.3 [CRAN]

## Changes in existing functions

* `dimdesc.MCA()`: now uses weights

## Bug fixes

* `dimdesc.MCA()`: problem of compatibility next to a FactoMineR update



# GDAtools 1.2

## New functions

* `dimvtest()`: computes test-values for supplementary variables

## Changes in existing functions

* `dimeta2()`: now allows `stMCA` objects



# GDAtools 1.1 [CRAN]

## New functions

* `wtable()`: works as `table()` but allows weights and shows NAs as default
* `prop.wtable()`: works as `prop.table()` but allows weights and shows NAs as default

## Changes in existing functions

* `multiMCA()`: RV computation is now an option, with FALSE as default, which makes the function execute faster

## Bug fixes

* `textvarsup()`: there was an error with the supplementary variable labels when `resmca` was of class `csMCA`.

## Error fixes

* `textvarsup()`: plots supplementary variables on the cloud of categories (and not the cloud of individuals as it was mentioned in help).


# GDAtools 1.0 [CRAN]