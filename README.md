# **GDAtools** <img src="man/figures/GDAtools.png" height=140px width=120px alt="" align="right" />

## Geometric Data Analysis

<!-- badges: start -->
  [![R build status](https://github.com/nicolas-robette/GDAtools/workflows/R-CMD-check/badge.svg)](https://github.com/nicolas-robette/GDAtools/actions)
  [![](https://www.r-pkg.org/badges/version/GDAtools?color=blue)](https://cran.r-project.org/package=GDAtools)
  [![](http://cranlogs.r-pkg.org/badges/last-month/GDAtools?color=orange)](https://cran.r-project.org/package=GDAtools)
<!-- badges: end -->

[`GDAtools`](https://nicolas-robette.github.io/GDAtools/) provides functions for Geometric Data Analysis :

* Specific Multiple Correspondence Analysis
* Class Specific Analysis
* two-table and k-table analyses : Multiple Factor Analysis, between- and within-class analysis, MCA and PCA with instrumental variables or orthogonal instrumental variables, discriminant analysis, coinertia analysis
* guides for interpretation (contributions, test-values, etc.)
* analysis of structuring factors (concentration ellipses, interactions, etc.)
* inductive tests
* bootstrap validation
* many graphical representations for MCA and variants (with and without [`ggplot2`](https://ggplot2.tidyverse.org/))
* plots for hierarchical clustering


## Documentation

Please visit [https://nicolas-robette.github.io/GDAtools/](https://nicolas-robette.github.io/GDAtools/) for documentation


## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/GDAtools")
```

## Citation

To cite `GDAtools` in publications, use :

Robette N. (2023), *`GDAtools` : Geometric Data Analysis in `R`*, version 2.0, https://nicolas-robette.github.io/GDAtools/


## References

Bry X., 1995, *Analyses factorielles multiples*, Economica.

Bry X., 1996, *Analyses factorielles multiples*, Economica.

Bry X., Robette N., Roueff O., 2016, « A dialogue of the deaf in the statistical theater? Adressing structural effects within a geometric data analysis framework », *Quality & Quantity*, 50(3), pp 1009–1020 [https://link.springer.com/article/10.1007/s11135-015-0187-z]

Escofier B. and Pagès J., 2008, *Analyses factorielles simples et multiples*, Dunod.

Lebart L., Morineau A. et Warwick K., 1984, *Multivariate Descriptive Statistical Analysis*, John Wiley and sons, New-York.

Le Roux B. and Rouanet H., 2004, *Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis*, Kluwer Academic Publishers, Dordrecht.

Le Roux B. and Rouanet H., 2010, *Multiple Correspondence Analysis*, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks.

Saporta G., 2006, *Probabilités, analyses des données et statistique*, Editions Technip.

