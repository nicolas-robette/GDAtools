# **GDAtools** <img src="man/figures/GDAtools.png" height=140px width=120px alt="" align="right" />

## Geometric Data Analysis and other descriptive techniques

<!-- badges: start -->
  [![R build status](https://github.com/nicolas-robette/GDAtools/workflows/R-CMD-check/badge.svg)](https://github.com/nicolas-robette/GDAtools/actions)
  [![](https://www.r-pkg.org/badges/version/GDAtools?color=blue)](https://cran.r-project.org/package=GDAtools)
  [![](http://cranlogs.r-pkg.org/badges/last-month/GDAtools?color=orange)](https://cran.r-project.org/package=GDAtools)
<!-- badges: end -->

[`GDAtools`](https://nicolas-robette.github.io/GDAtools/) provides functions for Geometric Data Analysis :

* specific Multiple Correspondence Analysis (speMCA)
* Class Specific Analysis (csMCA)
* Multiple Factor Analysis (multiMCA)
* "standardized" Multiple Correspondence Analysis (stMCA)
* guides for interpretation (test-values, contributions, etc.)
* inductive tests
* analysis of structuring factors (concentration ellipses, interactions, etc.)
* graphical representations (with and without [`ggplot2`](https://ggplot2.tidyverse.org/))

Besides, it also provides :

* several functions for bivariate associations between variables (phi coefficients, Cramer's V, correlation coefficients, eta-squared, etc.),
* plotting functions for bivariate associations between variables,
* the translation of logit models coefficients into percentages,
* weighted contingency tables,
* an underrated association measure for contingency tables ("Percentages of Maximum Deviation from Independence", aka PEM).

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

## References

Bry X., Robette N., Roueff O., 2016, « A dialogue of the deaf in the statistical theater? Adressing structural effects within a geometric data analysis framework », *Quality & Quantity*, 50(3), pp 1009–1020 [https://link.springer.com/article/10.1007/s11135-015-0187-z]

Le Roux B. and Rouanet H., 2010, *Multiple Correspondence Analysis*, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks.

Le Roux B. and Rouanet H., 2004, *Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis*, Kluwer Academic Publishers, Dordrecht.

Deauvieau J., 2019, « Comparer les resultats d’un modele logit dichotomique ou polytomique entre plusieurs groupes a partir
des probabilites estimees »,  *Bulletin de Methodologie Sociologique*, 142(1), 7-31.

Cibois P., 1993, « Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence », *Bulletin de Methodologie Sociologique*, 40, pp 43-63, [http://cibois.pagesperso-orange.fr/bms93.pdf]

Rakotomalala R., « Comprendre la taille d'effet (effect size) », [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
