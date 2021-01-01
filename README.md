# GDAtools <img src="man/figures/GDAtools.png" height=140px width=120px align="right" />

## Geometric Data Analysis and other descriptive techniques

<!-- badges: start -->
  [![R build status](https://github.com/nicolas-robette/GDAtools/workflows/R-CMD-check/badge.svg)](https://github.com/nicolas-robette/GDAtools/actions)
  [![](https://www.r-pkg.org/badges/version/GDAtools?color=blue)](https://cran.r-project.org/package=GDAtools)
  <!-- badges: end -->

This R package provides functions for Geometric Data Analysis :

* "specific" Multiple Correspondence Analysis
* Class Specific Analysis
* Multiple Factor Analysis
* "standardized" MCA
* guides for interpretation (test-values, contributions, etc.)
* inductive tests
* analysis of structuring factors (concentration ellipses, interactions, etc.)
* graphical representations (with and without ggplot2)

Besides, it also provides :

* several functions for bivariate associations between variables (phi, Cramer's V, correlation coefficient, eta-squared, etc.),
* the translation of logit models coefficients into percentages,
* weighted contingency tables,
* an underrated association measure for contingency tables ("Percentages of Maximum Deviation from Independence", aka PEM).


## installation

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

Deauvieau, J., 2010, « Comment traduire sous forme de probabilites les resultats d'une modelisation logit ? », *Bulletin de Methodologie Sociologique*, 105(1), 5-23.

Cibois P., 1993, « Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence », *Bulletin de Methodologie Sociologique*, 40, pp 43-63, [http://cibois.pagesperso-orange.fr/bms93.pdf]

Rakotomalala R., « Comprendre la taille d'effet (effect size) », [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
