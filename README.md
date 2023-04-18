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
* Nonsymmetric Correspondence Analysis
* two-table and k-table analyses : Multiple Factor Analysis, between- and within-class analysis, MCA and PCA with instrumental variables or orthogonal instrumental variables, discriminant analysis, coinertia analysis, etc.
* guides for interpretation (contributions, quality of representation, test-values, etc.)
* analysis of structuring factors (concentration ellipses, interactions, etc.)
* inductive analysis (typicality and homogeneity tests, confidence ellipses)
* bootstrap validation
* many graphical representations for MCA and variants (with and without [`ggplot2`](https://ggplot2.tidyverse.org/))
* plots for hierarchical clustering

\ 

Initially, I developed `GDAtools` because the [`FactoMineR`](http://factominer.free.fr/) package, which I was using at the time, did not offer some of the techniques I needed, in particular specific MCA. So I tried to program the main functions of `GDAtools` to be compatible with the MCA of [`FactoMineR`](http://factominer.free.fr/) and vice versa.
Then I discovered the [`ade4`](http://pbil.univ-lyon1.fr/ade4/home.php?lang=eng) package, which offers an incredibly rich range of possibilities. However, it is oriented towards ecology, which does not exactly correspond to the needs of social scientists (of which I am one). Still, I was very much inspired by it for the GDAtools 2.0 version, in particular for the multi-table methods, with instrumental variables, etc.
Lately, I have also tried to develop the package a bit beyond the GDA toolkit "à la Le Roux et Rouanet", which was the initial goal.

\ 

## Documentation

Please visit [https://nicolas-robette.github.io/GDAtools/](https://nicolas-robette.github.io/GDAtools/) for documentation

\ 

## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/GDAtools")
```

\ 

## Citation

To cite `GDAtools` in publications, use :

Robette N. (2023), *`GDAtools` : Geometric Data Analysis in `R`*, version 2.0, https://nicolas-robette.github.io/GDAtools/

\ 

## References

**A selective list of the handbooks that helped me to develop the package, although there are many other very useful ones (first of all Benzécri's books)**

Bry X., 1995, *Analyses factorielles simples*, Economica.

Bry X., 1996, *Analyses factorielles multiples*, Economica.

Escofier B. and Pagès J., 2008, *Analyses factorielles simples et multiples*, Dunod.

Lebart L., Morineau A. and Warwick K., 1984, *Multivariate Descriptive Statistical Analysis*, John Wiley and sons, New-York.

Le Roux B. and Rouanet H., 2004, *Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis*, Kluwer Academic Publishers, Dordrecht.

Le Roux B. and Rouanet H., 2010, *Multiple Correspondence Analysis*, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks.

Saporta G., 2006, *Probabilités, analyses des données et statistique*, Editions Technip.

\ 

**More specific references on some techniques present in the package**

Abdi H., 2007, "Discriminant Correspondence Analysis", In: Neil Salkind (Ed.), *Encyclopedia of Measurement and Statistics*, Thousand Oaks (CA): Sage. 

Bouchet-Valat M., 2015, "L'analyse statistique des tables de contingence carrées - L'homogamie socioprofessionnelle en
France - I, L'analyse des correspondances", *Bulletin de Méthodologie Sociologique*, 125, 65–88. [<doi>](doi:10.1177/0759106314555655)

Bry X., Robette N., Roueff O., 2016, "A dialogue of the deaf in the statistical theater? Adressing structural effects within a geometric data analysis framework", *Quality & Quantity*, 50(3), 1009–1020 [<doi>](https://link.springer.com/article/10.1007/s11135-015-0187-z)

Cibois P., 2014, *Les méthodes d’analyse d’enquêtes*. Nouvelle édition [en ligne](https://books.openedition.org/enseditions/1443). Lyon: ENS Éditions.

De Leeuw J et van der Heijden PGM, 1985, *Quasi-Correspondence Analysis*, Leiden University.
of Leiden.

Dolédec S. and Chessel D., 1994, "Co-inertia analysis: an alternative method for studying species-environment relationships", *Freshwater Biology*, 31, 277–294.

Escofier B., 1990, "Analyse des correspondances multiples conditionnelle", *La revue de Modulad*, 5, 13-28.

Escofier B. and Pages J., 1994, "Multiple Factor Analysis (AFMULT package)", *Computational Statistics and Data Analysis*, 18, 121-140.

Escoufier Y., 1973, "Le traitement des variables vectorielles", *Biometrics*, 29, 751–760.

Escoufier Y., 1987, "The duality diagram : a means of better practical applications". In *Development in numerical ecology*, Legendre, P. & Legendre, L. (Eds.) NATO advanced Institute, Serie G. Springer Verlag, Berlin, 139–156.

Kroonenberg P.M. and Lombardo R., 1999, "Nonsymmetric Correspondence Analysis: A Tool for Analysing Contingency Tables with a Dependence Structure", *Multivariate Behavioral Research*, 34 (3), 367-396.

Lebart L., 2006, "Validation Techniques in Multiple Correspondence Analysis". In M. Greenacre et J. Blasius (eds), *Multiple Correspondence Analysis and related techniques*, Chapman and Hall/CRC, p.179-196.

Lebart L., 2007, "Which bootstrap for principal axes methods?". In P. Brito et al. (eds), *Selected Contributions in Data Analysis and Classification*, Springer, p.581-588.

Saporta G., 1977, "Une méthode et un programme d'analyse discriminante sur variables qualitatives", *Premières Journées Internationales, Analyses des données et informatiques*, INRIA, Rocquencourt.

Tucker L.R., 1958, "An inter-battery method of factor analysis", *Psychometrik*, 23-2, 111-136.

Van der Heijden PGM, 1992, "Three Approaches to Study the Departure from Quasi-independence", *Statistica Applicata*, 4, 465-80.
