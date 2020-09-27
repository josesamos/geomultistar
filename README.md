
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geomultistar

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/josesamos/geomultistar.svg?branch=master)](https://travis-ci.com/josesamos/geomultistar)
<!-- badges: end -->

The goal of `geomultistar` is to build queries with geographical data in
`multistar` format, obtained using the `starschemar` package.

## Installation

You can install the released version of geomultistar from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geomultistar")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/geomultistar")
```

Multidimensional Queries Enriched with Geographic Data

Multidimensional systems allow complex queries to be carried out in an
easy way. The geographical dimension, together with the temporal
dimension, plays a fundamental role in multidimensional systems. Through
this package, vector layers can be associated to the attributes of
geographic dimensions, so that the results of multidimensional queries
can be obtained directly as vector layers. The multidimensional
structures on which we can define the queries can be created from a flat
table with the starschemar package or imported directly using functions
from this package.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geomultistar)
## basic example code
```
