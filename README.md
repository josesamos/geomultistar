
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geomultistar: Multidimensional Queries Enriched with Geographic Data

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/josesamos/geomultistar.svg?branch=master)](https://travis-ci.com/josesamos/geomultistar)
<!-- badges: end -->

*Multidimensional systems* allow complex queries to be carried out in an
easy way. The *geographical dimension*, together with the *temporal
dimension*, plays a fundamental role in multidimensional systems.
Through the `geomultistar` package, vector layers can be associated to
the attributes of geographic dimensions, so that the results of
multidimensional queries can be obtained directly as vector layers. In
other words, this package allows **enriching multidimensional queries
with geographic data**.

The multidimensional structures on which we can define the queries can
be created from flat tables with
[`starschemar`](https://CRAN.R-project.org/package=starschemar) package
or imported directly using functions from `geomultistar` package.

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

## Example

If we start from a flat table, we can generate a star schema using the
[`starschemar`](https://CRAN.R-project.org/package=starschemar) package,
as described in its examples.

If we have a star schema in another tool, we import the fact and
dimension tables into R in the form of tables implemented by `tibble`.
Once we have them in this format, we have to build a `multistar`
structure from them: This structure can contain multiple fact and
dimension tables, so facts can share dimensions.

Once we have a `multistar` structure, we will associate vector layers to
the attributes of the geographic dimensions. We can use existing layers
or generate them from the previous definitions. As a result we will have
a `geomultistar` structure.

Finally, we can define multidimensional queries on this structure using
the functions available in the
[`starschemar`](https://CRAN.R-project.org/package=starschemar) package.
When executing these queries, the vector layers of the attributes will
be taken into account to result in a new vector layer.

### Define a `multistar` structure

This is a basic example which shows you how to solve a common problem:

``` r
library(geomultistar)
## basic example code
```

### Define a `multistar` structure
