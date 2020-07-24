
# redlistManipulatr

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/matthewlewis896/redlistManipulatr.svg?branch=master)](https://travis-ci.com/matthewlewis896/redlistManipulatr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/matthewlewis896/redlistManipulatr?branch=master&svg=true)](https://ci.appveyor.com/project/matthewlewis896/redlistManipulatr)
<!-- badges: end -->

redlistManipulatr collects a group of functions used to pull and manipulate IUCN Red List habitat and elevation data.

## Installation

You can install the released version of redlistManipulatr from [github](https://github.com/matthewlewis896/redlistManipulatr) with:

``` r
remotes::install_github("matthewlewis896/redlistManipulatr", build_vignettes = TRUE)
```

## Description

All functions intended for end users are denoted by a preceeding `RL_`.

This package also contains several pieces of Red List data, including transcriptions of the [Red List Habitat Classification scheme v3.1](https://www.iucnredlist.org/resources/habitat-classification-scheme) and the [Red List Seasonal Distribution Codes](https://www.iucnredlist.org/resources/mappingstandards). See `?habitats` and `?seasons` for more information and `View(habitats)` or `View(seasons)` to view the data.

## Use

### Loading the Package

``` r
library(redlistManipulatr)
```

### Capabilities

To see a breakdown of the main functions of the package with examples see the `vignette`.

``` r
vignette("redlistManipulatr")
```
