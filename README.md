
# redlistManipulatr

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/matthewlewis896/NatureMapRedList.svg?branch=master)](https://travis-ci.com/matthewlewis896/NatureMapRedList)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/matthewlewis896/NatureMapRedList?branch=master&svg=true)](https://ci.appveyor.com/project/matthewlewis896/NatureMapRedList)
[![Codecov test coverage](https://codecov.io/gh/matthewlewis896/NatureMapRedList/branch/master/graph/badge.svg)](https://codecov.io/gh/matthewlewis896/NatureMapRedList?branch=master)
<!-- badges: end -->

redlistManipulatr collects a group of functions used to pull and manipulate IUCN Red List habitat and elevation data.

## Installation

You can install the released version of redlistManipulatr from [github](https://github.com/matthewlewis896/redlistManipulatr) with:

``` r
devtools::install_github("matthewlewis896/redlistManipulatr", build_vignettes = TRUE)
```

## Description

All functions intended for end users are denoted by a preceeding `RL_`.

This package also contains several pieces of Red List data, including transcriptions of the [Red List Habitat Classification scheme v3.1](https://www.iucnredlist.org/resources/habitat-classification-scheme) and the [Red List Seasonal Distribution Codes](https://www.iucnredlist.org/resources/mappingstandards). See `?habitats` and `?seasons` for more information.

## Use

### Loading the Package

``` r
library(redlistManipulatr)
```

### Workflow

To see a workflow which approximates that used for Nature Map v1, use:

``` r
vignette("redlistManipulatr")
```
