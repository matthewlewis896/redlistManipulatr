
# NatureMapRedList

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/matthewlewis896/NatureMapRedList.svg?branch=master)](https://travis-ci.com/matthewlewis896/NatureMapRedList)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/matthewlewis896/NatureMapRedList?branch=master&svg=true)](https://ci.appveyor.com/project/matthewlewis896/NatureMapRedList)
[![Codecov test coverage](https://codecov.io/gh/matthewlewis896/NatureMapRedList/branch/master/graph/badge.svg)](https://codecov.io/gh/matthewlewis896/NatureMapRedList?branch=master)
<!-- badges: end -->

NatureMapRedList collects a group of functions used in the process of obtaining and manipulating IUCN Red List data used in the creation of Nature Map v1.

## Installation

You can install the released version of NatureMapRedList from [github](https://github.com/matthewlewis896/NatureMapRedList) with:

``` r
devtools::install_github("matthewlewis896/NatureMapRedList")
```

## Description

All functions intended for end users are denoted by a preceeding `RL_`.

This package also contains several pieces of data used in the creation of Nature Map v1, including transcriptions of the [Red List Habitat Classification scheme v3.1](https://www.iucnredlist.org/resources/habitat-classification-scheme) and the [Red List Seasonal Distribution Codes](https://www.iucnredlist.org/resources/mappingstandards). See `?habitats` and `?seasons` for more information.

## Use

### Loading the Package

``` r
library(NatureMapRedList)
```

### Workflow

To see a workflow which approximates that used for Nature Map v1, use:

``` r
vignette(NatureMapRedList)
```
