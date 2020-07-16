
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

## Use

All functions intended for end users are denoted by a preceeding `RL_`. Other functions are internal.

This package also contains several pieces of data used in the creation of Nature Map v1, including transcriptions of the [Red List Habitat Classification scheme v3.1](https://www.iucnredlist.org/resources/habitat-classification-scheme) and the [Red List Seasonal Distribution Codes](https://www.iucnredlist.org/resources/mappingstandards). See `?habitats` and `?seasons` for more information.

## Workflow

### Loading the Package

``` r
library(NatureMapRedList)
```

### Obtaining data from the Red List

`RL_fetch()` obtains habitat and elevation data from the Red List API.

Species' names or IDs are supplied using the parameter `x`. `x` can be a `vector`, `list`, or `dataframe` of species' names or IUCN IDs (specify which using the parameter `query`). If `x` is a `dataframe`, the additional parameter `col.name` is needed to specify the column name of the species' names/IDs.

An API key for the Red List (`key`) must also be supplied. These are obtainable from https://apiv3.iucnredlist.org/api/v3/token.

A simple use could look like:

``` r
# Fetch data from the Red List:
df <- 
  RL_fetch(
    x = 
      c(
        "Panthera leo", 
        "Parus major",
        "Pinus oocarpa"
      ),
    query = "name",
    key = "your.api.key"
  )
```

`RL_fetch()` also allows for additional customisation. 

By default it runs in parallel, but this can be altered by setting the parameter `parallel` to `FALSE`. If running in parallel, the number of cores can also be specified manually using `num.cores`.

It can also be tested on a subset of data, set by changing `subset` to the proportion of data desired. 

The duration of sleep between each call to the Red List API can also be manually set using `sleep_dur`.

Finally, there are 3 options for `verbose`. `verbose = 0` gives no progress update, `verbose = 1` gives a progress bar (the default), and `verbose = 2` prints one line for each task completed.

Habitat suitability and major importance codes (see the `habitats` and `major_importance` data) are pasted together to give each cell value

For more details see `?RL_fetch()`.

### Recoding Level 1 or Level 2 habitats

`RL_code_fill()` allows filling of Level 1 codes with the maximum suitable corresponding Level 2 code, and filling of blank Level 2 codes with the value of the corresponding Level 1 code.

It takes an input of a wide format `dataframe` (as output by `RL_fetch()`).

Example use:

``` r
df2 <-
  RL_code_fill(
    x = df,
    level1.recode = TRUE,
    level2.recode = FALSE
  )
```

Additional parameters `subset`, `parallel`, `num.cores`, and `verbose` operate the same as for `RL_fetch()`.

See `?RL_code_fill()` for more details.

### Recoding NA seasons or habitat suitabilities

Some species have seasonal occurrence for a particular habitat missing on the Red List, or else have habitat suitability missing. In these cases, `RL_fetch()` pulls down this data as `999`. This behaviour may not be desired, and the functions `RL_season_recode()` and `RL_suit_recode()` exist to recode these values.

`RL_season_recode()` recodes `999` seasons as 'Resident', giving a code of `1` (see the `seasons` data).

`RL_suit_recode()` recodes `999` habitat suitabilities as 'Suitable', giving a code of `3` (see the `suitability` data).
