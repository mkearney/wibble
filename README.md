
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wibble <img src="man/figures/logo.png" width="160px" align="right" />

<!-- [![Build Status](https://travis-ci.org/mkearney/rtweet.svg?branch=master)](https://travis-ci.org/mkearney/rtweet)
[![CRAN status](https://www.r-pkg.org/badges/version/rtweet)](https://cran.r-project.org/package=rtweet)
[![Coverage Status](https://codecov.io/gh/mkearney/rtweet/branch/master/graph/badge.svg)](https://codecov.io/gh/mkearney/rtweet?branch=master)

![Downloads](https://cranlogs.r-pkg.org/badges/rtweet)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rtweet)-->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

> Web Data Frames

## Install

Install the dev version from Github:

``` r
## from github
remotes::install_github("mkearney/wibble")
```

## Example

Scrape/wrangle CNN US site

``` r
## load pkg
library(wibble)

## scrape CNN
cnn <- webble("https://cnn.com/us")

## wibble and wobble
cnn %>%
  wibble() %>%
  wobble("body div") %>%
  wobble("ul")
#> # A tibble: 173 x 3
#>    li         a          h2        
#>    <list>     <list>     <list>    
#>  1 <list [1]> <list [1]> <list [1]>
#>  2 <list [1]> <list [1]> <list [1]>
#>  3 <list [1]> <list [1]> <list [1]>
#>  4 <list [1]> <list [1]> <list [1]>
#>  5 <list [1]> <list [1]> <NULL>    
#>  6 <list [1]> <list [1]> <NULL>    
#>  7 <list [1]> <list [1]> <NULL>    
#>  8 <list [1]> <NULL>     <NULL>    
#>  9 <list [1]> <NULL>     <NULL>    
#> 10 <list [1]> <NULL>     <NULL>    
#> # … with 163 more rows
```
