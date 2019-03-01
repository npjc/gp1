
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gp1)](https://cran.r-project.org/package=readgp1)

Read, Validate, Simulate and Write GP1 instrument
files

## Installation

<!-- You can install the released version of gp1 from [CRAN](https://CRAN.R-project.org) with: -->

``` r
# install.packages("readgp1") # not yet
remotes::install_github("npjc/readgp1")
```

## Example

This is a basic example which shows you how to solve a common problem:

Read the raw data into tidy tibble:

``` r
d <- read_gp1(path)
d
#> # A tibble: 21,168 x 4
#>    plate datetime            well  value
#>    <chr> <dttm>              <chr> <dbl>
#>  1 0     2019-01-24 12:13:31 A01   0.141
#>  2 0     2019-01-24 12:26:18 A01   0.141
#>  3 0     2019-01-24 12:39:26 A01   0.142
#>  4 0     2019-01-24 12:52:34 A01   0.148
#>  5 0     2019-01-24 13:05:42 A01   0.146
#>  6 0     2019-01-24 13:18:50 A01   0.145
#>  7 0     2019-01-24 13:31:58 A01   0.147
#>  8 0     2019-01-24 13:45:06 A01   0.149
#>  9 0     2019-01-24 13:58:14 A01   0.151
#> 10 0     2019-01-24 14:11:22 A01   0.152
#> # … with 21,158 more rows
```

Allowing for easy visualisation:

<img src="man/figures/README-eg1plot-1.png" width="100%" />

Another example

``` r
path2 <- gp1_example("GP1-Results_feb11.txt")
cat(readr::read_lines(path2, n_max = 5), sep = '\n')
#> Plate[1], 2019/2/11, 13:55:52, 9.7e-002, 7.9e-002, 8.5e-002, 8.e-002, 0.743, 8.6e-002, 8.5e-002, 9.6e-002, 9.9e-002, 7.5e-002, 7.4e-002, 8.4e-002, 0.105, 7.6e-002, 8.6e-002, 9.6e-002, 9.6e-002, 7.6e-002, 8.2e-002, 7.4e-002, 9.9e-002, 7.4e-002, 7.6e-002, 8.5e-002, 9.5e-002, 7.9e-002, 7.9e-002, 8.e-002, 0.125, 8.8e-002, 8.7e-002, 1.095, 9.6e-002, 8.e-002, 7.9e-002, 8.3e-002, 0.117, 0.118, 8.8e-002, 0.112, 0.127, 8.2e-002, 8.e-002, 8.3e-002, 0.112, 8.4e-002, 0.146, 0.446
#> Plate[1], 2019/2/11, 14:02:16, 9.9e-002, 7.9e-002, 8.6e-002, 8.2e-002, 0.419, 9.2e-002, 8.6e-002, 9.6e-002, 0.103, 7.7e-002, 7.6e-002, 8.6e-002, 0.11, 7.8e-002, 8.8e-002, 9.6e-002, 9.8e-002, 7.8e-002, 8.3e-002, 7.5e-002, 0.101, 7.6e-002, 7.8e-002, 8.5e-002, 0.103, 8.1e-002, 8.e-002, 8.1e-002, 0.134, 9.6e-002, 9.4e-002, 0.813, 9.8e-002, 8.1e-002, 8.1e-002, 8.7e-002, 0.121, 8.9e-002, 9.8e-002, 9.5e-002, 0.128, 8.2e-002, 8.e-002, 8.4e-002, 0.114, 8.5e-002, 9.7e-002, 0.174
#> Plate[1], 2019/2/11, 14:08:42, 9.8e-002, 7.9e-002, 8.5e-002, 8.3e-002, 0.141, 9.2e-002, 8.6e-002, 9.6e-002, 0.101, 7.5e-002, 7.5e-002, 8.5e-002, 0.109, 7.8e-002, 8.8e-002, 9.6e-002, 9.7e-002, 7.7e-002, 8.2e-002, 7.5e-002, 0.103, 7.5e-002, 7.7e-002, 8.5e-002, 0.104, 8.1e-002, 8.1e-002, 8.1e-002, 0.136, 9.4e-002, 9.e-002, 0.141, 9.6e-002, 8.1e-002, 8.e-002, 8.4e-002, 0.12, 8.9e-002, 9.2e-002, 9.5e-002, 0.126, 8.2e-002, 8.e-002, 8.3e-002, 0.113, 8.5e-002, 9.2e-002, 9.8e-002
#> Plate[1], 2019/2/11, 14:15:22, 9.8e-002, 7.9e-002, 8.5e-002, 8.4e-002, 0.112, 9.1e-002, 8.6e-002, 9.6e-002, 0.102, 7.5e-002, 7.5e-002, 8.5e-002, 0.108, 7.8e-002, 8.9e-002, 9.6e-002, 9.7e-002, 7.7e-002, 8.2e-002, 7.5e-002, 0.104, 7.5e-002, 7.8e-002, 8.5e-002, 0.104, 8.1e-002, 8.e-002, 8.2e-002, 0.136, 8.9e-002, 9.e-002, 9.2e-002, 9.8e-002, 8.1e-002, 8.e-002, 8.4e-002, 0.12, 8.7e-002, 9.1e-002, 9.5e-002, 0.126, 8.2e-002, 8.e-002, 8.3e-002, 0.112, 8.4e-002, 9.1e-002, 9.5e-002
#> Plate[1], 2019/2/11, 14:22:02, 9.8e-002, 7.9e-002, 8.5e-002, 8.4e-002, 0.114, 9.e-002, 8.6e-002, 9.6e-002, 0.102, 7.5e-002, 7.5e-002, 8.5e-002, 0.108, 7.8e-002, 8.9e-002, 9.6e-002, 9.7e-002, 7.7e-002, 8.2e-002, 7.5e-002, 0.105, 7.5e-002, 7.8e-002, 8.5e-002, 0.104, 8.1e-002, 8.e-002, 8.2e-002, 0.135, 8.9e-002, 9.e-002, 9.1e-002, 9.7e-002, 8.1e-002, 8.e-002, 8.4e-002, 0.12, 8.7e-002, 9.1e-002, 9.5e-002, 0.125, 8.2e-002, 8.e-002, 8.3e-002, 0.113, 8.4e-002, 9.1e-002, 9.6e-002
```

``` r
d2 <- read_gp1(path2)
d2
#> # A tibble: 14,544 x 4
#>    plate datetime            well  value
#>    <chr> <dttm>              <chr> <dbl>
#>  1 1     2019-02-11 13:55:52 A01   0.097
#>  2 1     2019-02-11 14:02:16 A01   0.099
#>  3 1     2019-02-11 14:08:42 A01   0.098
#>  4 1     2019-02-11 14:15:22 A01   0.098
#>  5 1     2019-02-11 14:22:02 A01   0.098
#>  6 1     2019-02-11 14:28:42 A01   0.099
#>  7 1     2019-02-11 14:35:22 A01   0.098
#>  8 1     2019-02-11 14:42:02 A01   0.097
#>  9 1     2019-02-11 14:48:42 A01   0.097
#> 10 1     2019-02-11 14:55:22 A01   0.099
#> # … with 14,534 more rows
```

<img src="man/figures/README-eg2plot-1.png" width="100%" />
