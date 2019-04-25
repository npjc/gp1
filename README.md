
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gp1)](https://cran.r-project.org/package=readgp1)

Read, ~~Validate~~, ~~Simulate~~ and ~~Write~~ GP1 instrument
files

## Installation

<!-- You can install the released version of gp1 from [CRAN](https://CRAN.R-project.org) with: -->

``` r
# install.packages("readgp1") # not yet
remotes::install_github("npjc/readgp1")
```

## Example

Read the raw data into tidy tibble:

``` r
library(readgp1)
path <- gp1_example("GP1-Results_jan27.txt")
read_gp1(path)
#> # A tibble: 21,168 x 4
#>    plate well  runtime measure
#>    <int> <chr>   <int>   <dbl>
#>  1     0 A1          0   0.141
#>  2     0 A1        767   0.141
#>  3     0 A1       1555   0.142
#>  4     0 A1       2343   0.148
#>  5     0 A1       3131   0.146
#>  6     0 A1       3919   0.145
#>  7     0 A1       4707   0.147
#>  8     0 A1       5495   0.149
#>  9     0 A1       6283   0.151
#> 10     0 A1       7071   0.152
#> # … with 21,158 more rows
read_gp1(path, all_fields = TRUE)
#> # A tibble: 21,168 x 6
#>    plate well  datetime            runtime measure_type measure
#>    <int> <chr> <dttm>                <int> <chr>          <dbl>
#>  1     0 A1    2019-01-24 12:13:31       0 Epoch (GP-1)   0.141
#>  2     0 A1    2019-01-24 12:26:18     767 Epoch (GP-1)   0.141
#>  3     0 A1    2019-01-24 12:39:26    1555 Epoch (GP-1)   0.142
#>  4     0 A1    2019-01-24 12:52:34    2343 Epoch (GP-1)   0.148
#>  5     0 A1    2019-01-24 13:05:42    3131 Epoch (GP-1)   0.146
#>  6     0 A1    2019-01-24 13:18:50    3919 Epoch (GP-1)   0.145
#>  7     0 A1    2019-01-24 13:31:58    4707 Epoch (GP-1)   0.147
#>  8     0 A1    2019-01-24 13:45:06    5495 Epoch (GP-1)   0.149
#>  9     0 A1    2019-01-24 13:58:14    6283 Epoch (GP-1)   0.151
#> 10     0 A1    2019-01-24 14:11:22    7071 Epoch (GP-1)   0.152
#> # … with 21,158 more rows
```

-----

To visualize the parsed output with the `mtpview` pkg:

``` r
library(mtpview)
d <- read_gp1(path)

mtp_ggplot(d, aes(plate = plate, well = well)) + 
  mtp_spec_48well() + 
  geom_footprint() + 
  geom_notched_border() + 
  geom_row_label() + 
  geom_col_label()  +
  geom_well_rect(fill = 'white') + 
  geom_well_line(aes(x = runtime, y = measure)) + 
  facet_wrap(~plate, ncol = 1)
```

<img src="man/figures/README-eg1plot-1.png" width="100%" />

Or with vanilla `ggplot2`

``` r
library(tidyverse)
d %>%
    group_by(plate, well) %>%
    ggplot(aes(x = runtime, y = measure)) +
    geom_line(aes(color = plate, group = interaction(plate, well))) +
    geom_text(aes(x = 0, y = 3, label = well, group = well), alpha = 0.5,
              hjust = 'left', vjust = 'top', data = distinct(d, well)) +
    facet_wrap(~well, ncol = 8) +
    labs(x = 'time (elapsed, hours)', y = 'OD600 (raw, GP-1)',
         caption = paste0('source: ', basename(path))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.background = element_rect(fill = NULL, color = 'black'),
          plot.caption = element_text(family = 'mono', face = 'bold'))
```

<img src="man/figures/README-eg2plot-1.png" width="100%" />
