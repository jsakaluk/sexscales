
<!-- README.md is generated from README.Rmd. Please edit that file -->

\# sexscales
<img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

The goal of sexscales is to help you easily calculate (sub)scale scores
using the Qualtrics versions of measures in the 4th edition of the
*Handbook of Sexuality-Related Measures* (Milhausen et al., 2019).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsakaluk/sexscales")
```

## Example

To use sexscales, you will need to have exported data from a Qualtrics
survey (using numeric values) that includes one or more of the
pre-programmed measures from *the Handbook*. Once you have imported the
data into a data frame in R, all you need to do is use the appropriate
scoring function (e.g., scoreBerglas for Berglas et al.’s Sexual
Relationship Rights measure), specify your data frame (e.g., dat), and
tell the function the name of the column containing the first variable
for the measure in question in the firstq argument (e.g., “AA\_SSR\_1”).

``` r
library(sexscales)

newdat <- scoreBerglas(dat, firstq = "AA_SSR_1")
```
