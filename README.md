
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fciR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`fciR` is a companion package to the book *Fundamentals of Causal
Inference With R* by Babette A. Brumback, CRC Press 2022. It is
important to remember that this package is **not for commercial use** as
the functions have not been designed specifically to be to be efficient,
robust or with error checks. This package is a *learning tool*, not a
*working tool*.

## Installation

You can install the development version of `fciR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FrankLef/fciR")
```

## Aknowledgement

All my thanks to the author, Ms Babette Brumback, for her patience and
kindness in answering my queries and for all her work in creating this
so useful book.

## Overview

The package includes functions replacing the ones appearing in
*Fundamentals of Causal Inference* to provide improved documentations,
naming conventions and helper functions for plots, tables etc.

The relevant functions and their correspondence in the book are listed
in the following table. It is important to note that **all boostrapping
code are replaced by `fciR::boot_est`**.

|       Reference       | Book         | fciR                 | Description                                                          |
|:---------------------:|:-------------|:---------------------|:---------------------------------------------------------------------|
|  Section 2.4, p. 33   | lmodboot     | prob\_lmod           | Estimate the sampling distribution                                   |
|  Section 2.4, p. 33   | lmodboot     | prob\_lmod\_td       | Estimate the sampling distribution using tidyverse                   |
|  Section 2.4, p. 34   | lmodboot     | boot\_est            | The bootstrapping code section is replaced by this one               |
|  Section 3.3, p. 46   | bootu        | meas\_effect\_uncond | Estimate unconditional association measures                          |
|  Section 3.3, p. 50   | lmodboot     | meas\_effect\_uncond | Estimate conditional association measures                            |
|  Section 4.1, p. 60   | bootinside   | meas\_effect\_modif  | Estimate effect measure with modifications                           |
|  Section 6.1, p. 101  | stand        | backdr\_out\_npr     | backdoor method via outcome model using non parametric regression    |
| Section 6.1.1, p. 106 | standatt     | backdr\_out\_npr     | same as backdr\_out\_npr but with `att=TRUE`                         |
| Section 6.1.2, p. 111 | standout     | backdr\_out          | backdoor via outcome model using parametric method                   |
| Section 6.1.2, p. 112 | standout     | backdr\_out          | Same as previous one but using different parameters                  |
|  Section 6.2, p. 114  | mk.mordat    | backdr\_exp\_bb      | Standardization via exposure modeling                                |
|  Section 6.2, p. 115  | mordat.out   | backdr\_out          | Standardization via outcome modeling                                 |
| Section 6.2.1, p. 116 | attsem       | backdr\_out          | Standardization via outcome modeling with `att=TRUE`                 |
| Section 6.2.2, p. 118 | standexp     | backdr\_exp          | Standardization via parametric exposure model                        |
| Section 6.2.2, p. 119 | exp          | backdr\_exp\_gee     | Standardization via parametric exposure model with `geepack::geeglm` |
| Section 6.2.2, p. 119 | standep      | backdr\_exp          | Same as above for p. 118                                             |
|  Section 6.3, p. 125  | badstanddr   | backdr\_dr\_bad      | Misspecified doubly sobust standardization                           |
|  Section 6.3, p. 127  | simdr        | mc\_standdr          | Monte-Carlo simulation investigating small-sample robustness         |
|  Section 7.2, p. 141  | didlinear    | did\_linear          | Difference-in-Differences estimator with linear model                |
|  Section 7.2, p. 141  | didloglinear | did\_loglinear       | Difference-in-Differences estimator with loglinear model             |
|  Section 7.2, p. 142  | didlogistic  | did\_logistic        | Difference-in-Differences estimator with logistic model              |
|  Section 7.2, p. 142  | bootdid      | boot\_est            | Same bootstrapping function used everywhere else in the package      |
|  Section 8.3, p. 153  | frontdoor    | frontdr\_np          | Front-door method non-parametric                                     |

## Packages

The packages used by `fciR` include the usual great ones

|  Package   | Reference                                  | Description                     |
|:----------:|:-------------------------------------------|:--------------------------------|
|   rlang    | [rlang](https://rlang.r-lib.org)           | Core R functions with tidyverse |
|   dplyr    | [dplyr](https://dplyr.tidyverse.org)       | Data wrangling                  |
|   tidyr    | [tidyr](https://tidyr.tidyverse.org)       | Create tidy data                |
| tidyselect | [tidyselect](https://tidyselect.r-lib.org) | Select from a set of string     |
|   purrr    | [purrr](https://purrr.tidyverse.org)       | Functional programming toolkit  |
|  ggplot2   | [ggplot2](https://ggplot2.tidyverse.org)   | Nice plots                      |

and several packages used for more specialized tasks

|  Package   | Reference                                                           | Description                                    |
|:----------:|:--------------------------------------------------------------------|:-----------------------------------------------|
|    boot    | [boot](https://cran.rstudio.com/web/packages/boot/index.html)       | Boostrapping                                   |
|  rsample   | [rsample](https://rsample.tidymodels.org)                           | Boostrapping and jackknife                     |
|   broom    | [broom](https://broom.tidymodels.org)                               | Extract information from models                |
| MonteCarlo | [MonteCarlo](https://github.com/FunWithR/MonteCarlo)                | Monte Carlo simulations                        |
| formulaic  | [formulaic](https://dachosen1.github.io/formulaic/index.html)       | Dynamic creation and quality checks of formula |
|    gee     | [gee](https://cran.rstudio.com/web/packages/gee/index.html)         | Generalized estimation equation solver         |
|  geepack   | [geepack](https://cran.rstudio.com/web/packages/geepack/index.html) | Generalized estimation equation package        |
