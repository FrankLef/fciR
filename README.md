
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fciR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Lcense](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R
4.2.1](https://img.shields.io/badge/R-4.2.1-blueviolet.svg)](https://cran.r-project.org/bin/windows/base/)
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

|       Reference       | Book             | fciR               | Description                                                          |
|:---------------------:|:-----------------|:-------------------|:---------------------------------------------------------------------|
|  Section 2.4, p. 33   | lmodboot         | prob_lmod          | Estimate the sampling distribution                                   |
|  Section 2.4, p. 33   | lmodboot         | prob_lmod_td       | Estimate the sampling distribution using tidyverse                   |
|  Section 2.4, p. 34   | lmodboot         | boot_est           | The bootstrapping code section is replaced by this one               |
|  Section 3.3, p. 46   | bootu            | meas_effect_uncond | Estimate unconditional association measures                          |
|  Section 3.3, p. 50   | lmodboot         | meas_effect_uncond | Estimate conditional association measures                            |
|  Section 4.1, p. 60   | bootinside       | meas_effect_modif  | Estimate effect measure with modifications                           |
|  Section 6.1, p. 101  | stand            | backdr_out_npr     | backdoor method via outcome model using non parametric regression    |
| Section 6.1.1, p. 106 | standatt         | backdr_out_npr     | same as backdr_out_npr but with `att=TRUE`                           |
| Section 6.1.2, p. 111 | standout         | backdr_out         | backdoor via outcome model using parametric method                   |
| Section 6.1.2, p. 112 | standout         | backdr_out         | Same as previous one but using different parameters                  |
|  Section 6.2, p. 114  | mk.mordat        | backdr_exp_bb      | Standardization via exposure modeling                                |
|  Section 6.2, p. 115  | mordat.out       | backdr_out         | Standardization via outcome modeling                                 |
| Section 6.2.1, p. 116 | attsem           | backdr_out         | Standardization via outcome modeling with `att=TRUE`                 |
| Section 6.2.2, p. 118 | standexp         | backdr_exp         | Standardization via parametric exposure model                        |
| Section 6.2.2, p. 119 | exp              | backdr_exp_gee     | Standardization via parametric exposure model with `geepack::geeglm` |
| Section 6.2.2, p. 119 | standep          | backdr_exp         | Same as above for p. 118                                             |
| Section 6.2.2, p. 120 | prop             | prop_mod           | Fit the propensity score model                                       |
|  Section 6.3, p. 125  | badstanddr       | backdr_dr_bad      | Misspecified doubly robust standardization                           |
|  Section 6.3, p. 127  | simdr            | mc_standdr         | Monte-Carlo simulation investigating small-sample robustness         |
|  Section 7.2, p. 141  | didlinear        | did_linear         | Difference-in-Differences estimator with linear model                |
|  Section 7.2, p. 141  | didloglinear     | did_loglinear      | Difference-in-Differences estimator with loglinear model             |
|  Section 7.2, p. 142  | didlogistic      | did_logistic       | Difference-in-Differences estimator with logistic model              |
|  Section 7.2, p. 142  | bootdid          | boot_est           | Same bootstrapping function used everywhere else in the package      |
|  Section 8.3, p. 153  | frontdoor        | frontdr_np         | Front-door method non-parametric                                     |
|  Section 9.3, p. 164  | iv               | instr_vars         | ITT, CACE and ATT from instrument variables                          |
|  Section 9.3, p. 167  | ividentity       | instr_linear       | Estimate effect using instrument variables                           |
|  Section 9.3, p. 167  | ivlog            | instr_loglinear    | Estimate effect using instrument variables via logarithmic fit       |
|  Section 9.3, p. 168  | ivlogit          | instr_logistic     | Estimate effect using instrument variables via logistic fit          |
| Section 10.2, p. 179  | estand           | backdr_out         | Outcome-model standardization compared to propensity score           |
| Section 10.3, p. 181  | equartiles       | prop_quant         | Stratifying on the quantiles of the propensity score                 |
| Section 11.2, p. 190  | precision        | precision_eff      | Compute precision efficiency.                                        |
| Section 11.2, p. 190  | bootprecision    | precision_stats    | Compute stats on precision efficiency.                               |
| Section 12.3, p. 201  | mediation        | mediation_np       | Estimate non-parametric mediation effect.                            |
| Section 12.3, p. 202  | nonparamediation | mediation          | Estimate mediation effect with parametric assumptions.               |
| Section 13.1, p. 211  | msm              | time_msm           | Estimate using marginal structural models.                           |
| Section 13.2, p. 214  | snmm             | time_snmm          | Estimate using structural mested mean models.                        |
| Section 13.3, p. 216  | mkcogtab.r       | time_odtr_prop     | Optimal dynamic treatment regimes, step 1.                           |
| Section 13.3, p. 217  | A2opt.r          | time_odtr_optA2    | Optimal dynamic treatment regimes, step 2.                           |
| Section 13.3, p. 217  | A2opt.r          | time_odtr_optA2    | Optimal dynamic treatment regimes, step 3.                           |
| Section 13.3, p. 218  | A1opt.r          | time_odtr_optA1A2  | Optimal dynamic treatment regimes, step 4.                           |
| Section 13.3, p. 219  | optimal.r        | time_odtr_optimal  | Optimal dynamic treatment regimes, step 4.                           |

## Packages

The packages used by `fciR` include the usual great ones

| Package                                    | Description                     |
|--------------------------------------------|---------------------------------|
| [rlang](https://rlang.r-lib.org)           | Core R functions with tidyverse |
| [dplyr](https://dplyr.tidyverse.org)       | Data wrangling                  |
| [tidyr](https://tidyr.tidyverse.org)       | Create tidy data                |
| [tidyselect](https://tidyselect.r-lib.org) | Select from a set of string     |
| [purrr](https://purrr.tidyverse.org)       | Functional programming toolkit  |
| [ggplot2](https://ggplot2.tidyverse.org)   | Nice plots                      |

and several packages used for more specialized tasks

| Package                                                                 | Description                                    |
|-------------------------------------------------------------------------|------------------------------------------------|
| [boot](https://cran.rstudio.com/web/packages/boot/index.html)           | Boostrapping                                   |
| [rsample](https://rsample.tidymodels.org)                               | Boostrapping and jackknife                     |
| [broom](https://broom.tidymodels.org)                                   | Extract information from models                |
| [MonteCarlo](https://github.com/FunWithR/MonteCarlo)                    | Monte Carlo simulations                        |
| [formulaic](https://dachosen1.github.io/formulaic/index.html)           | Dynamic creation and quality checks of formula |
| [gee](https://cran.rstudio.com/web/packages/gee/index.html)             | Generalized estimation equation solver         |
| [geepack](https://cran.rstudio.com/web/packages/geepack/index.html)     | Generalized estimation equation package        |
| [AER](https://cran.r-project.org/web/packages/AER/index.html)           | Applied econometrics with R                    |
| [Matching](https://cran.r-project.org/web/packages/Matching/index.html) | Multivariate and propensity score matching     |
