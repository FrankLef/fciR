
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fciR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`fciR` is a companion package to the book *Fundamentals of Causal
Inference With R* by Babette A. Brumback, CRC Press 2022.

## Installation

You can install the development version of fciR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FrankLef/fciR")
```

## Overview

The package includes functions replacing the ones appearing in
*Fundamentals of Causal Inference* to provide improved documentations,
naming conventions and helper functions for plots, tables etc.

The relevant functions and their correspondence in the book are listed
in the following table. It is important to note thet **all boostrapping
code are replaced by `fciR::boot_est`**.

|       Reference       | Book       | fciR                 | Description                                                       |
|:---------------------:|:-----------|:---------------------|:------------------------------------------------------------------|
|  Section 2.4, p. 33   | lmodboot   | prob\_lmod           | Estimate the unconditional sampling distribution                  |
|  Section 2.4, p. 34   | lmodboot   | boot\_est            | The bootstrapping code section is replaced by this one            |
|  Section 3.3, p. 46   | bootu      | meas\_effect\_uncond | Estimate unconditional association measures                       |
|  Section 3.3, p. 50   | lmodboot   | meas\_effect\_uncond | Estimate conditional association measures                         |
|  Section 4.1, p. 60   | bootinside | meas\_effect\_modif  | Estimate effect measure with modifications                        |
|  Section 6.1, p. 101  | stand      | backdr\_out\_npr     | backdoor method via outcome model using non parametric regression |
| Section 6.1.1, p. 106 | standatt   | backdr\_out\_npr     | same as backdr\_out\_npr but with argument `att=TRUE`             |
| Section 6.1.2, p. 111 | standout   | backdr\_out          | backdoor via outcome model using parametric method                |
| Section 6.1.2, p. 112 | standout   | backdr\_out          | Same as previous one but using different parameters               |
