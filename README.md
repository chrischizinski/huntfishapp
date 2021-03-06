
<!-- README.md is generated from README.Rmd. Please edit that file -->

# huntfishapp

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/chrischizinski/huntfishapp.svg?branch=master)](https://travis-ci.com/chrischizinski/huntfishapp)
<!-- badges: end -->

The goal of **huntfishapp** is to provide state and provincial fish and
wildlife agencies and non-governmental organizations with a web-based,
exploratory data analysis application for hunting, fishing, and outdoor
recreation sales data.

  - **Package Website:** <https://chrischizinski.github.io/huntfishapp>
  - **Source Code:** <https://github.com/chrischizinski/huntfishapp>
  - **Live Demo App:** <https://chrischizinski.shinyapps.io/shiny_app/>

<img src="man/figures/app_screenshot.png" align="center" />

## Installation

The **huntfishapp** package is currently only available from Github.

``` r
# Install development version from GitHub
# install.packages("remotes")

remotes::install_github("chrischizinski/huntfishapp", build_vignettes = TRUE)
```

## Getting Started

To preview the app locally simply install the package and run
`launchApp()`.

``` r
library(huntfishapp)
launchApp()
```

For details on how to connect **huntfishapp** with your own database see
the installation and setup vignette
`vignette("installation-and-setup")`.

## Graphics Modules

### Revenue

<img src="man/figures/revenue.png" align="center" />

### Customers

<img src="man/figures/customers.png" align="center" />

### Gender Ratio

<img src="man/figures/gender.png" align="center" />

### Age Distribution

<img src="man/figures/age.png" align="center" />

### Map

<img src="man/figures/map.png" align="center" />

### Recruitment

<img src="man/figures/recruitment.png" align="center" />

### Churn

<img src="man/figures/churn.png" align="center" />

### Item Combinations (UpSet plot)

<img src="man/figures/upset.png" align="center" />

### Item Combinations (RadialSets plot)

<img src="man/figures/radialsets.png" align="center" />

## Code of Conduct

Please note that the ‘huntfishapp’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

## Acknowledgments

This project was funded by Federal Aid in Sport Fish Restoration and
Federal Aid in Wildlife Restoration project FW-23-R, which was
administered by the Nebraska Game and Parks Commission. Any use of
trade, firm, or product names is for descriptive purposes only and does
not imply endorsement by the U.S. Government. CJC was supported by Hatch
funds through the Agricultural Research Division at the University of
Nebraska-Lincoln and from Federal Aid in Wildlife Restoration project
W-120-T, administered by the Nebraska Game and Parks Commission.
