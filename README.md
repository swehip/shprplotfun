
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shprplotfun

The goal of shprplotfun is to provide functionality to produce the
annual report for the Swedish Hip Arthroplasty Register.

## Installation

You can install the latest version of shprplotfun with:

``` r
# install.packages("remotes")
remotes::install_github("swehip/shprplotfun")
```

Sometimes it occurs errors with the documention. Might work by
restarting the session with

``` r
.rs.restartR()
```

## Arial narrow extra font

The package requires *Arial Narrow* loaded through the `extrafont`
package. This can be time consuming and opted out through the
`shprplotfun_read_fonts` option. The default for this `interactive()`.
To skip the font option use:

``` r
options(shprplotfun_read_fonts = FALSE)
```

Note that in a Linux environment you may need to install the MS fonts,
e.g. for Ubuntu run `sudo apt-get install ttf-mscorefonts-installer`.
