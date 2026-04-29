
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eyemovements

<!-- badges: start -->

<!-- badges: end -->

Tools for processing raw eye-tracking samples and detecting fixations
and saccades.

## Installation

You can install the package from Github by running the following code:

``` r
# Check if "devtools" is installed:
if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
devtools::install_github("martin-vasilev/eyemovements")
```

## Sample data:

The package has some in-built datasets to use as an example:

1.  `"Oz" dataset`, containing the raw samples of a participant reading
    a page from the “Little Wizard Stories of Oz” (Slattery & Vasilev,
    2019). Data was recorded with an Eyelink 1000+ at 1000 Hz
    (monocular, from the right eye):

``` r
library(eyemovements)
dat= data_Oz
str(dat)
#> 'data.frame':    32586 obs. of  4 variables:
#>  $ time : num  1322956 1322957 1322958 1322959 1322960 ...
#>  $ x    : num  962 962 962 962 962 ...
#>  $ y    : num  541 541 541 540 539 ...
#>  $ pupil: num  1753 1752 1749 1747 1746 ...
```
