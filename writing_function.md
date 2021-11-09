writing_function
================
Yu He
11/9/2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.0455966483 -0.0004003762 -0.0747515310 -0.0204887473  0.2119287728
    ##  [6] -1.7997583300 -0.7572457806  0.2902834328  0.7778493869 -0.4022531839
    ## [11] -0.3984511412  1.4765239299  0.7362279159 -2.0056962924  1.0958361847
    ## [16] -0.6431192557 -1.4754604685 -0.0939592977  0.3265068024  1.0148461196
    ## [21]  1.5082343626  0.2954490809 -0.2363648591  0.8973874772 -1.7687208504

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  1.0455966483 -0.0004003762 -0.0747515310 -0.0204887473  0.2119287728
    ##  [6] -1.7997583300 -0.7572457806  0.2902834328  0.7778493869 -0.4022531839
    ## [11] -0.3984511412  1.4765239299  0.7362279159 -2.0056962924  1.0958361847
    ## [16] -0.6431192557 -1.4754604685 -0.0939592977  0.3265068024  1.0148461196
    ## [21]  1.5082343626  0.2954490809 -0.2363648591  0.8973874772 -1.7687208504

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1] -1.3271467 -0.3434219 -0.1587692  1.3692987  0.1631537 -1.2171640
    ##  [7] -0.6502987  0.4663733 -0.7397382 -1.1124103 -0.4092254  1.2062021
    ## [13]  1.3934125  0.7958832  0.9247576  0.3147979 -1.2533796  0.9804297
    ## [19] -1.8718878 -0.7611571 -1.1573798  1.1571359  0.1823148 -1.5316417
    ## [25]  0.6969363 -1.0063515 -1.2653484 -0.9376608  1.1138578  0.1113617
    ## [31] -0.8713326  1.1221895 -0.4573817  1.1409832  1.6791269  0.2991740
    ## [37]  0.5556872  1.3865622  0.5355072 -0.5234500

How great is this?

Let’s update our z_score

``` r
z_scores = function(x){
  
   if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  
  z = (x - mean(x)) / sd(x)
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores("gradeA")
```

    ## Error in z_scores("gradeA"): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -1.3271467 -0.3434219 -0.1587692  1.3692987  0.1631537 -1.2171640
    ##  [7] -0.6502987  0.4663733 -0.7397382 -1.1124103 -0.4092254  1.2062021
    ## [13]  1.3934125  0.7958832  0.9247576  0.3147979 -1.2533796  0.9804297
    ## [19] -1.8718878 -0.7611571 -1.1573798  1.1571359  0.1823148 -1.5316417
    ## [25]  0.6969363 -1.0063515 -1.2653484 -0.9376608  1.1138578  0.1113617
    ## [31] -0.8713326  1.1221895 -0.4573817  1.1409832  1.6791269  0.2991740
    ## [37]  0.5556872  1.3865622  0.5355072 -0.5234500
