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

    ##  [1]  0.16528489 -0.09703669  0.83374813 -0.35427180 -0.29152830  0.38239250
    ##  [7] -2.52799068  0.15428917  1.02788962  0.64520210 -0.28858048  2.21027606
    ## [13] -0.46119477  0.11912460 -0.09140329  0.12982965  1.26769894 -1.17342289
    ## [19]  0.66068562  0.46476794  1.21534360 -1.13192333 -1.15442093 -0.29438134
    ## [25] -1.41037832

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.16528489 -0.09703669  0.83374813 -0.35427180 -0.29152830  0.38239250
    ##  [7] -2.52799068  0.15428917  1.02788962  0.64520210 -0.28858048  2.21027606
    ## [13] -0.46119477  0.11912460 -0.09140329  0.12982965  1.26769894 -1.17342289
    ## [19]  0.66068562  0.46476794  1.21534360 -1.13192333 -1.15442093 -0.29438134
    ## [25] -1.41037832

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1] -0.0283424787  0.0827346915 -0.3899359684 -0.9880498834  2.5929243391
    ##  [6]  0.3415918795  1.5393712665  1.3943959862  0.1495693420 -0.8018941207
    ## [11]  0.7751614496  0.6386825036 -0.5504583554  0.9652008497  1.4122796155
    ## [16]  0.0024721090  0.6869519048 -1.0292450466 -0.2470494775 -1.0858730862
    ## [21] -0.6486526540 -0.6207145730  0.0004975427  0.2691770301 -0.3700321729
    ## [26]  0.6715836845  0.8109231277 -0.4334820521 -1.4637870422  0.6450186525
    ## [31] -2.5561279299 -0.8547496999  0.6683041016 -0.3069262483 -0.6299832397
    ## [36]  1.2415132838  0.7770885177 -1.4275658511 -1.3303620537  0.0977900561

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

    ##  [1] -0.0283424787  0.0827346915 -0.3899359684 -0.9880498834  2.5929243391
    ##  [6]  0.3415918795  1.5393712665  1.3943959862  0.1495693420 -0.8018941207
    ## [11]  0.7751614496  0.6386825036 -0.5504583554  0.9652008497  1.4122796155
    ## [16]  0.0024721090  0.6869519048 -1.0292450466 -0.2470494775 -1.0858730862
    ## [21] -0.6486526540 -0.6207145730  0.0004975427  0.2691770301 -0.3700321729
    ## [26]  0.6715836845  0.8109231277 -0.4334820521 -1.4637870422  0.6450186525
    ## [31] -2.5561279299 -0.8547496999  0.6683041016 -0.3069262483 -0.6299832397
    ## [36]  1.2415132838  0.7770885177 -1.4275658511 -1.3303620537  0.0977900561

## multiple outputs

restart R or ‘rm(x)’

``` r
mean_and_sd = function(x){
  
   if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  Mean_x = mean(x)
  sd_x = sd(x)
  
  output_df =
    tibble(
      mean = Mean_x,
      sd = sd_x
    )
 
  return(output_df)
}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.298
