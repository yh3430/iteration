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

    ##  [1]  0.96804824 -0.69279741  0.23858771  0.58108881 -0.14004900 -2.32683557
    ##  [7]  1.81696593  0.08724519 -0.22532599 -0.92000657 -1.57784276  0.58601313
    ## [13]  1.25528653  0.36566483  0.46896138 -0.64876835  0.90910766 -0.62470640
    ## [19]  1.72433791 -1.54357370  0.30954069 -0.08338693 -0.61401464  0.43622597
    ## [25] -0.34976667

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.96804824 -0.69279741  0.23858771  0.58108881 -0.14004900 -2.32683557
    ##  [7]  1.81696593  0.08724519 -0.22532599 -0.92000657 -1.57784276  0.58601313
    ## [13]  1.25528653  0.36566483  0.46896138 -0.64876835  0.90910766 -0.62470640
    ## [19]  1.72433791 -1.54357370  0.30954069 -0.08338693 -0.61401464  0.43622597
    ## [25] -0.34976667

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1] -1.093944443  2.291141540 -1.683140989 -0.723967970 -0.162660278
    ##  [6]  0.140190069 -1.108977031 -0.382398892 -1.565334987 -0.535891862
    ## [11]  1.085579144 -0.691070352  0.409111456  0.570325543  1.211291018
    ## [16]  0.171711045 -0.107676679  0.491758697 -1.755208003  1.039347028
    ## [21]  0.132226034  0.729610429 -1.477035876  1.040934701  0.260120313
    ## [26]  0.395019492  0.008723803  1.853845821 -0.124766261  0.442394948
    ## [31] -1.079291021  1.603360894  0.369187646 -0.721848456  0.445908008
    ## [36] -0.241963975 -2.066043547  0.573224644  0.199808994  0.056399351

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

    ##  [1] -1.093944443  2.291141540 -1.683140989 -0.723967970 -0.162660278
    ##  [6]  0.140190069 -1.108977031 -0.382398892 -1.565334987 -0.535891862
    ## [11]  1.085579144 -0.691070352  0.409111456  0.570325543  1.211291018
    ## [16]  0.171711045 -0.107676679  0.491758697 -1.755208003  1.039347028
    ## [21]  0.132226034  0.729610429 -1.477035876  1.040934701  0.260120313
    ## [26]  0.395019492  0.008723803  1.853845821 -0.124766261  0.442394948
    ## [31] -1.079291021  1.603360894  0.369187646 -0.721848456  0.445908008
    ## [36] -0.241963975 -2.066043547  0.573224644  0.199808994  0.056399351

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
    ## 1  12.0 0.378

## different sample size, means, and sds

``` r
sim_data =
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.52  3.02

## Multiple input

using a function that simulates data, computes the mean and sd.

tricky thing - which is things that are different inside or outside a
function?

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4){
  
  # do checks on inputs
  
  sim_data =
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
   
}


sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.52  2.82

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.56  4.10

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
```

More pages of reviews

write a function that gets reviews based on page url.

``` r
get_page_reviews = function(page_url){
  
  page_html = read_html(page_url)

review_titles = 
  page_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  page_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  page_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )

return(reviews)
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  2 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  3 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  4 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  5 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  7 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  8 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  9 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ## 10 Painful                                               1.0 ou~ "\n  I think I~

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  2 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  3 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  4 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  5 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  7 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  8 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  9 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ## 10 Painful                                               1.0 ou~ "\n  I think I~
    ## # ... with 40 more rows
