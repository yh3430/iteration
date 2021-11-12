Iteration and Listcols
================
Yu He
11/11/2021

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

## Define function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  
}
```

## List

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, FALSE),
  summary = summary(rnorm(1000, mean = 5, sd = 3))
  
)

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.425   2.994   4.962   5.081   7.155  15.285

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.425   2.994   4.962   5.081   7.155  15.285

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.425   2.994   4.962   5.081   7.155  15.285

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.86  1.20

## for loop

Let’s use a for loop to iterate ovr my list of normals

``` r
output = vector("list", length = 4)


for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
  
}
```

Let’s use map instead

``` r
output = map(list_norms, mean_and_sd)

output = map(list_norms, summary)

output = map_dbl(list_norms, median)
```

## list columns!!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )


listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  2.63316702  1.51946719  3.88109720  2.87698300  1.86224161  0.84078069
    ##  [7]  3.63156934  3.06070696  0.17806170  0.22300924  0.35362372  2.08416832
    ## [13]  1.49461659  4.03161683  2.05317651  0.23797370  2.42770714  2.07182173
    ## [19]  0.47028001  0.62969193 -0.85639621  0.82738673  2.11043161  0.05643716
    ## [25]  2.28820972  4.03779962  1.99103000  1.21857729  1.15964993  2.07588804
    ## [31]  1.87423723  0.83101394  3.94317280  1.39224694  2.34998690  2.89076983
    ## [37]  3.41499204  3.17293892  0.62908782  1.15576651  0.52682637  2.42183362
    ## [43]  2.03304804  0.27064162  2.57054500  2.74258368  3.25021860  1.19669369
    ## [49]  2.38989982  2.62903690
    ## 
    ## $b
    ##  [1]  4.3460875  0.8920772  2.0730217 15.8504590 10.8736295  2.2121391
    ##  [7]  7.5122074  5.4770165  4.4254032  5.3399015 10.6829037  8.0027785
    ## [13]  9.0384323  6.5010089  4.6957038  4.7448914  4.2681054 14.3537875
    ## [19]  8.9740837  7.2895813  2.6793429  7.3724033 -0.5091259  3.5974686
    ## [25]  5.6711085  8.1406470  1.9682244  1.0329324  5.3580563  6.8655978
    ## [31]  6.5963618  6.9718385  4.0238565  6.3835362  4.7798989  2.1095486
    ## [37]  4.3749498  6.2536155 -1.6157046  7.6571036  8.4986618  4.2500106
    ## [43]  0.7920124  9.5483889  2.3399912  2.8449688  4.3201097  2.1637320
    ## [49]  9.8337373  4.7242954
    ## 
    ## $c
    ##  [1] 22.24406 19.92520 20.17474 17.69037 21.05053 19.07624 21.38500 19.17705
    ##  [9] 18.81574 21.46447 20.09603 19.95959 19.83755 19.37311 18.41773 19.05336
    ## [17] 19.25974 19.66781 19.02451 21.07937 21.21179 20.20120 21.26968 19.09984
    ## [25] 21.61534 18.60107 19.59086 17.60025 20.26059 18.66027 19.98347 20.83129
    ## [33] 18.12259 20.33934 21.57993 20.29368 20.86446 21.71663 19.22611 19.96136
    ## [41] 19.10813 21.18450 20.07755 20.28213 20.31227 21.17320 20.46863 21.01201
    ## [49] 20.23106 19.43315
    ## 
    ## $d
    ##  [1] -12.02222 -12.58414 -11.71217 -12.28112 -11.47796 -12.46223 -11.62467
    ##  [8] -12.35068 -11.81757 -12.44567 -12.01559 -12.09449 -12.45380 -12.50049
    ## [15] -12.09610 -11.67383 -12.10486 -12.87413 -11.09002 -11.46871 -11.82432
    ## [22] -11.01240 -11.30874 -12.10963 -12.18097 -12.20998 -12.05388 -11.83215
    ## [29] -11.56040 -11.73096 -11.84531 -12.53033 -13.12644 -11.28339 -11.78493
    ## [36] -11.98106 -12.38774 -11.73754 -12.10154 -11.32757 -10.86448 -12.21654
    ## [43] -11.50799 -12.08581 -12.51756 -11.33500 -12.31227 -11.07400 -11.80913
    ## [50] -11.89717

``` r
listcol_df %>% 
  mutate(summaries = map(norms, mean_and_sd)) %>% 
  pull(summaries)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.86  1.20
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.53  3.49
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.0  1.09
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9 0.489

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\IT\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-11 20:39:39 (7.62)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: C:\Users\IT\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-11 20:36:57 (1.701)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: C:\Users\IT\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-11 20:39:53 (0.914)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

lm(tmax ~ tmin, data = weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_nested = nest(weather_df, data = date:tmin)

weath_lm = function(df){
  
  
  lm(tmax ~ tmin, data = df)

}


weath_lm(weather_nested$data
         [[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weath_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(
    lm_results = map(data, weath_lm)
  )
```

    ## # A tibble: 3 x 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 x 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 x 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 x 4]> <lm>

## Example - napoleon

Function to get review stars

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

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou~ "\n  mad good ~
    ##  2 Fun!                                                  4.0 ou~ "\n  Fun and e~
    ##  3 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  4 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  5 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  6 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  7 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  9 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ## 10 Classic Film                                          5.0 ou~ "\n  Had to or~
    ## 
    ## [[2]]
    ## # A tibble: 10 x 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 hehehehe                          5.0 out of 5 stars "\n  goodjobboys\n"     
    ##  2 Painful                           1.0 out of 5 stars "\n  I think I sneezed ~
    ##  3 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  4 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie~
    ##  5 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my~
    ##  6 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio~
    ##  7 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  8 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i~
    ##  9 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi~
    ## 10 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie~
    ## 
    ## [[3]]
    ## # A tibble: 10 x 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing ~
    ##  2 A classic                                   5.0 out of 5 stars "\n  If you d~
    ##  3 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g~
    ##  4 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t~
    ##  5 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf~
    ##  6 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  7 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo~
    ##  8 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  9 Love it                                     5.0 out of 5 stars "\n  What of ~
    ## 10 WORTH IT!                                   5.0 out of 5 stars "\n  It's the~
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    title                                           stars              text      
    ##    <chr>                                           <chr>              <chr>     
    ##  1 "Funny movie."                                  5.0 out of 5 stars "\n  Grea~
    ##  2 "Best movie ever!"                              5.0 out of 5 stars "\n  Got ~
    ##  3 "I was stuck in the oil patch back in the day." 5.0 out of 5 stars "\n  I wa~
    ##  4 "Funny Dork humor"                              5.0 out of 5 stars "\n  Humo~
    ##  5 "Still funny!"                                  5.0 out of 5 stars "\n  Stil~
    ##  6 "Love it!! \U0001f49c"                          5.0 out of 5 stars "\n  Love~
    ##  7 "LOVE it"                                       5.0 out of 5 stars "\n  cult~
    ##  8 "Perfect"                                       5.0 out of 5 stars "\n  Exac~
    ##  9 "Love this movie!"                              5.0 out of 5 stars "\n  Grea~
    ## 10 "Love it"                                       5.0 out of 5 stars "\n  Love~
    ## 
    ## [[5]]
    ## # A tibble: 10 x 3
    ##    title                               stars              text                  
    ##    <chr>                               <chr>              <chr>                 
    ##  1 "As described"                      3.0 out of 5 stars "\n  Book is as descr~
    ##  2 "GOSH!!!"                           5.0 out of 5 stars "\n  Just watch the m~
    ##  3 "Watch it right now"                5.0 out of 5 stars "\n  You need to watc~
    ##  4 "At this point it’s an addiction"   5.0 out of 5 stars "\n  I watch this mov~
    ##  5 "\U0001f495"                        5.0 out of 5 stars "\n  Hands down, one ~
    ##  6 "Good dumb movie"                   5.0 out of 5 stars "\n  I really wanted ~
    ##  7 "funny"                             5.0 out of 5 stars "\n  so funny and inv~
    ##  8 "Best Movie- Try to prove me wrong" 5.0 out of 5 stars "\n  Best movie ever\~
    ##  9 "Vote For Pedro!!"                  5.0 out of 5 stars "\n  What is NOT to l~
    ## 10 "So Funny"                          5.0 out of 5 stars "\n  This is such a g~

``` r
napoleon_df = 
  tibble(
    urls = urls
)
```

``` r
napoleon_df %>% 
  mutate(reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou~ "\n  mad good ~
    ##  2 Fun!                                                  4.0 ou~ "\n  Fun and e~
    ##  3 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  4 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  5 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  6 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  7 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  9 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ## 10 Classic Film                                          5.0 ou~ "\n  Had to or~
    ## # ... with 40 more rows
