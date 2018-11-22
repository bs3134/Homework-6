Homework 6
================
Bihui Sun

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
data=read_csv(file="C:/Users/lenovo/Desktop/p8105/data_import_examples/homicide-data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
data=data%>%
  mutate(city_state=str_c(city,state,sep=","))%>%
  mutate(ifsolved=ifelse(disposition=="Closed by arrest","solved","unsolved"))%>%
  filter(!(city_state%in%c("Dallas,TX","Phoenix,AZ","Kansas City,MO","Tulsa,AL")))%>%
  mutate(victim_age=as.numeric(victim_age))%>%
  mutate(victim_race=ifelse(victim_race=="White","white","non-white"))%>%
  mutate(victim_race=as.character(victim_race))%>%
  mutate(victim_race=fct_relevel(victim_race,"white"))%>%
  mutate(ifsolved=as.numeric(ifsolved=="solved"))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

``` r
data  
```

    ## # A tibble: 48,507 x 14
    ##    uid   reported_date victim_last victim_first victim_race victim_age
    ##    <chr>         <int> <chr>       <chr>        <fct>            <dbl>
    ##  1 Alb-~      20100504 GARCIA      JUAN         non-white           78
    ##  2 Alb-~      20100216 MONTOYA     CAMERON      non-white           17
    ##  3 Alb-~      20100601 SATTERFIELD VIVIANA      white               15
    ##  4 Alb-~      20100101 MENDIOLA    CARLOS       non-white           32
    ##  5 Alb-~      20100102 MULA        VIVIAN       white               72
    ##  6 Alb-~      20100126 BOOK        GERALDINE    white               91
    ##  7 Alb-~      20100127 MALDONADO   DAVID        non-white           52
    ##  8 Alb-~      20100127 MALDONADO   CONNIE       non-white           52
    ##  9 Alb-~      20100130 MARTIN-LEY~ GUSTAVO      white               56
    ## 10 Alb-~      20100210 HERRERA     ISRAEL       non-white           43
    ## # ... with 48,497 more rows, and 8 more variables: victim_sex <chr>,
    ## #   city <chr>, state <chr>, lat <dbl>, lon <dbl>, disposition <chr>,
    ## #   city_state <chr>, ifsolved <dbl>

``` r
fit_logistic=data%>%
  filter(city_state=="Baltimore,MD")%>%
  glm(ifsolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())
fit_logistic=broom::tidy(fit_logistic)%>%
  mutate(OR=exp(estimate))
fit_logistic
```

    ## # A tibble: 4 x 6
    ##   term                 estimate std.error statistic  p.value    OR
    ##   <chr>                   <dbl>     <dbl>     <dbl>    <dbl> <dbl>
    ## 1 (Intercept)           1.19      0.235        5.06 4.30e- 7 3.27 
    ## 2 victim_age           -0.00699   0.00326     -2.14 3.22e- 2 0.993
    ## 3 victim_racenon-white -0.820     0.175       -4.69 2.68e- 6 0.441
    ## 4 victim_sexMale       -0.888     0.136       -6.53 6.80e-11 0.412
