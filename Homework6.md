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
data_Bal=data%>%
  filter(city_state=="Baltimore,MD")
model=glm(ifsolved ~ victim_age + victim_race + victim_sex, data = data_Bal, family = binomial())
fit_logistic=broom::tidy(model)
fit_logistic%>%
  mutate(OR=exp(estimate))%>%
  mutate(upperCI=exp(estimate+qnorm(0.975)*std.error))%>%
  mutate(lowerCI=exp(estimate-qnorm(0.975)*std.error))
```

    ## # A tibble: 4 x 8
    ##   term         estimate std.error statistic  p.value    OR upperCI lowerCI
    ##   <chr>           <dbl>     <dbl>     <dbl>    <dbl> <dbl>   <dbl>   <dbl>
    ## 1 (Intercept)   1.19      0.235        5.06 4.30e- 7 3.27    5.19    2.07 
    ## 2 victim_age   -0.00699   0.00326     -2.14 3.22e- 2 0.993   0.999   0.987
    ## 3 victim_race~ -0.820     0.175       -4.69 2.68e- 6 0.441   0.620   0.313
    ## 4 victim_sexM~ -0.888     0.136       -6.53 6.80e-11 0.412   0.537   0.315

``` r
allcity=data%>%
  select(city_state,ifsolved,victim_age,victim_race,victim_sex)%>%
  group_by(city_state)%>%
  nest()%>%
  mutate(newmodel=map(data,~glm(ifsolved ~ victim_age + victim_race + victim_sex, data= .x,family = binomial())))%>%
  mutate(result=map(newmodel,broom::tidy))%>%
  select(city_state,result)%>%
  unnest()%>%
  mutate(city_OR=exp(estimate))%>%
  mutate(upperCI=exp(estimate+qnorm(0.975)*std.error))%>%
  mutate(lowerCI=exp(estimate-qnorm(0.975)*std.error))%>%
  select(city_state,term,city_OR,upperCI,lowerCI)
allcity
```

    ## # A tibble: 211 x 5
    ##    city_state     term                 city_OR upperCI lowerCI
    ##    <chr>          <chr>                  <dbl>   <dbl>   <dbl>
    ##  1 Albuquerque,NM (Intercept)            3.43    7.27    1.62 
    ##  2 Albuquerque,NM victim_age             0.977   0.991   0.964
    ##  3 Albuquerque,NM victim_racenon-white   0.741   1.22    0.451
    ##  4 Albuquerque,NM victim_sexMale         1.58    2.77    0.895
    ##  5 Albuquerque,NM victim_sexUnknown      8.19   30.8     2.18 
    ##  6 Atlanta,GA     (Intercept)            3.17    6.43    1.56 
    ##  7 Atlanta,GA     victim_age             0.988   0.997   0.979
    ##  8 Atlanta,GA     victim_racenon-white   0.753   1.31    0.432
    ##  9 Atlanta,GA     victim_sexMale         0.990   1.44    0.679
    ## 10 Baltimore,MD   (Intercept)            3.27    5.19    2.07 
    ## # ... with 201 more rows
