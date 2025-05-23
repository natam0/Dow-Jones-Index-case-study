Dow Jones Index Forecasting
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(e1071)
library(tree)
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
dow_data = read_csv("dow_jones_index.data")
```

    ## Rows: 750 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): stock, date, open, high, low, close, next_weeks_open, next_weeks_close
    ## dbl (8): quarter, volume, percent_change_price, percent_change_volume_over_l...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
str(dow_data)
```

    ## spc_tbl_ [750 × 16] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ quarter                           : num [1:750] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ stock                             : chr [1:750] "AA" "AA" "AA" "AA" ...
    ##  $ date                              : chr [1:750] "1/7/2011" "1/14/2011" "1/21/2011" "1/28/2011" ...
    ##  $ open                              : chr [1:750] "$15.82" "$16.71" "$16.19" "$15.87" ...
    ##  $ high                              : chr [1:750] "$16.72" "$16.71" "$16.38" "$16.63" ...
    ##  $ low                               : chr [1:750] "$15.78" "$15.64" "$15.60" "$15.82" ...
    ##  $ close                             : chr [1:750] "$16.42" "$15.97" "$15.79" "$16.13" ...
    ##  $ volume                            : num [1:750] 2.40e+08 2.43e+08 1.38e+08 1.51e+08 1.54e+08 ...
    ##  $ percent_change_price              : num [1:750] 3.79 -4.43 -2.47 1.64 5.93 ...
    ##  $ percent_change_volume_over_last_wk: num [1:750] NA 1.38 -43.02 9.36 1.99 ...
    ##  $ previous_weeks_volume             : num [1:750] NA 2.40e+08 2.43e+08 1.38e+08 1.51e+08 ...
    ##  $ next_weeks_open                   : chr [1:750] "$16.71" "$16.19" "$15.87" "$16.18" ...
    ##  $ next_weeks_close                  : chr [1:750] "$15.97" "$15.79" "$16.13" "$17.14" ...
    ##  $ percent_change_next_weeks_price   : num [1:750] -4.428 -2.471 1.638 5.933 0.231 ...
    ##  $ days_to_next_dividend             : num [1:750] 26 19 12 5 97 90 83 76 69 62 ...
    ##  $ percent_return_next_dividend      : num [1:750] 0.183 0.188 0.19 0.186 0.175 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   quarter = col_double(),
    ##   ..   stock = col_character(),
    ##   ..   date = col_character(),
    ##   ..   open = col_character(),
    ##   ..   high = col_character(),
    ##   ..   low = col_character(),
    ##   ..   close = col_character(),
    ##   ..   volume = col_double(),
    ##   ..   percent_change_price = col_double(),
    ##   ..   percent_change_volume_over_last_wk = col_double(),
    ##   ..   previous_weeks_volume = col_double(),
    ##   ..   next_weeks_open = col_character(),
    ##   ..   next_weeks_close = col_character(),
    ##   ..   percent_change_next_weeks_price = col_double(),
    ##   ..   days_to_next_dividend = col_double(),
    ##   ..   percent_return_next_dividend = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Remove '$' and ',' from price columns and convert to numeric
price_cols <- c("open", "high", "low", "close", "next_weeks_open", "next_weeks_close")

dow_data <- dow_data %>%
  mutate(across(all_of(price_cols), ~ as.numeric(gsub("[$,]", "", .))))

# Fix percentage columns
dow_data <- dow_data %>%
  mutate(across(starts_with("percent_change"), as.numeric))

# Convert categorical variables
dow_data$quarter <- as.factor(dow_data$quarter)
dow_data$stock <- as.factor(dow_data$stock)
dow_data$date = as.Date(dow_data$date, format = "%m/%d/%Y")
```

``` r
str(dow_data)
```

    ## tibble [750 × 16] (S3: tbl_df/tbl/data.frame)
    ##  $ quarter                           : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ stock                             : Factor w/ 30 levels "AA","AXP","BA",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ date                              : Date[1:750], format: "2011-01-07" "2011-01-14" ...
    ##  $ open                              : num [1:750] 15.8 16.7 16.2 15.9 16.2 ...
    ##  $ high                              : num [1:750] 16.7 16.7 16.4 16.6 17.4 ...
    ##  $ low                               : num [1:750] 15.8 15.6 15.6 15.8 16.2 ...
    ##  $ close                             : num [1:750] 16.4 16 15.8 16.1 17.1 ...
    ##  $ volume                            : num [1:750] 2.40e+08 2.43e+08 1.38e+08 1.51e+08 1.54e+08 ...
    ##  $ percent_change_price              : num [1:750] 3.79 -4.43 -2.47 1.64 5.93 ...
    ##  $ percent_change_volume_over_last_wk: num [1:750] NA 1.38 -43.02 9.36 1.99 ...
    ##  $ previous_weeks_volume             : num [1:750] NA 2.40e+08 2.43e+08 1.38e+08 1.51e+08 ...
    ##  $ next_weeks_open                   : num [1:750] 16.7 16.2 15.9 16.2 17.3 ...
    ##  $ next_weeks_close                  : num [1:750] 16 15.8 16.1 17.1 17.4 ...
    ##  $ percent_change_next_weeks_price   : num [1:750] -4.428 -2.471 1.638 5.933 0.231 ...
    ##  $ days_to_next_dividend             : num [1:750] 26 19 12 5 97 90 83 76 69 62 ...
    ##  $ percent_return_next_dividend      : num [1:750] 0.183 0.188 0.19 0.186 0.175 ...

``` r
stocks = unique(dow_data$stock)
```

## Create a linear model for each stock

``` r
columns= c("Stock","MSE") 
lm_acc_df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(lm_acc_df) = columns


for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  lm_fit = lm(percent_change_next_weeks_price ~ previous_weeks_volume, 
                data = train_stock)
  lm_preds  <- predict(lm_fit, test_stock)
  lm_mse  <- mean((lm_preds - test_stock$percent_change_next_weeks_price)^2)
  vec = c(i, lm_mse)
  lm_acc_df[i, ] = vec
}
```

## Create a svr model for each stock

``` r
columns= c("Stock","MSE") 
svr_acc_df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(svr_acc_df) = columns


for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  svr_fit = svm(percent_change_next_weeks_price ~ previous_weeks_volume, 
                data = train_stock, kernel = "radial")
  svr_preds  <- predict(svr_fit, test_stock)
  svr_mse  <- mean((svr_preds - test_stock$percent_change_next_weeks_price)^2)
  vec = c(i, svr_mse)
  svr_acc_df[i, ] = vec
}
```

## Create a decision tree model for each stock

``` r
columns= c("Stock","MSE") 
tree_acc_df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(tree_acc_df) = columns


for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  tree_fit = tree(percent_change_next_weeks_price ~ previous_weeks_volume, 
                data = train_stock, method = "anova")
  tree_preds  <- predict(tree_fit, test_stock)
  tree_mse  <- mean((tree_preds - test_stock$percent_change_next_weeks_price)^2)
  vec = c(i, tree_mse)
  tree_acc_df[i, ] = vec
}
```

## See which model is best

``` r
lm_mse = mean(as.numeric(lm_acc_df$MSE))
svr_mse = mean(as.numeric(svr_acc_df$MSE))
tree_mse = mean(as.numeric(tree_acc_df$MSE))

results <- tibble(
  Model = c("Linear Regression", "Decision Tree", "SVR"),
  MSE = c(lm_mse, tree_mse, svr_mse)
)

print(results)
```

    ## # A tibble: 3 × 2
    ##   Model               MSE
    ##   <chr>             <dbl>
    ## 1 Linear Regression  9.13
    ## 2 Decision Tree      9.15
    ## 3 SVR                8.44

## Create a linear stepwise model for each stock

``` r
columns= c("Stock","MSE") 
step_lm_acc_df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(step_lm_acc_df) = columns


for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  stock_df = na.omit(stock_df)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  lm_fit = lm(percent_change_next_weeks_price ~ percent_change_price +
                percent_change_volume_over_last_wk + days_to_next_dividend + 
                percent_return_next_dividend + high + low + close + volume +
                previous_weeks_volume, 
                data = train_stock)
  step_lm = step(lm_fit, direction = "both")
  lm_preds  <- predict(step_lm, test_stock)
  lm_mse  <- mean((lm_preds - test_stock$percent_change_next_weeks_price)^2)
  vec = c(i, lm_mse)
  step_lm_acc_df[i, ] = vec
}
```

## See which model is best

``` r
lm_mse = mean(as.numeric(lm_acc_df$MSE))
step_lm_mse = mean(as.numeric(step_lm_acc_df$MSE))
svr_mse = mean(as.numeric(svr_acc_df$MSE))
tree_mse = mean(as.numeric(tree_acc_df$MSE))

results <- tibble(
  Model = c("Linear Regression", "Decision Tree", "SVR", "Step LM"),
  MSE = c(lm_mse, tree_mse, svr_mse, step_lm_mse)
)

print(results)
```

    ## # A tibble: 4 × 2
    ##   Model                 MSE
    ##   <chr>               <dbl>
    ## 1 Linear Regression    9.13
    ## 2 Decision Tree        9.15
    ## 3 SVR                  8.44
    ## 4 Step LM           3786.

## Create Lag variables

``` r
for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  lag.plot(train_stock$percent_change_next_weeks_price, set.lags = 1:4)
}
```

![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-17.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-18.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-19.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-20.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-21.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-23.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-24.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-25.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-26.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-27.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-28.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-29.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-13-30.png)<!-- -->

``` r
for (i in stocks) {
  stock_df = dow_data %>%
    filter(dow_data$stock == i)
  train_stock = stock_df %>% 
    filter(quarter == 1)
  test_stock = stock_df %>% 
    filter(quarter == 2)
  lag_train_stock = train_stock %>% 
    mutate(
      lag1_previous_weeks_volume = lag(previous_weeks_volume, 1),
      lag2_previous_weeks_volume = lag(previous_weeks_volume, 2)
      )
  lag_train_stock = na.omit(lag_train_stock)
  par(mfrow = c(1,2))
  plot(y = lag_train_stock$percent_change_next_weeks_price, x = lag_train_stock$lag1_previous_weeks_volume)
  plot(y = lag_train_stock$percent_change_next_weeks_price, x = lag_train_stock$lag2_previous_weeks_volume)
}
```

![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-16.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-17.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-18.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-19.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-20.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-21.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-22.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-23.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-24.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-25.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-26.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-27.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-28.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-29.png)<!-- -->![](Case_Study_3_files/figure-gfm/unnamed-chunk-14-30.png)<!-- -->

## CAPM

``` r
dji = read_csv("DJI_CaseStudy3.csv")
```

    ## New names:
    ## Rows: 33 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," num
    ## (5): Open, High, Low, Close , Adj Close lgl (1): ...7 date (1): Date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...7`

``` r
dji = dji[,c(1:6)]
dji = na.omit(dji)
```

``` r
dji_df = as.data.frame(dji[,])
```

``` r
return_dji = na.omit(quantmod::Delt(dji_df[,5]))
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
return_df = data.frame(matrix(nrow = 24, ncol = 0)) 

for (i in stocks) {
  stock_df = dow_data %>%
  filter(dow_data$stock == i)
  stock_df = stock_df %>% 
    select(open, high, low, close)
  stock_df = as.data.frame(stock_df)
  return = na.omit(quantmod::Delt(stock_df[,4]))
  return_df = cbind(return_df, return)
}
```

``` r
return_data = cbind(return_df, return_dji)
colnames(return_data) = c(lm_acc_df$Stock, "DJI")
```

``` r
returns_mean = apply(return_data,2,mean)
returns_sd = apply(return_data,2,sd)
cbind(returns_mean,returns_sd)
```

    ##       returns_mean returns_sd
    ## AA   -0.0025562614 0.03450483
    ## AXP   0.0039787288 0.02867944
    ## BA    0.0014706601 0.02740461
    ## BAC  -0.0120307527 0.03342655
    ## CAT   0.0032371593 0.03336199
    ## CSCO -0.0132726501 0.03920114
    ## CVX   0.0032523639 0.02448295
    ## DD    0.0021434643 0.02664204
    ## DIS  -0.0016628865 0.02746833
    ## GE   -0.0007380844 0.02560781
    ## HD    0.0011017053 0.02335630
    ## HPQ  -0.0098428976 0.03914964
    ## IBM   0.0047343827 0.01806670
    ## INTC  0.0015586246 0.03211640
    ## JNJ   0.0018110711 0.02068670
    ## JPM  -0.0039127044 0.02246357
    ## KRFT  0.0044823469 0.01777007
    ## KO    0.0014376506 0.01628497
    ## MCD   0.0041737383 0.01919738
    ## MMM   0.0023999818 0.02069039
    ## MRK  -0.0028985966 0.02654569
    ## MSFT -0.0065998397 0.01858821
    ## PFE   0.0041023671 0.02598915
    ## PG   -0.0011006261 0.01777888
    ## T     0.0024128625 0.01926273
    ## TRV   0.0027203616 0.01934159
    ## UTX   0.0028884330 0.02084871
    ## VZ    0.0002326181 0.01783121
    ## WMT  -0.0011305653 0.01894121
    ## XOM   0.0009597025 0.02533114
    ## DJI   0.0010167236 0.01440911

#### Calculate Betas

``` r
calculate_betas <- function(return_data) {
  columns <- c("Stock", "Beta")
  betas <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(betas) <- columns
  
  stock_names <- colnames(return_data)[colnames(return_data) != "DJI"]
  
  for (stock in stock_names) {
    regression_data <- data.frame(
      stock_return = return_data[[stock]],
      market_return = return_data$DJI
    )

    lm_model <- lm(stock_return ~ market_return, data = regression_data)
    #svr_model <- svm(stock_return ~ market_return, data = regression_data, kernel = "linear")
    beta <- coef(lm_model)[2]
    betas <- rbind(betas, data.frame(Stock = stock, Beta = beta))
  }
  rownames(betas) = NULL
  
  return(betas)
}

betas <- calculate_betas(return_data)
print(betas)
```

    ##    Stock      Beta
    ## 1     AA 1.2996180
    ## 2    AXP 1.0879594
    ## 3     BA 1.6229882
    ## 4    BAC 0.6510668
    ## 5    CAT 1.4917179
    ## 6   CSCO 0.6541192
    ## 7    CVX 0.8431656
    ## 8     DD 1.1270510
    ## 9    DIS 1.4847032
    ## 10    GE 1.3526311
    ## 11    HD 0.9854805
    ## 12   HPQ 1.4221849
    ## 13   IBM 0.9208968
    ## 14  INTC 1.2509146
    ## 15   JNJ 0.7594810
    ## 16   JPM 0.8660191
    ## 17  KRFT 0.1895133
    ## 18    KO 0.6797966
    ## 19   MCD 0.7435486
    ## 20   MMM 1.2372817
    ## 21   MRK 0.5415464
    ## 22  MSFT 0.7130323
    ## 23   PFE 0.7249845
    ## 24    PG 0.4849129
    ## 25     T 0.8039250
    ## 26   TRV 0.9988462
    ## 27   UTX 1.1975557
    ## 28    VZ 0.7601467
    ## 29   WMT 0.6854153
    ## 30   XOM 1.1963249

#### SVR for predictions

``` r
columns <- c("Stock", "MSE") 
svr_acc_df <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(svr_acc_df) <- columns

for (stock_name in stocks) {
  stock_df <- dow_data %>%
    filter(stock == stock_name)
  
  train_stock <- stock_df %>% 
    filter(quarter == 1)
  test_stock <- stock_df %>% 
    filter(quarter == 2)
  
  svr_fit <- svm(percent_change_next_weeks_price ~ previous_weeks_volume, 
                data = train_stock, kernel = "radial")
  
  svr_preds <- predict(svr_fit, test_stock)
  
  svr_mse <- mean((svr_preds - test_stock$percent_change_next_weeks_price)^2)
  
  svr_acc_df <- rbind(svr_acc_df, data.frame(Stock = stock_name, MSE = svr_mse))
}

# Convert MSE column to numeric (if needed)
svr_acc_df$MSE <- as.numeric(as.character(svr_acc_df$MSE))

# Display results, sorted by MSE
svr_acc_df <- svr_acc_df %>% arrange(MSE)
print(svr_acc_df)
```

    ##    Stock       MSE
    ## 1     KO  2.011054
    ## 2      T  2.022104
    ## 3    WMT  2.295598
    ## 4    MCD  2.982867
    ## 5   KRFT  2.986695
    ## 6    IBM  3.031101
    ## 7     VZ  5.026785
    ## 8    MRK  5.234445
    ## 9     PG  5.476923
    ## 10    HD  5.636685
    ## 11   BAC  5.958973
    ## 12   MMM  6.053698
    ## 13   JNJ  6.319670
    ## 14    GE  6.928553
    ## 15   PFE  7.189441
    ## 16   TRV  7.291856
    ## 17   JPM  7.571263
    ## 18    BA  7.946320
    ## 19   UTX  9.406524
    ## 20    DD 10.442583
    ## 21  MSFT 10.454202
    ## 22   AXP 10.877980
    ## 23   CVX 11.257649
    ## 24   XOM 11.318532
    ## 25  CSCO 12.326586
    ## 26   DIS 13.086128
    ## 27  INTC 14.199438
    ## 28    AA 16.864395
    ## 29   HPQ 17.522851
    ## 30   CAT 23.458176
