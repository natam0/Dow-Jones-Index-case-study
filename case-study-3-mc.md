Dow Jones Index Monte Carlo Sim
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(purrr)
library(tibble)
stocks = read_csv("dow_jones_index.data")
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
price_cols <- c("open", "high", "low", "close", "next_weeks_open", "next_weeks_close")

stocks <- stocks %>%
  mutate(across(all_of(price_cols), ~ as.numeric(gsub("[$,]", "", .))))

# Fix percentage columns
stocks <- stocks %>%
  mutate(across(starts_with("percent_change"), as.numeric))

# Convert categorical variables
stocks$quarter <- as.factor(stocks$quarter)
stocks$stock <- as.factor(stocks$stock)
stocks$date = as.Date(stocks$date, format = "%m/%d/%Y")
```

``` r
stock_data = stocks %>% filter(stock == "IBM") %>%
  arrange(date) %>% mutate(
    weekly_return = percent_change_next_weeks_price / 100
  )
```

``` r
mu = mean(stock_data$weekly_return, na.rm = TRUE)
sigma = sd(stock_data$weekly_return, na.rm = TRUE)
```

``` r
set.seed(123)
simulations = 1000
weeks = 12
last_price = tail(stock_data$next_weeks_close, 1)
sim_matrix = matrix(NA, nrow = weeks, ncol = simulations)


for (i in 1:simulations) {
  returns = rnorm(weeks, mean = mu, sd = sigma)
  prices = cumprod(1 + returns) * last_price
  sim_matrix[, i] = prices
}
```

``` r
matplot(sim_matrix, type = "l", lty = 1, col = rgb(0, 0, 1, 0.1),
        xlab = "Week", ylab = "Simulated Stock Price", main = "Monte Carlo Simulation (12 Weeks)")
```

![](case-study-3-mc_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(dplyr)
library(purrr)
library(tibble)

# --- Parameters ---
n_simulations <- 1000
n_weeks <- 12
set.seed(42)

# --- Helper function to simulate for one stock ---
simulate_stock <- function(stock_name, stock_df) {
  df <- stock_df |>
    filter(stock == stock_name) |>
    arrange(date) |>
    mutate(weekly_return = percent_change_next_weeks_price / 100) |>
    filter(!is.na(weekly_return))

  mu <- mean(df$weekly_return)
  sigma <- sd(df$weekly_return)

  last_price <- df |>
    filter(!is.na(next_weeks_close)) |>
    arrange(desc(date)) |>
    slice(1) |>
    pull(next_weeks_close) |>
    as.numeric()

  # Skip if we can't simulate
  if (is.na(last_price) || is.na(mu) || is.na(sigma)) {
    return(tibble(stock = stock_name, mean_return = NA, sd = NA, var_95 = NA))
  }

  # Run simulation
  sim_matrix <- replicate(
    n_simulations,
    last_price * cumprod(1 + rnorm(n_weeks, mean = mu, sd = sigma))
  )

  final_prices <- sim_matrix[n_weeks, ]
  returns <- (final_prices - last_price) / last_price

  # Metrics
  mean_return <- mean(returns)
  sd_return <- sd(returns)
  var_95 <- quantile(returns, probs = 0.05)

  tibble(
    stock = stock_name,
    mean_return = mean_return,
    sd = sd_return,
    var_95 = var_95
  )
}

# --- Run simulation for all stocks ---
stock_list <- unique(stocks$stock)

simulation_results <- map_dfr(stock_list, ~simulate_stock(.x, stocks))

# --- Rank stocks ---
ranked_by_return <- simulation_results |>
  arrange(desc(mean_return))

ranked_by_volatility <- simulation_results |>
  arrange(sd)

ranked_by_risk <- simulation_results |>
  arrange(var_95)  # more negative = worse 5% scenario

# Print top 5 best expected returns
ranked_by_return |>
  slice_head(n = 15)
```

    ## # A tibble: 15 × 4
    ##    stock mean_return     sd  var_95
    ##    <fct>       <dbl>  <dbl>   <dbl>
    ##  1 AXP        0.0949 0.116  -0.0805
    ##  2 IBM        0.0928 0.0736 -0.0236
    ##  3 KRFT       0.0900 0.0705 -0.0224
    ##  4 CVX        0.0768 0.107  -0.101 
    ##  5 UTX        0.0762 0.0885 -0.0614
    ##  6 MCD        0.0697 0.0683 -0.0403
    ##  7 INTC       0.0693 0.129  -0.126 
    ##  8 PFE        0.0691 0.0930 -0.0742
    ##  9 DD         0.0669 0.105  -0.0991
    ## 10 CAT        0.0628 0.134  -0.137 
    ## 11 MMM        0.0621 0.0820 -0.0708
    ## 12 TRV        0.0574 0.0769 -0.0630
    ## 13 KO         0.0570 0.0643 -0.0462
    ## 14 XOM        0.0479 0.106  -0.117 
    ## 15 JNJ        0.0461 0.0753 -0.0713

``` r
# Print top 5 most volitile stocks
ranked_by_volatility |> 
  slice_head(n = 15)
```

    ## # A tibble: 15 × 4
    ##    stock mean_return     sd  var_95
    ##    <fct>       <dbl>  <dbl>   <dbl>
    ##  1 WMT       0.0238  0.0548 -0.0635
    ##  2 PG        0.0100  0.0591 -0.0793
    ##  3 T         0.0330  0.0618 -0.0712
    ##  4 KO        0.0570  0.0643 -0.0462
    ##  5 MCD       0.0697  0.0683 -0.0403
    ##  6 VZ        0.00790 0.0693 -0.102 
    ##  7 KRFT      0.0900  0.0705 -0.0224
    ##  8 IBM       0.0928  0.0736 -0.0236
    ##  9 JNJ       0.0461  0.0753 -0.0713
    ## 10 TRV       0.0574  0.0769 -0.0630
    ## 11 MMM       0.0621  0.0820 -0.0708
    ## 12 MSFT     -0.0317  0.0831 -0.159 
    ## 13 GE        0.00595 0.0879 -0.137 
    ## 14 UTX       0.0762  0.0885 -0.0614
    ## 15 MRK      -0.00407 0.0917 -0.146

``` r
# Print top 5 most risky (lowest VaR)
ranked_by_risk |>
  slice_head(n = 15)
```

    ## # A tibble: 15 × 4
    ##    stock mean_return     sd var_95
    ##    <fct>       <dbl>  <dbl>  <dbl>
    ##  1 CSCO     -0.120   0.130  -0.308
    ##  2 BAC      -0.104   0.0993 -0.258
    ##  3 HPQ      -0.0603  0.125  -0.255
    ##  4 AA       -0.0252  0.131  -0.238
    ##  5 MSFT     -0.0317  0.0831 -0.159
    ##  6 MRK      -0.00407 0.0917 -0.146
    ##  7 GE        0.00595 0.0879 -0.137
    ##  8 CAT       0.0628  0.134  -0.137
    ##  9 DIS       0.0258  0.104  -0.133
    ## 10 JPM       0.0134  0.0920 -0.131
    ## 11 INTC      0.0693  0.129  -0.126
    ## 12 XOM       0.0479  0.106  -0.117
    ## 13 BA        0.0275  0.0922 -0.116
    ## 14 HD        0.0458  0.0934 -0.104
    ## 15 VZ        0.00790 0.0693 -0.102

``` r
write_csv(ranked_by_return, "ranked.csv")
```
