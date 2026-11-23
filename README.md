# Bike-share Arrival Rates

This repository contains `bike_functions.R`, which has two functions for modeling bike-share requests with a non-homogeneous Poisson process (NHPP).

## Functions

- `estimate_arrival_rates(data, window_start, window_end)`: takes trip data and an observation window and returns the estimated hourly request rate for each origin-destination pair. It adjusts observed departures for the fraction of time the origin station had at least one bike available.

- `simulate_arrivals(rates, start_time, end_time)`: takes the rate table from `estimate_arrival_rates()` and simulates customer requests over [start_time, end_time) using thinning.

## Running an example

``` r
library(tidyverse)
library(lubridate)
source("bike_functions.R")

bikes <- read_csv("../Data/sample_bike.csv",
                  col_types = cols(.default = col_character())) %>%
  select(-1)

# estimate hourly rates from February 16-21, 2022
rates <- estimate_arrival_rates(bikes, "2022-02-16", "2022-02-22")

# simulate one day of requests using those rates
sim <- simulate_arrivals(rates, "2022-02-22", "2022-02-23")
head(sim)
```
