library(tidyverse)
library(lubridate)
library(testthat)

# Solution (for tests):

#' Estimate hourly NHPP arrival rates
#'
#' @param data Trip data with columns start_station, end_station, start_time,
#'   end_time.
#' @param window_start,window_end Observation window [start, end) as date-time
#'   strings (UTC). Must cover a whole number of days.
#' @return Tibble with origin, destination, hour (0-23), observed_rate,
#'   availability, and rate (observed_rate / availability, NA if availability
#'   is 0).
#' @details Stops if a required column is missing, the window is not a positive
#'   whole number of days, or any start_time or end_time cannot be parsed.
#'   Every station starts with 0 bikes, as in Week 10.

estimate_arrival_rates <- function(data, window_start, window_end) {
  
  # check required columns
  required_columns <- c("start_station", "end_station",
                        "start_time", "end_time")
  
  if (!all(required_columns %in% names(data))) {
    stop("data must contain: ", paste(required_columns, collapse = ", "))
  }
  
  # check observation window
  window_start_time <- as_datetime(window_start, tz = "UTC")
  window_end_time <- as_datetime(window_end, tz = "UTC")
  
  num_days <- as.numeric(difftime(window_end_time, window_start_time,
                                  units = "days"))
  
  if (is.na(num_days) || num_days <= 0 || num_days != round(num_days)) {
    stop("window must be a positive whole number of days")
  }
  
  # prepare trip data
  trips <- data %>%
    mutate(
      start_time = ymd_hms(start_time, tz = "UTC"),
      end_time = ymd_hms(end_time, tz = "UTC"),
      row_id = row_number(),
      event_type = if_else(start_station == "R" | end_station == "R",
                           "R", "C")
    )
  
  if (anyNA(trips$start_time) || anyNA(trips$end_time)) {
    stop("start_time and end_time must be parseable date-times")
  }
  
  # create station arrival and departure events
  station_events <- trips %>%
    pivot_longer(
      cols = c(start_station, end_station, start_time, end_time),
      names_to = c("event", ".value"),
      names_pattern = "(start|end)_(station|time)"
    ) %>%
    filter(station != "R") %>%
    mutate(
      bike_change = if_else(event == "start", -1, 1),
      event_priority = case_when(
        event_type == "R" & bike_change == 1 ~ 1,
        event_type == "C" & bike_change == -1 ~ 2,
        event_type == "C" & bike_change == 1 ~ 3,
        event_type == "R" & bike_change == -1 ~ 4
      )
    ) %>%
    arrange(station, time, event_priority, row_id) %>%
    group_by(station) %>%
    mutate(bikes_available = cumsum(bike_change)) %>%
    ungroup()
  
  hourly_times <- head(seq(window_start_time, window_end_time,
                           by = "hour"), -1)
  
  hour_start_seconds <- as.numeric(hourly_times)
  
  # calculate station availability
  availability_data <- station_events %>%
    group_by(station) %>%
    group_modify(function(station_data, station_info) {
      
      event_start <- pmax(as.numeric(station_data$time),
                          as.numeric(window_start_time))
      
      event_end <- pmin(c(as.numeric(station_data$time)[-1], Inf),
                        as.numeric(window_end_time))
      
      bike_available <- station_data$bikes_available >= 1
      
      available_seconds <- sapply(hour_start_seconds, function(hour_start) {
        sum(pmax(0,
                 pmin(event_end[bike_available], hour_start + 3600) -
                   pmax(event_start[bike_available], hour_start)))
      })
      
      tibble(hour = hour(hourly_times),
             available_seconds = available_seconds)
      
    }) %>%
    group_by(station, hour) %>%
    summarise(available_seconds = sum(available_seconds),
              .groups = "drop")
  
  # count observed trips by origin, destination, and hour
  observed_trips <- trips %>%
    filter(event_type == "C",
           start_time >= window_start_time,
           start_time < window_end_time) %>%
    count(origin = start_station,
          destination = end_station,
          hour = hour(start_time),
          name = "num_trips")
  
  # calculate observed rates, availability, and arrival rates
  rates <- observed_trips %>%
    distinct(origin, destination) %>%
    crossing(hour = 0:23) %>%
    left_join(observed_trips,
              by = c("origin", "destination", "hour")) %>%
    left_join(availability_data,
              by = c("origin" = "station", "hour")) %>%
    mutate(
      observed_rate = coalesce(num_trips, 0L) / num_days,
      availability = coalesce(available_seconds, 0) / (num_days * 3600),
      rate = if_else(availability > 0,
                     observed_rate / availability, NA_real_)
    ) %>%
    select(origin, destination, hour, observed_rate, availability, rate)
  
  return(rates)
}


# test data
tiny <- tibble(
  start_station = c("R", "A", "R"),
  end_station = c("A", "B", "A"),
  start_time = c(
    "2022-02-15 08:00:00",
    "2022-02-16 10:00:00",
    "2022-02-17 09:30:00"
  ),
  end_time = c(
    "2022-02-15 08:00:00",
    "2022-02-16 10:05:00",
    "2022-02-17 09:30:00"
  )
)

# tests
test_that("estimate_arrival_rates matches hand calculation", {
  
  out <- estimate_arrival_rates(tiny, "2022-02-16", "2022-02-18")
  
  # Hour 10: available 1 of 2 days
  r10 <- filter(out, origin == "A", destination == "B", hour == 10)
  expect_equal(r10$observed_rate, 0.5)
  expect_equal(r10$availability, 0.5)
  expect_equal(r10$rate, 1)
  
  # Hour 9: available 1.5 of 2 hours
  r9 <- filter(out, origin == "A", destination == "B", hour == 9)
  expect_equal(r9$availability, 0.75)
  expect_equal(r9$rate, 0)
  
  expect_equal(nrow(filter(out, origin == "A", destination == "B")), 24)
  
  # Never available at hour 10
  out2 <- estimate_arrival_rates(tiny[1:2, ], "2022-02-16", "2022-02-18")
  expect_true(is.na(filter(out2, hour == 10)$rate))
  
  # Input checks
  expect_error(estimate_arrival_rates(tiny[, 1:3], "2022-02-16", "2022-02-18"))
  expect_error(estimate_arrival_rates(tiny, "2022-02-18", "2022-02-16"))
  expect_error(estimate_arrival_rates(tiny, "2022-02-16", "2022-02-17 12:00:00"))
})

bikes <- read_csv("../Data/sample_bike.csv",
                  col_types = cols(.default = col_character())) %>%
  select(-1)

rates <- estimate_arrival_rates(bikes, "2022-02-16", "2022-02-22")

head(rates)

#' simulate customer requests by thinning
#'
#' @param rates Table with origin, destination, hour (0-23), and rate
#'   (requests per hour), as returned by estimate_arrival_rates().
#'   NA rates are treated as 0.
#' @param start_time,end_time Interval [start_time, end_time) as date-time strings (UTC).
#' @return Tibble with origin, destination, and arrival_time, sorted by
#'   arrival_time. Has zero rows if there are no arrivals.
#' @details Stops if end_time is not after start_time or if any rate is negative.
simulate_arrivals <- function(rates, start_time, end_time) {
  
  # check observation window and rates
  window_start <- as_datetime(start_time, tz = "UTC")
  window_end <- as_datetime(end_time, tz = "UTC")
  
  if (is.na(window_start) || is.na(window_end) ||
      window_start >= window_end) {
    stop("end_time must be after start_time")
  }
  
  if (any(rates$rate < 0, na.rm = TRUE)) {
    stop("rates must be nonnegative")
  }
  
  window_hours <- as.numeric(difftime(window_end, window_start,
                                      units = "hours"))
  
  # create empty results table
  empty_results <- tibble(
    origin = character(),
    destination = character(),
    arrival_time = .POSIXct(numeric(0), tz = "UTC")
  )
  
  # separate data by origin-destination pair
  trip_pairs <- rates %>%
    mutate(rate = replace_na(rate, 0)) %>%
    group_by(origin, destination) %>%
    group_split()
  
  # simulate arrivals for each pair
  simulated_trips <- lapply(trip_pairs, function(pair_data) {
    
    hourly_rates <- numeric(24)
    hourly_rates[pair_data$hour + 1] <- pair_data$rate
    
    max_rate <- max(hourly_rates)
    
    if (max_rate == 0) return(NULL)
    
    # generate candidate arrival times
    candidate_times <- numeric(0)
    last_time <- 0
    
    repeat {
      times <- last_time +
        cumsum(rexp(ceiling(max_rate * window_hours) + 10,
                    rate = max_rate))
      
      candidate_times <- c(candidate_times, times[times < window_hours])
      
      if (any(times >= window_hours)) break
      
      last_time <- max(times)
    }
    
    # keep candidates according to the hourly arrival rates
    candidate_arrivals <- window_start + candidate_times * 3600
    
    accepted <- runif(length(candidate_arrivals)) <
      hourly_rates[hour(candidate_arrivals) + 1] / max_rate
    
    tibble(
      origin = pair_data$origin[1],
      destination = pair_data$destination[1],
      arrival_time = candidate_arrivals[accepted]
    )
  })
  
  # combine and sort simulated arrivals
  bind_rows(c(list(empty_results), simulated_trips)) %>%
    arrange(arrival_time)
}


test_that("simulate_arrivals boundaries", {
  
  # all rates NA -> empty table with the right columns
  none <- simulate_arrivals(
    tibble(origin = "A", destination = "B", hour = 0:23,
           rate = NA_real_),
    "2022-02-22", "2022-02-23"
  )
  
  expect_equal(nrow(none), 0)
  expect_named(none, c("origin", "destination", "arrival_time"))
  
  # only hour 8 has demand -> all arrivals are in hour 8 and inside the window
  r <- tibble(
    origin = "A", destination = "B", hour = 0:23,
    rate = c(rep(0, 8), 20, rep(0, 15))
  )
  
  s <- simulate_arrivals(r, "2022-02-22", "2022-02-25")
  
  expect_true(all(hour(s$arrival_time) == 8))
  expect_true(all(s$arrival_time >= as_datetime("2022-02-22") &
                    s$arrival_time < as_datetime("2022-02-25")))
  expect_equal(nrow(s) / 3, 20, tolerance = 0.5)
  
  # constant rate 5 for 24 hours -> about 120 arrivals
  c5 <- tibble(origin = "A", destination = "B", hour = 0:23, rate = 5)
  
  expect_equal(
    nrow(simulate_arrivals(c5, "2022-02-22", "2022-02-23")),
    120,
    tolerance = 0.3
  )
  
  # invalid window
  expect_error(simulate_arrivals(r, "2022-02-23", "2022-02-22"))
})

sim <- simulate_arrivals(rates, "2022-02-22", "2022-02-23")

head(sim)
