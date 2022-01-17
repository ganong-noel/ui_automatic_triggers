library(tidyverse)
library(haven)
library(lubridate)
library(readxl)
library(RcppRoll)
library(caTools)

setwd("~/repo/ui_automatic_triggers")

# LOADING INPUT DATA ------------------------------------------------------

source("../lab_code/prelim.R")

sahm <- read.csv("analysis/input/SAHMREALTIME.csv")
state_emp <- read.csv("analysis/input/realtime-TUR.csv")
state_lab <- read_xlsx("analysis/input/ststdsadata.xlsx")

# CLEANING DATA -----------------------------------------------------------

sahm$DATE <- gsub('.{3}$', '', sahm$DATE)
sahm <- separate(sahm, DATE, c("year", "month"), sep = "-") %>%
  rename(sahm = SAHMREALTIME)

state_emp <- state_emp %>%
  rename(date = "release_month",
         unemp_rate = "UR") %>%
  filter(state != "Puerto Rico")

state_emp <- separate(state_emp, date, c("month", "day", "year"), sep = "/") %>%
  select(month, year, state, unemp_rate) %>%
  mutate(month = as.character(month),
         month = if_else(nchar(month) == 1, str_c(0, month), month))

state_lab <- state_lab %>%
  rename(fip = "States and selected areas:  Employment status of the civilian noninstitutional population,",
         state = "...2",
         year = "...3",
         month = "...4",
         lf = "...6",
         employed = "...8") %>%
  filter(!is.na(fip),
         nchar(fip) == 2) %>%
  mutate(employed = as.numeric(employed),
         lf = as.numeric(lf),
         epop = employed/lf) %>%
  select(state, year, month, epop)

state_list <- state_lab %>%
  select(state) %>%
  distinct(state) %>%
  rename(name = state)

# BASELINE (NATIONAL SAHM) MODEL FUNCTION ---------------------------------

# trigger start: national sahm = 0.5 (default)
# trigger stop: state unemployment rate <= 5.5% (default)

model_baseline <- function(name, number) {
  one_state <- left_join(state_emp, sahm) %>%
    filter(state == name) %>%
    mutate(
      sahm_trigger = if_else(sahm >= 0.5, 1, 0),
      unemp_rate = as.numeric(unemp_rate),
      trigger_on = sahm_trigger
    )
  
  for (i in 2:nrow(one_state)) {
    one_state$trigger_on[i] <- if_else(
      one_state$trigger_on[i] == 1 |
        (one_state$trigger_on[as.numeric(i) -
                                1]) == 1 &&
        one_state$unemp_rate[i] > number,
      1,
      0
    )
    thresholds <- one_state
  }
  
  one_state
  thresholds <- thresholds %>%
    mutate(
      start_date = if_else(trigger_on == 1 & lag(trigger_on) == 0 , 1, 0),
      stop_date = if_else(trigger_on == 0 &
                            lag(trigger_on) == 1, 1, 0)
    ) %>%
    filter (start_date == 1 | stop_date == 1) %>%
    unite(date, year, month, sep = "-", remove = TRUE) %>%
    select(state, date, unemp_rate, sahm, start_date, stop_date)
  
  thresholds <- if (is.na(thresholds$stop_date[1])) {
    thresholds
  } else if (thresholds$stop_date[1] == 1) {
    thresholds[-1,]
  } else {
    thresholds
  }
  
  thresholds
}

# FINDING BASELINE MODEL DATES --------------------------------------------

all_baseline <- (lapply(state_list$name, number = 5.5, model_baseline))
all_baseline <- do.call(rbind, all_baseline)

nineties_start_stop_date <- function(name, number) {
  state_emp %>%
    filter(state == name,
           unemp_rate <= number) %>%
    left_join(sahm) %>%
    unite(date, year, month, sep = "-", remove = TRUE) %>%
    mutate(date = ym(date)) %>%
    subset(date >= as.Date("1992-02-01")) %>%
    head(1)  %>%
    mutate(stop_date = 1,
           start_date = 0) %>%
    add_row(
      state = name,
      date = as.Date("1990-11-01"),
      sahm = 0.5,
      start_date = 1,
      stop_date = 0
    )
}

nineties_dates <-
  map2_dfr(state_list$name, 5.5, nineties_start_stop_date) %>%
  select(state, date, unemp_rate, sahm, start_date, stop_date)

baseline_mod <- all_baseline %>%
  mutate(date = ym(date)) %>%
  rbind(nineties_dates) %>%
  arrange(state, date)

# NATIONAL OR STATE SAHM MODEL FUNCTION -----------------------------------

# trigger start: national sahm = 0.5 or state sahm = 1
# trigger stop: state unemployment rate <= 5.5% (default)

model_state_sahm <- function(name, choose_state_sahm) {
  one_state <- left_join(state_emp, sahm) %>%
    filter(state == name) %>%
    mutate(
      sahm_trigger = if_else(sahm >= 0.5, 1, 0),
      unemp_rate = as.numeric(unemp_rate),
      moving_avg = (as.numeric(lag(unemp_rate)) +
                      as.numeric(lag(unemp_rate, n = 2)) +
                      unemp_rate) / 3
    )
  
  for (i in nrow(one_state)) {
    mins <- (lag(runmin(one_state$unemp_rate, 12, align = "right")))
    one_state <- cbind(one_state, mins)
  }
  
  one_state <- one_state %>%
    mutate(
      sahm_state = moving_avg - mins,
      sahm_state_trigger = if_else(sahm_state >= choose_state_sahm, 1, 0),
      trigger_on = if_else(sahm_trigger == 1 |
                             sahm_state_trigger == 1, 1, 0)
    ) %>%
    slice(-c(1:12)) %>%
    select(!c(moving_avg, mins))
  
  for (i in 2:nrow(one_state)) {
    one_state$trigger_on[i] <- if_else(
      one_state$trigger_on[i] == 1 |
        one_state$trigger_on[as.numeric(i) -
                               1] == 1 &&
        one_state$unemp_rate[i] > 5.5,
      1,
      one_state$trigger_on[i]
    )
    thresholds <- one_state
  }
  
  thresholds <- thresholds %>%
    mutate(
      start_date = if_else(trigger_on == 1 & lag(trigger_on) == 0 , 1, 0),
      stop_date = if_else(trigger_on == 0 &
                            lag(trigger_on) == 1, 1, 0)
    ) %>%
    filter (start_date == 1 | stop_date == 1) %>%
    unite(date, year, month, sep = "-", remove = TRUE) %>%
    select(state,
           date,
           unemp_rate,
           sahm,
           sahm_state,
           start_date,
           stop_date)
  
  thresholds <- if (is.na(thresholds$stop_date[1])) {
    thresholds
  } else if (thresholds$stop_date[1] == 1) {
    thresholds[-1,]
  } else {
    thresholds
  }
  
  thresholds
}

# FINDING STATE SAHM MODEL DATES ------------------------------------------

all_state_sahm_1 <- (lapply(state_list$name, choose_state_sahm = 1, model_state_sahm))
all_state_sahm_1 <- as.tibble(do.call(rbind, all_state_sahm_1))

all_state_sahm_0.5 <- (lapply(state_list$name, choose_state_sahm = 0.5, model_state_sahm))
all_state_sahm_0.5 <- as.tibble(do.call(rbind, all_state_sahm_0.5))

first_stop <- function(name) {
  state_emp %>%
    filter(state == name) %>%
    left_join(sahm) %>%
    unite(date, year, month, sep = "-", remove = TRUE) %>%
    mutate(date = ym(date)) %>%
    subset(date >= as.Date("1995-01-01"),
           date <= as.Date("2001-01-06")) %>%
    filter(unemp_rate <= 5) %>%
    head(1)
}

first_stops <- map_dfr(state_list$name, first_stop) %>%
  mutate(stop_date = 1, start_date = 0) %>%
  select(state, date, unemp_rate, sahm, start_date, stop_date)

sahm_1_mod <- all_state_sahm_1 %>%
  mutate(date = ym(date)) %>%
  select(!sahm_state) %>%
  rbind(first_stops) %>%
  arrange(state, date) %>%
  unique()

sahm_0.5_mod <- all_state_sahm_0.5 %>%
  mutate(date = ym(date)) %>%
  select(!sahm_state) %>%
  rbind(first_stops) %>%
  arrange(state, date) %>%
  unique()