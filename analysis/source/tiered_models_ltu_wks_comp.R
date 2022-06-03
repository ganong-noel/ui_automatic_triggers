library(tidyverse)
library(haven)
library(lubridate)
library(readxl)
library(RcppRoll)
library(caTools)
library(padr)

options(dplyr.summarise.inform = FALSE)
setwd("~/repo/ui_automatic_triggers")

# LOADING INPUT DATA ------------------------------------------------------

sahm <- read.csv("analysis/input/SAHMREALTIME.csv")
state_emp <- read.csv("analysis/input/realtime-TUR.csv")
state_lab <- read_xlsx("analysis/input/ststdsadata.xlsx")
state_jolts_raw <- read_dta("analysis/input/state-jolts.dta")
raw_sim_unemp <-
  read_dta("analysis/input/simulation_covid_fullcps_collapsed_states.dta")

source("../lab_code/prelim.R")

# source("analysis/source/tiered_models_creation.R")
#
# Only source this is you want to recreate the csv files that have the raw data
# about the tiered models created in tiered_models_creation.Rmd stored in
# ../input/tiered_model_results.
#
# I would avoid doing that as much as possible as the code is quite slow and
# each model takes HOURS to run (and there are 19 models).
#
# Otherwise, just load the data in the code below:

t1_sw0_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw0_lw0_mod.csv")
t1_sw0_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw0_lw13_mod.csv")
t1_sw26_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw26_lw0_mod.csv")
t1_sw26_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw26_lw13_mod.csv")
t1_sw52_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw52_lw13_mod.csv")
t1_sw52_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t1_sw52_lw0_mod.csv")

t3_sw0_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw0_lw0_mod.csv")
t3_sw0_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw0_lw13_mod.csv")
t3_sw26_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw26_lw0_mod.csv")
t3_sw26_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw26_lw13_mod.csv")
t3_sw52_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw52_lw13_mod.csv")
t3_sw52_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t3_sw52_lw0_mod.csv")

t4_sw0_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw0_lw0_mod.csv")
t4_sw0_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw0_lw13_mod.csv")
t4_sw26_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw26_lw0_mod.csv")
t4_sw26_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw26_lw13_mod.csv")
t4_sw52_lw13_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw52_lw13_mod.csv")
t4_sw52_lw0_raw <-
  read.csv("analysis/input/tiered_models_results/t4_sw52_lw0_mod.csv")

historical_raw <-
  read.csv("analysis/input/tiered_models_results/historical_mod.csv")

# CLEANING INPUT DATA -----------------------------------------------------

sahm$DATE <- gsub(".{3}$", "", sahm$DATE)
sahm <- separate(sahm, DATE, c("year", "month"), sep = "-") %>%
  rename(sahm = SAHMREALTIME)

state_emp <- state_emp %>%
  rename(date = "release_month",
         unemp_rate = "UR") %>%
  filter(state != "Puerto Rico")

state_emp <-
  separate(state_emp, date, c("month", "day", "year"), sep = "/") %>%
  select(month, year, state, unemp_rate) %>%
  mutate(month = as.character(month),
         month = if_else(nchar(month) == 1, str_c(0, month), month))

state_lab <- state_lab %>%
  rename(
    fip = str_c("States and selected areas:  Employment status of ",
                "the civilian noninstitutional population,"),
    state = "...2",
    year = "...3",
    month = "...4",
    lf = "...6",
    employed = "...8"
  ) %>%
  filter(!is.na(fip),
         nchar(fip) == 2) %>%
  mutate(employed = as.numeric(employed),
         lf = as.numeric(lf),
         epop = employed / lf) %>%
  select(state, year, month, epop)

state_list <- state_lab %>%
  select(state) %>%
  distinct(state) %>%
  rename(name = state)

sim_unemp <- raw_sim_unemp %>%
  mutate(lfs = as.numeric(lfs)) %>%
  filter(lfs %in% c(1, 2),
         duration > 26)

sim_unemp$lfs[sim_unemp$lfs == 1] <- "Ut"
sim_unemp$lfs[sim_unemp$lfs == 2] <- "Up"

sim_unemp <- sim_unemp %>%
  mutate(date = as.Date("1996-01-01") %m+% months(tm - 443)) %>%
  subset(date <= as.Date("2015-09-01")) %>%
  select(date, lfs, duration, num_benwks_eue_n4, stfips, num) %>%
  filter(num_benwks_eue_n4 != 0)

state_codes <- maps::state.fips %>%
  select(stfips = fips,
         state = polyname) %>%
  bind_rows(tibble(
    stfips = c(15, 02),
    state = c("hawaii", "alaska")
  )) %>%
  slice(-c(20:21, 23, 34:36, 38:39, 53:54, 56:59)) %>%
  mutate(state = str_to_title(state))
state_codes[8, 2] <- "District of Columbia"
state_codes[20, 2] <- "Massachusetts"
state_codes[21, 2] <- "Michigan"
state_codes[31, 2] <- "New York"
state_codes[32, 2] <- "North Carolina"
state_codes[45, 2] <- "Virginia"
state_codes[46, 2] <- "Washington"

sim_unemp <- left_join(sim_unemp, state_codes) %>%
  select(!c(stfips, num))

# FINDING NUMBER OF PEOPLE (UN)COVERED EACH MONTH UNDER EACH MODEL --------

joining_sim_unemp <- sim_unemp %>%
  subset(date >= as.Date("2001-06-01")) %>%
  group_by(state, date, duration)

clean_raw_data <- function(data) {
  data %>%
    mutate(date = as.Date(date)) %>%
    left_join(joining_sim_unemp) %>%
    unique() %>%
    filter(!is.na(num_benwks_eue_n4)) %>%
    mutate(
      covered = if_else(round(weeks_used) < max_wks,
                        num_benwks_eue_n4, 0),
      uncovered = if_else(round(weeks_used) >= max_wks,
                          num_benwks_eue_n4, 0)
    ) %>%
    group_by(date) %>%
    summarise(covered = sum(covered),
              uncovered = sum(uncovered))
}

t1_sw0_lw0 <- clean_raw_data(t1_sw0_lw0_raw)

t1_sw0_lw0_raw %>% filter(date == as.Date("2008-12-01"))
t1_sw26_lw0_raw %>% filter(date == as.Date("2008-12-01"))

t1_sw0_lw13 <- clean_raw_data(t1_sw0_lw13_raw)
t1_sw26_lw0 <- clean_raw_data(t1_sw26_lw0_raw)
t1_sw26_lw13 <- clean_raw_data(t1_sw26_lw13_raw)
t1_sw52_lw0 <- clean_raw_data(t1_sw52_lw0_raw)
t1_sw52_lw13 <- clean_raw_data(t1_sw52_lw13_raw)

t3_sw0_lw0 <- clean_raw_data(t3_sw0_lw0_raw)
t3_sw0_lw13 <- clean_raw_data(t3_sw0_lw13_raw)
t3_sw26_lw0 <- clean_raw_data(t3_sw26_lw0_raw)
t3_sw26_lw13 <- clean_raw_data(t3_sw26_lw13_raw)
t3_sw52_lw0 <- clean_raw_data(t3_sw52_lw0_raw)
t3_sw52_lw13 <- clean_raw_data(t3_sw52_lw13_raw)

t4_sw0_lw0 <- clean_raw_data(t4_sw0_lw0_raw)
t4_sw0_lw13 <- clean_raw_data(t4_sw0_lw13_raw)
t4_sw26_lw0 <- clean_raw_data(t4_sw26_lw0_raw)
t4_sw26_lw13 <- clean_raw_data(t4_sw26_lw13_raw)
t4_sw52_lw0 <- clean_raw_data(t4_sw52_lw0_raw)
t4_sw52_lw13 <- clean_raw_data(t4_sw52_lw13_raw)

historical <- historical_raw %>%
  mutate(date = as.Date(date)) %>%
  left_join(joining_sim_unemp) %>%
  unique() %>%
  filter(!is.na(num_benwks_eue_n4)) %>%
  mutate(
    covered = if_else(round(days_used) < days_available,
                      num_benwks_eue_n4, 0),
    uncovered = if_else(round(days_used) >= days_available,
                        num_benwks_eue_n4, 0)
  ) %>%
  group_by(date) %>%
  summarize(covered = sum(covered),
            uncovered = sum(uncovered))

# TABLE OF TOTAL WEEKS COVERED AND UNCOVERED ------------------------------

model_list <- list(t1_sw0_lw0, t1_sw0_lw13, t1_sw26_lw0,
                   t1_sw26_lw13, t1_sw52_lw0, t1_sw52_lw13,
                   t3_sw0_lw0, t3_sw0_lw13, t3_sw26_lw0,
                   t3_sw26_lw13, t3_sw52_lw0, t3_sw52_lw13,
                   t4_sw0_lw0, t4_sw0_lw13, t4_sw26_lw0,
                   t4_sw26_lw13, t4_sw52_lw0, t4_sw52_lw13)

total_wks_claimed <- function(data) {
  as.numeric(data %>%
               summarize(covered = sum(covered * 4.33)))
}

weeks_claimed <-  map_dfc(model_list, total_wks_claimed) %>%
  pivot_longer(col = 1:18) %>%
  select(claimed = value)

total_uncovered_wks <- function(data) {
  as.numeric(data %>%
               summarize(uncovererd = sum(uncovered * 4.33)))
}

weeks_uncovered <- map_dfc(model_list, total_uncovered_wks) %>%
  pivot_longer(col = 1:18) %>%
  select(uncovered = value)

model_info <- tibble(
  tiers = c(rep(1, 6), rep(3, 6), rep(4, 6)),
  sahm_wks = c(rep(c(0, 0, 26, 26, 52, 52), 3)),
  soft_landing_wks = c(rep(c("0", "13"), 9))
)

analysis_stats <-
  cbind(model_info, weeks_claimed, weeks_uncovered) %>%
  mutate(
    prop_wks_uncovered = round(uncovered / (uncovered + claimed), 2),
    uncovered = round(uncovered / 1000),
    claimed = round(claimed / 1000)
  )

analysis_stats <- analysis_stats %>%
  rbind(c("NA", "NA", "NA",
          round(total_wks_claimed(historical) / 1000),
          round(total_uncovered_wks(historical) / 1000),
          round(
            total_uncovered_wks(historical) /
              (total_wks_claimed(historical) +
                 total_uncovered_wks(historical)),
            2)))

# WEIGHTED TUR TABLE ------------------------------------------------------

state_emp <- state_emp %>%
  unite(date, year, month, sep = "-", remove = TRUE) %>%
  mutate(date = paste0(date, "-01"),
         date = as.Date(date))

weighted_tur <- function(data) {
  data %>%
    mutate(date = as.Date(date)) %>%
    left_join(joining_sim_unemp) %>%
    unique() %>%
    filter(!is.na(num_benwks_eue_n4)) %>%
    mutate(
      covered = if_else(round(weeks_used) < max_wks,
                        num_benwks_eue_n4, 0),
      uncovered = if_else(round(weeks_used) >= max_wks,
                          num_benwks_eue_n4, 0)
    ) %>%
    group_by(date, state) %>%
    summarize(covered = sum(covered),
              uncovered = sum(uncovered)) %>%
    left_join(state_emp) %>%
    ungroup() %>%
    mutate(unemp_rate = as.numeric(unemp_rate)) %>%
    summarize(weighted_tur = round(weighted.mean(unemp_rate, covered,
                                                 na.rm = TRUE), 2))
}

t1_sw0_lw0_tur <- weighted_tur(t1_sw0_lw0_raw)
t1_sw0_lw13_tur <- weighted_tur(t1_sw0_lw13_raw)
t1_sw26_lw0_tur <- weighted_tur(t1_sw26_lw0_raw)
t1_sw26_lw13_tur <- weighted_tur(t1_sw26_lw13_raw)
t1_sw52_lw0_tur <- weighted_tur(t1_sw52_lw0_raw)
t1_sw52_lw13_tur <- weighted_tur(t1_sw52_lw13_raw)

t3_sw0_lw0_tur <- weighted_tur(t3_sw0_lw0_raw)
t3_sw0_lw13_tur <- weighted_tur(t3_sw0_lw13_raw)
t3_sw26_lw0_tur <- weighted_tur(t3_sw26_lw0_raw)
t3_sw26_lw13_tur <- weighted_tur(t3_sw26_lw13_raw)
t3_sw52_lw0_tur <- weighted_tur(t3_sw52_lw0_raw)
t3_sw52_lw13_tur <- weighted_tur(t3_sw52_lw13_raw)

t4_sw0_lw0_tur <- weighted_tur(t4_sw0_lw0_raw)
t4_sw0_lw13_tur <- weighted_tur(t4_sw0_lw13_raw)
t4_sw26_lw0_tur <- weighted_tur(t4_sw26_lw0_raw)
t4_sw26_lw13_tur <- weighted_tur(t4_sw26_lw13_raw)
t4_sw52_lw0_tur <- weighted_tur(t4_sw52_lw0_raw)
t4_sw52_lw13_tur <- weighted_tur(t4_sw52_lw13_raw)

historical_tur <- historical_raw %>%
  mutate(date = as.Date(date)) %>%
  left_join(joining_sim_unemp) %>%
  unique() %>%
  filter(!is.na(num_benwks_eue_n4)) %>%
  mutate(
    covered = if_else(round(days_used) < days_available,
                      num_benwks_eue_n4, 0),
    uncovered = if_else(round(days_used) >= days_available,
                        num_benwks_eue_n4, 0)
  ) %>%
  group_by(date, state) %>%
  summarize(covered = sum(covered),
            uncovered = sum(uncovered)) %>%
  left_join(state_emp) %>%
  ungroup() %>%
  mutate(unemp_rate = as.numeric(unemp_rate)) %>%
  summarize(weighted_tur = round(weighted.mean(unemp_rate, covered,
                                               na.rm = TRUE), 2))

weighted_state_wtd_turs <- rbind(t1_sw0_lw0_tur, t1_sw0_lw13_tur,
                                 t1_sw26_lw0_tur, t1_sw26_lw13_tur,
                                 t1_sw52_lw0_tur, t1_sw52_lw13_tur,
                                 t3_sw0_lw0_tur, t3_sw0_lw13_tur,
                                 t3_sw26_lw0_tur, t3_sw26_lw13_tur,
                                 t3_sw52_lw0_tur, t3_sw52_lw13_tur,
                                 t4_sw0_lw0_tur, t4_sw0_lw13_tur,
                                 t4_sw26_lw0_tur, t4_sw26_lw13_tur,
                                 t4_sw52_lw0_tur, t4_sw52_lw13_tur,
                                 historical_tur) %>%
  mutate(
    tiers = c(rep(1, 6), rep(3, 6), rep(4, 6), NA),
    sahm_wks = c(rep(c(0, 0, 26, 26, 52, 52), 3), NA),
    soft_landing_wks = c(rep(c(0, 13), 9), NA)
  ) %>%
  select(tiers, sahm_wks, soft_landing_wks, everything())

turs <- weighted_state_wtd_turs

# WEIGHTED V OVER U TABLE -------------------------------------------------

`%not_in%` <- purrr::negate(`%in%`)

state_jolts <- state_jolts_raw %>%
  filter(
    ratelevel_code == "L",
    dataelement_text == "Job openings",
    state_code %not_in% c("00", "MW", "NE", "SO", "WE"),
    seasonal == "S"
  )

state_jolts <- state_jolts %>%
  mutate(
    state_code = as.numeric(state_code),
    date = as.Date("1960-01-01") %m+% months(monthly)
  ) %>%
  select(state = state_text, date, job_openings_k = value)

weighted_v_over_u <- function(data) {
  data %>%
    mutate(date = as.Date(date)) %>%
    left_join(joining_sim_unemp) %>%
    unique() %>%
    filter(!is.na(num_benwks_eue_n4)) %>%
    mutate(covered = if_else(round(weeks_used) < max_wks,
                             num_benwks_eue_n4, 0)) %>%
    group_by(state, date) %>%
    summarise(unemployed = sum(num_benwks_eue_n4),
              covered = sum(covered)) %>%
    left_join(state_jolts) %>%
    mutate(v_over_u = job_openings_k / unemployed) %>%
    ungroup() %>%
    summarise(weighted_v_over_u = round(weighted.mean(v_over_u, covered,
                                                      na.rm = TRUE), 2))
}

t1_sw0_lw0_vu <- weighted_v_over_u(t1_sw0_lw0_raw)
t1_sw0_lw13_vu <- weighted_v_over_u(t1_sw0_lw13_raw)
t1_sw26_lw0_vu <- weighted_v_over_u(t1_sw26_lw0_raw)
t1_sw26_lw13_vu <- weighted_v_over_u(t1_sw26_lw13_raw)
t1_sw52_lw0_vu <- weighted_v_over_u(t1_sw52_lw0_raw)
t1_sw52_lw13_vu <- weighted_v_over_u(t1_sw52_lw13_raw)

t3_sw0_lw0_vu <- weighted_v_over_u(t3_sw0_lw0_raw)
t3_sw0_lw13_vu <- weighted_v_over_u(t3_sw0_lw13_raw)
t3_sw26_lw0_vu <- weighted_v_over_u(t3_sw26_lw0_raw)
t3_sw26_lw13_vu <- weighted_v_over_u(t3_sw26_lw13_raw)
t3_sw52_lw0_vu <- weighted_v_over_u(t3_sw52_lw0_raw)
t3_sw52_lw13_vu <- weighted_v_over_u(t3_sw52_lw13_raw)

t4_sw0_lw0_vu <- weighted_v_over_u(t4_sw0_lw0_raw)
t4_sw0_lw13_vu <- weighted_v_over_u(t4_sw0_lw13_raw)
t4_sw26_lw0_vu <- weighted_v_over_u(t4_sw26_lw0_raw)
t4_sw26_lw13_vu <- weighted_v_over_u(t4_sw26_lw13_raw)
t4_sw52_lw0_vu <- weighted_v_over_u(t4_sw52_lw0_raw)
t4_sw52_lw13_vu <- weighted_v_over_u(t4_sw52_lw13_raw)

historical_vu <- historical_raw %>%
  mutate(date = as.Date(date)) %>%
  left_join(joining_sim_unemp) %>%
  unique() %>%
  filter(!is.na(num_benwks_eue_n4)) %>%
  mutate(covered = if_else(round(days_used) < days_available,
                           num_benwks_eue_n4, 0)) %>%
  group_by(state, date) %>%
  summarise(unemployed = sum(num_benwks_eue_n4),
            covered = sum(covered)) %>%
  left_join(state_jolts) %>%
  mutate(v_over_u = job_openings_k / unemployed) %>%
  ungroup() %>%
  summarise(weighted_v_over_u = round(weighted.mean(v_over_u, covered,
                                                    na.rm = TRUE), 2))

weighted_state_wtd_vu <- rbind(t1_sw0_lw0_vu, t1_sw0_lw13_vu,
                               t1_sw26_lw0_vu, t1_sw26_lw13_vu,
                               t1_sw52_lw0_vu, t1_sw52_lw13_vu,
                               t3_sw0_lw0_vu, t3_sw0_lw13_vu,
                               t3_sw26_lw0_vu, t3_sw26_lw13_vu,
                               t3_sw52_lw0_vu, t3_sw52_lw13_vu,
                               t4_sw0_lw0_vu, t4_sw0_lw13_vu,
                               t4_sw26_lw0_vu, t4_sw26_lw13_vu,
                               t4_sw52_lw0_vu, t4_sw52_lw13_vu,
                               historical_vu) %>%
  mutate(
    tiers = c(rep(1, 6), rep(3, 6), rep(4, 6), NA),
    sahm_wks = c(rep(c(0, 0, 26, 26, 52, 52), 3), NA),
    soft_landing_wks = c(rep(c(0, 13), 9), NA)
  ) %>%
  select(tiers, sahm_wks, soft_landing_wks, everything())

vus <- weighted_state_wtd_vu