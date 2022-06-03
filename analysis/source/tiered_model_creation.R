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

source("../lab_code/prelim.R")

sahm <- read.csv("analysis/input/SAHMREALTIME.csv")
state_emp <- read.csv("analysis/input/realtime-TUR.csv")
state_lab <- read_xlsx("analysis/input/ststdsadata.xlsx")
state_jolts_raw <- read_dta("analysis/input/state-jolts.dta")
raw_sim_unemp <- read_dta(str_c("analysis/input/",
                                "simulation_covid_fullcps_collapsed_states.dta")
                          )
historical_data <- read_dta("analysis/input/crck_ui_macro_dataset_weekly.dta")

# CLEANING UNEMPLOYMENT DATA ----------------------------------------------

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
    fip = str_c("States and selected areas:  Employment status of the",
                " civilian noninstitutional population,"),
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

# THRESHOLD DATES FUNCTION ------------------------------------------------

threshold_dates <- function(name, unemp_threshold, weeks) {
  one_state <- left_join(state_emp, sahm) %>%
    filter(state == name) %>%
    mutate(
      sahm_trig = if_else(sahm >= 0.5, 1, 0),
      unemp_rate = as.numeric(unemp_rate),
      unemp_trig = if_else(unemp_rate <= unemp_threshold |
                             is.na(unemp_rate), 0, 1)
    )  %>%
    unite(date, year, month, sep = "-", remove = TRUE) %>%
    mutate(date = ym(date)) %>%
    pad()

  for (i in 2:nrow(one_state)) {
    if (is.na(one_state$state[i]) &
        !is.na(one_state$state[i - 1]) &
        !is.na(one_state$state[i + 1])) {
      one_state$state[i] <- (one_state$state[i - 1])
      one_state$unemp_rate[i] <- ((one_state$unemp_rate[i + 1] +
                                     one_state$unemp_rate[i - 1]) / 2)
      one_state$sahm[i] <-
        ((one_state$sahm[i + 1] + one_state$sahm[i - 1]) / 2)
      one_state$sahm_trig[i] <-
        if_else(one_state$sahm[i] >= 0.5, 1, 0)
      one_state$unemp_trig[i] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
    }
    if (is.na(one_state$state[i]) &
        is.na(one_state$state[i + 1]) &
        !is.na(one_state$state[i - 1]) &
        !is.na(one_state$state[i + 2])) {
      one_state$state[i] <- (one_state$state[i - 1])
      one_state$state[i + 1] <- (one_state$state[i - 1])
      one_state$unemp_rate[i] <- ((one_state$unemp_rate[i + 2] +
                                     one_state$unemp_rate[i - 1]) / 2)
      one_state$unemp_rate[i + 1] <- ((one_state$unemp_rate[i + 2] +
                                         one_state$unemp_rate[i - 1]) / 2)
      one_state$sahm[i] <-
        ((one_state$sahm[i + 2] + one_state$sahm[i - 1]) / 2)
      one_state$sahm[i + 1] <-
        ((one_state$sahm[i + 2] + one_state$sahm[i - 1]) / 2)
      one_state$sahm_trig[i] <-
        if_else(one_state$sahm[i] >= 0.5, 1, 0)
      one_state$sahm_trig[i + 1] <-
        if_else(one_state$sahm[i] >= 0.5, 1, 0)
      one_state$unemp_trig[i] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
      one_state$unemp_trig[i + 1] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
    }
    if (is.na(one_state$unemp_rate[i]) &
        !is.na(one_state$unemp_rate[i - 1]) &
        !is.na(one_state$unemp_rate[i + 2])) {
      one_state$unemp_rate[i] <- ((one_state$unemp_rate[i + 1] +
                                     one_state$unemp_rate[i - 1]) / 2)
      one_state$unemp_trig[i] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
    }
    if (is.na(one_state$unemp_rate[i]) &
        is.na(one_state$unemp_rate[i + 1]) &
        !is.na(one_state$unemp_rate[i - 1]) &
        !is.na(one_state$unemp_rate[i + 2])) {
      one_state$unemp_rate[i] <- ((one_state$unemp_rate[i + 2] +
                                     one_state$unemp_rate[i - 1]) / 2)
      one_state$unemp_rate[i + 1] <- ((one_state$unemp_rate[i + 2] +
                                         one_state$unemp_rate[i - 1]) / 2)
      one_state$unemp_trig[i] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
      one_state$unemp_trig[i + 1] <-
        if_else(one_state$unemp_rate[i] > unemp_threshold, 1, 0)
    }
  }

  one_state <- one_state %>%
    mutate(
      sahm_wks_on = if_else(sahm_trig == 1,
                            roll_sum(sahm_trig, round(weeks / 4.33),
                                     align = "right", fill = NA),
                            0),
      sahm_wks_on = if_else(is.na(sahm_wks_on), cumsum(sahm_trig), sahm_wks_on)
    )

  one_state <- one_state %>%
    mutate(trig_on = if_else(unemp_trig == 1 |
                               (sahm_wks_on != 0 &
                                  lag(sahm_wks_on) != round(weeks / 4.33)),
                             1, 0)) %>%
    select(date, state, unemp_rate, trig_on)
}

# ONE TIER MODEL FUNCTION -------------------------------------------------

# note! this assumes 26 state weeks of UI (sahm weeks refers to federal weeks)

one_tier <- function(name, sahm_wks,
                     t1_ur, t1_wks,
                     t1_landing_wks) {
  sim_one_state <- sim_unemp %>%
    filter(state == name)

  thresholds_one_state <- threshold_dates(name, t1_ur, sahm_wks) %>%
    mutate(tier = if_else(as.double(unemp_rate) > t1_ur, 1, 0))

  one_state <- left_join(sim_one_state, thresholds_one_state,
                         by = c("date", "state")) %>%
    filter(!is.na(trig_on)) %>%
    subset(date >= as.Date("2001-06-01"))

  drop_tier_wks_min <- function(weeks_used,
                                higher_tier_landing_wks,
                                higher_tier_wks) {
    round(weeks_used + base::min(higher_tier_landing_wks,
                                 higher_tier_wks - weeks_used))
  }

  drop_tier_wks_max <- function(weeks_used,
                                higher_tier_landing_wks,
                                lower_tier_wks) {
    max(weeks_used + higher_tier_landing_wks, lower_tier_wks)
  }

  wks_used_max_wks <- function(input_date, input_duration) {
    subset_dates <- thresholds_one_state %>%
      subset(date <= as.Date(input_date)) %>%
      subset(date >= as.Date(input_date) %m-%
               months(as.integer((input_duration - 26) / 4.33))) %>%
      mutate(weeks_used = 0,
             max_wks = 0)

    subset_dates$max_wks[1] <- if (subset_dates$tier[1] == 1) {
      t1_wks
    } else if (subset_dates$trig_on[1] == 1) {
      sahm_wks
    } else if (subset_dates$tier[1] == 0) {
      0
    }

    if (nrow(subset_dates) > 1) {
      for (i in 2:nrow(subset_dates)) {
        subset_dates$weeks_used[i] <-
          if_else(
            round(subset_dates$weeks_used[i - 1]) < subset_dates$max_wks[i - 1],
            subset_dates$weeks_used[i - 1] + 4.33,
            subset_dates$weeks_used[i - 1]
          )

        subset_dates$max_wks[i] <-
          if (subset_dates$trig_on[i] == 1 &
              subset_dates$trig_on[i - 1] == 0 &
              subset_dates$tier[i] == 0) {
            if_else(subset_dates$max_wks[i - 1] > sahm_wks,
                    subset_dates$max_wks[i - 1],
                    sahm_wks)
          } else if (subset_dates$tier[i] == 0 &
                     subset_dates$tier[i - 1] == 1) {
            drop_tier_wks_min(subset_dates$weeks_used[i],
                              t1_landing_wks, t1_wks)
          } else if (subset_dates$tier[i] == 0) {
            subset_dates$max_wks[i - 1]
          } else if (subset_dates$trig_on[i] == 0) {
            0
          } else if (subset_dates$tier[i] == 1) {
            if_else(round(subset_dates$max_wks[i - 1]) > t1_wks,
                    subset_dates$max_wks[i - 1],
                    t1_wks)
          }
      }
    }

    tail(subset_dates, 1) %>%
      mutate(duration = input_duration,
             weeks_used = round(weeks_used))
  }

  one_state <-
    map2_dfr(one_state$date, one_state$duration, wks_used_max_wks)
}

# ONE TIER MODELS - 0 SAHM WEEKS -------------------------------------------

t1_sw0_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  one_tier
)
write_csv(t1_sw0_lw0_raw,
          "analysis/input/tiered_models_results/t1_sw0_lw0_mod.csv")

t1_sw0_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  one_tier
)
write_csv(t1_sw0_lw13_raw,
          "analysis/input/tiered_models_results/t1_sw0_lw13_mod.csv")

# ONE TIER MODELS - 26 SAHM WEEKS ------------------------------------------

t1_sw26_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  one_tier
)
write_csv(t1_sw26_lw0_raw,
          "analysis/input/tiered_models_results/t1_sw26_lw0_mod.csv")

t1_sw26_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  one_tier
)
write_csv(t1_sw26_lw13_raw,
          "analysis/input/tiered_models_results/t1_sw26_lw13_mod.csv")

# ONE TIER MODELS - 52 SAHM WEEKS ------------------------------------------

t1_sw52_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  one_tier
)
write_csv(t1_sw52_lw0_raw,
          "analysis/input/tiered_models_results/t1_sw52_lw0_mod.csv")

t1_sw52_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  one_tier
)
write_csv(t1_sw52_lw13_raw,
          "analysis/input/tiered_models_results/t1_sw52_lw13_mod.csv")


# THREE TIER MODEL FUNCTION ------------------------------------------------

# note! this assumes 26 state weeks of UI (sahm weeks refers to federal weeks)

three_tier <- function(name, sahm_wks,
                       t1_ur, t1_wks,
                       t1_landing_wks,
                       t2_ur, t2_wks,
                       t2_landing_wks,
                       t3_ur, t3_wks,
                       t3_landing_wks) {
  sim_one_state <- sim_unemp %>%
    filter(state == name)

  thresholds_one_state <- threshold_dates(name, t1_ur, sahm_wks) %>%
    mutate(tier = if_else(as.double(unemp_rate) > t3_ur,
                          3,
                          if_else(unemp_rate > t2_ur,
                                  2, if_else(unemp_rate > t1_ur,
                                             1, 0))))

  one_state <- left_join(sim_one_state, thresholds_one_state,
                         by = c("date", "state")) %>%
    filter(!is.na(trig_on)) %>%
    subset(date >= as.Date("2001-06-01"))

  drop_tier_wks_min <- function(weeks_used,
                                higher_tier_landing_wks,
                                higher_tier_wks) {
    round(weeks_used + base::min(higher_tier_landing_wks,
                                 higher_tier_wks - weeks_used))
  }

  drop_tier_wks_max <- function(weeks_used,
                                higher_tier_landing_wks,
                                lower_tier_wks) {
    max(weeks_used + higher_tier_landing_wks, lower_tier_wks)
  }

  wks_used_max_wks <- function(input_date, input_duration) {
    subset_dates <- thresholds_one_state %>%
      subset(date <= as.Date(input_date)) %>%
      subset(date >= as.Date(input_date) %m-%
               months(as.integer((input_duration - 26) / 4.33))) %>%
      mutate(weeks_used = 0,
             max_wks = 0)

    subset_dates$max_wks[1] <- if (subset_dates$tier[1] == 1) {
      t1_wks
    } else if (subset_dates$tier[1] == 2) {
      t2_wks
    } else if (subset_dates$tier[1] == 3) {
      t3_wks
    } else if (subset_dates$trig_on[1] == 1) {
      sahm_wks
    } else if (subset_dates$tier[1] == 0) {
      0
    }

    if (nrow(subset_dates) > 1) {
      for (i in 2:nrow(subset_dates)) {
        subset_dates$weeks_used[i] <-
          if_else(
            round(subset_dates$weeks_used[i - 1]) < subset_dates$max_wks[i - 1],
            subset_dates$weeks_used[i - 1] + 4.33,
            subset_dates$weeks_used[i - 1]
          )

        subset_dates$max_wks[i] <-
          if (subset_dates$trig_on[i] == 1 &
              subset_dates$trig_on[i - 1] == 0 &
              subset_dates$tier[i] == 0) {
            if_else(subset_dates$max_wks[i - 1] > sahm_wks,
                    subset_dates$max_wks[i - 1],
                    sahm_wks)
          } else if (subset_dates$tier[i] == 0 &
                     subset_dates$tier[i - 1] != 0) {
            if (subset_dates$tier[i - 1] == 3) {
              drop_tier_wks_min(subset_dates$weeks_used[i],
                                t3_landing_wks, t3_wks)
            } else if (subset_dates$tier[i - 1] == 2 &
                       round(subset_dates$weeks_used[i]) >= t2_wks) {
              if_else(
                subset_dates$max_wks[i - 1] > t2_wks,
                subset_dates$max_wks[i - 1],
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t2_landing_wks, t2_wks)
              )
            } else if (subset_dates$tier[i - 1] == 1) {
              if_else(
                subset_dates$max_wks[i - 1] > t1_wks,
                subset_dates$max_wks[i - 1],
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t1_landing_wks, t1_wks)
              )
            }
          } else if (subset_dates$tier[i] == 0) {
            subset_dates$max_wks[i - 1]
          } else if (subset_dates$trig_on[i] == 0) {
            0
          } else if (subset_dates$tier[i - 1] > subset_dates$tier[i]) {
            if (subset_dates$tier[i] == 2) {
              if_else(
                round(subset_dates$weeks_used[i]) >= t2_wks,
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t3_landing_wks, t3_wks),
                drop_tier_wks_max(subset_dates$weeks_used[i],
                                  t3_landing_wks, t2_wks)
              )
            } else if (subset_dates$tier[i] == 1) {
              if_else(
                round(subset_dates$weeks_used[i]) >= t1_wks,
                if_else(
                  subset_dates$tier[i - 1] == 3,
                  drop_tier_wks_min(subset_dates$weeks_used[i],
                                    t3_landing_wks, t3_wks),
                  drop_tier_wks_min(
                    subset_dates$weeks_used[i],
                    t2_landing_wks,
                    subset_dates$max_wks[i - 1]
                  )
                ),
                if_else(
                  subset_dates$tier[i - 1] == 3,
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t3_landing_wks, t1_wks),
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t2_landing_wks, t1_wks)
                )
              )
            }
          } else if (subset_dates$tier[i] == 1) {
            if_else(round(subset_dates$max_wks[i - 1]) > t1_wks,
                    subset_dates$max_wks[i - 1],
                    t1_wks)
          } else if (subset_dates$tier[i] == 2) {
            if_else(round(subset_dates$max_wks[i - 1]) > t2_wks,
                    subset_dates$max_wks[i - 1],
                    t2_wks)
          } else if (subset_dates$tier[i] == 3) {
            t3_wks
          }
      }
    }

    tail(subset_dates, 1) %>%
      mutate(duration = input_duration,
             weeks_used = round(weeks_used))
  }

  one_state <-
    map2_dfr(one_state$date, one_state$duration, wks_used_max_wks)
}

# THREE TIER MODELS - 0 SAHM WEEKS ----------------------------------------

t3_sw0_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 0,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 0,
  three_tier
)
write_csv(t3_sw0_lw0_raw,
          "analysis/input/tiered_models_results/t3_sw0_lw0_mod.csv")

t3_sw0_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 13,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 13,
  three_tier
)
write_csv(t3_sw0_lw13_raw,
          "analysis/input/tiered_models_results/t3_sw0_lw13_mod.csv")

# THREE TIER MODELS - 26 SAHM WEEKS ---------------------------------------

t3_sw26_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 0,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 0,
  three_tier
)
write_csv(t3_sw26_lw0_raw,
          "analysis/input/tiered_models_results/t3_sw26_lw0_mod.csv")

t3_sw26_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 13,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 13,
  three_tier
)
write_csv(t3_sw26_lw13_raw,
          "analysis/input/tiered_models_results/t3_sw26_lw13_mod.csv")

# THREE TIER MODELS - 52 SAHM WEEKS ---------------------------------------

t3_sw52_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 0,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 0,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 0,
  three_tier
)
write_csv(t3_sw52_lw0_raw,
          "analysis/input/tiered_models_results/t3_sw52_lw0_mod.csv")

t3_sw52_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 6.5,
  t1_wks = 26,
  t1_landing_wks = 13,
  t2_ur = 7.5,
  t2_wks = 39,
  t2_landing_wks = 13,
  t3_ur = 8.5,
  t3_wks = 52,
  t3_landing_wks = 13,
  three_tier
)
write_csv(t3_sw52_lw13_raw,
          "analysis/input/tiered_models_results/t3_sw52_lw13_mod.csv")


# FOUR TIER MODEL FUNCTION ------------------------------------------------

# note! this assumes 26 state weeks of UI (sahm weeks refers to federal weeks)

four_tier <- function(name, sahm_wks,
                      t1_ur,  t1_wks,
                      t1_landing_wks,
                      t2_ur,  t2_wks,
                      t2_landing_wks,
                      t3_ur, t3_wks,
                      t3_landing_wks,
                      t4_ur, t4_wks,
                      t4_landing_wks) {
  sim_one_state <- sim_unemp %>%
    filter(state == name)

  thresholds_one_state <- threshold_dates(name, t1_ur, sahm_wks) %>%
    mutate(tier = if_else(as.double(unemp_rate) > t4_ur,
                          4,
                          if_else(unemp_rate > t3_ur,
                                  3,
                                  if_else(unemp_rate > t2_ur,
                                          2, if_else(unemp_rate > t1_ur,
                                                     1, 0)))))

  one_state <- left_join(sim_one_state, thresholds_one_state,
                         by = c("date", "state")) %>%
    filter(!is.na(trig_on)) %>%
    subset(date >= as.Date("2001-06-01"))

  drop_tier_wks_min <- function(weeks_used,
                                higher_tier_landing_wks,
                                higher_tier_wks) {
    round(weeks_used + base::min(higher_tier_landing_wks,
                                 higher_tier_wks - weeks_used))
  }

  drop_tier_wks_max <- function(weeks_used,
                                higher_tier_landing_wks,
                                lower_tier_wks) {
    max(round(weeks_used + higher_tier_landing_wks),
        lower_tier_wks)
  }

  wks_used_max_wks <- function(input_date, input_duration) {
    subset_dates <- thresholds_one_state %>%
      subset(date <= as.Date(input_date)) %>%
      subset(date >= as.Date(input_date) %m-%
               months(as.integer((input_duration - 26) / 4.33))) %>%
      mutate(weeks_used = 0,
             max_wks = 0)

    subset_dates$max_wks[1] <- if (subset_dates$tier[1] == 1) {
      t1_wks
    } else if (subset_dates$tier[1] == 2) {
      t2_wks
    } else if (subset_dates$tier[1] == 3) {
      t3_wks
    } else if (subset_dates$tier[1] == 4) {
      t4_wks
    } else if (subset_dates$trig_on[1] == 1) {
      sahm_wks
    } else if (subset_dates$tier[1] == 0) {
      0
    }

    if (nrow(subset_dates) > 1) {
      for (i in 2:nrow(subset_dates)) {
        subset_dates$weeks_used[i] <-
          if_else(
            round(subset_dates$weeks_used[i - 1]) < subset_dates$max_wks[i - 1],
            subset_dates$weeks_used[i - 1] + 4.33,
            subset_dates$weeks_used[i - 1]
          )

        subset_dates$max_wks[i] <-
          if (subset_dates$trig_on[i] == 1 &
              subset_dates$trig_on[i - 1] == 0 &
              subset_dates$tier[i] == 0) {
            if_else(subset_dates$max_wks[i - 1] > sahm_wks,
                    subset_dates$max_wks[i - 1],
                    sahm_wks)
          } else if (subset_dates$tier[i] == 0 &
                     subset_dates$tier[i - 1] != 0) {
            if (subset_dates$tier[i - 1] == 4) {
              drop_tier_wks_min(subset_dates$weeks_used[i],
                                t4_landing_wks, t4_wks)
            } else if (subset_dates$tier[i - 1] == 3) {
              if_else(
                subset_dates$max_wks[i - 1] > t3_wks,
                subset_dates$max_wks[i - 1],
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t3_landing_wks, t3_wks)
              )
            } else if (subset_dates$tier[i - 1] == 2 &
                       round(subset_dates$weeks_used[i]) >= t2_wks) {
              if_else(
                subset_dates$max_wks[i - 1] > t2_wks,
                subset_dates$max_wks[i - 1],
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t2_landing_wks, t2_wks)
              )
            } else if (subset_dates$tier[i - 1] == 1) {
              if_else(
                subset_dates$max_wks[i - 1] > t1_wks,
                subset_dates$max_wks[i - 1],
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t1_landing_wks, t1_wks)
              )
            }
          } else if (subset_dates$tier[i] == 0) {
            subset_dates$max_wks[i - 1]
          } else if (subset_dates$trig_on[i] == 0) {
            0
          } else if (subset_dates$tier[i - 1] > subset_dates$tier[i]) {
            if (subset_dates$tier[i] == 3) {
              if_else(
                round(subset_dates$weeks_used[i]) >= t3_wks,
                drop_tier_wks_min(subset_dates$weeks_used[i],
                                  t4_landing_wks, t4_wks),
                drop_tier_wks_max(subset_dates$weeks_used[i],
                                  t4_landing_wks, t3_wks)
              )
            } else if (subset_dates$tier[i] == 2) {
              if_else(
                round(subset_dates$weeks_used[i]) >= t2_wks,
                if_else(
                  subset_dates$tier[i - 1] == 4,
                  drop_tier_wks_min(subset_dates$weeks_used[i],
                                    t4_landing_wks, t4_wks),
                  drop_tier_wks_min(
                    subset_dates$weeks_used[i],
                    t3_landing_wks,
                    subset_dates$max_wks[i - 1]
                  )
                ),
                if_else(
                  subset_dates$tier[i - 1] == 4,
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t4_landing_wks, t2_wks),
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t3_landing_wks, t2_wks)
                )
              )
            } else if (subset_dates$tier[i] == 1) {
              if_else(
                round(subset_dates$weeks_used[i]) >= t1_wks,
                if (subset_dates$tier[i] == 4) {
                  drop_tier_wks_min(subset_dates$weeks_used[i],
                                    t4_landing_wks,
                                    t4_wks)
                } else if (subset_dates$tier[i - 1] == 3) {
                  drop_tier_wks_min(subset_dates$weeks_used[i],
                                    t3_landing_wks,
                                    subset_dates$max_wks[i - 1])
                } else{
                  drop_tier_wks_min(subset_dates$weeks_used[i],
                                    t2_landing_wks,
                                    subset_dates$max_wks[i - 1])
                },
                if (subset_dates$tier[i - 1] == 4) {
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t4_landing_wks,
                                    t1_wks)
                } else if (subset_dates$tier[i - 1] == 3) {
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t3_landing_wks,
                                    t1_wks)
                } else{
                  drop_tier_wks_max(subset_dates$weeks_used[i],
                                    t2_landing_wks,
                                    t1_wks)
                }
              )
            }
          } else if (subset_dates$tier[i] == 1) {
            if_else(round(subset_dates$max_wks[i - 1]) > t1_wks,
                    subset_dates$max_wks[i - 1],
                    t1_wks)
          } else if (subset_dates$tier[i] == 2) {
            if_else(round(subset_dates$max_wks[i - 1]) > t2_wks,
                    subset_dates$max_wks[i - 1],
                    t2_wks)
          } else if (subset_dates$tier[i] == 3) {
            if_else(round(subset_dates$max_wks[i - 1]) > t3_wks,
                    subset_dates$max_wks[i - 1],
                    t3_wks)
          } else if (subset_dates$tier[i] == 4) {
            t4_wks
          }
      }
    }

    tail(subset_dates, 1) %>%
      mutate(duration = input_duration,
             weeks_used = round(weeks_used))
  }

  one_state <-
    map2_dfr(one_state$date, one_state$duration, wks_used_max_wks)
}


# FOUR TIER MODELS - 0 SAHM WEEKS -----------------------------------------

t4_sw0_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 0,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 0,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 0,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 0,
  four_tier
)
write_csv(t4_sw0_lw0_raw,
          "analysis/input/tiered_models_results/t4_sw0_lw0_mod.csv")

t4_sw0_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 0,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 13,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 13,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 13,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 13,
  four_tier
)
write_csv(t4_sw0_lw13_raw,
          "analysis/input/tiered_models_results/t4_sw0_lw13_mod.csv")

# FOUR TIER MODELS - 26 SAHM WEEKS ----------------------------------------

t4_sw26_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 0,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 0,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 0,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 0,
  four_tier
)
write_csv(t4_sw26_lw0_raw,
          "analysis/input/tiered_models_results/t4_sw26_lw0_mod.csv")

t4_sw26_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 26,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 13,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 13,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 13,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 13,
  four_tier
)
write_csv(t4_sw26_lw13_raw,
          "analysis/input/tiered_models_results/t4_sw26_lw13_mod.csv")

# FOUR TIER MODELS - 52 SAHM WEEKS ----------------------------------------

t4_sw52_lw0_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 0,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 0,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 0,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 0,
  four_tier
)
write_csv(t4_sw52_lw0_raw,
          "analysis/input/tiered_models_results/t4_sw52_lw0_mod.csv")

t4_sw52_lw13_raw <- pmap_dfr(
  list(state_codes$state),
  sahm_wks = 52,
  t1_ur = 5.5,
  t1_wks = 13,
  t1_landing_wks = 13,
  t2_ur = 6.5,
  t2_wks = 26,
  t2_landing_wks = 13,
  t3_ur = 7.5,
  t3_wks = 39,
  t3_landing_wks = 13,
  t4_ur = 8.5,
  t4_wks = 52,
  t4_landing_wks = 13,
  four_tier
)
write_csv(t4_sw52_lw13_raw,
          "analysis/input/tiered_models_results/t4_sw52_lw13_mod.csv")

# HISTORICAL MODEL FUNCTION -----------------------------------------------

historical_wks <- historical_data %>%
  select(date = date_effective,
         state,
         Tstar_EB,
         Tstar_TEUC02,
         Tstar_EUC08) %>%
  rowwise() %>%
  mutate(wks_available = sum(Tstar_EB, Tstar_TEUC02, Tstar_EUC08,
                             na.rm = TRUE)) %>%
  select(date, state, wks_available)

historical_model <- function(name) {
  one_state <- sim_unemp %>%
    filter(state == name) %>%
    subset(date >= as.Date("2001-06-01"))

  one_state_hist_wks <- historical_wks %>%
    filter(state == name) %>%
    subset(date >= as.Date("2001-05-15")) %>%
    rbind(tibble(
      date = as.Date("2015-09-01"),
      state = name,
      wks_available = NA
    ))

  padded_one_state_hist_wks <- one_state_hist_wks %>%
    pad(interval = "day") %>%
    mutate(state = name)

  for (i in 2:nrow(padded_one_state_hist_wks)) {
    padded_one_state_hist_wks$wks_available[i] <-
      ifelse(
        is.na(padded_one_state_hist_wks$wks_available[i]),
        padded_one_state_hist_wks$wks_available[as.numeric(i - 1)],
        padded_one_state_hist_wks$wks_available[i]
      )
  }

  joined_one_state <-
    left_join(one_state, padded_one_state_hist_wks) %>%
    filter(wks_available != 0)

  wks_used_max_wks <- function(input_date, input_duration) {
    subset_dates <-  padded_one_state_hist_wks %>%
      subset(date <= as.Date(input_date)) %>%
      subset(date >= as.Date(input_date) %m-%
               months(as.integer((input_duration - 26) / 4.33))) %>%
      mutate(
        days_used = 0,
        days_available = wks_available * 7,
        keep_date = 1
      ) %>%
      select(!wks_available)

    if (nrow(subset_dates) > 1) {
      for (i in 2:(nrow(subset_dates) - 1)) {
        subset_dates$keep_date[i] <-
          if_else(
            subset_dates$days_available[i] !=
              subset_dates$days_available[i - 1],
            1,
            0)
      }

      subset_dates <- subset_dates %>%
        filter(keep_date == 1)

      for (i in 2:nrow(subset_dates)) {
        subset_dates$days_used[i] <-
          if_else(
            round(
              subset_dates$days_used[i - 1]) <
              subset_dates$days_available[i - 1],
            subset_dates$days_used[i - 1] +
              difftime(subset_dates$date[i], subset_dates$date[i - 1],
                       unit = "days"),
            subset_dates$days_used[i - 1]
          )
      }
    }

    tail(subset_dates, 1) %>%
      select(!keep_date) %>%
      mutate(duration = input_duration)
  }

  joined_one_state <-
    map2_dfr(joined_one_state$date,
             joined_one_state$duration,
             wks_used_max_wks)

  one_state %>%
    select(date, state, duration) %>%
    left_join(joined_one_state) %>%
    unique() %>%
    mutate(
      days_used = ifelse(is.na(days_used),
                         0, days_used),
      days_available = ifelse(is.na(days_available),
                              0, days_available)
    )
}

# HISTORICAL MODEL --------------------------------------------------------

historical_raw <- map_dfr(state_codes$state, historical_model)
historical_raw
write_csv(historical_raw,
          "analysis/input/tiered_models_results/historical_mod.csv")