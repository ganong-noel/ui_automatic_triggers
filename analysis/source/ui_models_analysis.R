library(stargazer)
library("RColorBrewer")
library(patchwork)
#library(gridtext)
library(grid)

setwd("~/repo/ui_automatic_triggers")

# LOADING INPUT DATA ------------------------------------------------------

source("../lab_code/prelim.R")
source("analysis/source/threshold_dates_models.R")
source("analysis/source/tiered_models_ltu_wks_comp.R")

# reading in a new untouched version of this data for false positive and false
# negative distribution plots (fie was loaded in in sourced R files, but was
# cleaned and altered in them)
state_unemp <- read_xlsx("analysis/input/ststdsadata.xlsx")

# FALSE POSITIVES DATA MANIPULATION ---------------------------------------

state_unemp <- state_unemp %>%
  rename(
    fip = "States and selected areas:  Employment status of the civilian noninstitutional population,",
    state = "...2",
    year = "...3",
    month = "...4",
    unemp_rate = "...11"
  ) %>%
  filter(!is.na(fip),
         nchar(fip) == 2) %>%
  unite(date, year, month, sep = "-", remove = TRUE) %>%
  mutate(unemp_rate = as.numeric(unemp_rate),
         date = ym(date)) %>%
  select(state, date, unemp_rate) %>%
  subset(date <= as.Date("2015-09-01"))

update_ur <- function(data) {
  data %>%
    select(!unemp_rate) %>%
    left_join(state_unemp)
}

call_sahm_data <- function(data) {
  if(data == "0.5") {
    sahm_0.5_mod
  } else if (data == "1") {
    sahm_1_mod
  }
}

filter_sahm_mod_dates <- function(name, data){
  sahm_data <- call_sahm_data(data)
  
  stop_dates <- baseline_mod %>%
    filter(state == name, stop_date == 1) %>%
    arrange(date)
  
  first_stop_date <- stop_dates[1, 2]
  
  state_only <- sahm_data %>%
    filter(state == name) %>%
    subset(date >= first_stop_date)
  
  state_only
}

sahm_0.5_mod <- map2_dfr(state_list$name,
                         "0.5",
                         filter_sahm_mod_dates)

sahm_1_mod <- map2_dfr(state_list$name,
                       "1",
                       filter_sahm_mod_dates)

baseline_mod <- update_ur(baseline_mod)
sahm_1_mod <- update_ur(sahm_1_mod)
sahm_0.5_mod <- update_ur(sahm_0.5_mod)

# DETERMINING FALSE POSITIVES ---------------------------------------------

false_positives <- function(data, time) {
  pre_post_dates <- data %>%
    filter(start_date == 1) %>%
    mutate(later_date = date %m+% period(time)) %>%
    rename(trigger_date = date)
  post_rates <- pre_post_dates %>%
    select(later_date, state) %>%
    rename(date = later_date)
  post_rates <- left_join(post_rates, state_unemp) %>%
    rename(later_date = date,
           post_rate = unemp_rate)
  false_pos <- left_join(pre_post_dates, post_rates) %>%
    rename(pre_rate = unemp_rate) %>%
    select(state, trigger_date, pre_rate, later_date, post_rate) %>%
    mutate(false_pos = if_else(post_rate - pre_rate < 1, 1, 0))
}

false_pos_baseline_ext <- false_positives(baseline_mod, "6 months") %>%
  subset(trigger_date > as.Date("1990-11-01"))
false_pos_sahm_1_ext <- false_positives(sahm_1_mod, "6 months")
false_pos_sahm_0.5_ext <- false_positives(sahm_0.5_mod, "6 months")

# FALSE POS DIST PLOTS ----------------------------------------------------

dist_plot_pos <- function(data) {
  plot_data <- data %>%
    mutate(excess_unemp = post_rate - pre_rate)
  
  ggplot(data = plot_data, aes(excess_unemp)) +
    geom_histogram(aes(y = stat(width * density)),
                   bins = 30,
                   fill = blues[2]) +
    fte_theme() +
    theme(
      axis.title.y = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(size = 11.5),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_blank(),
      plot.subtitle = element_text(size = 10)
    ) +
    # labs(subtitle = "Share of observations") +
    scale_x_continuous(# name = "Excess unemployment in 6 months (p.p.)",
      limits = c(-1.5, 2.5),
      breaks = c(-1,-0.5, 0, 0.5, 1, 1.5, 2))
}

dist_plot_baseline_ext <- dist_plot_pos(false_pos_baseline_ext) +
  labs(title = "National Sahm Trigger",
       subtitle = "Share of observations") +
  geom_vline(xintercept = 1,
             color = blues[1],
             linetype = "dashed") +
  geom_text(x = 0.8,
            y = 0.155,
            size = 1.7,
            hjust = 1.02,
            color = blues[1],
            label = "True state U\nrises by < 1%,\n55 state-months")+
  geom_text(x = 1.22,
            y = 0.155,
            size = 1.7,
            hjust = -0.02,
            color = blues[1],
            label = "True state U\nrises by >= 1%,\n45 state-months") +
  geom_segment(x = 1.24, y = 0.138,
               xend = 1.75, yend = 0.138,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) +
  geom_segment(x = 0.79, y = 0.138,
               xend = 0.34, yend = 0.138,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) 

dist_plot_sahm_1_ext <- dist_plot_pos(false_pos_sahm_1_ext) +
  labs(title = "State Sahm Trigger (1)") +
  geom_vline(xintercept = 1,
             color = blues[1],
             linetype = "dashed") +
  geom_text(x = 1,
            y = 0.121,
            size = 1.7,
            hjust = 1.13,
            color = blues[1],
            label = "True state U\nrises by < 1%,\n136 state-months")+
  geom_text(x = 1,
            y = 0.121,
            size = 1.7,
            hjust = -0.13,
            color = blues[1],
            label = "True state U\nrises by >= 1%,\n51 state-months") +
  geom_segment(x = 1.13, y = 0.108,
               xend = 1.73, yend = 0.108,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) +
  geom_segment(x = 0.87, y = 0.108,
               xend = .27, yend = 0.108,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) 

dist_plot_sahm_0.5_ext <- dist_plot_pos(false_pos_sahm_0.5_ext) +
  labs(title = "State Sahm Trigger (0.5)") +
  geom_vline(xintercept = 1,
             color = blues[1],
             linetype = "dashed") +
  geom_text(x = 1,
            y = 0.19,
            size = 1.7,
            hjust = 1.13,
            color = blues[1],
            label = "True state U\nrises by < 1%,\n352 state-months")+
  geom_text(x = 1,
            y = 0.189,
            size = 1.7,
            hjust = -0.13,
            color = blues[1],
            label = "True state U\nrises by >= 1%,\n30 state-months") +
  geom_segment(x = 1.13, y = 0.1693,
               xend = 1.75, yend = 0.1693,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) +
  geom_segment(x = 0.87, y = 0.1693,
               xend = .25, yend = 0.1693,
               lineend = "butt",
               size = 0.3, 
               arrow = arrow(length = unit(0.06, "inches")),
               colour = blues[1]) 

# DIST PLOT PANNEL -----------------------------------------------

# bottom <- grid.text("Excess unemployment in 6 months (p.p)",
#                     gp = gpar(fontsize = 10, fontfamily = "serif"))
# 
# all_plots <- dist_plot_baseline_ext +
#   dist_plot_sahm_1_ext + dist_plot_sahm_0.5_ext
# 
# all_plots / bottom +
#   plot_layout(heights = c(100, 1))
# 
# ggsave(
#   "analysis/release/false_pos_plot_pannel_ext.png",
#   width = 8,
#   height = 3.5,
#   unit = "in"
# )

# LTU CLAIMED VS UNCOVERED WKS,  TUR,  AND V/U TABLE ----------------------

output_table <- analysis_stats %>%
  mutate(
    tiers = as.numeric(tiers),
    sahm_wks = as.numeric(sahm_wks),
    soft_landing_wks = as.numeric(soft_landing_wks)
  ) %>%
  left_join(vus) %>%
  left_join(turs) %>%
  group_by(sahm_wks) %>%
  arrange(tiers) %>%
  ungroup() %>%
  arrange(sahm_wks) %>%
  mutate(
    soft_landing_wks = as.character(soft_landing_wks),
    soft_landing_wks = if_else(
      tiers != 1 & !is.na(tiers),
      paste0("All ", soft_landing_wks),
      soft_landing_wks
    )
  ) %>%
  select(
    sahm_wks,
    tiers,
    soft_landing_wks,
    wks_claimed = claimed,
    wks_uncovered = uncovered,
    prop_wks_uncovered,
    weighted_v_over_u,
    weighted_tur
  )

final_output_table <- output_table %>%
  mutate(sahm_wks = as.character(sahm_wks),
         tiers = as.character(tiers)) %>%
  rename(
    "Sahm weeks" = sahm_wks,
    "Tiers" = tiers,
    "Landing weeks" = soft_landing_wks,
    "Total weeks claimed (millions)" = wks_claimed,
    "Total weeks uncovered (millions)" = wks_uncovered,
    "Proportion of weeks uncovered" = prop_wks_uncovered,
    "Weighted v over u" = weighted_v_over_u,
    "Weighted tur" = weighted_tur
  )

write.csv(final_output_table,
          "analysis/release/LTU_wks_turs_vus_table.csv")

# NORMALIZED WKS CLAIMED VS V OVER U PLOT --------------------------------

plotting_data <- output_table %>%
  mutate(
    sahm_wks = as.character(sahm_wks),
    tiers = paste0(as.character(tiers), " Tier"),
    soft_landing_wks = str_remove_all(soft_landing_wks, "[All ]")
  )

hist_v_over_u <- as.numeric(plotting_data[19, 7])
hist_prop_claimed <- as.numeric(1- as.numeric(plotting_data[19, 6]))

plotting_data %>%
  filter(!is.na(sahm_wks)) %>%
  mutate(sahm_wks = paste(sahm_wks, "Sahm Weeks"),
         prop_wks_covered = 1 - as.numeric(prop_wks_uncovered)) %>%
  ggplot(mapping = aes(x = prop_wks_covered,
                       y = weighted_v_over_u)) +
  fte_theme() +
  geom_point(mapping = aes(
    shape = sahm_wks,
    color = tiers,
    fill =  ifelse(soft_landing_wks == 0,
                   tiers, NA)
  )) +
  scale_color_manual(values = c("grey 50", "red", "blue")) +
  scale_shape_manual(values = c(24, 22, 21)) +
  geom_point(
    mapping = aes(x = hist_prop_claimed,
                  y = hist_v_over_u,
                  fill = "Historical"),
    shape = 21
  ) +
  geom_point(
    mapping = aes(x = hist_prop_claimed,
                  y = hist_v_over_u,
                  fill = "Hard Landing"),
    alpha = 0
  ) +
  geom_point(
    mapping = aes(x = hist_prop_claimed,
                  y = hist_v_over_u,
                  fill = "Soft Landing"),
    alpha = 0
  ) +
  scale_fill_manual(
    values = c("grey 50", "red", "blue",
               "#d9ba57", "black", "white"),
    breaks = c("Hard Landing", "Soft Landing", "Historical")
  ) +
  theme(
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    title = element_blank(),
    plot.subtitle = element_text(size = 8.5),
    legend.position = "bottom",
    legend.box.margin = margin(0.7, 0, 0, 0, unit = "lines"),
    legend.box = "vertical",
    legend.spacing.x = unit(-.2, "lines"),
    axis.title.x = element_text(size = 8.5),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.margin = margin(-0.8,0,0,-1, unit="lines")
  ) +
  labs(
    fill = element_blank(),
    color = element_blank(),
    shape = element_blank(),
    x = "Proportion of weeks covered",
    subtitle = "Weighted vacancy/unemployment ratio"
  ) +
  geom_point(mapping = aes(
    x = hist_prop_claimed,
    y = hist_v_over_u),
    size = 3) +
  xlim(0.32, 0.61)

ggsave(
  "analysis/release/prop_wks_claimed_v_over_u.png",
  width = 2.625,
  height= 3.2,
  unit = "in"
)