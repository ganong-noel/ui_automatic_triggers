To replicate the results in the paper only the master script `master.R` needs to be run. The sourcing of `source/tiered_models_creation.R` in `source/tiered_models_ltu_wks_comp.R` is commented out to avoid exessive computing time. If `source/tiered_models_creation.R` is run then the compilation time could exceed more than a day.

The master script calls `source/ui_models_analysis.R` which in turn calls `source/threshold_dates_models.R` and `source/tiered_models_ltu_wks_comp.R`. If the tiered models are rebuilded then `source/tiered_models_creation.R` is run.

The four scripts use the following inputs from the folder `input`:
- `crck_ui_macro_dataset_weekly.dta` based on Chodorow-Reich, Coglianese, and Karabarbounis, QJE 2019
- `realtime-TUR.csv` was hand collected from archived BLS releases
- `SAHMREALTIME.csv` from FRED
- `simulation_covid_fullcps_collapsed_states.dta` based on Chodorow-Reich and Coglianese, JPubE 2021
- `state-jolts.dta` from BLS JOLTS
- `ststdsadata.xlsx` from BLS LAUS
