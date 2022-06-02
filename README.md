To replicate the results in the paper only the [`master.R`](https://github.com/ganong-noel/ui_automatic_triggers/blob/main/analysis/source/master.R)  needs to be run. 

The master script calls a single script: [`ui_models_analysis.R`](https://github.com/ganong-noel/ui_automatic_triggers/blob/main/analysis/source/ui_models_analysis.R) which in turn calls
* `source/threshold_dates_models.R`
* `source/tiered_models_ltu_wks_comp.R`

The replicator may choose to run `tiered_models_creation.R` in `tiered_models_ltu_wks_comp.R`. It is commented out by default to avoid excessive computing time. Run time likely could exceeds a day.

The four scripts use the following inputs from the folder `input`:
- `crck_ui_macro_dataset_weekly.dta` based on Chodorow-Reich, Coglianese, and Karabarbounis, QJE 2019
- `realtime-TUR.csv` was hand collected from archived BLS releases
- `SAHMREALTIME.csv` from FRED
- `simulation_covid_fullcps_collapsed_states.dta` based on Chodorow-Reich and Coglianese, JPubE 2021
- `state-jolts.dta` from BLS JOLTS
- `ststdsadata.xlsx` from BLS LAUS
