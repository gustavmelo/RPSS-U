# RPSS-U: Renewable-Plant Site-Selection under Uncertainty

Code, data, and results supporting the paper *"Planning Renewable Energy Futures: A Sustainability-Oriented Framework for Plant Site Selection Under Uncertainty"*, applied to a case study of 167 municipalities in Rio Grande do Norte (RN), Brazil.

## Data

Hourly wind speeds at 100 m (MERRA-2 reanalysis) from January 1984 to December 2023 were obtained from the Renewables.ninja platform for the coordinates of each municipal center and averaged into monthly means (Section 5.2.1 of the paper and Supplementary Material, Section S4).

- `00_Data/Raw/Wind_Series/` — monthly mean wind speed series at 100 m, with columns `Date` and `Wind_speed` (one file per municipality, 167 total).
- `00_Data/Processed/TimeSeries.csv` — monthly wind speed series per municipality, output of `01_Pre_proc.R`.
- `00_Data/Processed/SDG_Criteria.csv` — the seven SDG-related criteria (g1–g7) per municipality.

See `DATA_DICTIONARY.md` for a full description of every variable, including municipality identification and the SDG criteria.

## Pipeline

**`Functions.R`** — helper functions (clustering, transition matrices, scenario sampling), sourced by `02_Forecasting.R`. 

Run the remaining scripts in this order:

1. **`01_Pre_proc.R`** — reads the raw files in `00_Data/Raw/Wind_Series/`, builds the monthly wind speed series, and writes `00_Data/Processed/TimeSeries.csv`.
2. **`02_Forecasting.R`** — clusters monthly wind speed into states, estimates transition matrices, and generates 1,000 Monte Carlo scenarios per municipality. Outputs to `01_Modeling_files_output/` and `02_Simulation_files_output/`.
3. **`03_Indicator.R`** — builds the composite site-selection indicator: CRITIC/Entropy weighting, MABAC/TOPSIS ranking, and the four sensitivity analyses (wind-only indicator, wind-criteria granularity, weighting method, ranking method). Outputs to `03_Indicator_files_output/`.
4. **`Map/map_ranking.R`** — produces the final ranking maps.

Run each script either by opening it in RStudio, or from a terminal with `Rscript <script_name>.R`. Running via `source()` in a plain R console is not supported, since the scripts locate their own folder from the RStudio editor context or from the command-line arguments.

## Paper results

| Paper item | Script | Output |
|---|---|---|
| Figure 2 (monthly boxplots) | `02_Forecasting.R` | `00_Data/Processed/*_box.png` |
| Figure 3 (SDG criteria distributions) | `03_Indicator.R` | `00_Data/Processed/Combined_histograms.png` |
| Figure 4 (scenarios and out-of-sample year) | `02_Forecasting.R` | `02_Simulation_files_output/Without_last_year/*_Cloud.png` |
| Figures 5–7 (criteria weights, Top 10 frequency, ranking distributions) | `03_Indicator.R` | `03_Indicator_files_output/CRITIC_MABAC/*.png` |
| Figures 8–9 (ranking maps) | `Map/map_ranking.R` | `Map/wind_sdgwind_v2.png`, `Map/wind_windonly_v2.png` |
| Supplementary Tables S1–S2 (Touros clusters and transition matrix) | `02_Forecasting.R` | Printed to the console |
| Supplementary Figure S1 (state frequencies) | `02_Forecasting.R` | `02_Simulation_files_output/*_Freq.png` |
| Section 5.2.1 (wind speed statistics and ADF test) and Section 6.1.2 (Kolmogorov-Smirnov test, coverage percentage, correlation MAE) | `02_Forecasting.R` | Printed to the console |
| Sections 6.2–6.3 (criteria weights, rankings, and sensitivity analyses) | `03_Indicator.R` | Printed to the console; full results in `03_Indicator_files_output/` |

## Reproducibility

- **Random seed**: `set.seed(100)` is fixed in `02_Forecasting.R` and before each k-means run in `Functions.R`, so the clusters and the Monte Carlo scenarios are exactly reproducible.
- **R and package versions**: see `sessionInfo.txt` for the complete environment (R 4.5.1) used to produce the results reported in the paper.

## Methods reference

The full mathematical formulation of each method (clustering, transition matrices, Monte Carlo sampling, CRITIC, MABAC, Entropy, TOPSIS) is in the paper's Supplementary Material.
