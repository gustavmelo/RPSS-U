# RPSS-U: Renewable-Plant Site-Selection under Uncertainty

Code, data, and results supporting the paper *"Planning Renewable Energy Futures: A Sustainability-Oriented Framework for Plant Site Selection Under Uncertainty"*, applied to a case study of 167 municipalities in Rio Grande do Norte (RN), Brazil.

## Data

Wind speed series were collected via the Renewables.ninja platform (Section 5.2.1 of the paper describes the data source and processing).

- `00_Data/Raw/Wind_Series/` — raw hourly wind speed series (one file per municipality, 167 total).
- `00_Data/Processed/TimeSeries.csv` — monthly wind speed series per municipality, output of `01_Pre_proc.R`.
- `00_Data/Processed/SDG_Criteria.csv` — the seven SDG-related criteria (g1–g7) per municipality.

See `DATA_DICTIONARY.md` for a full description of every variable, including municipality identification and the SDG criteria.

## Pipeline

**`Functions.R`** — helper functions (clustering, transition matrices, scenario sampling), sourced by `02_Forecasting.R`. 

Run the remaining scripts in this order:

1. **`01_Pre_proc.R`** — reads the raw files in `00_Data/Raw/Wind_Series/`, builds the monthly wind speed series, and writes `00_Data/Processed/TimeSeries.csv`.
2. **`02_Forecasting.R`** — clusters monthly wind speed into states, estimates transition matrices, and generates 1,000 Monte Carlo scenarios per municipality. Outputs to `01_Modeling_files_output/` and `02_Simulation_files_output/`.
3. **`03_Indicator.R`** — builds the composite site-selection indicator: CRITIC/Entropy weighting, MABAC/TOPSIS ranking, and the three sensitivity analyses (wind-criteria granularity, weighting method, ranking method). Outputs to `03_Indicator_files_output/`.
4. **`Map/map_ranking.R`** — produces the final ranking maps.

Run each script either by opening it in RStudio, or from a terminal with `Rscript <script_name>.R`. Running via `source()` in a plain R console is not supported, since the scripts locate their own folder from the RStudio editor context or from the command-line arguments.

## Reproducibility

- **Random seed**: `set.seed(100)` is fixed in `02_Forecasting.R`, so the Monte Carlo scenarios are exactly reproducible.
- **R and package versions**: see `sessionInfo.txt` for the complete environment (R 4.5.1) used to produce the results reported in the paper.

## Methods reference

The full mathematical formulation of each method (clustering, transition matrices, Monte Carlo sampling, CRITIC, MABAC, Entropy, TOPSIS) is in the paper's Supplementary Material.
