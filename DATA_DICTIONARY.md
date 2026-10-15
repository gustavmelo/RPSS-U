# Data Dictionary

This document describes every variable used in the RPSS-U pipeline for the Rio Grande do Norte (RN) wind-siting case study (167 municipalities).

## Municipality identification

Every data file is keyed by the official IBGE municipality code.

| Field | Description | Source file |
|---|---|---|
| `Code` | 7-digit IBGE municipality code | `00_Data/Processed/SDG_Criteria.csv` |
| `Municipality` | Municipality name | `00_Data/Processed/SDG_Criteria.csv` |

## Wind-speed criteria

| Variable | Description | Unit | Source |
|---|---|---|---|
| `wind_1` ... `wind_12` | Monthly wind speed criteria (January to December), one simulated scenario value per month | m/s | `02_Simulation_files_output/Future_scenarios.RData`, used in `03_Indicator.R` |
| `wind_summer`, `wind_autumn`, `wind_winter`, `wind_spring` | Seasonal aggregation of the monthly criteria (mean of Dec–Feb, Mar–May, Jun–Aug, Sep–Nov respectively), used only in the seasonal-granularity sensitivity analysis | m/s | Computed in `03_Indicator.R` |

All wind criteria are benefit-type (higher wind speed = better site).

## SDG-related criteria (g1–g7)

| Abbreviation | SDG (target) | Related official indicator | Criterion (unit) | Type | Preference |
|---|---|---|---|---|---|
| `Low_Income_Pop` (g1) | SDG 1: No Poverty (1.2) | Population below the national poverty line | Share of the population with a nominal monthly per capita income of up to 1/2 minimum wage (%) | Proxy | Max (benefit) |
| `Harvest_Area` (g2) | SDG 2: Zero Hunger (2.4) | Agricultural area under productive and sustainable agriculture | Share of the municipal area planted or designated for harvest (%) | Proxy | Min (cost) |
| `Reservoir_Cap` (g3) | SDG 6: Clean Water and Sanitation (6.4) | Level of water stress | Maximum storage capacity of water reservoirs (m³) | Proxy | Min (cost) |
| `Elec_Access` (g4) | SDG 7: Affordable and Clean Energy (7.1) | Population with access to electricity | Share of permanent private households with electricity from a distribution company (%) | Direct | Min (cost) |
| `Employment` (g5) | SDG 8: Decent Work and Economic Growth (8.5) | Unemployment rate | Share of the total population in employment (%) | Proxy | Min (cost) |
| `Natural_Forest` (g6) | SDG 15: Life on Land (15.1) | Forest area as a proportion of total land area | Share of the municipal area covered by natural forests (%) | Direct | Min (cost) |
| `Protected_Area` (g7) | SDG 15: Life on Land (15.1) | Important biodiversity sites covered by protected areas | Share of the municipal area demarcated as nature conservation units (%) | Proxy | Min (cost) |

**Units**: criteria in percentage (%) are stored as proportions (0–1) in `00_Data/Processed/SDG_Criteria.csv`.

**Type**: `Direct` means the criterion corresponds directly to the official SDG indicator; `Proxy` means it is an indirect measure used due to limited public data availability at the municipal level.

**Preference**: `Max`/`Min` indicates whether higher or lower values make a municipality more preferable for siting (see Section 5.2.2 of the paper for the full rationale behind each direction).

**Raw column names** in `00_Data/Processed/SDG_Criteria.csv` (before the g1–g7 renaming applied in `03_Indicator.R`):

| Raw column name | Renamed to |
|---|---|
| `Percentage of population living with at most 1/2 minimum wage (%)` | g1: Low_Income_Pop |
| `Employed population (%)` | g5: Employment |
| `Natural forests (%)` | g6: Natural_Forest |
| `Households with access to electric energy (%)` | g4: Elec_Access |
| `Planted or destined to harvest area (%)` | g2: Harvest_Area |
| `Maximum capacity of reservoirs (m^3)` | g3: Reservoir_Cap |
| `Nature conservation unit demarcated area (%)` | g7: Protected_Area |

## Sources

SDG-related criteria were compiled from three Brazilian public institutions: the Brazilian Institute of Geography and Statistics (IBGE), the Institute for Sustainable Development and Environment of Rio Grande do Norte (IDEMA), and the Ministry of the Environment and Climate Change (MMA). All data were collected at the municipal level.
