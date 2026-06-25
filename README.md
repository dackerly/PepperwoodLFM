# PepperwoodLFM

Analysis code and data for a study linking live fuel moisture (LFM) to plant water potential (Ψ) and functional traits across California woody species. The project uses field measurements from Pepperwood Preserve, Sedgwick Reserve, and Sierra Nevada sites to build a physiologically grounded model of drought-driven fire risk.


## Repository structure

```
scripts_final/          # Cleaned analysis pipeline (run in order)
  00_figure_setup.R       Setup: shared color palettes and theme
  00_functions.R          Setup: shared helper functions
  01_data_prep.R          Data preparation and cleaning
  02_combine_data.R       Combine site-level datasets
  03_traits_compile.R     Compile trait data (TRY database + local)
  04_Q1_models_and_figures.Rmd   Q1 mixed models and main figures
  05_Q1_bayesian.Rmd      Q1 Bayesian species-interaction model (brms)
  06_Q1_cross_study.Rmd   Q1 cross-study comparisons
  07_Q2_model_params.Rmd  Q2 species parameter analysis
  08_Q3_trait_correlations.Rmd   Q3 trait–parameter correlations
  09_Q4_model_prediction.Rmd     Q4 predictive model with traits
  10_Fig8_hydraulics.Rmd  Hydraulic trait figures

scripts/                # Original working scripts (kept for reference)
data/                   # Field measurements, species codes, traits, external datasets
  other_studies/          Data from collaborating studies (Sedgwick, Sierra, TRY)
figures/                # Generated figures (not tracked; regenerate from scripts)
results/                # Model outputs and summary tables (not tracked)
results_final/          # Final manuscript figures and tables (not tracked)
```

## Dependencies

R (≥ 4.0) with key packages: `lme4`, `brms`, `MuMIn`, `ggplot2`, `tidyverse`, `patchwork`.

## Usage

Run scripts in `scripts_final/` in numeric order. Scripts `00–03` prepare data; scripts `04–10` produce analyses and figures. The `.Rmd` files can be knit individually once the data-prep scripts have been run.
