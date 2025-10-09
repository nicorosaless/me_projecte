# Statistical Modelling Project (GIA, UPC)

A compact repository for the Statistical Modelling course project (2025–26). The project uses NHANES demographic data to practice end‑to‑end data work: preprocessing, descriptive analysis, GLM (numeric and binary), PCA, clustering, and profiling. A small, related time series dataset will be added for the TS component.

## Dataset
- Source: NHANES Demographics (DEMO_H, 2013–2014 cycle)
- File: `data/demographic.csv`
- Dictionary: see `data/data_description/hey.txt` (short notes) and in‑file headers
- Rows: ~10k; rich mix of numeric, binary, and categorical variables

Primary target variable
- INDFMPIR (family income‑to‑poverty ratio) — numeric, continuous, socially relevant, good variability. We’ll model it with GLM and use other variables (age, education, ethnicity, household size, etc.) as predictors.

Note on time series
- The demographic file has no internal date/time evolution (cycle code is constant for 2013–14). For the TS task, we will include an additional, related dataset across multiple years (e.g., NHANES or public health indicators) and build an annual series (e.g., obesity prevalence).

## Project scope
- Data preprocessing: missing values, outliers, recoding, transformations
- EDA: univariate/bivariate summaries and visuals
- Modelling: GLM (numeric target: INDFMPIR) and GLM (binary target, e.g., DMQMILIZ)
- PCA on numeric features; clustering (hierarchical and k‑means) + cluster profiling
- Time series on an external, thematically related dataset

## Quickstart (R)
- R ≥ 4.2 recommended
- Suggested packages: `tidyverse`, `readr`, `ggplot2`, `mice`, `VIM`, `forcats`, `FactoMineR`, `factoextra`, `cluster`, `caret`, `broom`
- Entry points:
  - `data_preprocessing.Rmd` — load, clean, and explore
  - `processing.ipynb` — optional notebook experiments

## Repository layout
- `data/` — raw data and descriptions
- `data_preprocessing.Rmd` — preprocessing and EDA workflow
- `processing.ipynb` — notebook for ad‑hoc analysis
- `preprocessingtodo.md` — task list and project checklist
- `projectguidelines/` — course guidelines (PDF and TXT)
- `DEADLINES.MD` — key dates and deliverables

## References & guidelines
- See `projectguidelines/pdf_txt/` for official course guidance
- Deadlines and deliverables tracked in `DEADLINES.MD`

Feedback and small improvements welcome. Let’s keep it simple, reproducible, and well‑documented.
