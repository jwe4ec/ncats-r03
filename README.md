# ncats-r03
Preliminary analysis scripts for Gibson's NCATS R03 submission

Author: Jeremy W. Eberle

## Background

The present scripts conduct preliminary analyses for [Professor Jessie Gibson](https://www.nursing.virginia.edu/people/js6zn/)'s NCATS R03 (INSERT FOA) submission titled "Evaluating Fit of Psychosocial Interventions for Anxious Populations With Emotion Recognition Deficits."

## Data

The present scripts import deidentified datasets (stored privately on UVA Box) from the MDIB Development Study.

### MDIB Development Study

`final HD Aim 1 data_deid_2023-01-09_1525.csv` (collected INSERT-INSERT) and `final PD Aim 1 data_deid_2022-12-08_1339.csv` (collected 11/12/21-11/1/22) in `data/mdib/bot_cleaned` are long-format data Dr. Gibson collected via a RedCap survey of patients with Huntington's disease (HD) and Parkinson's disease (PD), respectively, administered at two time points.

The study's purpose is to develop the Movement Disorders Interpretation Bias Scale (MDIB). Bot responses have already been cleaned from both datasets by Dr. Gibson and a graduate research assistant.

## Scripts

The following scripts in the `code` folder are to be run in order.

`1_define_functions.R` defines functions for use in later scripts.

`2_compute_scores.R` imports the MDIB datasets from `data/mdib/bot_cleaned`. It cleans the data, scores selected scales, and outputs scored datasets to `data/mdib/scored` and lists of each scale's items to `data/mdib/helper`.

`3_run_mdib_analyses.R` imports the scored MDIB datasets, runs the analyses below, and outputs results to `results/mdib`.
- TODO
