# Replication Package for "A Counterfactual Choice Approach to the Study of Partner Selection"

Aaron Gullickson

The files contained in this repository will reproduce the full analysis for the paper "A Counterfactual Choice Approach to the Study of Partner Selection."

The entire analysis is conducted in *R* version 3.6.2. Required packages for the analysis will be installed by running the `check_packages.R` script which is run by default in all of the other scripts. 

All of the scripts necessary to run the analysis are placed directly into the top-level directory. The raw data for the analysis is located in the `input` directory. The `output` directory contains all constructed analytical data. The `logs` directory provides logs (in html format) of the various scripts run.

Below is a description of each of the files included in the top-level repository. 

- `check_packages.R` - This script will check to make sure they are installed and if they are not installed it will attempt to install them.
- `useful_functions.R` - A script to keep track of custom R functions that are used by multiple scripts.
- `organize_data.R` - A script that will read in the raw data, code necessary variables and create the analytic datasets of actual couples and alternate partners.
- `run_models.R` - This script will run the conditional logit models on the analytic data. Because multiple models are run on five separate datasets with millions of observations, this script will take some time to run.
- `run_samplesize_tests.R`  - This script will run the simulation used to determine how much variance there was between parameters depending on the number of partners sampled. It runs a lot of models and will therefore take some time to run.
- `analysis.Rmd` - The main analysis for the paper in R Markdown format. This draws on the output from the models in the output directory. The report from this analysis is available in `analysis.html`. 
- `run_entire_project.sh` that will run the entire analysis from scratch. Keep in mind that this script will remove any HTML output and all files in the `logs` and `output` directories before attempting to re-run the scripts, in order to ensure that old output does not get mixed in with new output.
