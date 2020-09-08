# analysis directory

This is the main directory for the analysis. All of the scripts necessary to run the analysis are placed directly into this directory. All datasets should be placed in the `input` or `output` folder.

Because most of the scripts depend on a custom package that is excluded for anonymity reasons, most of the scripts will not run without error. However, all of the output from prior runs is available in the output directory and the main analysis.Rmd file will run without error.

Here are the files included in this directory.

- `check_packages.R` - You can put a list of all libraries used in any R script for the analysis and this script will check to make sure they are installed and if they are not installed it will attempt to install them.
- `useful_functions.R` - A script to keep track of R functions that might have uses across multiple scripts.
- `organize_data.R` - A script that will read in the raw Census data, code necessary variables and create the analytic datasets of actual couples and alternate patners.
- `run_models.R` - This script will run the actual conditional logit models on the analytic data. Because multiple models are run on five separate datasets with millions of observations, this script will take some time to run.
- `run_samplesize_tests.R`  - This script will run the simulation used to determine how much variance there was between parameters depending on the number of partners sampled. It runs a lot of models and will therefor take some time to run.
- `analysis.Rmd` - The main analysis for the paper in R Markdown format. This draws on the output from the models in the output directory. The report from this analysis is available in `analysis.html`. 
- `run_entire_project.sh` that will run the entire analysis from scratch. Keep in mind that this script will remove any HTML output and all files in the `logs` and `output` directories before attempting to re-run the scripts, in order to ensure that old output does not get mixed in with new output. As other scripts are added to the project, this shell script should be updated. Any user should be able to view this shell script and see how all the various scripts in this directory are used to reproduce the analysis.