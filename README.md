# README

Supporting code for the publication entitled: "Forecasting influenza hospital admissions within English sub-regions using hierarchical generalised additive models" in Nature Communications Medicine.

## Data

The data used for this study was live operational data from NHS England [definition given here](https://www.england.nhs.uk/long-read/process-and-definitions-for-the-daily-situation-report-web-form/). This influenza data is not in the public domain, we have therefore provided simulated data in this repository to show the models with.

The data is generated assuming a national epidemic wave, represented by local units that follow the same basic trend + perturbations.


## Structure

The running of this code assumes the working directory is at the root folder level (ie where show_models.R sits).

The main file to run to test the models and visualise them is `~/show_models.R`.

This script will:

1. install the package dependencies, from `~/src/depends.R` (you may want to install `here` first to set working directories)
2. generate the simulated epidemic data from `~/simulate_hierarchical_epidemic.R`
3. source and run the ARIMA model, from `~/run_arima.R`
4. source and run the GAM model, from `~/run_gam.R`
5. write out the different data to `~/data/`
6. read in the data, plot it and score the models

The configuration for data generation, overall modelling, and GAM parameters can be adjusted in `~/config.yaml`

Note that the models have not been pre-tuned.

- The bounds of available ranges for the ARIMA can be selected within `~/src/arima.R`, though the model will select sensible values itself.
- The days per basis function for the GAM can be updated in the `~/config.yaml` file.

The GAM model can either be run in parallel or sequentially. It runs on the scale of 1-5 minutes per fit, so adjust expectations on run time if going sequentially. The option to change this is in `~/config.yaml`. In the default setting, it requires ~12 cores to run in parallel, one core per `n_lookbacks`.


## Running

To test the code and different components you can:

1. run `~/simulate_heirarchical_epidemic.R`, `~/run_arima.R` and `~/run_gam.R` separately to inspect each step or
2. bring it all together in `~/show_models.R` which also has plotting and scoring code
