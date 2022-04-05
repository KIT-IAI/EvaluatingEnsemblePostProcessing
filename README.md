## Evaluating Ensemble Post-Processing for Wind Power Forecasts

This GitHub repository contains the code used in the paper "Evaluating Ensemble Post-Processing for Wind Power Forecasts".



The code is structured as follows:

- The folder **calibration** includes the code for performing the EMOS calibration on weather and wind power ensembles.
- The folder **evaulation** includes functions used for evaluating the forecasting performance.
- The folder **forecasting** includes the forecast models used.
- The folder **pre_processing** includes scripts for preprocessing the data as well as all open source data used in our work.
- The folder **workflows** includes the workflows used to compare our post-processing strategies.



Please note that all data is included in this repository, apart from the Observation data from the Open Power Systems Data project. This data can be downloaded directly from the Open Power Systems Data project, here https://open-power-system-data.org/. 

The Ensemble Prediction System data for sweden was downloaded from the MARS archive and modified as follows:
- The data was separated into both bidding zones based on geographical locations.
- The data was aggregated with a weighted average, with more weight given to the areas with high install wind generation capacity.
- This final aggregated ensemble prediction system was used in further experiments.
