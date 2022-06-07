## Evaluating Ensemble Post-Processing for Wind Power Forecasts

This GitHub repository contains the code used in the paper "Evaluating Ensemble Post-Processing for Wind Power Forecasts" [1].



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


### References

[1] Phipps, K, Lerch, S, Andersson, M, Mikut, R, Hagenmeyer, V, Ludwig, N. Evaluating ensemble post-processing for wind power forecasts. Wind Energy. 2022; 1- 27. doi:10.1002/we.2736


### Funding

This work is funded by the German Research Foundation (DFG) as part of the Research Training Group 2153 ‘Energy Status Data – Informatics Methods for its Collection, Analysis and Exploitation’ and by the Helmholtz Association's Initiative and Networking Fund through Helmholtz AI, the Helmholtz Association under the Program ‘Energy System Design’ and the Joint Initiative ‘Energy System Design - A Contribution of the Research Field Energy’. Nicole Ludwig acknowledges financial support by the DFG under Germany's Excellence Strategy – EXC number 2064/1 – Project number 390727645. Sebastian Lerch acknowledges support by the DFG through SFB/TRR 165 ‘Waves to Weather’ and by the Vector Stiftung through the Young Investigator Group ‘Artificial Intelligence for Probabilistic Weather Forecasting’. The research detailed in the current paper was based on data from the ECMWF obtained through an academic licence for research purposes.
