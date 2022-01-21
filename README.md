This repository contains the codes from my final project 'Modelling survival data from hepatorenal syndrome acute kidney injury' of the master's degree in statistics and operations research (UPC).  

The files have the following structure:

- 0-prepare-data: Contains two numbered SAS files that have to be run in order. The objective of the first file is to select the subjects that have the characteristics to enter the study. The second file contains the codes to include all the variables of interest in the study for the selected sample.
  
- 0-data.R: Prepare the R data-set to be used through the other R scripts.
- 1-descriptive-analysis.R: Tables and figures from the descriptive section.
- 2-0-survival.R: Tables and figures from the introduction to the survival analysis section.
- 2-1-competing-risks-basal-covariates.R: Tables and figures from the competing risks with basal variables section.
- 2-2-competing-risks-time-varying-covariates.R: Tables and figures from the competing risks with time-varying covariates section.
- 2-3-competing-risks-landmarks.R: Tables and figures from the competing risks with landmarks section.
- 2-4-multi-state-model.R: Tables and figures from the multi-state models section.
