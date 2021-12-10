# manipulated media

## File Descrption

Three different files are included:
 1. *InCombined.csv* is a dataset containing daily level time series data related to Iran, COVID-19, misinformation tweets, and related correlates.
 2. *pests.r* is an R program for Poisson Estimators for State-space Time Series (PESTS) models and plots. We use the PEWMA model and ACF code contained in this script.
 3. *PEWMA Analysis.R* an R script implements the main time series anaylsis, using PEWMA models. It sources in pests.r and IrnCombined.csv.

## Analysis Location
The initial analysis is in a Google Colab Notebook: https://colab.research.google.com/drive/1k3RaQavLwEnZCui30PlimqRJmcj8dmns

## Data location
Data-prep.r generates a data.table called Combined and writes this table to "InCombined.csv" 

## Data location
Data dictionary can be found in the google sheet containing the data as it's being collected, in the Dictionary tab.
https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=1663522016
