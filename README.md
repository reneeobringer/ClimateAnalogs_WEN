# WaterEnergyDemandNexus

Code and data for an analysis of the water-energy demand nexus using climate analogs of 46 major US cities. The results from the analysis are currently in preparation for publication: 

**add citation**

Two categories of data were collected: utility data and climate data. The utility data (folder: `UtilityData`) includes water and electricity use data obtained from local utilities. The climate data (folder: `ClimateData`) were obtained from the North American Regional Reanalysis. All data were collected in 2019.

The code was developed in R version 4.1.2 and last ran on 26 April 2022. The code is contained in the file `climateanaloganalysis.R` and the associated Rdata files can be found in the `rdatafiles` folder. In order to run the code, the following R packages are required: 

*  mvtboost (v0.6.0) Note, this package is no longer available on CRAN and must be downloaded from the author's GitHub (https://github.com/patr1ckm/mvtboost).
*  stringr (v1.4.0)
*  measurements (v1.4.0)
*  gbm (v2.2)
*  ggplot2 (v3.3.5)

To run the code, we recommend the following steps: 

**add directions for changing working directories, etc.**
