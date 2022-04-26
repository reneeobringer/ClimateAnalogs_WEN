# WaterEnergyDemandNexus

Code and data for an analysis of the water-energy demand nexus using climate analogs of 46 major US cities. The results from the analysis are currently in preparation for publication: 

**add citation**

Two categories of data were collected: utility data and climate data. The utility data (folder: `UtilityData`) includes water and electricity use data obtained from local utilities. The climate data (folder: `ClimateData`) were obtained from the North American Regional Reanalysis. All data were collected in 2019.

The code was developed in R version 4.1.2 and last ran on 26 April 2022. The code is contained in the file `climateanaloganalysis.R` and the associated Rdata files can be found in the `rdatafiles` folder. In order to run the code, the following R packages are required, with the version we used in parentheses: 

*  mvtboost (v0.6.0) 
*  stringr (v1.4.0)
*  measurements (v1.4.0)
*  gbm (v2.2)
*  ggplot2 (v3.3.5)
*  cowplot (v1.1.1)
*  scatterpie (v0.1.7)
*  dplyr (v1.0.8)

To run the code, we recommend the following steps: 

**add directions for changing working directories, etc.**
