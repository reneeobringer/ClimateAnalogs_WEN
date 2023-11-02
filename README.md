# WaterEnergyDemandNexus

Code and data for an analysis of the water-energy demand nexus using climate analogs of 46 major US cities. The results from the analysis have been published in [_One Earth_](https://doi.org/10.1016/j.oneear.2023.10.004). The manuscript can be cited as:

```bibtex
@article{obringer2023,
  title = {Contemporary Climate Analogs Project Strong Regional Differences in the Future Water and Electricity Demand across {{US}} Cities},
  author = {Obringer, Renee and Nateghi, Roshanak and Knee, Jessica and Madani, Kaveh and Kumar, Rohini},
  year = {2023},
  journal = {One Earth},
  doi = {10.1016/j.oneear.2023.10.004}
}
```

A permanent version of this repository is available on Zenodo: [![DOI](https://zenodo.org/badge/484154465.svg)](https://zenodo.org/badge/latestdoi/484154465).

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
*  sf (v1.0.6)

To run the code, users need to update the path to the folder downloaded or cloned from this repository. This change can be made on line 37 of `climateanaloganalysis.R`. Once this path is changed, the data directories will be assigned automatically after running lines 41-48 in `climateanaloganalysis.R`. The code can then be run sequentially or users can choose to run different sections, provided they load the rdata files in the `rdatafiles` folder prior to each section. 
