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

An additional analysis investigating the scaling properties of water and electricity consumption has been conducted. These results are currently under review.

A permanent version of this repository is available on Zenodo: [![DOI](https://zenodo.org/badge/484154465.svg)](https://zenodo.org/badge/latestdoi/484154465).

Two categories of data were collected: utility data and climate data. The utility data (folder: `UtilityData`) includes water and electricity use data obtained from local utilities. The climate data (folder: `ClimateData`) were obtained from the North American Regional Reanalysis. All data were collected in 2019.

There are two R scripts included in this repository, both were developed in R version 4.1.2. 

The code in the file `climateanaloganalysis.R` was developed for the initial climate analog study, which was published in [_One Earth_](https://doi.org/10.1016/j.oneear.2023.10.004). This script was last run on 26 April 2022 and had several associated Rdata files in the `rdatafiles` folder. In order to run the code, the following R packages are required, with the version we used in parentheses: 

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

The code in the file `scalinganalysis.R` was developed for the recent investigation into the scaling properties of water and electricity consumption, which is currently under review. This script was last run on 17 October 2024 and does not have any associated Rdata files. In order to run the code, the following R packages are required, with the vesrion we used in parentheses: 

* dplyr (v1.0.10)
* ggplot2 (v3.4..0)
* ggrepel (v0.9.2)
* stringr (v1.4.1)
* data.table (v1.14.6)
* rlist (v0.4.6.2)
* tidyverse (v1.3.2)
  
To run the code, users need to update the path to the folder downloaded or cloned from this repository. This change can be made on line 29 of `scalinganalysis.R`. Once this path is changed, the data directories will be assigned automatically after running lines 32-37 in `scalinganalysis.R`. The code can then be run sequentially or users can choose to run different sections, provided they load the data first.
