# Data Processing for the State of the Salmon Scanner Tool

**This is an internal working version of the data processing code**


## Overview

### Current Scope

* Fraser Sockeye: TEXT
* Southern BC Chinook: TEXT
* Interior Fraser Coho: TEXT
* Fraser Chum: TEXT
* Fraser Coho: TEXT


### Overview of data processing steps

1) data prep -> clean files

2) merge files into single data set

3) calculate WSP metrics using [WSPMetrics package](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg) and [MetricsCOSEWIC package](https://github.com/SOLV-Code/MetricsCOSEWIC)




### Repository Structure

short overview with links



## Data Sources

### Skeena and Nass Sockeye

Two input files need to be updated when incorporating updates for Skeena and Nass sockeye.

**DATA_IN/SkeenaNassSockeye_Generated_SR_Data.csv**

This file contains cleaned and processed SR data by stock. It is copied over here from the [Skeena and Nass spawner-recruit model fitting repository]( https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits). That repository requires several input files from different sources:

* run reconstructions developed by LGL for DFO
* age composition data from NC StAD
* stock to CU matching file reviewed by NC StAD

R scripts in the [SRFits repo](https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits) filter out brood years with unlikely numbers (RpS>45)and infill missing values where possible.



Other Notes:

- Most Skeena and Nass stocks line up with CUs
- Spawner estimates are generally based on index stream escapements that are scaled up.





















