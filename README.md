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

Source files  used here are the outputs from data processing in a separate github repository,
as described in the [Skeena and Nass Data Source README](https://github.com/SOLV-Code/Scanner-Data-Processing/tree/main/DATA_IN/SOURCES/Skeena_Nass%20Sockeye).

The data needed to feed into that processing repo comes from 2 sources: 

* run reconstructions developed periodically for DFO by an external contractor. The two most recent updates were done up to 2019 and up to 2022, using funding from an escapememt goal review required under the PST. Follow-up with NC StAD is needed to determine how annual updates would be coordinated and funded.
* stock-level age composition data collected and compiled by NC StAD.  Updating requires both a compilation of new records and a review of assumptions (i.e., which annual or average samples to use for which CU).

Subsequent steps are fully automated (spawner-recruit model fits, benchmark calculations, output files), but should be reviewed periodically to ensure that assumptions and scoping decisions are still valid.




### Southern BC Chinook



* Main source 1:  nuSEDS data dump - specific query   -> get from nuSEDS team   sample file where?
* need to manually fix headers and some popID. Specifically:
   * List


* Main source 2: EPAD data dump - specific query -> get from SEP team  (Sample file at WHERE?)

















