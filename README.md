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

4) calculate WSP rapid statuses

5) prep data for the Scanenr


### Repository Structure

short overview with links


## Data Needs

Three information streams are needed for the Scanner:

* CU-level escapement time series (aggregated across consistently-assessed sites that can be used for analysis)
* Population-level data (all available populations in each CU)
* Attributes (CU and pop)

### CU-level Data

Steps that may be required:
* Review CU site lists and revise as necessary with the Data Management Group
* Select enumeration sites to include in the aggregated CU time series
    * Review historical information (supplementation, transplants, etc) 
    * Filter for high quality (classification level 4 or higher using NUSEDs system)
    * Filter for consistently assessed sites (>50% of the time series [70% for Fraser sockeye])
* Truncate start years if necessary
* Validate zeros
* Calculate hatchery components (PNI?)
* Infill if needed and appropriate
    * Mean proportion method OR (cycle-line) average if only one site or no data across sites or sites not correlated

### Population-level Data

Two types of data:
* Treated data for sites that are included in the CU-level aggregated time series
* Raw data for sites not included

These can exist in the same population level csv output int he data prep steps but are differentiated in the PopAttributes file

### Attributes
* CU
    * attributes from NUSEDs
    * additional CU information: generation length, SMU, life history type, run timing
    * metric calculation details 
* Pop 
    * attributes from NUSEDs
    * expert validated CU associations
    * POP_IDs
 
* additional information for Scanner-specific attribute files (scannerdata repo)
    * CU: contact, data messages, etc.
    * Pop: inclusion in aggregated CU timeseries, data messages 


## Data Sources

### Skeena and Nass Sockeye

Source files  used here are the outputs from data processing in a separate github repository,
as described in the [Skeena and Nass Data Source README](https://github.com/SOLV-Code/Scanner-Data-Processing/tree/main/DATA_IN/SOURCES/Skeena_Nass%20Sockeye).

The data needed to feed into that processing repo comes from 2 sources: 

* run reconstructions developed periodically for DFO by an external contractor. The two most recent updates were done up to 2019 and up to 2022, using funding from an escapememt goal review required under the PST. Follow-up with NC StAD is needed to determine how annual updates would be coordinated and funded.
* stock-level age composition data collected and compiled by NC StAD.  Updating requires both a compilation of new records and a review of assumptions (i.e., which annual or average samples to use for which CU).

Subsequent steps are fully automated (spawner-recruit model fits, benchmark calculations, output files), but should be reviewed periodically to ensure that assumptions and scoping decisions are still valid.


### Southern BC Chinook

Source files  used here are the outputs from data processing in a separate github repository,
as described in the [SBC Ck Data Source README](https://github.com/SOLV-Code/Scanner-Data-Processing/blob/main/DATA_IN/SOURCES/SBC%20Chinook/README.md).

The data needed to feed into that processing repo comes from 2 sources: 

* raw spawner records by site from the nuSEDS database. Current database holdings are available upon request from DFO's regional nuSEDS team on short notice any time of year. However, the data entry of new records by area staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the nuSEDS query extract are currently required to prepare the input file, but could be automated.
* records of spawner enumaration and broodstock collection by SEP hatchery staff from the EPAD database. Current database holdings are available upon request from DFO's regional nuSEDS team on short notice any time of year. However, the data entry of new records by hatchery staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the nuSEDS query extract are currently required to prepare the input file, but could be automated.

Subsequent steps are fully automated (record matching  between data bases, quality filtering, infilling), but generate a large amount of diagnostic information that needs to be reviewed for each major update, typically leading to several iterations of tweaking data and/or settings. 

Site-specific settings, such as sites to include in each CU time series, should be reviewed periodically to ensure that assumptions and scoping decisions are still valid.   

Important notes:

* there is no established process for re-estimating biological benchmarks for those CUs where they are currently available
* For some CUs, the method for determining enhancement level was recently revised, but this approach has not been expanded to cover all SBC Ck CUs.

### Fraser Sockeye

Details provided in the [Fraser Sockeye Data Source README](https://github.com/BronwynMacDonald/Scanner-Data-Processing/tree/main/DATA_IN/SOURCES/Fraser%20Sockeye#readme)

### Interior Fraser Coho

Details provided in the [Interior Fraser Coho Data Source README](https://github.com/BronwynMacDonald/Scanner-Data-Processing/blob/main/DATA_IN/SOURCES/Fraser%20Coho/README.md)

### Fraser Chum

Details provided in the [Fraser Chum Data Source README](https://github.com/BronwynMacDonald/Scanner-Data-Processing/blob/main/DATA_IN/SOURCES/Fraser%20Chum/README.md)

### Fraser Pink

Details provided in the [Fraser Pink Data Source README](https://github.com/BronwynMacDonald/Scanner-Data-Processing/blob/main/DATA_IN/SOURCES/Fraser%20Pink/README.md)













