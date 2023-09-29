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

* *Stock-level Run reconstructions*: These are currently developed by LGL for DFO. They are not done routinely every year. A stand-alone contract is issued periodically to add a few yers of data.
* *Stock-level age composition estimates*: These are compiled from NC StAD, and rely on scale sampling programs with variable annual coverage. Assumptions regarding which sample sets to use for which stock need to be periodically reviewed.


R scripts in the [SRFits repo](https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits) filter out brood years with unlikely numbers (RpS>45)and infill missing values where possible. This data processing step, which was adopted after extensive sensitivity testing, was peer-reviewed as part of the [escapement goal review process](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2023/2023_008-eng.html). The details are documented in a technical report (**In Prep**).


**MOD_MAIN_CU_LOOKUP_FOR_SOS.csv**


* stock to CU matching file reviewed by NC StAD

Other Notes:

- Most Skeena and Nass stocks line up with CUs
- Spawner estimates are generally based on index stream escapements that are scaled up.





















