## SOURCE FILES FOR SKEENA AND NASS SOCKEYE

### Required Files

This folder requires 2 files:

* *Generated_SR_Data_ForRapidStatus.csv*: This file contains the cleaned and processed spawner and recruit data 
* *Generated_StockInfo_ForRapidStatus.csv*: This file has the benchmarks for each CU


### Where do these files come from?

These file need to be copied over from the [Skeena and Nass spawner-recruit model fitting repository]( https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits). 

That repository requires several input files from different sources:

* *Stock-level Run reconstructions*: These are currently developed by LGL for DFO. They are not done routinely every year. A stand-alone contract is issued periodically to add a few yers of data.
* *Stock-level age composition estimates*: These are compiled from NC StAD, and rely on scale sampling programs with variable annual coverage. Assumptions regarding which sample sets to use for which stock need to be periodically reviewed.


R scripts in the [SRFits repo](https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits) filter out brood years with unlikely numbers (RpS>45)and infill missing values where possible. This data processing step, which was adopted after extensive sensitivity testing, was peer-reviewed as part of the [escapement goal review process](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2023/2023_008-eng.html). The details are documented in a technical report (**In Prep**).


### What are the required steps for updating?

The annual estimates are pulled from this folder into a merged data file, so no additional steps 
are required for that component. 

**However, the benchmark estimates need to be copied manually into the file DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv**. Note
that the CU lookup file has some data deficient CUs that are not listed in the generated source file, so need to be careful pasting the values over (rows don't line up!).


### Important Notes

- Run reconstructions are done at the stock level
- Most Skeena and Nass stocks line up with CUs
- Spawner estimates are generally based on index stream escapements that are scaled up.


