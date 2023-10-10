## SOURCE FILES FOR SBC Chinook

### Required Files

This folder requires 6 files:

* *Esc_Enh-Data_Stage1_MergeSources_CleanedforDB.csv*
* *Esc_Enh-Data_Stage2_QualFiltered_CleanedforDB.csv*
* *Esc_Enh-Data_Stage3_Infilled_CleanedforDB.csv*
* *SBC_Chinook_VerifiedSiteLookup.csv*

FlatFile_For_COSEWIC_ByCU.csv

FlatFile_For_COSEWIC_BySite.csv

### Where do these files come from?



These file need to be copied over from the [SBC CHinook Data processing repo](https://github.com/SOLV-Code/SBC-Ck-Dashboards-2.0). 

That repository requires several input files from different sources:
* 

* Main source 1:  nuSEDS data dump - specific query   -> get from nuSEDS team   sample file where?
* need to manually fix headers and some popID. Specifically:
   * List


* Main source 2: EPAD data dump - specific query -> get from SEP team  (Sample file at WHERE?)



R scripts in the [SBC CHinook Data processing repo](https://github.com/SOLV-Code/SBC-Ck-Dashboards-2.0) DESCRIBE DATA PROCESSING. This data processing step was developed and peer-reviewed as part of the [integrated status assessment for SBC Chinook](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2016/2016_042-eng.html). Data and data processing steps have been reviewed periodically since then, as documented in a [technical report]\(https://science-catalogue.canada.ca/record=4089004~S6).


* diagnostics to look at when running new extract:
   * missing popID
   * duplicate records (after popID fix)

*NEED TO RUN LINE BY LINE TO ENSURE ANY ERRORS ARE CAUGHT (e.g. X26 vs X29 in EPAD source)



### What are the required steps for updating?



### Important Notes

