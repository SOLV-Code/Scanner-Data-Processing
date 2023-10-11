## SOURCE FILES FOR INTERIOR FRASER COHO

### Required Files

#### Annual Update

Updates requires:

* *IFC Data 1975-2021 - created 2023-04-03 for Salmon Scanner.csv*: Full escapement and return data file 

#### Occasional Update

Additional files needed (mostly static though should be looked over when updates are made)
* *IFC Infill exceptions.csv*: infilling not actually run anymore so this part is irrelevant unless we start doing this again
* *FRSK_CU_Info_masterUpdate.csv*: just used to calc age proportions

#### Other files used

* *MOD_MAIN_CU_LOOKUP_FOR_SOS.csv*
* *Fraser Coho POPID Lookup_adj with HO revisions Feb 25 2020.csv*: Pop ID crosswalk


### Where do these files come from?

* *IFC Data 1975-2021 - created 2023-04-03 for Salmon Scanner.csv*: FIA Coho Analytical Program (currently Michael Arbeider)
* Any updates to other files should be run through the same program


### What are the required steps for updating?

Files can be read directly into the code once converted into csv format.

Check: 
* variable names
* double entries
* stream name changes
* are all populations included or only the WSP "green" and "amber" sites?
* check the sites included against those listed as included in the PopAttributes csv if Graser Coho changes any coded as "green/amber"

Run with no infilling (this is how it is set up in the code sub file currently)




