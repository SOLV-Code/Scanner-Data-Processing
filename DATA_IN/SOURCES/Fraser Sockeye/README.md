## SOURCE FILES FOR FRASER SOCKEYE

### Required Files

#### Annual Update

Updates require 2 files:

* *Fraser Sockeye SR Data_2023.csv*: This file contains spawner and recruit data used for forecasting though the code only pulls recruits from this due to the stock groupings
* *SKAll (November 2022).csv*: This file contains all spwaner estimates and is used for the CU and pop-level spawner timeseries

#### Occasional Update

Additional files needed (mostly static though should be looked over annually in case revisions needed)
* *Sockeye_Fraser_CU_Streams.csv*: defines sites in each CU using the name that corresponds to the PopName in SKAll
* *Sockeye_Fraser_Expansion_Years.csv*: years that are gap filled in applicable CUs
* *Sockeye_Fraser_SR_Streams.csv*: defines sites in each CU corresponding to the Stock Group in the SR data

#### Other files used

* *MOD_MAIN_CU_LOOKUP_FOR_SOS.csv*
* *Site_Info_GeneratedDecoder_Fraser_Sk.csv*: used for set up but not in updating


### Where do these files come from?

* *Fraser Sockeye SR Data_2023*: Comes from the Program Head for Fraser SK Analytical, currently Kaitlyn Dionne
* *SKAll*: From the Fraser Interior Area data manager, currently Tanya Vivian


### What are the required steps for updating?

Files can be read directly into the code once converted into csv format.
Update file names and *last.yr* on line 170


### Important Notes

- SpnforTrend = EFS. This uses the streams used in the WSP assessment to assess trend metrics
- SpnforAbd = ETS. This uses the streams that are included in the SR data because it is compared to SR-derived benchmarks
- Shuswap-ES currently has no recruit estimates. Consider adding Scotch and Seymour for this. Stock ID 99 is the combined CU.



