
# NOTE: THIS SCRIPT DOES NOT YET RUN ON THE GITHUB VERSION OF THE REPO! It reads the output files! It also reads in the metrics
# calculated in Script #2. These SHOULD BE THE SAME  as those output by the algorithm output file and can be read from this file
# once we add in a script to run the algorithm in here!


# Data prep for Scanner
# Aug 4 2021
# BMac changed this to build the file GP needs to run synoptic and also the file that is read into the Scanner tool
#  USE THE dataframes built in script 3 they have the filtering by expert assessment included now!

# JULY 2023 UPDATE
# Data set needed for algorithm input is now in script 3 (recreated this script). This now includes only steps needed
# to make the Scanner input files


#source('SCRIPTS2_Main/3_Generate_Synoptic_Input.R')
source("CODE/synopticFunction_Source.R")   # Added July 2023

library(dplyr)
library(tidyr)

# Longform metrics
metrics.long <- read.csv("DATA_PROCESSING/Metrics_Longform_SUB.csv")

# Full metrics dummy file with all CUS
metrics.dummy <- read.csv("DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")

# Lookup file
cu.lookup <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE) %>%
  dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

# Published WSP statuses
publ.status <- read.csv("DATA_LOOKUP_FILES/Publ_Status_Reorg_Status.csv") 

# Algorithm outputs IF reading these in
#algo.results <- read.csv("DATA_PROCESSING/Retro_Synoptic_Details.csv")

# OR

# Algorithm LT3 Run
retro.values <- read.csv("DATA_OUT/Retrospective_Metrics_Values.csv",stringsAsFactors = FALSE) %>%
                        left_join(cu.lookup %>% select("CU_ID",Group), by="CU_ID" )


retro.lt3 <- rapid_status(data.df = retro.values, algorithm = "StateOfTheSalmon3" ,group.var = "Species") 
# sots3.trial <- synoptic(data.df = metrics.synoptic.values, algorithm = "StateOfTheSalmon3",
#                                                  group.var = "Species")
#
write.csv(retro.lt3$data,"DATA_PROCESSING/Retro_Results_LT3.csv",row.names = FALSE)


# ----- Get the ratios and build the dataframe for metrics
# metrics.tmp <- metrics.raw %>% rename(Compare=Value) %>%
#                                relocate(Data_Type, .after=Label)%>%
#                               mutate(Value= case_when( (Metric == "RelAbd" | Metric == "AbsAbd") ~ (Compare/LBM),
#                                                               (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
#                               )
#                               ) %>%
#                               mutate(Metric = recode(Metric, RelAbd = "RelLBM", AbsAbd = "AbsLBM")) %>%
#                               rbind(metrics.raw %>% filter(Metric == "RelAbd"| Metric =="AbsAbd") %>%
#                                                     rename(Compare=Value) %>%
#                                                     mutate(Value=Compare/UBM) %>%
#                                                     mutate(Metric=recode(Metric, RelAbd = "RelUBM", AbsAbd = "AbsUBM"))
#                               ) %>%

# Check that metrics in algo.results and metrics.long line up
# compare.metrics <-  algo.results %>% select(CU_ID, Year, LongTrend) %>%
#                                      right_join(metrics.long%>%select(CU_ID, Year, Metric, Value) %>%filter(Metric =="LongTrend"), by=c("CU_ID","Year"))
# identical(compare.metrics$LongTrend, compare.metrics$Value)
# CU_13 does not align on the trend metrics - these are unstable for this CU


# Now create the dataframes for the Scanner from metrics.tmp2
metrics.scanner <-  metrics.long  %>%   select(-c(X, Label, DataType)) %>%
                                        filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
                                        # mutate(Confidence =NA) %>%
                                        rbind(
                                             publ.status %>% select(CU_ID,Year,IntStatus) %>%
                                                             pivot_longer(cols=IntStatus, names_to = "Metric", values_to="Status") %>%
                                                             inner_join( metrics.long %>% select(CU_ID, Species, Stock) %>% unique() , by="CU_ID") %>%
                                                             mutate(Compare=NA, LBM=NA, UBM=NA, Value=NA)
                                        ) %>%
                                        rbind(
                                          retro.lt3$data %>% select(CU_ID, Species, Stock, Year, RapidStatus=SynStatus) %>%
                                                             mutate(RapidStatus = na_if(RapidStatus, "None")) %>%
                                                             pivot_longer(cols=c(RapidStatus), names_to = "Metric", values_to="Status") %>%
                                                             mutate(Compare=NA, LBM=NA, UBM=NA, Value=NA) %>%
                                                             relocate(Status, .before=Value)
                                        )%>%
                                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                                        left_join((algo.results %>% select(c(CU_ID,Year,Confidence=ConfidenceRating3)) %>% mutate(Metric="RapidStatus")),
                                                  by=c("CU_ID", "Year", "Metric")) %>%
                                        select(-CU_ID) %>%
                                        rename(CU_ID=CU_ID_Alt2_CULookup)%>%
                                        relocate(CU_ID, .before=Species)%>%
                                        relocate(c(Status,Confidence), .after=last_col())


# metrics.scanner <-  metrics.tmp2  %>%   filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
#                                         select(-c(DataType, AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) %>%
#                                         rbind(
#                                             publ.status %>% select(CU_ID,Year,IntStatus) %>%
#                                                             pivot_longer(cols=IntStatus, names_to = "Metric", values_to="Status") %>%
#                                                             inner_join( metrics.raw %>% select(CU_ID, Species, Stock, Data_Type) %>% unique() , by="CU_ID") %>%
#                                                             mutate(Label="SpnForAbd_Wild", Compare=NA, LBM=NA, UBM=NA, Value=NA)
#                                         ) %>%
#                                         rbind(
#                                             sots3.trial$data %>% select(CU_ID, Species, Stock, DataType, Year, SynStatus) %>%
#                                                                  rename(Status=SynStatus, Data_Type=DataType) %>%
#                                                                  mutate(Status = na_if(Status, "None")) %>%
#                                                                  mutate(Label="SpnForAbd_Wild", Metric="SynStatus", Compare=NA, LBM=NA, UBM=NA, Value=NA)
#                                         )%>%
#                                         left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
#                                         select(-CU_ID, -Data_Type) %>%
#                                         rename(CU_ID=CU_ID_Alt2_CULookup)%>%
#                                         relocate(CU_ID, .before=Species)%>%
#                                         relocate(Status, .after=last_col())
#



# syn.statuses <-  pivot_longer(select(retro.status.df.stat, -c(ProbDeclBelowLBM, DataType, Percentile, NumStdMetrics)), cols= -c(CU_ID, Species, Stock, Year),
#                           names_to="Metric", values_to="Status")
#
# long.retro <- retro.status.df.val %>%
#                                     select(-c(ProbDeclBelowLBM, Percentile, NumStdMetrics)) %>%
#                                     pivot_longer(cols = -c(CU_ID, Species, Stock, DataType, Year),names_to="Metric", values_to="Value") %>%
#                                     left_join(statuses, by = c("CU_ID","Species","Stock","Year","Metric"))

# Could pull in the Benchmarks and comparison values from metrics.raw also if wanted
# but will require wrangling because the metrics names are different for the LBM and UBM ones
#setwd("../")
#metrics.dummy <- read.csv("DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")


# Metrics.dummy uses CU_IDs from NuSEDs for the tool NOT the ones used in the data prep code, so must match these
# CU_ID_Alt2_CULookup in the MAIN_CU_LOOKUP_FOR_SOS.csv

# Filter OUT the metrics data we have just calculated so this is just the EMPTY CU datasets

# Throw warning if there is data in the dummy file
if(nrow(metrics.dummy %>%  filter_at(vars(Compare, LBM, UBM, Value, Status), any_vars(!is.na(.)))) > 0){
  stop("Data present in the dummy file!! Clear this.")
}


metrics.out <- metrics.dummy %>% select(-X, -Label) %>%
                                 filter(!CU_ID %in% metrics.scanner$CU_ID) %>%
                                 filter(!is.na(CU_ID))%>%
                                 filter(!is.na(Stock))%>%
                                 filter(!Metric == "IntStatus") %>%
                                 rbind(metrics.scanner)
# metrics.out <- metrics.dummy %>% select(-X) %>%
#                                  filter(!CU_ID %in% metrics.scanner$CU_ID) %>%
#                                  filter(!is.na(CU_ID))%>%
#                                  filter(!is.na(Stock))%>%
#                                  filter(!Metric == "IntStatus") %>%
#                                  rbind(metrics.scanner)
# #setwd("../")
#write.csv(metrics.out, "build_PStat_data/data/METRICS_FILE_BY_CU_forPSST.csv")
write.csv(metrics.out, "DATA_OUT/METRICS_FILE_BY_CU_SCANNER.csv")


# ========================= NOW fix the escapement/recruit datasets, since I had to sub values into the WILD columns for (Chum) to get the metrics to run!
# ALSO remove the Trends data for Fraser SK

cu.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv")

# Remove the Trends data for all CUs - no longer needed and was only different for FR SK
  # Addded Oct 25 2021 to add Rel Abd metric values to time series
  cu.lookup$CU_ID <- gsub("-","_", cu.lookup$CU_ID)
  relabd.metric <- metrics.scanner %>% filter(Metric=="RelLBM")

cu.clean <-  cu.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild, Abd_StartYr, UseYear)) %>%
                         mutate(SpnForAbd_Wild = case_when(CU_ID %in% c("CM-02", "CM-03", "CM-04", "CM-05", "CM-06", "CM-07", "CM-08", "CM-09") ~ NA_real_,
                                                           TRUE ~ SpnForAbd_Wild)) %>%
                        rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                        # Added Oct 25 2021 to have the timeseries used for assessing relative abunance metrics as an option for plotting in the Scanner
                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                        left_join(cu.lookup %>% select(CU_ID=CU_ID_Report, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                        mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                        left_join(select(relabd.metric, CU_ID, Year, Compare), by=c("CU_ID","Year")) %>%
                        rename(RelAbd_metric_ts=Compare)%>%
                        relocate(c(CU_ID, DU_ID, CU_Name), .after=Species) %>%
                        select(-c(CU_ID_Report, CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                        filter(!CU_ID=="")


write.csv(cu.clean, "DATA_OUT/MERGED_FLAT_FILE_BY_CU_SCANNER.csv")
# REmove the "wild" from FR CM


# Same for the POP file
#pop.info <- read.csv("build_PStat_data/data/PopLookup.csv")
pop.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_POP_SUB.csv")



pop.clean <- pop.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild)) %>%
                          rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                          # Fix the Coho - some CUs have no hatchery origin spawners so can be put into the wild column - other CUs had hatchery which cannot be
                          # separated at teh population level (or at least HAS not been but I need to look at this and my Lynda notes to be sure)
                          # CUs with NO hatchery = Fraser Canyon (CO-05)
                          left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                          left_join(cu.lookup %>% select(CU_ID=Pop_TimeSeriesData_CU_ID, CU_ID_Alt2_CULookup), by="CU_ID") %>%
                          mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          select(-c(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          mutate(Escapement_Wild = case_when(CU_ID == "CO_02" ~ Escapement_Total, TRUE ~  Escapement_Wild)) # Note this is the CU_ID from the WSP process not NuSEDs






write.csv(pop.clean, "DATA_OUT/MERGED_FLAT_FILE_BY_POP_SCANNER.csv")



#
# pop.info.adj <- pop.info %>% mutate(Comment = case_when(WSP_ts == "yes"~ "treated data used to produce CU level timeseries",
#                                                         WSP_ts == "no" ~ "raw data - do not use for analysis"))
# write.csv(pop.info.adj,"build_PStat_data-main/data/PopLookup.csv")
































