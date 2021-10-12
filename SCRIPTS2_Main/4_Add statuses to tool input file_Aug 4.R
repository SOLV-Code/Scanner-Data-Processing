# Data prep for Scanner
# Aug 4 2021
# BMac changed this to build the file GP needs to run synoptic and also the file that is read into the Scanner tool
#  USE THE dataframes built in script 3 they have the filtering by expert assessment included now!

#source('SCRIPTS2_Main/3_Generate_Synoptic_Input.R')
library(dplyr)
library(tidyr)

setwd("../")
setwd("SOS-Synoptic-Status-Algorithm")

# Run SotS Learning Tree 3
source("CART/synopticFunction_Source.R")

#retro.status.df.val <- metrics.reorg.values
#retro.status.df.stat <- metrics.reorg.status
publ.status <- read.csv("DATA/Publ_Status_Reorg_Status.csv")

setwd("../"); setwd("SOS-Data-Processing")

# From GPs 3_Synoptic code
  cu.lookup <- read.csv("DATA_LOOKUP_FILES/MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE)
  # Fix the syntax issue with the CU_IDs in the CU lookup file
  cu.lookup$CU_ID <- gsub("_","-", cu.lookup$CU_ID)


  # read in the merged flat file and lookup file
  metrics.raw <- read.csv("DATA_OUT/METRICS_FILE_BY_CU.csv",stringsAsFactors = FALSE) 
  metrics.raw[metrics.raw$Metric == "LongTrend","Value"] <- metrics.raw[metrics.raw$Metric == "LongTrend","Value"] * 100 # change to the units the the algorithms are using
  
  # BMac added Aug 4 2021 - change BMs as well so downloaded data is consistent
  metrics.raw[metrics.raw$Metric == "LongTrend","LBM"] <- metrics.raw[metrics.raw$Metric == "LongTrend","LBM"] * 100 
  metrics.raw[metrics.raw$Metric == "LongTrend","UBM"] <- metrics.raw[metrics.raw$Metric == "LongTrend","UBM"] * 100 


# Get the ratios
  metrics.tmp1 <- metrics.raw %>% rename(Compare=Value) %>%
                                  #relocate(Data_Type, .after=Label) %>%
                                  left_join(cu.lookup %>% select(CU_ID, DataType=DataQualkIdx, AbdMetric,  ShortTrendMetric, LongTrendMetric, PercentileMetric), by="CU_ID" ) %>%
                                  # Replace Value with NA when expert defined column says this metric is not appropriate for the data
                                  mutate(Compare = replace(Compare, ((Metric == "RelAbd" | Metric == "AbsAbd") & AbdMetric==FALSE) , NA)) %>%
                                  mutate(Compare = replace(Compare, ((Metric == "PercChange" | Metric == "ProbDeclBelowLBM") & ShortTrendMetric==FALSE) , NA)) %>%
                                  mutate(Compare = replace(Compare, ((Metric == "LongTrend") & LongTrendMetric==FALSE) , NA)) %>%
                                  mutate(Compare = replace(Compare, (( Metric == "Percentile" ) & PercentileMetric==FALSE) , NA)) %>%
                                  mutate(Value= case_when( (Metric == "RelAbd" | Metric == "AbsAbd") ~ (Compare/LBM), 
                                                           (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
                                  )
                                  ) %>%
                                  mutate(Metric = recode(Metric, RelAbd = "RelLBM", AbsAbd = "AbsLBM")) 
    
    
  metrics.tmp2 <- metrics.tmp1 %>% rbind(metrics.tmp1 %>% filter(Metric == "RelLBM"| Metric =="AbsLBM") %>% 
                                                          mutate(Value=Compare/UBM) %>%
                                                          mutate(Metric=recode(Metric, RelLBM = "RelUBM", AbsLBM = "AbsUBM"))
                                        ) %>%
                                   mutate(Status = replace(Status, is.na(Value), NA))
  
  std.metrics <- c("AbsLBM","AbsUBM","LongTrend","PercChange","RelLBM","RelUBM")
  
  metrics.synoptic.values <- metrics.tmp2 %>% 
                                          select(-c(Label, Data_Type, Compare, LBM, UBM, Status,  AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) %>%
                                          pivot_wider(names_from = Metric, values_from=Value)    
  
  metrics.synoptic.status <- metrics.tmp2 %>% 
                                          select(-c(Label, Data_Type, Compare, LBM, UBM, Value,  AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) %>%
                                          pivot_wider(names_from = Metric, values_from=Status) %>%
                                          mutate(RelAbd = RelUBM, AbsAbd = AbsLBM)
  
  metrics.synoptic.values[["NumStdMetrics"]] <-  rowSums(!is.na(metrics.synoptic.values[,std.metrics]))
  metrics.synoptic.status[["NumStdMetrics"]] <-  rowSums(!is.na(metrics.synoptic.status[,std.metrics]))
  
  
  # Write files for running the algorithms in retro
  write.csv( metrics.synoptic.values,"DATA_OUT/Retrospective_Metrics_Values.csv", row.names = FALSE)
  write.csv( metrics.synoptic.status,"DATA_OUT/Retrospective_Metrics_Status.csv", row.names = FALSE)                     
            
  # mutate(Value = case_when( ((Metric == "RelAbd" | Metric == "AbsAbd") & AbdMetric==FALSE) ~ NA_real_, 
  #                           (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
  #                           
           
           
                    
# Algorithm LT3 Run
sots3.trial <- synoptic(data.df = metrics.synoptic.values, algorithm = "StateOfTheSalmon3",
                                                 group.var = "Species")

write.csv(sots3.trial$data,"Retro_Results_SotS3_Aug42021.csv",row.names = FALSE)


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


# Now create the dataframes for the Scanner from metrics.tmp2

metrics.scanner <-  metrics.tmp2  %>%   filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
                                        select(-c(DataType, AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) %>%
                                        rbind(
                                            publ.status %>% select(CU_ID,Year,IntStatus) %>%
                                                            pivot_longer(cols=IntStatus, names_to = "Metric", values_to="Status") %>%
                                                            inner_join( metrics.raw %>% select(CU_ID, Species, Stock, Data_Type) %>% unique() , by="CU_ID") %>%
                                                            mutate(Label="SpnForAbd_Wild", Compare=NA, LBM=NA, UBM=NA, Value=NA)
                                        ) %>%
                                        rbind(
                                            sots3.trial$data %>% select(CU_ID, Species, Stock, DataType, Year, SynStatus) %>% 
                                                                 rename(Status=SynStatus, Data_Type=DataType) %>%
                                                                 mutate(Status = na_if(Status, "None")) %>%
                                                                 mutate(Label="SpnForAbd_Wild", Metric="SynStatus", Compare=NA, LBM=NA, UBM=NA, Value=NA)
                                        )%>%
                                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                                        select(-CU_ID, -Data_Type) %>% 
                                        rename(CU_ID=CU_ID_Alt2_CULookup)%>%
                                        relocate(CU_ID, .before=Species)%>%
                                        relocate(Status, .after=last_col())
  



# syn.statuses <-  pivot_longer(select(retro.status.df.stat, -c(ProbDeclBelowLBM, DataType, Percentile, NumStdMetrics)), cols= -c(CU_ID, Species, Stock, Year),
#                           names_to="Metric", values_to="Status")
# 
# long.retro <- retro.status.df.val %>% 
#                                     select(-c(ProbDeclBelowLBM, Percentile, NumStdMetrics)) %>%
#                                     pivot_longer(cols = -c(CU_ID, Species, Stock, DataType, Year),names_to="Metric", values_to="Value") %>%
#                                     left_join(statuses, by = c("CU_ID","Species","Stock","Year","Metric"))
  
# Could pull in the Benchmarks and comparison values from metrics.raw also if wanted
# but will require wrangling because the metrics names are different for the LBM and UBM ones
setwd("../")
metrics.dummy <- read.csv("SOS-Data-Processing/DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")


# Metrics.dummy uses CU_IDs from NuSEDs for the tool NOT the ones used in the data prep code, so must match these
# CU_ID_Alt2_CULookup in the MAIN_CU_LOOKUP_FOR_SOS.csv

# Filter OUT the metrics data we have just calculated so this is just the EMPTY CU datasets 

metrics.out <- metrics.dummy %>% select(-X) %>%
                                 filter(!CU_ID %in% metrics.scanner$CU_ID) %>%
                                 filter(!is.na(CU_ID))%>%
                                 filter(!is.na(Stock))%>%
                                 filter(!Metric == "IntStatus") %>%
                                 rbind(metrics.scanner)

write.csv(metrics.out, "build_PStat_data/data/METRICS_FILE_BY_CU_forPSST.csv")

# ========================= NOW fix the escapement/recruit datasets, since I had to sub values into the WILD columns for (Chum) to get the metrics to run!
# ALSO remove the Trends data for Fraser SK

cu.data <- read.csv("SOS-Data-Processing/DATA_OUT/MERGED_FLAT_FILE_BY_CU.csv")

# Remove the Trends data for all CUs - no longer needed and was only different for FR SK
cu.clean <-  cu.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild)) %>%
                         mutate(SpnForAbd_Wild = case_when(CU_ID %in% c("CM-02", "CM-03", "CM-04", "CM-05", "CM-06", "CM-07", "CM-08", "CM-09") ~ NA_real_, 
                                                           TRUE ~ SpnForAbd_Wild)) %>%
                        rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild )   

write.csv(cu.clean, "build_PStat_data/data/MERGED_FLAT_FILE_BY_CU_CLEAN.csv")
# REmove the "wild" from FR CM


# Same for the POP file
#pop.info <- read.csv("build_PStat_data/data/PopLookup.csv")
pop.data <- read.csv("SOS-Data-Processing/DATA_OUT/MERGED_FLAT_FILE_BY_POP.csv")



pop.clean <- pop.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild)) %>%
                          rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                          # Fix the Coho - some CUs have no hatchery origin spawners so can be put into the wild column - other CUs had hatchery which cannot be 
                          # separated at teh population level (or at least HAS not been but I need to look at this and my Lynda notes to be sure)                 
                          # CUs with NO hatchery = Fraser Canyon (CO-05)
                          mutate(Escapement_Wild = case_when(CU_ID == "CO_02" ~ Escapement_Total, TRUE ~  Escapement_Wild)) # Note this is the CU_ID from the WSP process not NuSEDs





                          
write.csv(pop.clean, "build_PStat_data/data/MERGED_FLAT_FILE_BY_POP_CLEAN.csv")



# 
# pop.info.adj <- pop.info %>% mutate(Comment = case_when(WSP_ts == "yes"~ "treated data used to produce CU level timeseries", 
#                                                         WSP_ts == "no" ~ "raw data - do not use for analysis"))  
# write.csv(pop.info.adj,"build_PStat_data-main/data/PopLookup.csv")  
  

