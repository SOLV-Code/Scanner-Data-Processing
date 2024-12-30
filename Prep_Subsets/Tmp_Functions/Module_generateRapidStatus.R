
#' generateRapidStatus
#'
#' this function reorganizes the outputs from prepDataForRapidStatus(), applies the function applyRapidStatusTree(), and then does some post-processing. Note: "proxy" statuses for some CUs are being explored, but are not yet included in this function!
#' @param cu.info a data frame with specifications for each CU. For details, see help file for calculateMetricsByCU().
#' @param cu.data a data frame with CU time series. This is the same data frame used for the cu.file argument in the call to calculateMetricsbyCU(). For details, see that help file.
#' @keywords trend
#' @export



generateRapidStatus <- function(cu.info,cu.data){

library(tidyverse)


# fix the CU_ID ("_" vs. "-") (just in case, this is a recurring issue)
cu.info$CU_ID <- gsub("_","-",cu.info$CU_ID)

qual.score.tab <- data.frame(
	BinLabel = c("Node3","Node17","Node19","Node20","Node21","Node22","Node23","Node33","Node36","Node37","Node64","Node65"),
	ConfidenceRating5 = c("High","Moderate","High","Moderate","Moderate","High","High","Moderate","High","High","Low","Low"),
	ConfidenceRating3 = c("High","Moderate","High","Moderate","Moderate","High","High","Moderate","High","High","Low","Low"),
	ConfidenceRating2 = c("High","High","High","Moderate","High","High","High","High","High","Moderate","High","Moderate")
	)



# add grouping variable to spn data file
cu.data.group <- cu.data %>%
                    mutate(CU_ID = gsub("_","-",CU_ID)) %>% 
                    dplyr::filter(!is.na(CU_Name)) %>%
                    left_join(cu.info %>% select(CU_Name,Group), by="CU_Name" )

# Retrospective metrics
retro.values <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Retrospective_Metrics_Values_",paste(datastage, collapse=""),".csv"),stringsAsFactors = FALSE) %>%
                left_join(cu.info %>% select("CU_ID",Group), by="CU_ID" )
retro.status <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Retrospective_Metrics_Status_",paste(datastage, collapse=""),".csv"),stringsAsFactors = FALSE)  %>%
                 left_join(cu.info %>% select("CU_ID",Group), by="CU_ID" )
metrics.long <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Metrics_Longform_SUB_",paste(datastage, collapse=""),".csv"))

# WSP integrated statuses
publ.status.raw <- read.csv("DATA_LOOKUP_FILES/Published_Integrated_Status_Summary.csv",stringsAsFactors = FALSE)
publ.int.status <- publ.status.raw %>%  dplyr::filter(Metric == "IntStatus") %>%
                                        select(CU_ID,Year, Status) %>% 
                                        dplyr::rename(IntStatusRaw = Status) %>%
                                        mutate(IntStatus = recode(IntStatusRaw, RedAmber = "Red", AmberGreen = "Amber")) %>%
                                        mutate(IntStatusRaw_Short = recode(IntStatusRaw, Red = "R",RedAmber = "RA", Amber = "A",AmberGreen = "AG",Green = "G"),
                                               IntStatus_Short = recode(IntStatus, Red = "R", Amber = "A",Green = "G"))

# Algorithm outputs IF reading these in
#algo.results <- read.csv("DATA_PROCESSING/Retro_Synoptic_Details.csv")



# ========================= RUN THE RETROSPECTIVE WITH THE RAPID STATUS FUNCTION ========================================= #

retro.lt3 <- rapid_status(data.df = retro.values, algorithm = "StateOfTheSalmon3" ,group.var = "Species") 
write.csv(retro.lt3$data, paste0("DATA_PROCESSING/FILTERED_DATA/Retro_Results_LT3_",paste(datastage, collapse=""),".csv"),row.names = FALSE)


retro.summary.tbl <- retro.lt3$data %>%
                              dplyr::rename(RapidStatus = SynStatus,RapidScore = SynScore) %>%
                              select(-IntStatus,-IntScore,-ErrorScore,-ErrorType) %>%
                              left_join(retro.status %>% select(-NumStdMetrics,-Group) %>% 
                                                         rename(RelAbdCat = RelAbd, AbsAbdCat = AbsAbd, LongTrendCat = LongTrend,              
                                                                 PercChangeCat = PercChange, ProbDeclBelowLBMCat = ProbDeclBelowLBM,
                                                                 PercentileCat = Percentile, RelLBMCat = RelLBM, RelUBMCat = RelUBM,
                                                                 AbsLBMCat = AbsLBM, AbsUBMCat = AbsUBM   )      , 
                                                          by = c("CU_ID","Species","Stock","DataType","Year")) %>%
                              left_join(metrics.long %>% filter(Metric == "RelLBM") %>% 
                                                         select(CU_ID, Stock, Year, RelAbd_LBM=LBM, RelAbd_UBM=UBM),
                                                         by = c("CU_ID","Stock","Year")) %>%
                              left_join( as.data.frame(publ.int.status),by=c("CU_ID","Year")) %>%
                              arrange(Species,Stock,Year) %>%
                              left_join(cu.data.group  %>%   
                                                 select(CU_ID,Year,SpnForAbd_Wild, SpnForTrend_Wild, SpnForAbd_Total, SpnForTrend_Total),
                                                 by= c("CU_ID","Year")   ) %>%
                              select(CU_ID,Species,Stock,	DataType,Year,SpnForAbd_Wild, SpnForTrend_Wild, everything()) %>%
                              left_join(qual.score.tab %>% 
                                                      dplyr::filter(Algorithm == "SotS3") %>% 
                                                      select(BinLabel, ConfidenceRating5, ConfidenceRating3, ConfidenceRating2),
                                                      by = "BinLabel")
                                



retro.summary.tbl$IntStatus5 <- retro.summary.tbl$IntStatusRaw
retro.summary.tbl$IntStatus3 <- dplyr::recode(retro.summary.tbl$IntStatusRaw,"RedAmber" = "Red","AmberGreen" = "Amber")
retro.summary.tbl$IntStatus2 <- dplyr::recode(retro.summary.tbl$IntStatus3, "Amber" = "NotRed","Green" = "NotRed")

if(!dir.exists("OUTPUT/DASHBOARDS")){dir.create("OUTPUT/DASHBOARDS")}
write.csv(retro.summary.tbl, paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_",paste(datastage, collapse=""),".csv"), row.names = FALSE)   


}



























