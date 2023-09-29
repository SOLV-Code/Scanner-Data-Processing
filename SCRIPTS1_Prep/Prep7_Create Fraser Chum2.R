# Make sure FR Chum PopIDs are matching - see what is actually in the data and compare to what is in the Conservation Unit file in NuSEDs
#
# CU-level data: Using Joe's extensive estimate included in the sum for the CU + main streams (except Alouette) all found in the LFA Major
#                Chum Systems Escapement excel file
#
# Population-level data: Using the Fraser systems in the Extensive sheet [Chum Extensive summaries 2001-2017] for the individual populations
#                        data + prepped main streams (except Alouette though maybe can include this here) from the Major systems file.
#                     - will need to infill for 2003 using proportion method to the main streams
#
# Will have to note that the CU-level data will not be equal to the sum of population level data


# *** IMPORTANT NOTES ***

# Infilling of Stave 2003  will occur in the code so can be flexible. I am not infilling Weaver becasue the estimate is not our of range of others, AND
# infilling on 4-year cycle gives almost the same number
# The infilled Stave estimate is in the Populations themselves AND IT IS INCLUDED IN THE CU TOTAL.

# I altered the infilling method used for Chilliwack 2002 - 2015. Previously was using proportions of Chilliwack to Stave and Harrison from the previous year,
# expanded by the current year abundances. Since years infilled are consecutive this means the proportions shift over time. So I took the overall proportions for
# Years where there is data for both and used these proportions to infill, expanded by current Stave and Harrison abundances. I included the additional estimates
# (squakum, weaver rack returns etc in the Harrison estimates since they were likely included in the early data)


library(dplyr)
library(tidyr)
library(stringr)

FRchum_main <- read.csv("DATA_IN/SOURCES/Fraser Chum/FR Chum Major Systems.csv")
FRchum_extensive <- read.csv("DATA_IN/SOURCES/Fraser Chum/Chum extensive pivot.csv")
FRchum_Harrison <- read.csv("DATA_IN/SOURCES/Fraser Chum/Harrison Chum Escapement Summary.csv")
pop_ids <- read.csv("DATA_IN/SOURCES/Fraser Chum/FR Chum POPID Crosswalk.csv")

#mapdata <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv",stringsAsFactors=FALSE)





# ==================== CU level data ====================== #
# Changed May 7 2021 to reflect work with Matt Townsend and Joe Tadey
# Current version uses the sum of the Stave and the total Harrison Mark-recapture estiamte as the CU estimate
# Harrison estimate includes Chehalis in-river & swim-ins, Weaver in-river & swim-ins spawners, and Squawkum.
# This should be reflected in the sites that highlight for the CU totals on the map but no data can be shown since it cannot be
# pulled apart in every years (except Squawkum) Squawkum estimates are available starting in 2002 hence the timeseries has been truncated.

FRchum_selected <- FRchum_main %>%
                           # mutate(Stave.River = case_when(Year == 2003 ~ mean(c(FRchum_main$Stave.River[FRchum_main$Year==1999],FRchum_main$Stave.River[FRchum_main$Year==2007])),
                           #                                TRUE ~ as.numeric(Stave.River))) %>%
                            select(Year,Stave.River) %>%
                            right_join(select(FRchum_Harrison, Year, Harrison.River.total.estimate))


FRchum_CU <- FRchum_selected %>%
                            filter(Year >= 2002) %>%
                            mutate(SpnForAbd_Total=rowSums( select(.,-Year), na.rm=T)) %>%
                            select(Year, SpnForAbd_Total) %>%
                            mutate(Species="Chum", CU_ID="CM-02", CU_Name = "Lower Fraser Chum", DU_ID=NA, SpnForAbd_Wild=SpnForAbd_Total, SpnForTrend_Wild=SpnForAbd_Total,
                                   SpnForTrend_Total=SpnForAbd_Total,Recruits_Wild=NA, Recruits_Total=NA,)  %>%
                            relocate( SpnForAbd_Total, .after=SpnForTrend_Wild)



write.csv(FRchum_CU, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserChum.csv")




# ==================== POP Level data ===================== #

# Combine the two major systems used for the CU total with the Extensive information
# must subtract Squawkum from Harrison totoal in order to have this as a separate estimate in the map
FRchum_all <- FRchum_selected %>%
                                    right_join(FRchum_extensive) %>%
                                    mutate(Squawkum.Creek = replace_na(Squawkum.Creek, 0)) %>%
                                   #mutate(Squawkum.Creek = case_when(Year == 2003 ~
                                    #       mean(c(FRchum_extensive$Squawkum.Creek[FRchum_extensive$Year==2002],FRchum_extensive$Squawkum.Creek[FRchum_extensive$Year==2004])),
                                     #      TRUE ~ as.numeric(Squawkum.Creek))) %>%
                                    mutate(Harrison.River = Harrison.River.total.estimate - Squawkum.Creek) %>%
                                    select(-Harrison.River.total.estimate) %>%
                                    mutate(Squawkum.Creek = na_if(Squawkum.Creek,0)) %>%
                                    filter(Year >= 2002)



# ---------- NON-INFILLED or FILTERED POP DATA -----------------------#
  # Add in popids and other info and transform into longform
  Just_CM <-  FRchum_all  %>%
                            pivot_longer(-Year,names_to="Pop_Name", values_to="SpnForAbd_Total") %>%
                            mutate(Species="Chum", CU_ID="CM-02", CU_Name = "Lower Fraser Chum") %>%
                            mutate(Pop_Name = gsub("\\.", " ", Pop_Name)) %>%
                            mutate(Pop_Name = recode(Pop_Name, "Chilqua Slough   Creek" = "Chilqua Slough & Creek")) %>%
                            left_join(pop_ids, by="Pop_Name") %>%
                            mutate(SpnForAbd_Wild=NA, SpnForTrend_Wild=NA, SpnForTrend_Total=SpnForAbd_Total, Pop_ID = POPID,
                                   DU_ID=NA, Recruits_Total=NA, Recruits_Wild=NA) %>%
                            relocate( SpnForAbd_Total, .after=SpnForTrend_Wild)

   write.csv(Just_CM,"DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserChum_all.csv")



# # ---------------------- INFILLED POP LEVEL DATA -------------------- #
#
#   # Count years in the dataset
#   minyears <- ceiling(length(FRchum_all$Year)/2)+1
#
#   # evaluate if # is >50%
#   Over.min <- function(x) sum(!is.na(x))>=minyears
#   keep <- apply(X=FRchum_all, MARGIN = 2, FUN = Over.min)
#
#   # filter out extensive with fewer than 50% of years of data and join this to main streams
#   FRchum_filtered <- FRchum_all[keep]
#
#   # Find average for all systems across years when we have data for all
#   n.pops <- ncol(FRchum_filtered)
#   row.full<- function(x) sum(!is.na(x))
#   years.ave <- FRchum_filtered$Year[apply(X=FRchum_filtered, MARGIN=1, FUN=row.full) == n.pops]
#
#   averages <- FRchum_filtered %>% filter(Year %in% years.ave) %>%
#                                   colMeans()
#   proportions <- averages/sum(averages[-1])
#
#   # Infill populations
#   # Create expansion vector
#   calc.expansion <- function(x) 1/(1- sum(proportions[is.na(x)]) )
#   Expand <- apply(X=FRchum_filtered, MARGIN=1, FUN=calc.expansion)
#   ExpandedCM <- FRchum_filtered %>% mutate(Sum = rowSums( select(.,-Year), na.rm=T)) %>%
#                                     mutate(ExpandedSum = Sum*Expand)
#   # Calc individual expanded populations
#   for(i in names(ExpandedCM)){
#     ExpandedCM[ is.na(select(ExpandedCM, i)),i ] <- proportions[names(proportions)==i] *  select(ExpandedCM,ExpandedSum)[ is.na(select(ExpandedCM, i)) ]
#   }
#
#   # Add in popids and other info and transform into longform
#   Infilled_CM <- ExpandedCM %>%
#                           select(-Sum, -ExpandedSum) %>%
#                           pivot_longer(-Year,names_to="Pop_Name", values_to="SpnforAbd") %>%
#                           mutate(CU_ID="CM-02", CU_Name = "Lower Fraser Chum") %>%
#                           mutate(Pop_Name = recode(Pop_Name, "Chilliwack_OG"="Chilliwack River")) %>%
#                           mutate(Pop_Name = gsub("\\.", " ", Pop_Name)) %>%
#                           mutate(Pop_Name = recode(Pop_Name, "Chilqua Slough   Creek" = "Chilqua Slough & Creek")) %>%
#                           left_join(pop_ids, by="Pop_Name")
#
# # Write to Pop Level data file
# write.csv(Infilled_CM,"DATA_OUT/Cleaned_FlatFile_ByPop_FraserChum_Infilled.csv")
#






