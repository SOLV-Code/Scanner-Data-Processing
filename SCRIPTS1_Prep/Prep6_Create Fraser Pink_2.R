# Make sure FR Pink PopIDs are matching - see what is acually in the data and compare to what is in the CosevationUnit file in NuSEDs
# Can only use the summing of individual sites until 1991 then pull in the Fraser-wide estimates (after removal of upstreamt catch!)
# Fraser-wide sites can be linked spatially to the Fraser River site in the Pop_Lookup file

# TRUNCATE
library(dplyr)
library(tidyr)


#pink_exceptions <- read.csv("DATA_IN/SOURCES/Fraser Pink/Fraser Pink Data Caveats.csv",stringsAsFactors=FALSE)

#mapdata <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv",stringsAsFactors=FALSE)



# ====== CU level data ============= #

rec_esc <- read.csv("DATA_IN/SOURCES/Fraser Pink/Fraser Pink esc_rec CU.csv")


# --------------- If using NuSeds ----------------------------------- #

nuseds.pink <- read.csv("DATA_IN/SOURCES/Fraser Pink/FR_Pink_Conservation_Unit_Data_NUSEDs_March 2021.csv",stringsAsFactors=FALSE)

#pk.mapdata <- mapdata  %>% filter(MapData_CU_ID =="PKO-001")
#unique(nuseds.pink$POP_ID[nuseds.pink$POP_ID %in% pk.mapdata$Pop_ID])


# Sites listed in the CU data but not in NuSEDs = 920 (this is all species) - 0 when only this CU used

#pk.mapdata$Pop_ID[!(pk.mapdata$Pop_ID %in% nuseds.pink$POP_ID)]

# GEOGRAPHICAL_EXTNT_OF_ESTIMATE is what matches the TimeSeriesData_Pop_Name in the Pop_Lookup file


# This takes the official data from the Grant et al report - Fraser and Tribs Net Escapement
# The next few steps takes the Nuseds data and filters only Years < 1993 then sums across sites
# These sums are then inserted into the Pink_Official dataframe for 1957-1991

Pink_CU <- rec_esc %>% 
                      mutate(CU_ID="PKO-01", CU_Name = "Fraser Pink", DU_ID=NA, Recruits_Wild=NA) %>%
                      mutate(Net.Escapement=replace(Net.Escapement, Brood.Year<1993, NA)) %>%
                      rename(Year=Brood.Year, SpnForAbd_Total=Net.Escapement, Recruits_Total=Recruits) %>%
                      mutate(SpnForTrend_Total=SpnForAbd_Total, SpnForAbd_Wild=SpnForAbd_Total, SpnForTrend_Wild=SpnForAbd_Total, Species="Pink")



pink.streams <- nuseds.pink %>% 
                      select(GEOGRAPHICAL_EXTNT_OF_ESTIMATE, ANALYSIS_YR, MAX_ESTIMATE, POP_ID) %>%
                      arrange(GEOGRAPHICAL_EXTNT_OF_ESTIMATE) %>%
                      filter(ANALYSIS_YR<1993 & ANALYSIS_YR != 1953) 

pink.streams.sum <- pink.streams %>%
                      select(-POP_ID)%>%
                      pivot_wider(names_from=GEOGRAPHICAL_EXTNT_OF_ESTIMATE, values_from=MAX_ESTIMATE) %>%
                      arrange(ANALYSIS_YR) %>%
                      mutate(SpnForAbd_Total = rowSums(select(., -ANALYSIS_YR), na.rm=T)) %>%
                      select(SpnForAbd_Total) 
                      #plyr::rbind.fill(Pink_official) %>%
                      #arrange(Year)


# Only if we are using the sum of streams for pre-1993 which I don't think we are!  ************************* DECISION: NO *******************
#Pink_CU$SpnForAbd_Total[Pink_CU$Year < 1993] <- pink.streams.sum$SpnForAbd_Total


# Replicate the net escapement (Spn_for_Abd) as the Spn for trends and the wild etc so the metrics can be calculated
#Pink_CU <- Pink_CU %>% mutate(SpnForTrend_Total=SpnForAbd_Total, Species="Pink")

#write.csv(pink.data,"Pink_data_trial.csv")


write.csv(Pink_CU, "DATA_OUT/Cleaned_FlatFile_ByCU_FraserPink.csv")
# ------------------------------------------------------------------- #



# =====  For POP Level data ================= #

# -------- NuSEDs for 1957-1991; official net escapements to Fraser and Tribs for 1993 onwards ------------------#

# Filter in only the >1993 data (which is the net escapement from the Grant et al report - remember the pre-1993 data was overwritten with the sum of Nuseds site data) ** NO **
# Filter in the entire thing and write it under FRASER MAIN AND TRIBS Site - this is the ONE site that will be included as the WSP treated site. I have changed the Pink_CU dataframe to be just the Net Escapememt
# from Sues file. It is no longer being overwritten in the early years because we want to use the official data. 

Fraser_main <- Pink_CU %>% #filter(Year >= 1993) %>%
                                 mutate(Pop_Name = "FRASER RIVER AND TRIBUTARIES", POP_ID="44551") %>%
                                 select(-Recruits_Total, -Recruits_Wild) %>%
                                 rename(Pop_ID = POP_ID) %>%
                                 mutate(SpnForTrend_Total = NA)

pink_pop_data <- pink.streams %>% 
                          rename(Pop_Name=GEOGRAPHICAL_EXTNT_OF_ESTIMATE, SpnForTrend_Total = MAX_ESTIMATE, Pop_ID=POP_ID, Year=ANALYSIS_YR) %>%
                          mutate(CU_ID = "PKO-01", CU_Name="Fraser Pink", DU_ID=NA, SpnForAbd_Total=SpnForTrend_Total, 
                          SpnForAbd_Wild=SpnForTrend_Total, SpnForTrend_Wild=SpnForTrend_Total, Species="Pink") %>%
                          rbind(Fraser_main) %>%
                          mutate(Recruits_Total=NA, Recruits_Wild=NA)
  

# Write to Pop Level data file
write.csv(pink_pop_data,"DATA_OUT/Cleaned_FlatFile_ByPop_FraserPink_All_Nuseds.csv")

# # Add populations = "yes" to the Pop_Lookup file for those used
# Pop_Lookup <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv")
#  
#     #Pop_Lookup.pk <- Pop_Lookup %>% filter(TimeSeriesData_Species=="Pk_Fraser") %>%
#     #                                mutate(WSP_ts = case_when(Pop_ID %in% pink_pop_data$Pop_ID == TRUE ~ "yes", TRUE ~ WSP_ts))
#   Pop_Lookup.pk <- Pop_Lookup %>% filter(TimeSeriesData_Species=="Pk_Fraser") %>%
#                                   mutate(WSP_ts = case_when(MapData_Pop_Name == "FRASER RIVER AND TRIBUTARIES" ~ "yes", TRUE ~ "no"))
#     
#   Pop_Lookup <- Pop_Lookup %>% filter(!TimeSeriesData_Species=="Pk_Fraser") %>%
#                                  rbind(Pop_Lookup.pk)
# 
#   write.csv(Pop_Lookup, "DATA_LOOKUP_FILES/PopLookup.csv")
# 




# ------------ If using tab attached to the main coloured tab ---------------------------- #
# 
# pink_byarea<- read.csv("DATA_IN/SOURCES/Fraser Pink/Pink by area.csv")
# 
# 
# 
# pk.mapdata <- mapdata  %>% filter(MapData_Species=="PK")
# 
# 
# # Feb 10 2021 - working with the data used by Twonsend et al by area now 
# pink_byarea$Stream <- toupper(pink_byarea$Stream)
# length(unique(pink_byarea$Stream))  # 105 streams we need POP_IDs for
# length(unique(pk.mapdata$TimeSeriesData_Pop_Name[pk.mapdata$TimeSeriesData_Pop_Name %in% pink_byarea$Stream])) # 74 matches
# 
# # Fix the names that are not matching
# pink_byarea$Stream[pink_byarea$Stream=="ANDERSON CREEK"] <- "ANDERSON RIVER"
# pink_byarea$Stream[pink_byarea$Stream=="KAWKAWA CREEK"] <- "SUCKER CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="WAHLEACH CHANNEL (JONES CHANNEL)"] <- "JONES CREEK CHANNEL"
# pink_byarea$Stream[pink_byarea$Stream=="WAHLEACH CREEK (JONES CREEK)"] <- "WAHLEACH CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="POPKUM CREEK"] <- "POPCUM CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="SILVERDALE CREEK "] <- "SILVERDALE CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="DEADMAN CREEK"] <- "DEADMAN RIVER"
# pink_byarea$Stream[pink_byarea$Stream=="SWELTZER CREEK"] <- "SWELTZER RIVER"
# pink_byarea$Stream[pink_byarea$Stream=="LORENZETTI CREEK"] <- "LORENZETTA CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="SETON CHANNEL UPPER AND LOWER"] <- "UPPER & LOWER SETON CHANNELS"
# pink_byarea$Stream[pink_byarea$Stream=="SETON CREEK"] <- "SETON RIVER"
# pink_byarea$Stream[pink_byarea$Stream=="VEDDER-CHILLIWACK RIVER"] <- "CHILLIWACK RIVER"
# pink_byarea$Stream[pink_byarea$Stream=="CHILCOTIN RIVER"] <- "CHILCOTIN RIVER (LOWER)"
# pink_byarea$Stream[pink_byarea$Stream=="TAMAHI CREEK"] <- "TAMIHI CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="MAHOOD CREEK (MISSION)"] <- "MAHOOD CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="TROUT LAKE CREEK (MISSION)"] <- "TROUT LAKE CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="MIDDLE CREEK"] <- "NESAKWATCH CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="BEAR CREEK"] <- "HIUIHILL CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="GOLD CREEK"] <- "NIKWIKWAIA CREEK"
# pink_byarea$Stream[pink_byarea$Stream=="GIESBRECHT CHANNEL"] <- "GIESBRECHT SPAWNING CHANNEL"
# pink_byarea$Stream[pink_byarea$Stream=="HOPEDALE CREEK"] <- "HOPEDALE SLOUGH"
# 
# 
# pop_id_lookup <- pk.mapdata %>% filter(TimeSeriesData_Pop_Name %in% pink_byarea$Stream) %>%
#                                 select(Pop_ID, TimeSeriesData_Pop_Name)
# 
# 
# # Add in the POP_IDs
# Pink_use <- pink_byarea %>% select(-System, -Above.Below.Qualark, -Method, -Source,-X95.High, -X95.Low, -Timing,-Flag, -Comments, -Precision) %>%
#                             mutate(POP_ID = pop_id_lookup$Pop_ID[match( pink_byarea$Stream,pop_id_lookup$TimeSeriesData_Pop_Name)]) %>%
#                             rename(Pop_Name=Stream, SpnForTrend_Total=Estimate, Pop_ID=POP_ID) %>%
#                             mutate(CU_ID = "PKO-01", CU_Name="Fraser Pink", DU_ID=NA, Recruits_Total=NA, Recruits_Wild=NA, SpnForAbd_Total=NA, 
#                                    SpnForAbd_Wild=NA, SpnForTrend_Wild=NA)
# 
# 
# 
# 
# # Streams with nO POPID for records
# unique(Pink_use %>% filter(is.na(Pop_ID)) %>% select(Pop_Name))
#   
# 
#                         
# 
# 
# # --------------------- Filtered and Infilled  --------------------  # 
# 
#    # Filter High Precision Estimates Only  
#     pink_hp_only <- pink_byarea %>% filter(Precision =="High Precision") 
#     pop_id_HP_lookup <- pk.mapdata %>% filter(TimeSeriesData_Pop_Name %in% pink_hp_only$Stream) %>%
#                                        select(Pop_ID, TimeSeriesData_Pop_Name)
#   
#     # Pink_HighPrec <- pink_hp_only %>% select(-System, -Above.Below.Qualark, -Method, -Source,-X95.High, -X95.Low, -Timing,-Flag, -Comments, -Precision) %>%
#     #                                   mutate(POP_ID = pop_id_HP_lookup$Pop_ID[match( pink_hp_only$Stream, pop_id_HP_lookup$TimeSeriesData_Pop_Name)]) %>%
#     #                                   rename(Pop_Name=Stream, SpnForTrend_Total=Estimate, Pop_ID=POP_ID) %>%
#     #                                   mutate(CU_ID = "PKO-01", CU_Name="Fraser Pink", DU_ID=NA, Recruits_Total=NA, Recruits_Wild=NA, SpnForAbd_Total=NA, 
#     #                                          SpnForAbd_Wild=NA, SpnForTrend_Wild=NA)
# 
#    # evaluate if # is >50%
#      tmp <- pink_hp_only %>% select(-System, -Above.Below.Qualark, -Method, -Source,-X95.High, -X95.Low, -Timing,-Flag, -Comments, -Precision) %>%
#                               pivot_wider(names_from = Stream, values_from="Estimate") %>%
#                               filter(Year <=1991)  
#      
#    # Count years in the dataset
#      minyears <- ceiling(length(unique(tmp$Year))/2)
#      Over.min <- function(x) sum(!is.na(x))>=minyears
#      keep <- tmp[apply(tmp,MARGIN = 2, FUN = Over.min)]
#      
#      # Now infill!
#      # Find average for all systems across years when we have data for all
#      n.pops <- ncol(keep)
#      row.full<- function(x) sum(!is.na(x))
#      years.ave <- keep$Year[apply(X=keep, MARGIN=1, FUN=row.full) == n.pops]
#      averages <- keep %>% filter(Year %in% years.ave) %>%
#                           colMeans()
#      proportions <- averages/sum(averages[-1])
#      
#      # Infill populations
#      # Create expansion vector
#      calc.expansion <- function(x) 1/(1- sum(proportions[is.na(x)]) )
#      Expand <- apply(X=keep, MARGIN=1, FUN=calc.expansion)
#      ExpandedPK <- keep %>% mutate(Sum = rowSums( select(.,-Year), na.rm=T)) %>%
#                             mutate(ExpandedSum = Sum*Expand)
#      
#      # Calc individual expanded populations
#      for(i in names(ExpandedPK)){
#        ExpandedPK[ is.na(select(ExpandedPK, i)),i ] <- proportions[names(proportions)==i] *  select(ExpandedPK,ExpandedSum)[ is.na(select(ExpandedPK, i)) ]  
#      }
#      
#      # change column name in here so I can left_join
#      pop_id_HP_lookup_rename <- pop_id_HP_lookup %>% rename(Pop_Name = TimeSeriesData_Pop_Name, POP_ID=Pop_ID)
#      
#      Fraser.mainstem <- pink_byarea %>% filter(Stream == 'FRASER RIVER', Year >1991)%>%  # **************** we already decided not to infill the test fishery years but can change this here by using the HP dataframe and infilling using averages
#                                         select(Year, Stream, Estimate) %>%
#                                         rename(Pop_Name=Stream, SpnForTrend_Total=Estimate)%>%
#                                         tibble()
#      
#      
#      
#      Infilled_PK <- ExpandedPK %>%
#                                select(-Sum, -ExpandedSum) %>%
#                                pivot_longer(-Year,names_to="Pop_Name", values_to="SpnForTrend_Total") %>%
#                                rbind(Fraser.mainstem) %>%
#                                left_join(pop_id_HP_lookup_rename, by="Pop_Name") %>%
#                                mutate(CU_ID = "PKO-01", CU_Name="Fraser Pink", DU_ID=NA, Recruits_Total=NA, Recruits_Wild=NA, SpnForAbd_Total=NA, 
#                                SpnForAbd_Wild=NA, SpnForTrend_Wild=NA)
# 
#      write.csv(Infilled_PK,"DATA_OUT/Cleaned_FlatFile_ByPop_FraserPink_HighPrecisionInfilled.csv") 
#      # Add back in the Fraser system post-1991 when only this system was surveyed
#      
# # ====== finish infilling =============== #
# 
# 
#      
# # Summing the Populations for testing
# Pop_sum<- Pink_use %>% select(-CU_Name, -CU_ID, -Pop_ID) %>%
#                        pivot_wider(names_from = Pop_Name, values_from = SpnForTrend_Total) %>%
#                        mutate(Sum = rowSums( select(.,-Year), na.rm=T)) %>%
#                        select(Year,Sum)
# Pop_HP_sum<- Pink_HighPrec %>% select(-CU_Name, -CU_ID, -Pop_ID) %>%
#                                pivot_wider(names_from = Pop_Name, values_from = SpnForTrend_Total) %>%
#                                mutate(Sum = rowSums( select(.,-Year), na.rm=T)) %>%
#                                select(Year,Sum)
# 
# 
# 
# 
# 
# 
# 
# ## ======= NOT USING NUSEDS ======THIS IS THE OLD STUFF======= #
# 
# 
# 
# # Remove sites in years where they were not part of the total for some reason
# for(i in nrows(pink_exceptions)){
#   
# }
# 
# 
# pink_exceptions %>% filter(is.na(Addition))
# 
# 
# # Can only use the summing of individual sites until 1991 then pull in the Fraser-wide estimates (after removal of upstreamt catch!)
# # Fraser-wide sites can be linked spatially to the Fraser River site in the Pop_Lookup file


