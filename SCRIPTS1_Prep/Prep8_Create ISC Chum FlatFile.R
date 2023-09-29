# Sept 2021

# Inside South Coast Chum Prep Code
# Infilling etc already performed on the input files by Luke Warkentin
# May need to pull additional sites from NuSEDs

# NOTE: set the wild data to the total for no - this gets reset after metrics are calcuated but need wild to calc metrics


library(dplyr)

homedir <- getwd()
ISCchum.sites.raw <- read.csv("DATA_IN/SOURCES/Inside SC Chum/wild_spawners_stream_infilled_by_site_CU.csv")
ISCchum.CU.raw <- read.csv("DATA_IN/SOURCES/Inside SC Chum/wild_spawners_CU_infilled_by_site_CU.csv")

setwd("../")
Pop_lookup <- read.csv("build_PStat_data/data/PopDecoder.csv")
CU_lookup.CM <- read.csv("build_PStat_data/data/CUAttributes.csv") %>% filter(Species =="Chum")

setwd(homedir)

# ========================== BY CU =================================== #


ISCchum.CU.out <- ISCchum.CU.raw %>%
                                mutate(CU_Name = gsub("^.{0,4}", "", CU_Name)) %>%
                                mutate(CU_Name = recode(CU_Name, "North East Vancouver Island"= "Northeast Vancouver Island"))%>%
                                left_join(select(CU_lookup.CM, CU_Name, CU_ID), by="CU_Name") %>%
                                select(-c(X, EscAvg, GroupSum, Props, Present, PresentProps, inverse_prop, GroupCount, GroupEsc, ContrEsc, SumRawEsc,  SiteEsc)) %>%
                                rename(SpnForTrend_Total = Escape) %>%
                                mutate(Dataset = "Cm_ISC", Species = "Chum", SpnForAbd_Total=SpnForTrend_Total, SpnForAbd_Wild=SpnForTrend_Total,
                                       SpnForTrend_Wild=SpnForTrend_Total,
                                Recruits_Total = "NA", Recruits_Wild="NA", DU_ID="NA")


write.csv(ISCchum.CU.out,"DATA_PROCESSING/Cleaned_FlatFile_byCU_ISCChum.csv", row.names=FALSE)



# ======================== BY POP ================================= #

unique(ISCchum.sites.raw$NME)[!unique(ISCchum.sites.raw$NME) %in% Pop_lookup$Pop_Name]

ISCchum.sites.sub <- ISCchum.sites.raw %>%
                                    mutate(CU_Name = gsub("^.{0,4}", "", CU_Name)) %>%
                                    mutate(CU_Name = recode(CU_Name, "North East Vancouver Island"= "Northeast Vancouver Island"))%>%
                                    left_join(select(CU_lookup.CM, CU_Name, CU_ID), by="CU_Name") %>%
                                    rename(Pop_Name = NME) %>%
                                    left_join(select(Pop_lookup, Pop_Name, Pop_ID, CU_ID), by=c("Pop_Name", "CU_ID")) %>%
                                    rename(SpnForTrend_Total = SiteEsc) %>%
                                    mutate(Dataset = "Cm_ISC", SpnForAbd_Total=SpnForTrend_Total, SpnForAbd_Wild="NA", SpnForTrend_Wild="NA",
                                           Recruits_Total = "NA", Recruits_Wild="NA", DU_ID="NA") %>%
                                    select(-c(Props, Rabcode, CUEsc, Escape, CU,X,Area))



# -- Now indicate in the Poplookup file that these are the pops used for the CU data -- #


setwd("../")
Pop_attributes <- read.csv("build_PStat_data/data/PopAttributes.csv")
setwd(homedir)

ISCchum_WSP_pops <- Pop_attributes %>%
                            filter(CU_ID %in% ISCchum.sites.sub$CU_ID) %>%
                            select(-WSP_ts) %>%
                            mutate(WSP_ts = case_when(Pop_Name %in% ISCchum.sites.sub$Pop_Name ~ "yes",
                                                      TRUE ~ "no"))
new_pop_attributes <- Pop_attributes %>%
                                filter(!(CU_ID %in% ISCchum.sites.sub$CU_ID))  %>%
                                rbind(ISCchum_WSP_pops)

# write.csv(new_pop_attributes, "build_PStat_data/data/PopAttributes_update.csv", row.names=FALSE) # COMMENT THIS IN IF THESE CHANGE but otherwise no need!

# Pull in the sites that are not in the CU roll-ups

nuseds.data <- read.csv("DATA_IN/SOURCES/NUSEDs/Conservation_Unit_Data.csv")

no.data <- ISCchum_WSP_pops %>% filter(WSP_ts=="no")

ISCchum.sites.out<- nuseds.data %>% filter(SPECIES_QUALIFIED=="CM") %>%
                                         filter(CU_INDEX %in% (gsub("^.{0,4}", "", ISCchum.sites.sub$CU_ID)))  %>%
                                         filter(POP_ID %in% no.data$Pop_ID) %>%
                                         select(Year=ANALYSIS_YR, SpnForTrend_Total=MAX_ESTIMATE, GEOGRAPHICAL_EXTNT_OF_ESTIMATE, Pop_ID=POP_ID) %>%
                                         mutate(Pop_ID=as.character(Pop_ID)) %>%
                                         left_join(select(new_pop_attributes, Pop_ID, Pop_Name, CU_ID)) %>%
                                         left_join(select(CU_lookup.CM, CU_ID, CU_Name)) %>%
                                         mutate(Dataset="Cm_ISC", SpnForAbd_Total=SpnForTrend_Total, SpnForAbd_Wild=NA, SpnForTrend_Wild=NA, Recruits_Total=NA, Recruits_Wild=NA, DU_ID=NA) %>%
                                         select(-GEOGRAPHICAL_EXTNT_OF_ESTIMATE) %>%
                                         rbind(ISCchum.sites.sub)

write.csv(ISCchum.sites.out,"DATA_PROCESSING/Cleaned_FlatFile_byPop_ISCChum.csv", row.names=FALSE)

# check
#not.incl <- nuseds.ISC.cm.notincl %>% select(GEOGRAPHICAL_EXTNT_OF_ESTIMATE) %>% unique()
#not.incl$GEOGRAPHICAL_EXTNT_OF_ESTIMATE %in% (ISCchum_WSP_pops %>% filter(WSP_ts=="no") %>% select(Pop_Name) %>% unique())
#True cases are the sites in the Pop_Attributes.csv that have no dat ain the CU rollups (WSP_ts=no)
