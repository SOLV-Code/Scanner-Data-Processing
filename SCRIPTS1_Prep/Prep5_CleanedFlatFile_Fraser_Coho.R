#############################################
# CREATE FLAT FILE VERSIONS FOR SoS - SBC CK
#############################################

library(tidyverse)

# *****************************************************************************BMAC ADDED
source("SCRIPTS1_Prep/SUB_Data extraction and clean up (Coho for now)_2023.R")
pop.info <- read.csv("DATA_LOOKUP_FILES/Fraser Coho POPID Lookup_adj with HO revisions Feb 25 2020.csv",stringsAsFactors = FALSE)
#****************************************************************************************


cu.info.main <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE)



# BY POP ----------------------------------------------------------------------------

# Variables matched up as follows:

# SpnForTrend_Total = Estimate
# SpnForTrend_Wild = NA
# SpnForAbd_Total =  Estimate
# SpnForAbd_Wild =  NA
# Recruits_Total =  NA
# Recruits_Wild =   NA

# ********************************************************************************************** CHANGED - NOW reads all streams which are filtered by Pop_Lookup$WSP_ts in PStat
fr.co.bypop.raw <- read.csv("DATA_IN/ALL IFC streams spawners_EC.new=NA, infill=FALSE_ALL STREAMS.csv",stringsAsFactors = FALSE)

# If wanting to see only the WSP data for some reason - thouth the Pop_Lookup file filters these in the tool
# This can be useful if the Pop_Lookup file needs to be updated if the streams included in the CU sums is changed (see lines 117-130)
  #fr.co.bypop.raw <- read.csv("DATA_IN/ALL IFC streams spawners_EC.new=NA, infill=FALSE_WSP ONLY.csv", stringsAsFactors = FALSE)

# *************************************************************************************************************


names(fr.co.bypop.raw )


# *******************************************************************************************************ADDED
  library(Hmisc);library(stringr); library(tidyverse); library(mgsub)
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }

  n.pop.info <- pop.info %>% mutate(Name = ifelse(STAD_TribDeme!="",STAD_TribDeme,
                                                    ifelse(NUSEDS_SYSTEM_SITE!="", NUSEDS_SYSTEM_SITE,
                                                                                         ifelse(RPA_River_Mainstems!="",RPA_River_Mainstems,RPA_Tributaries)))) %>%
                             select(NUSEDS_CU, NUSEDS_POP_ID, NUSEDS_GFE_ID, Name)
  n.pop.info$Name <- (sapply(X = tolower(n.pop.info$Name), FUN = simpleCap))
  pats = c('Creek')
  n.pop.info$Name <- stringr::str_replace_all(n.pop.info$Name, pats, c('Cr'))
  pats = c('River')
  n.pop.info$Name <- stringr::str_replace_all(n.pop.info$Name, pats, c('R'))


  # Must do same treatments to the other file
  fr.co.bypop.raw$TribDeme <- (sapply(X = tolower(  fr.co.bypop.raw$TribDeme), FUN = simpleCap))
  pats = c('Creek')
  fr.co.bypop.raw$TribDeme <- stringr::str_replace_all(fr.co.bypop.raw$TribDeme, pats, c('Cr'))
  pats = c('River')
  fr.co.bypop.raw$TribDeme <- stringr::str_replace_all(fr.co.bypop.raw$TribDeme, pats, c('R'))

  # MATCHING - lists missing
  (unique(fr.co.bypop.raw$TribDeme))[!(unique(fr.co.bypop.raw$TribDeme)) %in% c(n.pop.info$Name)]
# ************************************************************************************************************ END


# Filtering / Renaming / Modifying:
# - no modifications to the values, adding NA columns for recruits and wild spn

fr.co.bypop.cleaned <-  fr.co.bypop.raw  %>%

                            # ********************************************************************ADDED
                            left_join(n.pop.info, by=c("TribDeme" = "Name")) %>%
                            # ***********************************************************************

                            rename(SpnForTrend_Total = Estimate,
                                 CU_Name = CU_name,
                                 Pop_Name = TribDeme,

                                 # *********************************************** ADDED
                                 Pop_ID=NUSEDS_POP_ID,
                                 # ************************************************

                                 Year = Return.Year)  %>%
                           mutate(SpnForTrend_Wild =  NA,
                                  SpnForAbd_Total =  SpnForTrend_Total,
                                 SpnForAbd_Wild =  NA,
                                 Recruits_Total =  NA, Recruits_Wild =   NA

                                 #******************************************** Removed POPID from here
                            ) %>%
                            select(-X,-NUSEDS_GFE_ID, - NUSEDS_CU) # *************** ADDED to this

# add in CU, DU, info

fr.co.bypop.cleaned <- fr.co.bypop.cleaned %>%
                        left_join(select(cu.info.main,CU_ID,CU_Name,DU_ID),by="CU_Name") %>%
                        filter(Year >= 1998)


head(fr.co.bypop.cleaned )

num.rec.cleaned <- dim(fr.co.bypop.cleaned)[1]
num.rec.raw <- dim(fr.co.bypop.raw)[1]

print(paste0("SBC CK By Pop: Retained Records= ",num.rec.cleaned,"/",num.rec.raw))

write.csv(fr.co.bypop.cleaned, "DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserCoho.csv",row.names=FALSE)

names(fr.co.bypop.cleaned)


# BMac April 6 2021 - Updating the PopLookup WSP_ts field to say "yes" for those streams that are included in the CU data (aka WSP streams)
# To update this in the future it needs to be used with fr.co.bypop.raw reading in only the WSP streams (aka, run SUB_Data extraction... with
# the Pop level data extraction using WSP_only = TRUE @ line 356)

  # pop_lookup_coho <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv") %>% filter(TimeSeriesData_Species=="Co_Fraser")
  #
  # pop_lookup_coho$WSP_ts[pop_lookup_coho$TimeSeriesData_Pop_Name %in% unique(fr.co.bypop.cleaned$Pop_Name)] <- "yes"
  # # not all of the streams included have their TribDeme name in the PopLookup file, or it is not spelled the same. Grab POP_IDs for these
  # # Note that the two Barriere R entries have the same POP_ID, as do Fennell and Saskum, so there were 9 misssing but only 7 popids
  # missing.popids <- fr.co.bypop.cleaned %>% filter(!(fr.co.bypop.cleaned$Pop_Name) %in% pop_lookup_coho$TimeSeriesData_Pop_Name) %>% select(Pop_ID) %>%distinct()
  # pop_lookup_coho$WSP_ts[pop_lookup_coho$Pop_ID %in% missing.popids$Pop_ID] <- "yes"
  #
  # Pop_Lookup <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv") %>% filter(!TimeSeriesData_Species=="Co_Fraser") %>%
  #   rbind(pop_lookup_coho)
  #
  # write.csv(Pop_Lookup, "DATA_LOOKUP_FILES/PopLookup.csv")


# BY CU ----------------------------------------------------------------------------

# Variables matched up as follows:

# SpnForTrend_Total = Total_Spawners
# SpnForTrend_Wild = Natural_Origin_Spawners
# SpnForAbd_Total =  Total_Spawners
# SpnForAbd_Wild =  Natural_Origin_Spawners
# Recruits_Total =  Total.Rec
# Recruits_Wild =   Rec.Nat.Total

# ***************************************************************************************** Changed this to infill = FALSE Sept 2021 becasue infilling has been looked at by STAD to 2019 already
fr.co.bycu.raw <- read.csv("DATA_IN/All IFC CUs BY Table_EC.max=NA_infill=FALSE.csv", stringsAsFactors = FALSE) %>%
                    select(-X) # remove row names
# ********************************************************************************************

names(fr.co.bycu.raw )


# Filtering / Renaming / Modifying:
# - No filtering
# - rename a few as listed above
# - no modifications to the values


# ********************************************************************************************** ADDED
# remove years not appropriate for the analyses
frco.list <-  fr.co.bycu.raw  %>%split(.$CU_ID)
  for(cu in names(frco.list)){
      frco.list[[cu]] <- filter(data.frame(frco.list[[cu]]), Brood_Year >= cu.info.main$Trends_StartYr[cu.info.main$CU_ID==cu])
  }
  fr.co.bycu.trimmed <- do.call(rbind,frco.list)
# ********************************************************************************************** END


fr.co.bycu.cleaned <-  fr.co.bycu.trimmed  %>%                                                  #***** CHANGED
                                              rename(SpnForTrend_Total = Total_Spawners,
                                                   SpnForTrend_Wild = Natural_Origin_Spawners,
                                                   Recruits_Total =  Total.Rec,
                                                   Recruits_Wild =   Rec.Nat.Total,
                                                   Year = Brood_Year,
                                                   CU_Name =  CU)  %>%
                                                   mutate(SpnForAbd_Total =  SpnForTrend_Total ,
                                                   SpnForAbd_Wild =  SpnForTrend_Wild,
                                                   Total_ER = NA)

fr.co.bycu.cleaned <- cbind(Species="Coho",fr.co.bycu.cleaned )


fr.co.bycu.cleaned <- fr.co.bycu.cleaned %>%
                                   left_join(select(cu.info.main,CU_Name,DU_ID),by="CU_Name") %>%
                                   filter(Year >= 1998)




head(fr.co.bycu.cleaned )

num.rec.cleaned <- dim(fr.co.bycu.cleaned)[1]
num.rec.raw <- dim(fr.co.bycu.raw)[1]

print(paste0("Fraser Coho By CU: Retained Records= ",num.rec.cleaned,"/",num.rec.raw))

write.csv(fr.co.bycu.cleaned, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserCoho.csv",row.names=FALSE)






















