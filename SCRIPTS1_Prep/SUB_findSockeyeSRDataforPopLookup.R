library(dplyr)
library(stringr)

fr.sk.bypop.raw <- read.csv("DATA_IN/SOURCES/Sockeye All (June 2020).csv",stringsAsFactors = FALSE)
fr.sk.bypop.raw <- fr.sk.bypop.raw %>% mutate( Timing.Group = ifelse(Stock.Name.stream.=="Crazy Creek" & CU.Name == "Harrison (D/S) - Late", "T5" ,Timing.Group))

SR_Streams <- read.csv("DATA_IN/Sockeye_Fraser_SR_Streams.csv", header = TRUE)

not.in.cu <- read.csv("Sites not in CUs.csv")
# Instead of looking through the raw data use this because it is the same but with the popids. Only difference is Nadina streams have been filtered out, summed, and replaced with Late Nadina River
fr.sk.bypop.clean <- read.csv( "DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserSockeye.csv") %>%  filter(!Pop_Name %in% not.in.cu$Pop_Name)


Pop_Lookup.sk <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv") %>% filter(TimeSeriesData_Species == "Sk_Fraser")

# CU name crosswalk
cu.info.sos  <- read.csv("DATA_LOOKUP_FILES/SoS_Data_CU_Info_ForDataPrep.csv",stringsAsFactors = FALSE)



# Go through the CUs in the lookup file, pull the sites, check the CU in the SR_Streams lookup to see which are included in the CU data.
# If select sites are identified then get the POP_IDs for these and they get a "yes", all others get a "no".
# If the entry in SR_Streams is NA then all sites in the STAD file were used. This correlates to fr.sk.bypop.clean with not.in.cu filtered out.


# clear all WSP_ts entries
Pop_Lookup.sk$WSP_ts <- "no"

for(cu in unique(Pop_Lookup.sk$MapData_CU_ID)){
    find.cu <- str_replace(string = cu, pattern = "SE",replacement = "")
    cu.name <- fr.sk.bypop.clean %>% filter(CU_ID==find.cu) %>% select(CU_Name) %>% distinct()

    if(nrow(cu.name)==0) next

    cu.name <- ifelse(length(cu.name$CU_Name)==1, cu.name$CU_Name, fr.sk.bypop.clean %>% filter(CU_ID==find.cu) %>% select(CU_Name) %>% # select the one with the most entries
                                                                                         table() %>% which.max() %>% names())

             # if(length(cu.name$CU_Name)==1) cu.name <- cu.name$CU_Name
            #  if(length(cu.name$CU_Name)>1) cu.name <- fr.sk.bypop.clean %>% filter(CU_ID==find.cu) %>% select(CU_Name) %>% # select the one with the most entries
             #                                          table() %>% which.max() %>% names()

    sr.data.name <- cu.info.sos$Conservation_Unit[cu.info.sos$SK_Data_CU_Name==cu.name]

  # Get list of sites included if they are defined
  if(length(which(!is.na(SR_Streams[colnames(SR_Streams)==sr.data.name]))) > 0){
                                              streams <- SR_Streams %>% select(all_of(sr.data.name)) %>% unlist() %>% as.vector()
                                              Pop_Lookup.sk <- Pop_Lookup.sk %>%
                                                                      mutate(WSP_ts = case_when( (TimeSeriesData_Pop_Name %in% streams &
                                                                                                    MapData_CU_ID == cu) == TRUE ~ "yes", TRUE ~ WSP_ts))
                                              print(streams)
                                              #Pop_Lookup.sk$WSP_ts[Pop_Lookup.sk$TimeSeriesData_Pop_Name %in% streams] <- "yes"

  }
  if(length(which(!is.na(SR_Streams[colnames(SR_Streams)==sr.data.name]))) == 0){

                                              include <- fr.sk.bypop.clean %>% filter(CU_Name == cu.name) %>%
                                                                               select(MapPOP_ID, Pop_Name) %>%
                                                                               distinct()
                                              Pop_Lookup.sk$WSP_ts[Pop_Lookup.sk$Pop_ID %in% include$MapPOP_ID]<- "yes"
                                              print(include)
  }


  print(c(cu,cu.name))
} # end CU loop

Full_Pop_Lookup <-  read.csv("DATA_LOOKUP_FILES/PopLookup.csv") %>% filter(!TimeSeriesData_Species == "Sk_Fraser") %>%
                                                                 rbind(Pop_Lookup.sk)

  write.csv(Full_Pop_Lookup, "DATA_LOOKUP_FILES/PopLookup.csv")


