library(tidyverse)


skeenanass.sk.src <- read_csv("DATA_IN/SkeenaNassSockeye_Generated_SR_Data.csv") %>%
                      mutate(CU_Name = Stock, 
                             TotalER = 1- (Spn/Total.Run),
                             Recruits_Total = Rec, 
                             sr_spn = Spn, 
                             Recruits_Wild = Rec) %>%
                      dplyr::rename(STK_NAME = Stock)
head(skeenanass.sk.src )

cu.info.main <- read_csv("DATA_LOOKUP_FILES/MAIN_CU_LOOKUP_FOR_SOS.csv")
names(cu.info.main)

skeenanass.sk.src <- left_join(skeenanass.sk.src, cu.info.main %>% select(CU_Name,CU_ID,DU_ID) )


skeenanass.sk.clean <- skeenanass.sk.src %>% 
                                              mutate(Species = "Sockeye",
                             SpnForAbd_Total	 = Spn,
                             SpnForTrend_Total	 = Spn,	
                             SpnForAbd_Wild		 = Spn,
                             SpnForTrend_Wild	 = Spn,
                             STK_ID = CU_ID,
                             
                             ) %>%
                        select(Species,	Year,	SpnForAbd_Total,	SpnForTrend_Total,	SpnForAbd_Wild,	
                          SpnForTrend_Wild,	CU_ID,	CU_Name,	DU_ID,	STK_ID,	STK_NAME,	
                          TotalER,	Recruits_Total,	sr_spn,	Recruits_Wild)

head(skeenanass.sk.clean)


write.csv(skeenanass.sk.clean, "DATA_OUT/Cleaned_FlatFile_ByCU_SkeenaNassSockeye.csv",row.names=FALSE)
