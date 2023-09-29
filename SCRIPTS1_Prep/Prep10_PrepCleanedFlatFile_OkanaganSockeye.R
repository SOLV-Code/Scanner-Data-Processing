library(tidyverse)


ok.sk.src <- read_csv("DATA_IN/OkanaganSockeye_GENERATED_SR_Diagnostic_Total.csv") %>%
                dplyr::filter(Variable=="Spawners") %>% select(-Variable,-Total,-PercSkaha) %>%
                pivot_longer(cols=all_of(c("Osoyoos","Skaha")),names_to = "CU_Name",
                             values_to = "Spn") %>%
                arrange(CU_Name,Year) %>% mutate(STK_NAME = CU_Name)
head(ok.sk.src )


cu.info.main <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv")
names(cu.info.main)

ok.sk.src <- left_join(ok.sk.src, cu.info.main %>% select(CU_Name,CU_ID,DU_ID) )
ok.sk.src

ok.sk.clean <- ok.sk.src %>%
                                              mutate(Species = "Sockeye",
                             SpnForAbd_Total	 = Spn,
                             SpnForTrend_Total	 = Spn,
                             SpnForAbd_Wild		 = Spn,
                             SpnForTrend_Wild	 = Spn,
                             STK_ID = CU_ID,

                             ) %>%
                        select(Species,	Year,	SpnForAbd_Total,	SpnForTrend_Total,	SpnForAbd_Wild,
                          SpnForTrend_Wild,	CU_ID,	CU_Name,	DU_ID,	STK_ID,	STK_NAME)

head(ok.sk.clean)


write.csv(ok.sk.clean, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_OkanagansSockeye.csv",row.names=FALSE)





