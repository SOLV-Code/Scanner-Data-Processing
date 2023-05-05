
library(tidyverse)

# BY POP -----------------------------------------

# read in the cleaned flat files
flat.fr.sk <- read.csv("DATA_OUT/Cleaned_FlatFile_ByPop_FraserSockeye.csv",stringsAsFactors = FALSE)
flat.sbc.ck <- read.csv("DATA_OUT/Cleaned_FlatFile_ByPop_SBC_Ck.csv",stringsAsFactors = FALSE)
flat.fr.co<- read.csv("DATA_OUT/Cleaned_FlatFile_ByPop_FraserCoho.csv",stringsAsFactors = FALSE)
flat.fr.pk<- read.csv("DATA_OUT/Cleaned_FlatFile_ByPop_FraserPink_All_Nuseds.csv",stringsAsFactors = FALSE)
flat.fr.cm<- read.csv("DATA_OUT/Cleaned_FlatFile_ByPop_FraserChum_all.csv",stringsAsFactors = FALSE)
flat.isc.cm<- read.csv("DATA_OUT/Cleaned_FlatFile_byPop_ISCChum.csv",stringsAsFactors = FALSE)

# find common variables
vars.use <- intersect(intersect(intersect(intersect(intersect(names(flat.fr.sk),names(flat.sbc.ck)),names(flat.fr.co)), names(flat.fr.pk)),
                                                              names(flat.fr.cm)), names(flat.isc.cm))

flat.merged <- bind_rows(list(Sk_Fraser= select(flat.fr.sk,all_of(vars.use)),Ck_SBC = select(flat.sbc.ck,all_of(vars.use)),
                              Co_Fraser = select(flat.fr.co,all_of(vars.use)), Pk_Fraser = select(flat.fr.pk,vars.use),
                              Cm_Fraser = select(flat.fr.cm,all_of(vars.use)), Cm_ISC = select(flat.isc.cm, all_of(vars.use))),
                .id = "DataSet")
head(flat.merged)
write.csv(flat.merged,"DATA_OUT/MERGED_FLAT_FILE_BY_POP.csv",row.names = FALSE)



# BY CU

# read in the cleaned flat files
flat.fr.sk.cu <- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_FraserSockeye.csv",stringsAsFactors = FALSE)
flat.sbc.ck.cu <- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_SBC_Ck.csv",stringsAsFactors = FALSE)
flat.fr.co.cu <- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_FraserCoho.csv",stringsAsFactors = FALSE)
flat.fr.pk.cu <- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_FraserPink.csv",stringsAsFactors = FALSE)
flat.fr.cm.cu <- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_FraserChum.csv",stringsAsFactors = FALSE)
flat.isc.cm.cu<- read.csv("DATA_OUT/Cleaned_FlatFile_byCU_ISCChum.csv",stringsAsFactors = FALSE)
flat.skeenanass.sk.cu<- read.csv("DATA_OUT/Cleaned_FlatFile_ByCU_SkeenaNassSockeye.csv",stringsAsFactors = FALSE)



# find common variables
vars.use <- intersect(intersect(intersect(intersect(intersect(names(flat.fr.sk.cu),names(flat.sbc.ck.cu)),names(flat.fr.co.cu)),
                                names(flat.fr.pk.cu)),names(flat.fr.cm.cu)), names(flat.isc.cm.cu))

flat.merged.cu <- bind_rows(list(Sk_Fraser= select(flat.fr.sk.cu,all_of(vars.use)),
                              Ck_SBC = select(flat.sbc.ck.cu,all_of(vars.use)),
                              Co_Fraser= select(flat.fr.co.cu,all_of(vars.use)),
                              Pk_Fraser = select(flat.fr.pk.cu, all_of(vars.use)),
                              Cm_Fraser = select(flat.fr.cm.cu, all_of(vars.use)),
                              Cm_ISC = select(flat.isc.cm.cu, all_of(vars.use)), 
                              Sk_SkeenaNass = select(flat.skeenanass.sk.cu, all_of(vars.use))) , 
                              .id = "DataSet")



# GP ADDED March 2023: Filter out any records before CU-specific start year
start.yrs.df <-  read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE) %>%
                    select(CU_ID_Report,Abd_StartYr)
  
start.yrs.df


flat.merged.cu <- flat.merged.cu %>% 
                      mutate(CU_ID_Report = gsub("_","-", CU_ID)) %>%
                      left_join(start.yrs.df, by = "CU_ID_Report") %>%
                      mutate(UseYear = Year >= Abd_StartYr) %>% dplyr::filter(UseYear)
head(flat.merged.cu)



write.csv(flat.merged.cu,"DATA_OUT/MERGED_FLAT_FILE_BY_CU.csv",row.names = FALSE)


