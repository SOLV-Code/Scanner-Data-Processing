# This script extracts subsets of the spec files and data files, 
# which can then be used as the starting point for stand-alone
# implementations (worked examples public repo, Fraser/Interior 
# status sandbox)


library(tidyverse)

# get list of settings files
settings.folder <- "Prep_Subsets/Settings"
output.folder <- "Prep_Subsets/Output"
sets.vec <- list.files(settings.folder, pattern = "Settings_",full.names=TRUE)


cu.specs.source <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv")
cu.data.source <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",stringsAsFactors = FALSE)
cyclic.bm.source  <- read.csv("DATA_LOOKUP_FILES/FRSK_Cycle_Rel_BMs.csv", stringsAsFactors=FALSE)
publ.status.source <- read.csv("DATA_LOOKUP_FILES/Published_Integrated_Status_Summary.csv",stringsAsFactors = FALSE)


for(i in 1:length(sets.vec)){

set.path <- sets.vec[i]  
set.label <- gsub("/Settings_","",gsub(".csv", "", gsub(settings.folder, "",set.path)))
                  
print("-------------------------------")
print(paste("Starting:",set.label))
print("-------------------------------")

# get and check settings
settings.use <- read_csv(set.path)
print(head(settings.use))


# fix the CU_ID  ("_" vs. "-")
settings.use$CU_ID <- gsub("_","-",settings.use$CU_ID)
cu.specs.source$CU_ID <- gsub("_","-",cu.specs.source$CU_ID)
cu.data.source$CU_ID <- gsub("_","-",cu.data.source$CU_ID)
cyclic.bm.source$CU_ID <- gsub("_","-",cyclic.bm.source$CU_ID)



# create output folder
path.out.set <- paste0(output.folder,"/",set.label)
if(!dir.exists(path.out.set)){dir.create(path.out.set)}

# subset and put CU spec file
specs.sub <- cu.specs.source %>% dplyr::filter(CU_ID %in% settings.use$CU_ID) %>%
                select(CU_ID,CU_Acro,CU_Name,DataQualkIdx,AbdMetric,	AbsAbdMetric,	ShortTrendMetric,
                       LongTrendMetric,	PercentileMetric,	Avg_Gen,	Cyclic,	Cyc_Dom,	Cyc_Dom_Year,
                       TrendNumGen,	TrendExtraYears,	TrendLog,	TrendSmooth,	AvgType,	AvgSmooth,
                       AvgRecentExcl,	LongTrendMinYears,	RelAbd_AvgData,	RelAbd_LBM,	RelAbd_UBM,
                       AbsAbd_LBM,	AbsAbd_UBM,	LongTrend_LBM,	LongTrend_UBM,	PercChange_LBM,
                       PercChange_UBM,	ProbDeclBelowLBM_LBM,	ProbDeclBelowLBM_UBM,	Trends_StartYr,	
                       Abd_StartYr) %>% select(-CU_Acro,-CU_Name) %>% 
              left_join(settings.use ,by="CU_ID") %>% select(-CU_ID, -CU_Acro, -Group) %>%
              dplyr::rename(CU_ID = New_CU_ID, CU_Acro = New_CU_Acro, CU_Name = New_CU_Name,
                            Group = New_Group) %>%
              select(CU_ID,CU_Acro,CU_Name,Group,Description, everything()) %>%
              arrange(CU_ID)
              

print(head(specs.sub))
write_csv(specs.sub,paste0(path.out.set,"/","CU_Specs_",set.label,".csv"))


cu.data.sub <- cu.data.source %>%  
  dplyr::filter(CU_ID %in% settings.use$CU_ID) %>%
  select(-DataSet,-DU_ID) %>%
  left_join(settings.use %>% select(-Description),by="CU_ID") %>% select(-CU_ID, -CU_Name,-CU_Acro,-New_CU_Acro) %>%
  dplyr::rename(CU_ID = New_CU_ID, CU_Name = New_CU_Name ) %>%
  select(CU_ID,CU_Name, everything()) %>%
  arrange(CU_ID)

print(head(cu.data.sub))
write_csv(cu.data.sub,paste0(path.out.set,"/","CU_Data_",set.label,".csv"))



# need an additional input file for highly cyclic CUs
# generate regardless, so that always have the same set of input files

cyclic.bm.sub <- cyclic.bm.source %>% dplyr::filter(CU_ID %in% settings.use$CU_ID) %>%
  select(-CU_Name) %>% 
  left_join(settings.use ,by="CU_ID") %>% select(-CU_ID, -CU_Acro) %>%
  dplyr::rename(CU_ID = New_CU_ID, CU_Acro = New_CU_Acro, CU_Name = New_CU_Name ) %>%
  select(CU_ID,CU_Acro,CU_Name,Description, everything()) %>%
  arrange(CU_ID)
print(head(cyclic.bm.sub))
write_csv(cyclic.bm.sub,paste0(path.out.set,"/","CU_CyclicBM_",set.label,".csv"))



publ.status.sub <- publ.status.source %>% dplyr::filter(CU_ID %in% settings.use$CU_ID,Metric=="IntStatus") %>%
  select(-Stock) %>% 
  left_join(settings.use ,by="CU_ID") %>% select(-CU_ID, -CU_Acro) %>%
  dplyr::rename(CU_ID = New_CU_ID, CU_Acro = New_CU_Acro, CU_Name = New_CU_Name ) %>%
  select(CU_ID,CU_Acro,CU_Name,Description, everything()) %>%
  arrange(CU_ID)
print(head(publ.status.sub))
write_csv(publ.status.sub,paste0(path.out.set,"/","CU_PublishedIntegratedStatuses_",set.label,".csv"))



  
} # end looping through settings files



















