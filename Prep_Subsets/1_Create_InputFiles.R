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


for(i in 1:length(sets.vec)){

set.path <- sets.vec[i]  
set.label <- gsub("/Settings_","",gsub(".csv", "", gsub(settings.folder, "",set.path)))
                  
print("-------------------------------")
print(paste("Starting:",set.label))
print("-------------------------------")

# get and check settings
settings.use <- read_csv(set.path)
print(head(settings.use))

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
                       Abd_StartYr) %>% select(-CU_Acro) %>% 
              left_join(settings.use %>% select(-CU_Acro),by="CU_ID") %>% select(-CU_ID) %>%
              dplyr::rename(CU_ID = NewID, CU_Acro = NewLabel) %>%
              select(CU_ID,CU_Acro,CU_Name,Description, everything()) %>%
              arrange(CU_ID)
              

print(head(specs.sub))


write_csv(specs.sub,paste0(path.out.set,"/","CU_Specs_",set.label,".csv"))


  
}
















