# March 12 2024
# Run all integrated data prep steps

print("---merging flat files---")
source("SCRIPTS2_Main/1_MergeFlatFiles.R")
print("---calculating WSP metrics---")
source("SCRIPTS2_Main/2_CalculateMetrics_By_CU.R")
print("---reformatting data---")
source("SCRIPTS2_Main/3_PrepData_for_RapidStatusAlgorithm.R")
print("---running WSP algorithm and creating Scanner input files---")
source("SCRIPTS2_Main/4_Runalgorithm_CreateScannerinputs.R")
print("---building metric and status dashboards---")
source("SCRIPTS2_Main/5_GenerateSummaries.R")
print("---building timeline summary plots---")
source("SCRIPTS2_Main/6_Summary_Timeline_Plots.R")
print("---building SMU timeline plots---")
source("SCRIPTS2_Main/7_SMU_Summary_Timeline_Plots.R")
print("---building alt SMU summary plots for SOPO---")
source("SCRIPTS2_Main/8_SMU_Summary_Timeline_PlotsforSOPO.R")