# March 12 2024
# Run all integrated data prep steps

# Filtering occurs in the scripts
# datastage: "All", "Approved", "Explore", "Prep", c(list stages to include)
# runAllout: set to FALSE to just run dashboards 



Run <- function(datastage="Approved"){
      print(paste("---running", toupper(datastage),"data---"))
      if(datastage=="All") datastage=c("Approved", "Explore", "Prep")
      print("---merging flat files---")
      source("SCRIPTS2_Main/1_MergeFlatFiles.R")
      print("---calculating WSP metrics---")
      source("SCRIPTS2_Main/2_CalculateMetrics_By_CU.R", local=TRUE)
      print("---reformatting data---")
      source("SCRIPTS2_Main/3_PrepData_for_RapidStatusAlgorithm.R", local=TRUE)
      print("---running WSP algorithm---")
      source("SCRIPTS2_Main/4_Runalgorithm_CreateScannerinputs.R", local=TRUE)
      print("---building metric and status dashboards---")
      source("SCRIPTS2_Main/5_GenerateSummaries.R", local=TRUE)
      print("---building timeline summary plots---")
      source("SCRIPTS2_Main/6_Summary_Timeline_Plots.R", local=TRUE)
      print("---creating Scanner input files---")
      source("SCRIPTS2_Main/10_GenerateScannerInputs.R", local=TRUE)
}

#Run(c("Approved", "Prep"))
 
Run("All")

