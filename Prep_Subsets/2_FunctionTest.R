# testing script for the new functions.
# these will move into WSPMEtrics package


library(tidyverse)
library(WSPMetrics)

# function to replace script 2_CalculateMetrics_By_CU.R
# ONLY FOR LOCAL DEBUGGING. USE PACKAGE FUNCTIONS NOW!
#source("Prep_Subsets/Tmp_Functions/Module_calculateMetricsByCU.R")
#source("Prep_Subsets/Tmp_Functions/Module_prepDataForRapidStatus.R")
#source("Prep_Subsets/Tmp_Functions/Module_applyRapidStatusTree.R")
#source("Prep_Subsets/Tmp_Functions/Module_generateRapidStatus.R")
#source("Prep_Subsets/Tmp_Functions/Module_plotStatusDashboards.R")
#source("Prep_Subsets/Tmp_Functions/Module_generateReportTemplate.R")



############################################################################################
# Run on "Worked Examples" set
############################################################################################

worked.examples.metrics.out <- calculateMetricsByCU(cu.file = read_csv("Prep_Subsets/Output/WorkedExamples/CU_Data_WorkedExamples.csv"),  
                               cu.info = read_csv("Prep_Subsets/Output/WorkedExamples/CU_Specs_WorkedExamples.csv")     ,
                               cyclic.cu.bm = NULL,
                               retro.start.use = 1995,
                               out.label = "WorkedExamples",
                               out.filepath = "Prep_Subsets/Output/WorkedExamples")
names(worked.examples.metrics.out)


names(worked.examples.metrics.out$Metrics)

rapid.status.input <- prepDataForRapidStatus(
                          cu.info = worked.examples.metrics.out$Specs,
                          metrics.raw = worked.examples.metrics.out$Metrics,
                          gen.avg.src = worked.examples.metrics.out$GenAvg, 
                          out.label = "WorkedExamples",
                          out.filepath = "Prep_Subsets/Output/WorkedExamples")
  
names(rapid.status.input)
head(rapid.status.input$Values)


names(worked.examples.metrics.out$Specs)
names(worked.examples.metrics.out$Data)
names(rapid.status.input$Status)
names(rapid.status.input$Values)

source("Prep_Subsets/Tmp_Functions/Module_generateRapidStatus.R")
rapid.status.results <- generateRapidStatus(cu.info = worked.examples.metrics.out$Specs,
                                cu.data = worked.examples.metrics.out$Data,
                                publ.status.src = read_csv("Prep_Subsets/Output/WorkedExamples/CU_PublishedIntegratedStatuses_WorkedExamples.csv"),  
                                retro.values = rapid.status.input$Values, 
                                retro.status = rapid.status.input$Status,
                                metrics.long = rapid.status.input$LongForm,
                                group.var = "Species",
                                out.label = "WorkedExamples",
                                out.filepath = "Prep_Subsets/Output/WorkedExamples")
names(rapid.status.results)
rapid.status.results$Summary
rapid.status.results$SummaryTable
rapid.status.results$Data
rapid.status.results$Rules
rapid.status.results$ConfusionMatrix
rapid.status.results$RawConfusionMatrix
rapid.status.results$Accuracy

range(rapid.status.results$Data$Year)



plotStatusDashboards(
    cu.info =  worked.examples.metrics.out$Specs,
    retro.summary.tbl = rapid.status.results$SummaryTable,
    cu.data = worked.examples.metrics.out$Data, 
    out.label = "RapidStatusOut",
    out.filepath = "Prep_Subsets/Output/WorkedExamples/Dashboards")


source("Prep_Subsets/Tmp_Functions/Module_generateReportTemplate.R")
generateReportTemplate(type = "readme", 
                       file.label = "Report", # doesn't apply to readme
                       files.path = "Prep_Subsets/Output/WorkedExamples", 
                       repo.path = "https://github.com/SOLV-Code/Scanner-Data-Processing/tree/main/")


generateReportTemplate(type = "quarto", 
                       file.label = "Report", # doesn't apply to readme
                       files.path = "Prep_Subsets/Output/WorkedExamples", 
                       repo.path = NULL)







############################################################################################
# Run on "Fraser Sandbox" set
############################################################################################

fraser.sandbox.metrics.out <- calculateMetricsByCU(cu.file = read_csv("Prep_Subsets/Output/FraserSandbox/CU_Data_FraserSandbox.csv"),  
                                                    cu.info = read_csv("Prep_Subsets/Output/FraserSandbox/CU_Specs_FraserSandbox.csv")     ,
                                                    cyclic.cu.bm = NULL,
                                                    retro.start.use = 1995,
                                                    out.label = "FraserSandbox",
                                                    out.filepath = "Prep_Subsets/Output/FraserSandbox")
names(fraser.sandbox.metrics.out)


names(fraser.sandbox.metrics.out$Metrics)

rapid.status.input <- prepDataForRapidStatus(
  cu.info = fraser.sandbox.metrics.out$Specs,
  metrics.raw = fraser.sandbox.metrics.out$Metrics,
  gen.avg.src = fraser.sandbox.metrics.out$GenAvg, 
  out.label = "FraserSandbox",
  out.filepath = "Prep_Subsets/Output/FraserSandbox")

names(rapid.status.input)
head(rapid.status.input$Values)


names(fraser.sandbox.metrics.out$Specs)
names(fraser.sandbox.metrics.out$Data)
names(rapid.status.input$Status)
names(rapid.status.input$Values)

sort(unique(rapid.status.input$Values$CU_ID))


source("Prep_Subsets/Tmp_Functions/Module_applyRapidStatusTree.R")
source("Prep_Subsets/Tmp_Functions/Module_generateRapidStatus.R")

rapid.status.results <- generateRapidStatus(cu.info = fraser.sandbox.metrics.out$Specs,
                                            cu.data = fraser.sandbox.metrics.out$Data,
                                            publ.status.src = read_csv("Prep_Subsets/Output/FraserSandbox/CU_PublishedIntegratedStatuses_FraserSandbox.csv"),  
                                            retro.values = rapid.status.input$Values, 
                                            retro.status = rapid.status.input$Status,
                                            metrics.long = rapid.status.input$LongForm,
                                            group.var = "Species",
                                            out.label = "FraserSandbox",
                                            out.filepath = "Prep_Subsets/Output/FraserSandbox")
names(rapid.status.results)
rapid.status.results$Summary
rapid.status.results$SummaryTable
rapid.status.results$Data
rapid.status.results$Rules
rapid.status.results$ConfusionMatrix
rapid.status.results$RawConfusionMatrix
rapid.status.results$Accuracy

range(rapid.status.results$Data$Year)



plotStatusDashboards(
  cu.info =  fraser.sandbox.metrics.out$Specs,
  retro.summary.tbl = rapid.status.results$SummaryTable,
  cu.data = fraser.sandbox.metrics.out$Data, 
  out.label = "RapidStatusOut",
  out.filepath = "Prep_Subsets/Output/FraserSandbox/Dashboards")


source("Prep_Subsets/Tmp_Functions/Module_generateReportTemplate.R")
generateReportTemplate(type = "readme", 
                       file.label = "Report", # doesn't apply to readme
                       files.path = "Prep_Subsets/Output/FraserSandbox", 
                       repo.path = "https://github.com/SOLV-Code/Scanner-Data-Processing/tree/main/")


generateReportTemplate(type = "quarto", 
                       file.label = "Report", # doesn't apply to readme
                       files.path = "Prep_Subsets/Output/FraserSandbox", 
                       repo.path = NULL)


