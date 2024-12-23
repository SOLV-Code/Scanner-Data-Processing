# testing script for the new functions.
# these will move into WSPMEtrics package


library(tidyverse)
library(WSPMetrics)

# function to replace script 2_CalculateMetrics_By_CU.R
source("Prep_Subsets/Tmp_Functions/2_CalculateMetrics_By_CU_FUNCTION.R")





# Run on "Worked Examples" set

worked.examples.metrics.out <- calculateMetricsByCU(cu.file = read_csv("Prep_Subsets/Output/WorkedExamples/CU_Data_WorkedExamples.csv"),  
                               cu.info = read_csv("Prep_Subsets/Output/WorkedExamples/CU_Specs_WorkedExamples.csv")     ,
                               cyclic.cu.bm = NULL,
                               retro.start.use = 1995,
                               out.label = "WorkedExamples",
                               out.filepath = "Prep_Subsets/Output/WorkedExamples/RetrospectiveMetrics_Output.csv")





