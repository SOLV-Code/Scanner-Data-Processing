#############################################################
# Calc statuses and generate summaries
# (basically some key bits from the case study paper, so we can always have the
#  the latest outputs  here, without changing anything in the tech report, and without
# having to copy files back and forth)

# NOTE: IMAGE OUTPUTS ARE NOT TRACKED ON GIT, BUT
# THE REQUIRED INPUT FILES ARE. JUST RUN THIS SCRIPT TO
# GET THE PLOTS.


source("CODE/synopticFunction_Source.R")  # make sure the latest version is here!
metrics.src <- read_csv("DATA_OUT/Retrospective_Metrics_Values.csv")


# NEED TO DISCUSS HOW MUCH TO REPRODUCE HERE
# OR HAVE A SEPARATE REPO WITH "LATEST SUMMARY REPORT"



