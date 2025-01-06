#' generateReportTemplate
#'
#' this function creates a basic markdown template after the other functions have been used to calculate metrics, apply the rapid status decision tree, and plot dashboards.
#' @param type one of "readme" or "quarto". "readme" creates a basic README.md file that will display formatted text including figures when viewed on Github or Gitlab. "quarto" creates a "*.qmd" file as a starting point for a more comprehensive document. 
#' @param file.label label to be used for markdown file (if type = "quarto")
#' @param files.path path to folder with input files and output files. The following files are required: "CU_Specs_LABEL.csv",
#' @param plots.path path to folder with dashboard plots. For a readme this is the full path to the png images on Github/Gitlab (e.g., "https://github.com/USER_NAME/REPO_NAME/blob/main/OUTPUT/Dashboards"). For the quarto option, this is a relative path within the R working directory (e.g., "OUTPUT/Dashboards").
#' @param report.path path to folder where markdown output goes
#' @keywords markdown, report, readme
#' @export



generateReportTemplate <- function(type = "readme", file.label = "Report",
				files.path, plots.path, report.path){


library(tidyverse)
#library(plotrix) CHECK IF STILL NEEDED


if(!dir.exists(report.path)){dir.create(report.path)}


if(type=="readme"){file.name <- paste0(report.path,"/README.md")}
if(type=="quarto"){file.name <- paste0(report.path,"/",file.label,".qmd")}


if(type=="readme"){

cat("# Status Report \n",file = file.name)


}


if(type=="quarto"){

cat("
---	
title: \"Status Report\"
format: 
  docx:
    toc: true
    number-sections: true
    highlight-style: github
editor: visual
execute: 
  warning: false
  message: false
  echo: false
---	
	
	
	
	
	
	",file = file.name)

}

# Part 1 - Intro Materials

cat("
## Introduction

### Background

This report summarizes data, status metrics, and rapid status status results for INSERT LABEL.

### WSP Status Methods

put some brief text here with links to publications

## CU Status

Put some brief text that describes the dashboards


",file = file.name,append=TRUE)


# read in spec file

spec.file.path <- list.files(path=files.path,pattern = "CU_Specs_",full.names=TRUE)
cu.specs <- read_csv(spec.file.path)
cu.list <- cu.specs %>% select(CU_ID) %>% unlist()
print(cu.list)

fig.list <- list.files(path=plots.path,pattern = ".png",full.names=FALSE)
print(fig.list)

# loop through CUs


for(cu.do in cu.list)

specs.sub <- cu.specs %>% dplyr::filter(CU_ID==cu.do)

cu.name <- specs.sub$CU_Name


cat("{{< pagebreak >}}

",file = file.name,append=TRUE)

cat(paste0("### ",cu.name," (",specs.sub$CU_ID,", ",specs.sub$Group,")\n"),file = file.name,append=TRUE)


cat(specs.sub$Description,file = file.name,append=TRUE)

cat("


",file = file.name,append=TRUE)

if(type=="readme"){


#cat(paste0("<img src=\" ", #https://github.com/SOLV-Code/Open-Source-Env-Cov-PacSalmon/blob/main/OUTPUT/DataOverview_Part2.png"
	#width="600">
#"
#,file = file.name,append=TRUE)


}


if(type=="quarto"){

fig.link <- grep(cu.do,fig.list,value=TRUE)
print(fig.link)

cat(paste0('![Status Metrics and Rapid Status for ',cu.name,'](',fig.link,')'),
file = file.name,append=TRUE)


}









} # end report function
