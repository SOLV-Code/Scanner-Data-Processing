#' generateReportTemplate
#'
#' this function creates a basic markdown template after the other functions have been used to calculate metrics, apply the rapid status decision tree, and plot dashboards.
#' @param type one of "readme" or "quarto". "readme" creates a basic README.md file that will display formatted text including figures when viewed on Github or Gitlab. "quarto" creates a "*.qmd" file as a starting point for a more comprehensive document. 
#' @param file.label label to be used for markdown file (if type = "quarto")
#' @param files.path path to folder with input files and output files. LIST REQUIRED FILES
#' @param plots.path path to folder with dashboard plots. LIST REQUIRED FILES
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
",file = file.name,append=TRUE)



} # end report function
