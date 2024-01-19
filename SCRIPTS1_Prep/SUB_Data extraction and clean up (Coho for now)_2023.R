# Data Exploration and Org for IFCoho data 
# Written by Bronwyn MacDonald
# October 21 2019
# Revised April 2023 due to changes in input data file from StAD
## Changes input columns, now use the prefishery hatchery vs natural returns provided to calculate recruits instead of calculating breakdown

# Info:
#   MUST HAVE dplyr version >= 0.7



##################################################

# DESCRIPTION OF CODE

# Produces 3 things all using COHO.clean dataframe, which is filtered version of the data provided by Lynda

# 1) CUsums: Sum of chosen variable across the Tribs in a CU (Final.Spawner.Estimate, Total.Returns, Pre.Fishery.Abundance, Hatchery.Returns.corrected, Natural.Returns)
#            EC.max: Filters based on data quality of Spawner estimates. Must enter a selection from "Type-1" (best) to "Type-7", "UNK", or "NA" (worst). 
#                    It will filter out all levels above that chosen, i.e. lower quality.  
#            start.yr: first year in the data to use 
#            wsp.infill: choose whether or not to include the infilled data from the WSP process (this refers to data infilled by StaAD)
#            run.infill: infill data using English method. Can run this on data post-WSP (set infill.start.yr = 2014) or the entire dataframe (infill.start.yr = FALE - sets it to start.yr)
#            WSP_only: only use the WSP approved systems (WSP.2014= green OR amber) - exclude 'not used' (if FALSE then should not be infilling) 
#   RUN:
#    CUsums(COHO.clean, "Fraser Canyon", "Final.Spawner.Estimate",EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=FALSE, WSP_only=TRUE)


# 2)  Cal.Nat: Calculates the Natural component of the Pre-fishery Return using the ratio of the Hatchery Returns to the Total Returns as a multiplier.
#              Enhanced Component (EC) is calculated for each TribDeme in each year then applied to the Final Spawner estimates to get the natural component.
#              Compares the calculated Natural return (spawners + SEP/FN removals) to the one in the data file as a check of the ratio. 
#              Also calculates natural and hatchery spawner numbers based on this same ratio (have to check if this is done in WSP not sure if appropriate - YES) 
#
#     RUN:
#     Calc.Nat(data=COHO.clean, CU="Middle Fraser", EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=TRUE, infill.start.yr=FALSE)

# 3) Brood Year Table with Natural, Hatchery, and Total Spawner estimates by BY, age-3 and age-4 Natural, Hatchery, and Total Recruits (based on average age props in WSP 2014)

#    Run:
#    BY.Table(data=COHO.clean, CU="Middle Fraser", EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=TRUE, infill.start.yr=FALSE)

# Start at 1998 for Coho 



#rm(list=ls())

library(dplyr)
library(tidyr)
library(forcats)
#homedir <- getwd()

#setwd("Coho/Lynda Ritchie Sept 19 2019")

COHO <- read.csv("DATA_IN/SOURCES/Fraser Coho/IFC Data 1975-2022 - created 2023-10-31 for Salmon Scanner.csv")
exceptions <- as.vector(read.csv("DATA_IN/SOURCES/Fraser Coho/IFC Infill exceptions.csv",header=FALSE))

#setwd(homedir)
#setwd("Sockeye")

CU.file <- read.csv("DATA_IN/SOURCES/Fraser Coho/FRSK_CU_Info_masterUpdate.csv")

#setwd(homedir)


## ========================================= DATA CLEANING AND CONFIGURATION ================================================================ ##

CUs <- CU.file %>% filter(Base.Unit.Species == "CO")


Coho.sub<- COHO %>% select(Brood.Year=BroodYear, Return.Year=ReturnYear, Conservation.Unit=ConservationUnit, Tributary.Deme=TribDeme,
                           Final.Spawner.Estimate, 
                           #Hatchery.Returns.corrected = Hatchery.Returns...use.this.one...corrected.for.total.return, 
                           #Total.Return,
                           Total.Return=Total.Escapement,  # This appears to have been renamed
                           #Natural.Returns = Natural.Returns...use.this.one, 
                           Natural.Returns = Natural.Escapement,  # renamed in 2023 - calling what was called returns escapements
                           Total.Hatchery.Clipped.Escapement.Corrected, Total.Hatchery.Unclipped.Escapement.Corrected,
                           Total.Pre.Fishery.Abundance=Total.Prefishery.Abundance, Prefishery.Natural=prefishery.return.natural, prefishery.return.hatchery.clipped, prefishery.return.hatchery.unclipped,
                           Final.Estimate.Type=FinalEstType, Estimate.Classification=EstClass, Data.Quality.Class=DataQualClass, 
                           WSP.2014.Data.Use.Categories, 
                           #FN.Removals, Classroom.Removals,
                           #SEP.Removals.BELOW = Brood.Removed.below.or.at.estimation.site,
                           #SEP.Removals.ABOVE = Brood.Removed.above.estimation.site,
                           #Total.Fry,Total.Smolts, 
                           #ER.Unclipped = ER.Rate...Unclipped,
                           #ER.Clipped = Final.Clipped.ER,
                           #Final.Fry.Survival = Final.Fry.Survival,
                           #Final.Smolt.Survival = Final.Smolt.Survival,
                           #Hatchery.Fry.Return,Hatchery.Smolt.Return
                          ) %>%
                           mutate(Hatchery.Returns = rowSums(across(c(Total.Hatchery.Clipped.Escapement.Corrected, Total.Hatchery.Unclipped.Escapement.Corrected)))) %>%
                           mutate(Prefishery.Hatchery = rowSums(across(c(prefishery.return.hatchery.clipped, prefishery.return.hatchery.unclipped)))) %>%
                           select(-c(Total.Hatchery.Clipped.Escapement.Corrected, Total.Hatchery.Unclipped.Escapement.Corrected,
                                     prefishery.return.hatchery.clipped, prefishery.return.hatchery.unclipped)) %>%
                           relocate(Hatchery.Returns, .after=Natural.Returns) %>%
                           relocate(Prefishery.Hatchery, .after=Prefishery.Natural)


rem.perc <-  function(var,symbol){ as.numeric(gsub(symbol, '', var, fixed=TRUE))}
rep.blanks <- function(var,notation){ replace(var, var == notation, NA) }
rep.zero <- function(var,notation){ replace(var, var == notation, 0) }

# run.variables <- function(data,var){
#   print(var)
#   print(sym(var))
#   data %>%
#     mutate( new = rep.blanks(!!sym(var)," -   ")) %>% 
#     mutate(new = (rep.blanks(new,""))) %>%
#     mutate(new = rem.perc(new,',')) %>%
#     select(-!!sym(var))
# }

run.variables <- function(var){
   new = rep.blanks(var," -   ")
   new = (rep.blanks(new,""))
   new = rem.perc(new,',') 
   return(new)
}

# Dec 2021 added step to replace "-  " with "0" where Final Estimate Type = 'NO' since these are true 0's
sub.zeros <- Coho.sub %>% filter(Final.Estimate.Type == "NO") %>%
                          mutate(Final.Spawner.Estimate = as.numeric(rep.zero(Final.Spawner.Estimate," -   "))) %>%
                          mutate(Total.Return = as.numeric(rep.zero(Total.Return," -   "))) %>%
                          mutate(Total.Pre.Fishery.Abundance  = as.numeric(rep.zero(Total.Pre.Fishery.Abundance ," -   "))) 

COHO.clean <- Coho.sub %>% 
                         filter(Final.Estimate.Type != "NO") %>% 
                         mutate(Final.Spawner.Estimate = run.variables(Final.Spawner.Estimate)) %>%
                         mutate(Total.Return = run.variables(Total.Return)) %>%
                         mutate(Total.Pre.Fishery.Abundance = run.variables(Total.Pre.Fishery.Abundance)) %>%
                         rbind(sub.zeros) %>%
                         mutate(Natural.Returns = as.numeric(Natural.Returns)) %>%
                         mutate(Hatchery.Returns = as.numeric(Hatchery.Returns)) %>%
                        # mutate(ER.Unclipped= rem.perc(var=ER.Unclipped,'%')) %>%  mutate(ER.Clipped= rem.perc(var=ER.Clipped,'%')) %>%
                        # mutate(Final.Fry.Survival = rem.perc(var=Final.Fry.Survival,'%')) %>% 
                        # mutate(Final.Smolt.Survival = rem.perc(var=Final.Smolt.Survival,'%'))  %>%
                         mutate(WSP.2014.Data.Use.Categories = ifelse(WSP.2014.Data.Use.Categories == "not used", 0,1)) %>% # Reset data use categories to 1's and 0's v   ***** MAY need to change this once I talk to Lynda!!
                         mutate(Estimate.Classification = factor(gsub(pattern=":.*",replacement="",x=Coho.sub$Estimate.Classification), 
                                                                 ordered=TRUE, 
                                                                 levels = c("Type-1", "Type-1B", "Type-1C", "Type-2", "Type-3", "Type-4", 
                                                                            "Type-5", "Type-6", "Type-7", "UNK", ""))) %>%
                         mutate(Estimate.Classification = fct_recode(Estimate.Classification, "NA" = "")) %>%
                         mutate(Data.Quality.Class = factor(gsub("(\\d).*", x=Coho.sub$Data.Quality.Class, replacement = "\\1"), ordered=TRUE,
                                                            levels=c("1","2","3","4","5","6","7","8","9",""))) %>%
                         mutate(Data.Quality.Class = fct_recode(Data.Quality.Class, "NA" = ""))
                       

                 

# Data exploration - Trib/Year combos where there is no info on Estimate Classification but the data was used for WSP
no.quality.est <- COHO.clean %>% 
                          select(Tributary.Deme, Return.Year, Final.Spawner.Estimate, Total.Return, Total.Pre.Fishery.Abundance,WSP.2014.Data.Use.Categories ,Estimate.Classification, Data.Quality.Class) %>% 
                          filter(Estimate.Classification == "NA" | Estimate.Classification == "UNK"| Data.Quality.Class == "NA") %>%               
                          filter(Final.Spawner.Estimate != "NA" |  Total.Return != "NA" | Total.Pre.Fishery.Abundance != "NA") %>%
                          filter(WSP.2014.Data.Use.Categories == 1)

was.used <- COHO.clean %>% 
                          select(Tributary.Deme, Return.Year, Final.Spawner.Estimate, Total.Return, Total.Pre.Fishery.Abundance,WSP.2014.Data.Use.Categories ,Estimate.Classification, Data.Quality.Class) %>% 
                          #  filter(Estimate.Classification == "NA" | Estimate.Classification == "UNK"| Data.Quality.Class == "NA") %>%               
                          filter(Final.Spawner.Estimate != "NA" |  Total.Return != "NA" | Total.Pre.Fishery.Abundance != "NA") %>%
                          filter(WSP.2014.Data.Use.Categories == 1)

was.not.used <- COHO.clean %>% 
                          select(Tributary.Deme, Return.Year, Final.Spawner.Estimate, Total.Return, Total.Pre.Fishery.Abundance,WSP.2014.Data.Use.Categories ,Estimate.Classification, Data.Quality.Class) %>% 
                          filter(Final.Spawner.Estimate != "NA" |  Total.Return != "NA" | Total.Pre.Fishery.Abundance != "NA") %>%                        
                          filter(WSP.2014.Data.Use.Categories == 0)


print(table(COHO.clean$Estimate.Classification, COHO.clean$Data.Quality.Class))


print("Data used in the WSP assessment")
print( table(was.used$Estimate.Classification,was.used$Data.Quality.Class))

print("Data filtered out of the WSP assessment")
print(table(was.not.used$Estimate.Classification))
print(table(was.not.used$Data.Quality.Class))

## ============================================================================================================================================================ ##

## ============================================================= FUNCTIONS ==================================================================================== ##

# By CU
# Pulls the Tributary Demes for a CU, filters out data NOT used in the WSP 2014 analysis (i.e. data that is categorized as "not used" in this column is given value of 0, 
# while 'yellow' and 'green' categories given value of 1 and this is multiplied by the estimate values). Then sums across demes

# If infill.start.yr == FALSE it is set to start.yr so infills the entire thing
# Must output one number - just the infilled value
Run_Infill <- function(df, exceptions,infill.start.yr,start.yr){
                   if(infill.start.yr == FALSE) infill.start.yr <- start.yr
                   df1 <- df %>% select(- Return.Year, - total)
                   
                   # Find index of start year and streams not being infilled
                   idx.start.yr <- which(df$Return.Year==infill.start.yr)   # find out uding df not df1 becasue remove this column
                   idx.exceptions <- which(names(df1) %in% as.vector(exceptions$V1)) # use df1 because row #'s
                   
                   # Start Infill process
                   col.means<-t(data.frame(colMeans(df1,na.rm=TRUE)))
                   col.means[,is.nan(col.means) == TRUE] <- NA
                   col.prop<-(col.means)/rowSums(col.means, na.rm=TRUE)
                   # Create dataframe of prop values and fill with NA where data is missing
                   data.prop<-matrix(rep(col.prop, each=dim(df1)[1]),
                                     nrow=dim(df1)[1], ncol=dim(df1)[2]
                                    )
                   data.prop.with.na<-data.prop 
                   # This code fills in ONLY years after the infilling start year with NA's! 
                   # Also excludes the columns that are EXCEPTIONS
                   logical.mat <- matrix(FALSE, nrow=dim(df1)[1], ncol=dim(df1)[2])
                   logical.mat[idx.start.yr:dim(df1)[1],] <- is.na(df1)[idx.start.yr:dim(df1)[1],]
                   logical.mat[,idx.exceptions] <- FALSE
                   
                   # Fill missing data with NAs
                   data.prop.with.na[logical.mat]<-NA
                   Factor <- 1/rowSums(data.prop.with.na, na.rm=T)
                   agg.interp<-rowSums(df1,na.rm=TRUE)*Factor
                   
                   
                   # Fill in the matrix with stream values infilled
                   agg.interp.mat <- matrix(rep(agg.interp, each=dim(df1)[2]),nrow=dim(df1)[1],ncol=dim(df1)[2],byrow=T)
                   data.interp<-df1
                   data.interp[is.na(data.prop.with.na)] <- data.prop[is.na(data.prop.with.na)] * agg.interp.mat[is.na(data.prop.with.na)] 
                   
                   out.data <- cbind(Return.Year=df$Return.Year, data.interp,total=agg.interp)
                   
                   return(out.data)
} # end Run_Infill

                  
# Enter in max data categories for the Estimate Classification and Data Quality categories to filter data based on Quality
# Also must now select whether or not to filter out infilled estimates (infilled by StAD - Classification Type-7)
# AND whether or not to infill missing values and what year to start this at. Default to FALSE since only Spawners should be infilled. 
# Also for CO will start at 2014 since prior to this has been filled already (old notes from early versions whe we still used the infilling here)
CUsums <- function(data,CU,variable, EC.max, start.yr=1998, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=start.yr, WSP_only=TRUE){
                    if(wsp.infill == FALSE) data <- filter(data, Estimate.Classification != "Type-7")
                    if(run.infill == TRUE & variable != "Final.Spawner.Estimate"){
                                                                                print("Infilling non-spawner data!")
                                                                                stop()
                    }
                    df <- data %>% select(Conservation.Unit, Return.Year, Tributary.Deme,WSP.2014.Data.Use.Categories, Estimate.Classification, variable) %>%
                                   filter(Conservation.Unit==CU) %>%
                                   filter(Return.Year >= start.yr)%>%
                                   filter(Estimate.Classification <= EC.max | Estimate.Classification == "Type-7" ) 
                  
                    if(WSP_only == TRUE){
                      df2 <- df %>% mutate(Estimate = case_when(WSP.2014.Data.Use.Categories==0 ~ NA_real_, WSP.2014.Data.Use.Categories==1~!!sym(variable)) ) %>%
                                    filter(WSP.2014.Data.Use.Categories == 1) %>%
                                    select(-Conservation.Unit, -Estimate.Classification, -WSP.2014.Data.Use.Categories, -variable) %>%
                                    spread(Tributary.Deme, Estimate) %>%
                                    mutate(total = rowSums(select(.,-Return.Year), na.rm=TRUE))
                    }
                    # if(WSP_only == TRUE){
                    #     df2 <- df %>%
                    #                   mutate(total = rowSums(select(.,-Return.Year), na.rm=TRUE))
                    # }
                  
                    if(WSP_only == FALSE){
                        df2 <- df %>%  mutate(Estimate = !!sym(variable))  %>%
                                       #mutate(Estimate = WSP.2014.Data.Use.Categories * !!sym(variable)) %>%
                                       # when there is an actual estimate on the variable column it gives a 0 instead of an NA because 0*#=0, so must replace with NA  ***** I AM HERE
                                       #mutate(Estimate = case_when(WSP.2014.Data.Use.Categories==0 ~ NA_real_, WSP.2014.Data.Use.Categories==1~!!sym(variable)) ) %>%
                                       select(-Conservation.Unit, -Estimate.Classification, -WSP.2014.Data.Use.Categories, -variable) %>%
                                       spread(Tributary.Deme, Estimate) %>%
                                       mutate(total = rowSums(select(.,-Return.Year), na.rm=TRUE))
                    }
                    
                    if(run.infill==TRUE){ df3 <- Run_Infill(df2,exceptions,infill.start.yr,start.yr)} else{df3 <- df2}
                    return(df3)  
}

# Get estimates by CU
CUsums(COHO.clean, "Fraser Canyon", "Final.Spawner.Estimate",EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=TRUE, infill.start.yr=FALSE,WSP_only=TRUE)





# Calculate Enhanced Contribution using the PRE FISHERY ABUNDANCE as is used in WSP
# EC = Hatchery.Returns.corrected/Total.Returns

# METHOD:
# 1) calculate EC using the RETURNS Hatchery/Total (Returns are the Final Spawner Estimate + SEP Removals and AFC Removals)
# 2) compare the calculated natural RETURNS to the one in the source file ($Natural.Returns)
# 3) Use the EC to calculate the natural pre-fishery abundance
# 4) Compare natural prefishery abundance to the WSP document tables

# Step 1: Calc EC using the RETURNS
Calc.EC <- function(Hat.data, Total.data){
  EC = Hat.data/Total.data
  return(EC) 
}

# Calls CU.sums to get each variable within it and spits out the things that can be compared to the WSP document
# Also has a check comparing the calculated natural returns to the one in the file
# Add in a filter for estimate class

Calc.Nat <- function(data=COHO.clean, CU="Middle Fraser", EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=2014, WSP_only=TRUE){

  Ret.Hat = CUsums(data, CU, "Hatchery.Returns", EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)
  Ret.Total = CUsums(data, CU, "Total.Return",EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)
  PF.Total = CUsums(data, CU, "Total.Pre.Fishery.Abundance",EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)
  Spawn.Total =  CUsums(data, CU, "Final.Spawner.Estimate",EC.max, start.yr, wsp.infill, run.infill, infill.start.yr, WSP_only)
  
  EC <- Ret.Hat/Ret.Total
  
  # Compare to the natural returns estimate in the file which should map on exactly
  Ret.Nat <- Ret.Total * (1-EC)
  Comparison <- Ret.Nat$total - CUsums(data, CU, "Natural.Returns", EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)$total
  if(length(which(Comparison < -1.1 | Comparison > 1.1)) > 0) paste("Calculated natural returns does not match the column in the input file") # Added April 2023

  # should we just use the provided natural and hatchery pre-fishery components here now!!? YES (M. Arbieder May 5 2023)  
  PF.Nat <- CUsums(data, CU, "Prefishery.Natural", EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)
  PF.Hat <- CUsums(data, CU,  "Prefishery.Hatchery", EC.max, start.yr, wsp.infill, run.infill=FALSE, infill.start.yr, WSP_only)

  Spawn.Nat <- Spawn.Total * (1-EC)
  Spawn.Hat <- Spawn.Total * EC
  
  out.data <-  data.frame(cbind(Year = PF.Total$Return.Year,
                                Check.Natural.Returns=round(Comparison,0),  
                                Total_Spawners=round(Spawn.Total$total,0),
                                Natural_Origin_Spawners=round(Spawn.Nat$total,0), 
                                Hatchery_Origin_Spawners=round(Spawn.Hat$total,0), 
                                Ret.Nat = Ret.Nat$total,
                                Ret.Hat=Ret.Hat$total,
                                Ret.Total=Ret.Total$total,
                                PF.Nat=round(PF.Nat$total,0), 
                                PF.Hat=round(PF.Hat$total,0),
                                PF.Total=round(PF.Total$total,0)
                                )
              )
  write.csv(out.data, paste("DATA_IN/SOURCES/Fraser Coho/Calc.Nat Table for", CU,".csv"))
  return(out.data)
}

Calc.Nat(data=COHO.clean, CU="Middle Fraser", EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=FALSE, WSP_only=TRUE)

BY.Table <- function(data, CU, EC.max="NA", start.yr, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=FALSE, WSP_only=TRUE){
              start.data <- Calc.Nat(data=COHO.clean, CU, EC.max, start.yr, wsp.infill,run.infill, infill.start.yr, WSP_only)
              first <- start.data$Year[min(which(is.finite(start.data$Check.Natural.Returns)==TRUE))]
              
              
              # Age Proportions of Pre-Fishery Abundance
              cu.age.prop <- CUs$Base.Unit.Prop.AvgAge[CUs$Base.Unit.CU.ShortName==CU]
              age <- CUs$BaseUnit.AvgAge[CUs$Base.Unit.CU.ShortName==CU]
              altage <- CUs$BaseUnit.AltAge[CUs$Base.Unit.CU.ShortName==CU]
              age.colname <- c(paste0("PF.Nat.Age",age)); altage.colname <- c(paste0("PF.Nat.Age",altage))
         
              
              sub <- start.data %>% rename(Brood_Year=Year) %>%
                             select(-Check.Natural.Returns, -Ret.Nat, -Ret.Hat,-Ret.Total) %>%
                             mutate(CU_ID=case_when(CU=="Middle Fraser" ~ "CO_01", CU=="Fraser Canyon" ~ "CO_02", CU=="Lower Thompson" ~ "CO_03", CU=="North Thompson" ~ "CO_04", CU=="South Thompson" ~ "CO_05") ) %>%
                             mutate(CU=CU) %>%
                             select(CU_ID, CU, Brood_Year, Total_Spawners, Natural_Origin_Spawners, Hatchery_Origin_Spawners, PF.Nat, PF.Hat, PF.Total) %>%
                             mutate( !!c(paste0("Rec.Nat.Age",age)) := lead(round(PF.Nat*cu.age.prop,0), age) ) %>%
                             mutate( !!c(paste0("Rec.Nat.Age",altage)) := lead(round(PF.Nat*(1-cu.age.prop),0), altage) ) %>%
                             mutate( !!c(paste0("Rec.Hat.Age",age)) := lead(round(PF.Hat*cu.age.prop,0),age) ) %>%
                             mutate( !!c(paste0("Rec.Hat.Age",altage)) := lead(round(PF.Hat*(1-cu.age.prop),0),altage)) %>%
                             select(-PF.Nat, - PF.Hat, -PF.Total) %>%
                             mutate(Rec.Nat.Total = Rec.Nat.Age3 + Rec.Nat.Age4) %>%
                             mutate(Rec.Hat.Total = Rec.Hat.Age3 + Rec.Hat.Age4) %>%
                             mutate(Rec.3.Total = Rec.Nat.Age3 + Rec.Hat.Age3 ) %>%
                             mutate(Rec.4.Total = Rec.Nat.Age4 + Rec.Hat.Age4 ) %>%
                             mutate(Total.Rec = Rec.3.Total + Rec.4.Total) %>%
                             filter(Brood_Year >= first)
              #end.data <- sub[-(1: (first-1)),]   
              return(sub)
}

#BY.Table(data=COHO.clean, CU="Middle Fraser", EC.max="NA", start.yr=1998, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=FALSE)


## ======================================================= RUN FOR IFC CUs ========================================================== ##

CU.list <- c("Fraser Canyon", "Lower Thompson", "Middle Fraser", "North Thompson", "South Thompson")
#CU.list <- c("Fraser Canyon", "Lower Thompson", "Middle Fraser", "South Thompson")

Run_write <- function(EC.max="NA", st.yr=1998,wsp.infill=TRUE,run.infill=FALSE, infill.start.yr=FALSE, WSP_only=TRUE){
                 
                  for(CU in CU.list){
                    start.yr <- st.yr
                    if(CU == "Fraser Canyon") { 
                                                all.cus <- data.frame(matrix(NA, nrow=1,ncol=15))
                                                start.yr <- 1998  
                    }
                    by.tbl <- BY.Table(data=COHO.clean, CU, EC.max, start.yr, wsp.infill, run.infill, infill.start.yr, WSP_only)
                    if(CU == "Fraser Canyon"){  
                                              colnames(all.cus) <-colnames(by.tbl)
                    }
                    # write.csv(by.tbl, c(paste(CU, "EC.max=",EC.max, "infill=", run.infill,".csv")))
                    all.cus <- rbind(all.cus, by.tbl)
                             
                  }
  
                  # Added Nov 2023 - write csv of TribDemes used for the CU timeseries data
                 # if(WSP_only=TRUE) write.csv(COHO.clean %>% filter(WSP.2014.Data.Use.Categories==1) %>% select(Conservation.Unit, Tributary.Deme) %>%unique() %>% arrange(Conservation.Unit),"DATA_TRACKING/IFC.CU.Tribs.csv")
                  return( all.cus)
}

All_CU_BYtable <- Run_write(EC.max="NA", st.yr=1901,wsp.infill=TRUE,run.infill=FALSE, infill.start.yr=2018, WSP_only=TRUE)
write.csv(All_CU_BYtable[-1,], "DATA_IN/All IFC CUs BY Table_EC.max=NA_infill=FALSE.csv")



# Population level data for flat file

for(CU in CU.list){
    
    temp <- CUsums(COHO.clean, CU, "Final.Spawner.Estimate",EC.max="NA", start.yr=1908, wsp.infill=TRUE, run.infill=FALSE, infill.start.yr=2018, WSP_only=FALSE)
    tmp <- temp %>%
              select(-total) %>%
              select(-any_of(c("infill","Infill")))%>%        # replace one_of with any_of as previous threw error when columns do not exist and has been superceeded  
              pivot_longer(-Return.Year, names_to = "TribDeme", values_to = "Estimate") %>%
              mutate(CU_name = CU) %>%
              select(CU_name, Return.Year, TribDeme, Estimate)
    if(CU=="Fraser Canyon") new <- tmp
    else new <- rbind(new,tmp)
}
# if WSP_only = FALSE
write.csv(new, "DATA_IN/ALL IFC streams spawners_EC.new=NA, infill=FALSE_ALL STREAMS.csv")

# if WSP_only = TRUE
#write.csv(new, "DATA_IN/ALL IFC streams spawners_EC.new=NA, infill=FALSE_WSP ONLY.csv")

# # =================== APRIL 7 2021 - Update the Pop_Lookup WSP coloumn to define which streams were and were not included in the CU dataset
# 
# included_streams <- COHO.clean %>% filter(WSP.2014.Data.Use.Categories == 1) %>% select(Tributary.Deme) %>% distinct()
# 
#   Pop_Lookup <- read.csv("DATA_LOOKUP_FILES/PopLookup.csv")
#   
#    Pop_Lookup.coho <- Pop_Lookup %>% filter(TimeSeriesData_Species=="Co_Fraser") %>%
#                                      mutate(WSP_ts = case_when(MapData_Pop_Name == "FRASER RIVER AND TRIBUTARIES" ~ "yes", TRUE ~ "no"))
#   
#   Pop_Lookup <- Pop_Lookup %>% filter(!TimeSeriesData_Species=="Co_Fraser") %>%
#                                rbind(Pop_Lookup.coho)
#   
#   write.csv(Pop_Lookup, "DATA_LOOKUP_FILES/PopLookup.csv")

