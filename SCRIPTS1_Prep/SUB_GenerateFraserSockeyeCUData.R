# This code by Bronwyn MacDonald is sourced within the script Prep4_CleanedFlatFile_Fraser_Sk.R
# This reads in the functions needed to generate the CU level data
# Gottfried Pestal added user-specified file paths, and moved the inputs into Prep4_CleanedFlatFile_Fraser_Sk.R


# May 5 2020
# Updating for the SOS-SSET tool. 
# Updated the CU_Streams crosswalk file to use the Gazetted name, as some names in the previous version did not match the more recent Stad Escapement
# data files anymore. Name aliases were found in the old DFO maps primarily. 
# Notes:
#   Pitt - at some point ~2011 we started including all streams in the Pitt system instead of just upper Pitt, due to the change in methods that occurred 
# ~ 2000 from M-R to visual. Therefore this varies from the documentation in the 2011 WSP document, which says it just includes the upper Pitt river

# May 9, 2013

# SK Stock Status Data Management Code
# Reads in the Sockeye Escapement Data, sorts into Cu's, Calculates EFS or ETS depending on what is input into the arguments, 
# Gap Fills EFS based on the method used for each CU in the 2011 Stock Status paper 
# Outputs data to be read into analysis code for Stock Status Analysis 

# Updates:
# Sept 2013 - BLM updated code to read in separate csv files with CU streams and gap filling years so they can be more easily
# changed as CU's evolve
# Dec 2017 - Updatd to re-format data to longform format 


# INPUTS:

# CU's:
#   The streams in each CU are input into the file "CU_Streams.csv", under the CU name header
#   For some CU's that require gap filling using the mean proportion method (Takla_Trembleur_EStu, Quesnel_S, Shuswap_L), 
#   CUs are divided into sub-groups, and the CU name header contains a list of the subgroups.

# Escapement Data (OLD NOTES NO LONGER VALID NOW THAT CU NAMES ARE INCLUDED IN THE STAD FILES):
#   When new data becomes available, data must be input into the file b Final Stock Status Files > d 2012 escapement updates > Data > 
#   Excel versions > 'All SK Data.xls', making sure that the stream names align with those that are currently in the file. Pay particular 
#   attention to the early summer shuswap streams, many of which also have escapements  during the late run, therefore the early component 
#   has  '-Early' attached to it. [Make sure to add "-Early" to Scotch, McNomee and Seymour & add "ES" to Kazchek Creek in Early Stuart] 
#   This file is  then sorted by stream and saved as ' b Final Stock Status Files > d 2012 escapement updates > 
#   Data > a final csv for code > 'All SK Data - by Stream.csv', which is read into the code. 

# Gap Filling
#   The file "Expansion Years.csv" contains the years that are gap-filled for each CU, CU sub-group, or stream, depending on how complicateed the gap-filling procedure for that CU is. 
#   These years are excluded when calculating the averages, and are then gap-filled. 
#   For Quesnel, different years are gap-filled (Quesnel_Fill & McKin_Mitch_Fill) than are excluded (Quesnel_Except). 
#   Similar to the CU file, the subgroups/streams contained in each CU are listed under the CU_Name for those with more complicated methods. 


# TO RUN:

# Enter GetCUData(CU_Name, Update, Data)
# CU_Name appears in quotations
# Update refers to the last year of data
# Data refers to whether to pull out EFS or ETS data (however, ETS will not be the same as the ETS used for stock status as this file does not pull in ALL streams.
# Future updates Will have to re-write CU's to have all streams included, and it is easier to just do this in Excel)




# ---------------------------------------- Read in Data Files ---------------------------------------------------

# Moved this into Prep4_CleanedFlatFile_Fraser_Sk.R

#CU_Streams <- read.csv("CU_Streams.csv", header = TRUE)               # Defines streams in each CU
#Expansion_Years <- read.csv("Expansion Years.csv", header = TRUE)     # Defines years being gap-filled in applicable CUs
#SK_Data <- read.csv("All SK Data - by Stream_2017.csv", header = T)        # Contains all escapement data

# CUs with mean proportion method gap-filling
#GapCUs <- c("Nahatlatch_ES", "Quesnel_S", "Shuswap_L", "Takla_Trem_EStu", "Takla_Trem_S_S")


# --------------------------------------Gap Filling Function for Nahatlatch_ES and Takla_Trem_S_S -----------------------------------------------
 
  GapFill <- function(CU, CU_Data, CU_Name, Exceptions){     
    
      # Calculate proportions using only the cells that are being included
      Non_Except <- CU_Data[ is.na(match(CU_Data$Year, Exceptions)),CU]
      Data_Incl  <- Non_Except[!is.na(rowSums(Non_Except)),] 
      Averages <- colMeans(Data_Incl, na.rm=T)
      Proportions <- Averages/sum(Averages)
       
      Expansion <- matrix(NA, nrow = length(CU_Data[,1]), ncol = length(CU) )
      
      for(j in 1:length(Expansion[,1])){
        for(k in 2:(length(CU)+1)){
          if(is.na(CU_Data[j,k]))                        Expansion[j,(k-1)] <- 0
          else                                           Expansion[j,(k-1)] <- Proportions[k-1]   
        }# end k loop  
      }# end j loop
    
      Expan_Frac <- 1/(rowSums(Expansion)) 
      Orig_StreamSum <- rowSums(CU_Data[,-1], na.rm=TRUE)

      New_Sum <- as.data.frame(cbind(Year = CU_Data[,1], EFS = (Orig_StreamSum * Expan_Frac)))
      
      New_Streams <- CU_Data
      for(i in 2:ncol(New_Streams)){
        for(j in 1:nrow(New_Streams)){
          if(Expansion[j,(i-1)] ==0){  New_Streams[j,i] <- New_Sum$EFS[j]*Proportions[(i-1)][[1]] } 
        }
      }  

      if(CU_Name == "Nahatlatch_ES") New_Sum$EFS[New_Sum$Year == 1979] <- Orig_StreamSum[which(New_Sum$Year == 1979)]
  write.csv(New_Streams, c(paste0("DATA_PROCESSING/FraserSockeyePrep/", CU_Name, "_Infilled_Streams.csv")))
      return(New_Sum)
  } # End regular GapFill Function



# -----------Gap Filling Function for Quesnel -------------------------------------------------------------------------------
# (separate proportions for cycles lines, pre/post 1980 and only fills in certain 0's)
  
  GapFill_Quesnel <- function(CU_Data, CU_Name, Exceptions){
     
    CU_Q <- CU_Streams[colnames(CU_Streams) == CU_Name]
    CU_Q <- CU_Streams[,!is.na(match(colnames(CU_Streams), levels(as.factor(CU_Q[,1]))))]
    
    All_Q_Data <- as.data.frame(cbind(Year = CU_Data[,1],  # Added 2023 so experts can review the systems that feed into the Quesnel sum for potential infilling
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Horse))))], 
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_McKin))))],
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Mitchell))))],
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Little))))], 
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Lake))))])) 
    CU_DataQ <- as.data.frame(cbind(Year = CU_Data[,1],
                                      cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                      Horse = rowSums( CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Horse))))], na.rm=TRUE ), 
                                      McKin = rowSums( CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_McKin))))], na.rm=TRUE ),
                                      Mitchell = rowSums( CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Mitchell))))], na.rm=TRUE ),
                                      Little = CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Little))))], 
                                      Lake  = rowSums( CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_Q$Quesnel_S_Lake))))], na.rm=TRUE ) ))
      
      
      # Pre and post 1980 averages on the cycle line
      # Note: In the Averages and Props dataframes cyc 3 represents the pre-1980 off-cycle 
      # 
      Averages <- as.data.frame(matrix(NA, nrow=4, ncol=5))
      colnames(Averages) <- colnames(CU_DataQ[2:6])
      CU_DataQ_Keep <- CU_DataQ[is.na(match(CU_DataQ$Year,Exceptions$Quesnel_Except)),]
    
      # Off cycle years pre-1980
      Averages[3,] <- c(3, colMeans(CU_DataQ_Keep[CU_DataQ_Keep$Year < 1980 & (CU_DataQ_Keep$cyc==3 | CU_DataQ_Keep$cyc==2),], na.rm=T)[3:6])
      # Post-1980 cycles
      Averages[4,] <- c(4, colMeans(CU_DataQ_Keep[CU_DataQ_Keep$cyc==4 & CU_DataQ_Keep$Year>1980,], na.rm=TRUE)[3:6])
      Averages[1,] <- c(1, colMeans(CU_DataQ_Keep[CU_DataQ_Keep$cyc==1 & CU_DataQ_Keep$Year>1980,],na.rm=TRUE)[3:6])
      Averages[2,] <- c(2, colMeans(CU_DataQ_Keep[CU_DataQ_Keep$Year > 1980 & (CU_DataQ_Keep$cyc==2 | CU_DataQ_Keep$cyc==3),], na.rm=TRUE)[3:6])
      
      # Proportions for each cycle/pre and post 1980
      Props <- as.data.frame(matrix(NA, ncol=5, nrow=4))  
      colnames(Props) <- colnames(Averages)
      Props[,1] <- seq(1,4,1)     
      # Do off-cycle calculation separately
      for(k in 1:4){
        Props[Props$cyc==k,-1] <- Averages[k,2:5]/sum(Averages[k,2:5])
      }
    
      # Fill in Expansions
      Fill_Yrs <- Exceptions$Quesnel_Fill
      Fill_cyc <- CU_DataQ$cyc[!is.na(match(CU_DataQ$Year,Exceptions$Quesnel_Fill))]      
      Fill_cyc[5] <- 2
      Expansion <- matrix(0, nrow = length(CU_DataQ[,1]), ncol = 4 )      
      
      for (i in Fill_Yrs){
        Col_Index <- which(CU_DataQ[CU_DataQ$Year==i,3:6] != 0 )
        Expansion[!is.na(match(CU_DataQ$Year,i)),Col_Index] <- as.numeric(Props[Fill_cyc[!is.na(match(Fill_Yrs,i)) ], Col_Index+1])        
        #Expansion[which(!is.na(match(CU_Data2$Year,i))),1:4] <- as.numeric(Props[Props$cyc== CU_Data2$cyc[CU_Data2$Year==i],-1])        
      }
      Expand_Frac <- 1/rowSums(Expansion) 
      Expand_Frac[Expand_Frac == Inf] <- 1
      
      # Do McKinley/Mitchell
      Ave_MM <- colMeans(CU_DataQ[CU_DataQ$cyc==4 & CU_DataQ$Year>1980 & (is.na(match(CU_DataQ$Year,Exceptions$McKin_Mitch_Fill))),], na.rm=TRUE)[4:5]
      Props_MM <-Ave_MM/sum(Ave_MM)
      Expand_MM <- 1/(Props_MM[1]) 
      
      # Expand
      Orig_Sum <- rowSums(CU_DataQ[,-c(1,2,7)], na.rm=T)
      New_Sum <- (Orig_Sum*Expand_Frac) + CU_DataQ$Lake     # add the Lake sums
      # Over-write 2005 with the Expanded Mitchell value
      New_Sum[!is.na(match(CU_DataQ$Year,2005))] <- Orig_Sum[!is.na(match(CU_DataQ$Year,2005))] + (Expand_MM*CU_DataQ[CU_DataQ$Year==2005,4]) +
                                                    CU_DataQ[CU_DataQ$Year == 2005,7]
      New_Data <- cbind(Year = CU_DataQ[,1], EFS=New_Sum)
 
    # Can use Fill_Yrs to pull out Stream Data
    colnames(Expansion) <- colnames(Props[-1])
    New_Streams <- CU_DataQ
    New_Data <- as.data.frame(New_Data)
    idx_Fill <- na.omit(match(Fill_Yrs, New_Streams$Year))
    for(i in 3:(ncol(New_Streams)-1)){
      for(j in idx_Fill){
        if(Expansion[j,(i-2)] ==0){  New_Streams[j,i] <- (New_Data$EFS[j])* Props[Props$cyc==Fill_cyc[which(idx_Fill==j)],(i-1)] } 
        #Fix the Mitchell estimate for 2005
        if((j==56) & (Expansion[j,(i-2)] ==0)){  New_Streams[j,i] <- (Expand_MM*CU_DataQ[CU_DataQ$Year==2005,4]) }
      }
    }  
    write.csv(All_Q_Data, "DATA_PROCESSING/FraserSockeyePrep/Quesnel_All_Streams.csv")
    write.csv(New_Streams, "DATA_PROCESSING/FraserSockeyePrep/Quesnel_Infilled_Streams.csv")
      return(New_Data)

  }# End GapFill Quesnel
  


# ------------------------ Late Shuswap Gap Filling Function -----------------------------------------------

  GapFill_LShu <- function(CU_Data, CU_Name, Exceptions){        
    CU_SL <- CU_Streams[colnames(CU_Streams) == CU_Name]
    CU_SL <- CU_Streams[,!is.na(match(colnames(CU_Streams), levels(as.factor(CU_SL[,1]))))]
    
    CU_Data_SL <- as.data.frame(cbind(Year = CU_Data[,1],
                                      cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                      CU_Data[ ,!is.na(match(colnames(CU_Data), levels(as.factor(CU_SL$Shuswap_L_Stocks))))]))
    #Take out years not included in averages
    Non_Except <- CU_Data_SL[is.na(match(CU_Data_SL$Year, Exceptions)),]
    
    # Calc averages and proportions
    Dom_Average <- colMeans(Non_Except[Non_Except$cyc==1,])
    Dom_Props <- Dom_Average[3:12]/sum(Dom_Average[3:12])
    SubDom_Average <- colMeans(Non_Except[Non_Except$cyc==2,])
    SubDom_Props <- SubDom_Average[3:12]/sum(SubDom_Average[3:12])
    
    Expansion <- matrix(0, nrow=nrow(CU_Data_SL), ncol = ncol(CU_Data_SL))
    # Remove NAs in Exceptions
    Exceptions <- Exceptions[-(which(is.na(Exceptions)))]
    
    # Write fxn to test whether number is even
    is.even <- function(x){i %% 2 == 0} 
    
    for (i in Exceptions){
      Col_Index <- which(CU_Data_SL[CU_Data_SL$Year==i,] != 0 )
      # For some stocks the 0's aren't gap filled in some years
      if( is.na(sum(match(Col_Index,8))) == TRUE) Col_Index = c(Col_Index,8)  # Momich is always included
      if( i == 1975 ) Col_Index <- c(Col_Index, 4)
      if( i == 1983 ) Col_Index <- c(Col_Index, 6, 12)
      Col_Index <- Col_Index[order(Col_Index)]
      if(is.even(i) == TRUE)  Props = c(0,0, Dom_Props)
      if(is.even(i) == FALSE) Props = c(0,0, SubDom_Props)
      Expansion[(which(!is.na(match(CU_Data_SL$Year,i)))),Col_Index] <- as.numeric(Props[Col_Index])       
    }
   
    Expand_Frac <- 1/rowSums(Expansion) 
    Expand_Frac[Expand_Frac == Inf] <- 1
      
    # Find Original River Sum
    Orig_Sum <- rowSums(CU_Data_SL[,-c(1,2)], na.rm=T)
    
    # Find Lake sum
    Lake  <- rowSums(CU_Data[ ,!is.na(match(colnames(CU_Data), levels(CU_SL$Shuswap_L_Lake)))], na.rm=TRUE )
    
    # Expand Rivers and Add Lakes
    New_Sum <- cbind(Year = CU_Data_SL[,1], EFS = ((Orig_Sum*Expand_Frac) + Lake) )
    
    write.csv(CU_Data_SL, "DATA_PROCESSING/FraserSockeyePrep/Shuswap_L_All_streams.csv")  
    return(New_Sum)        
  } # End GapFill_LShu


# ------------------------Gap Filling function for Takla_Trembleur_ Early Stuart-----------------------------------------
  
  GapFill_Takla_EStu <- function(CU_Data, CU_Name, Exceptions){
      CU_TT <- CU_Streams[colnames(CU_Streams) == CU_Name]
      #CU_TT <- CU_Streams[,!is.na(match(colnames(CU_Streams), levels(CU_TT[,1])))]
      CU_TT <- CU_Streams[,!is.na(match(colnames(CU_Streams), (CU_TT[,1])))]
      
      TT_allstreams <- unique(unlist(CU_TT))[! unique(unlist(CU_TT)) %in% c("")] 
      TT_Alldata <- as.data.frame(cbind(Year = CU_Data[,1], CU_Data[ ,!is.na(match(colnames(CU_Data),  TT_allstreams))]))
      
      Driftwood <-   as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                     Drift = rowSums( CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_EStu_Drift))], na.rm=TRUE ),
                                     Dust = CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_Dust_Sinta[1]))],
                                     Sinta = CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_Dust_Sinta[2]))] ) )
         
      Beav <-  as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                   CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_EStu_Beav))] ))
                                  # colnames(Beav) <- c("Year","cyc","Felix","Paula","Point") # 2021 -  Felix is now Sidney
                                   colnames(Beav) <- (sub(" Creek", "", colnames(Beav)))
      
      Main <-   as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                    CU_Data[ ,!is.na(match(colnames(CU_Data), (CU_TT$Takla_Trem_EStu_Main)))] ))
                                    #colnames(Main) <- c("Year","cyc","Forfar","Gluske","Kynock", "Rossette")  # 2021 - Kynock is O'Ne-ell; Rossette is Van Decar
                                    colnames(Main) <- (sub(" Creek", "", colnames(Main)))
                                    colnames(Main)<- sub("'",".", colnames(Main))
                                    colnames(Main)<- sub("-",".", colnames(Main))
                                    colnames(Main)<- sub(" ",".", colnames(Main))
      
      Low <-   as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                    CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_EStu_Low))] ))  
                                    #colnames(Low) <- c("Year","cyc","X15Mile","X25Mile","X5Mile", "Crow", "Shale") # 2021 5 Mile is Maclaing
                                    colnames(Low) <- (sub(" Creek", "", colnames(Low)))
                                    Low <- Low %>% rename("X15Mile"="15 Mile","X25Mile"="25 Mile")
                        
      
      Upper <- as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                   CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_EStu_Upper))] ))  
                                   #colnames(Upper) <- c("Year","cyc","Ankwill", "Forsythe", "Frypan") # 2021 Forsythe is Lovell
                                   colnames(Upper) <- (sub(" Creek", "", colnames(Upper)))
      
      
      Sand <- as.data.frame(cbind(Year = CU_Data[,1],cyc= rep(seq(1,4,1), length = length(CU_Data[,1])),
                                   CU_Data[ ,!is.na(match(colnames(CU_Data), CU_TT$Takla_Trem_EStu_Sand))] ))
                                   #colnames(Sand) <- c("Year","cyc","Bivouac","Narrows", "Sakeniche", "Sandpoint")
                                   colnames(Sand) <- (sub(" Creek", "", colnames(Sand)))
                                   colnames(Sand) <- (sub(" River", "", colnames(Sand)))
      
      Systems <- c("Driftwood", "Beav", "Main", "Low", "Upper", "Sand")
      
      New_Sums <- as.data.frame(matrix(NA, nrow=length(CU_Data[,1]), ncol=length(Systems)+1))
      New_Sums[,1] <- CU_Data[,1]
      colnames(New_Sums) <- c("Year", Systems)
       
      # For loop to calc proportions and expansion factors for each grouping
      for(s in 1:6){
          System <- get(noquote(Systems[s]))
          Averages <- as.data.frame(matrix(NA, nrow=4, ncol=(ncol(System)-2)))
          colnames(Averages) <- colnames(System[3:(ncol(System))])
          Props <- as.data.frame(matrix(NA, ncol=ncol(Averages), nrow=4))  
          colnames(Props) <- colnames(Averages)
          Fill_Yrs <- Exceptions[s]
          
          # Cycle 1 is the Sub-Dominant Year
          for(i in 1:4){
              Averages[i,] <- c( colMeans(System[System$cyc==i & is.na(match(System$Year, Fill_Yrs[[1]])),],na.rm=TRUE)[-(1:2)]) 
              Props[i,] <- Averages[i,]/sum(Averages[i,])
          }
          
          # Fill in the Expansion Matrix
          Expansion <- data.frame(matrix(0, nrow=nrow(System), ncol = ncol(System)))
          Expansion[,1] <- System[,1];  Expansion[,2] <- System[,2]
          colnames(Expansion) <- colnames(System)         
          for(f in Fill_Yrs[[1]]){
              Col_Index <- which(System[System$Year==f,-(1:2)] != 0 )
              Expansion[(Expansion[,1]==f),Col_Index+2]   <-   as.numeric(Props[System$cyc[System$Year==f], Col_Index])
          } # End Fill_Yrs
                    
          # Correct Expansion matrix for year/stream combos that don't need to be filled          
          Exception_Streams <- Exceptions[,match(colnames(System)[-(1:2)], colnames(Exceptions))]
          #Exception_Streams <- Exceptions[,match(colnames(System)[-(1:2)], Exceptions)]
          
          for(i in 1:ncol(Exception_Streams)){
              if(length(na.omit(Exception_Streams[[i]])) == 0) next
              Expansion[ match(na.omit(Exception_Streams[[i]]),Expansion[,1]), (2+i)] <- Props[Expansion$cyc[match(na.omit(Exception_Streams[[i]]),Expansion[,1])], i]   
          } # End Exception_Streams
          
          if(s == 1){ 
            Expansion$Drift[!is.na(match(Expansion$Year, Exceptions$Drift))] <- 0
          }   
      
          Expand_Frac <- 1/rowSums(Expansion[,-(1:2)]) 
          Expand_Frac[Expand_Frac == Inf] <- 1  
        
         # Find Original River Sum
          Orig_Sum <- rowSums(System[,-(1:2)], na.rm=T)
          
         # Expand Rivers and Add Lakes
          New_Sums[,(s+1)] <- Orig_Sum*Expand_Frac         
          
      } # End for loop calculating system proportions and Expansion Lines
     
      write.csv(TT_Alldata, "DATA_PROCESSING/FraserSockeyePrep/Takla_Trem_EStu_AllData.csv")
      New_EFS <- cbind(Year = CU_Data[,1], EFS = rowSums(New_Sums[,-1]))
      return(New_EFS)
      
  } # End Gap Fill Takla_Trem_S


# ----------------------------------------- Gap Fill with Averages -------------------------------------------   
  
  GapFill_Ave <- function(CU, CU_Data){
    Fills <- which(CU_Data[,CU] == 0 | is.na(CU_Data[,CU]))
    for(f in Fills){
        Fill_Lower <- f-4
        Fill_Upper <- f+4
        if(sum(match(CU_Data[Fill_Lower,CU], 0), na.rm=T) > 0 )  Fill_Lower <- Fill_Lower-4 
        if(sum(match(CU_Data[Fill_Upper,CU], 0), na.rm=T) > 0 )  Fill_Upper <- Fill_Lower+4
        CU_Data[f,CU] <- mean(c(CU_Data[Fill_Lower,CU], CU_Data[Fill_Upper,CU]))
    } # Fill each gap independently but identify first
    
    New_Data <- cbind(Year = CU_Data$Year, EFS = CU_Data[,CU])  
    return(New_Data)  
  }


# --------------------------------------- Main Function ----------------------------------------------------

# Set up function to read in CU names as argument
 GetCUData <- function(CU_Name = "Shuswap_ES", Update = 2012, Data = "EFS",
                       #GP Edits
                        cu.info.file = "SoS_Data_CU_Info.csv", 
                       out.folder = ""
                                ){
   
   
   # Additions Dec 2017 to manipulate into long form for SOS Data Tool
   # GP moved this up
   library(dplyr);library(tidyr)
   #ts.info <- read.csv(cu.info.file,stringsAsFactors = FALSE)
   ts.info <- cu.info.file
   #CU_ID <-   ts.info %>%
  #   filter(Conservation_Unit==CU_Name) %>%
  #   select(CU_ID)
  # CU_ID <- unlist(CU_ID)
   #print(CU_ID)
   
   
   #CU <- get(noquote(CU_Name))
   if(Data =="EFS"){ CU <- CU_Streams[colnames(CU_Streams) == CU_Name]   # NOTE CU Streams = WSP Streams
   
        #print(CU)
        if(!is.na(match(CU_Name, GapCUs) )){
            Exceptions <- Expansion_Years[,(match(CU_Name, colnames(Expansion_Years)))] 
            if(CU_Name == "Quesnel_S" | CU_Name == "Takla_Trem_EStu" | CU_Name == "Shuswap_L"){
               CU <- CU_Streams[,!is.na(match(colnames(CU_Streams), levels(as.factor(CU[,1]))))]                     
               if(CU_Name != "Shuswap_L") Exceptions <- Expansion_Years[,!is.na(match(colnames(Expansion_Years), levels(as.factor(Exceptions))))]
            } # end is multi-section systems
        }
    
   }
   if(Data =="ETS"){ CU <- SR_Streams[colnames(SR_Streams) == CU_Name]   # Bmac March 2021 to pull all streams in the CU if t he SR list has NA for the CU meaning take all streams
                    if(sum(!is.na(CU)) == 0){
                      
                      # must first match up the CU name with the one used in the STAD file
                      stad.cu.name <- ts.info$CU_Name_tsdata[ts.info$CU_Name == CU_Name]
                      CU <- data.frame(x=unique(SK_Data$Stock.Name.stream.[SK_Data$CU.Name== stad.cu.name]))  
                    }
   
   }  
    Timing <- ts.info$RunTiming[ts.info$CU_Name==CU_Name] 
    T.group <- ifelse(Timing == "Early Stuart","T1", ifelse(Timing == "Early Summer", "T2", 
                                                            ifelse(Timing=="Summer", "T3", 
                                                                  ifelse(Timing == "Late", "T4", NA))))
    if(CU_Name == "Kamloops_ES") T.group <- "T3"
    
    CU <- (levels(as.factor(unlist(CU))))[(levels(as.factor(unlist(CU))))!=""]
   
    
    
    #print("Pop to group")
    #print(CU)
    CU_Data <- data.frame(matrix(NA, nrow= length(1950:Update), ncol=length(CU)+1) )
      colnames(CU_Data) <- c("Year", (CU))
      CU_Data[,1] <- 1950:Update  

    # Fill in CU_Data with the EFS or ETS for each stream in the CU
    for(i in as.factor(CU)){ 
        
      #print(paste("starting pop",i))
      
       #Stream_Data <-  SK_Data[SK_Data$Stream == i,]  
       Stream_Data <-  SK_Data[(SK_Data$Stock.Name.stream.== i  & SK_Data$Timing.Group == T.group),]  
      # print(Stream_Data)
       
       #Stream_Data$Eff.Total <- (Stream_Data$Males + Stream_Data$Females)*Stream_Data$Per..Spawn/100
       Stream_Data$Eff.Total <- (Stream_Data$males + Stream_Data$females)*Stream_Data$spawn./100
       
       if(Data == "EFS") 
         #CU_Data[,i]<- as.numeric(Stream_Data$Eff..Females[match(CU_Data$Year, Stream_Data$Year)])
         CU_Data[,i]<- as.numeric(Stream_Data$eff_fem[match(CU_Data$Year, Stream_Data$Year)])
         #CU_Data[,i]<- as.numeric(Stream_Data$Eff.Total[match(CU_Data$Year, Stream_Data$Year)])       
      
          if(Data == "ETS"){
          #CU_Data[,i]<- as.numeric(Stream_Data$Eff.Total[match(CU_Data$Year, Stream_Data$Year)])   
         CU_Data[,i]<- as.numeric(Stream_Data$Eff.Total[match(CU_Data$Year, Stream_Data$Year)])   
              }
    } # end for loop

    # Need to add an exception for Chilko because the timing is Summer AND Early Summer! ******************
      if(CU_Name=="Chilko_S_ES"){
                st.data <- SK_Data[(SK_Data$Stock.Name.stream.== "S. End Chilko Lake"),]   
                st.data$Eff.Total <- (st.data$males + st.data$females)*st.data$spawn./100
                if(Data == "EFS") 
                  #CU_Data[,i]<- as.numeric(Stream_Data$Eff..Females[match(CU_Data$Year, Stream_Data$Year)])
                  CU_Data[,'S. End Chilko Lake']<- as.numeric(st.data$eff_fem[match(CU_Data$Year, st.data$Year)])
                #CU_Data[,'S. End Chilko Lake']<- as.numeric(st.data$Eff.Total[match(CU_Data$Year, st.data$Year)])  
                
                if(Data == "ETS"){
                  #CU_Data[,i]<- as.numeric(Stream_Data$Eff.Total[match(CU_Data$Year, Stream_Data$Year)])   
                  CU_Data[,'S. End Chilko Lake']<- as.numeric(st.data$Eff.Total[match(CU_Data$Year, st.data$Year)])   
                }
      }
   
    # Calc the total for the CU
    if(Data == "ETS"){
      
      if(CU_Name == "Lillooet_Harr_L") CU_Data[CU_Data$Year==2002, 2] <- mean( c(CU_Data[CU_Data$Year==1998, 2], CU_Data[CU_Data$Year==2006, 2]) )
      
      if(length(CU) > 1) CU_Data$ETS <- rowSums(select(CU_Data,-Year), na.rm=T)
      if(length(CU)==1)   CU_Data$ETS <- CU_Data[,CU]
      if(CU_Name == "Taseko_ES"){ CU_Data[,"ETS"][CU_Data[,"Year"]%in%c(1965,1968:1992)]<- NA  # added Jan 2024 - these years were previously removed manually for calculating WSP metrics
      } # end if Taseko
    }
    if(Data == "EFS"){      
          if(!is.na(match(CU_Name, "Quesnel_S")))    CU_Data <- GapFill_Quesnel(CU_Data=CU_Data, CU_Name=CU_Name, Exceptions = Exceptions)
          if(!is.na(match(CU_Name, "Shuswap_L")))    CU_Data <- GapFill_LShu(CU_Data=CU_Data, CU_Name=CU_Name, Exceptions)
          if(!is.na(match(CU_Name, "Takla_Trem_EStu"))) CU_Data <- GapFill_Takla_EStu(CU_Data=CU_Data, CU_Name=CU_Name, Exceptions = Exceptions)
          if(!is.na(match(CU_Name, "Nahatlatch_ES"))  | !is.na(match(CU_Name, "Takla_Trem_S_S")))  CU_Data <- GapFill(CU=CU, CU_Data=CU_Data, CU_Name = CU_Name, Exceptions = Exceptions); 
          if(!is.na(match(CU_Name, "Lillooet_Harr_L")) | !is.na(match(CU_Name, "Widgeon_RT")) | !is.na(match(CU_Name, "Taseko_ES")) ) CU_Data <- GapFill_Ave(CU=CU, CU_Data=CU_Data)       
        # Nahatlatch_ES amd Takla_Trem_S_S don't need special functions - use the regular Gap Fill Function
        if( sum(match(GapCUs,CU_Name), na.rm=T) == 0 ){
           if(ncol(CU_Data) > 2)  CU_Data$EFS <- rowSums(CU_Data[,CU], na.rm=T)
           else{
              if(is.na(match(CU_Name, "Lillooet_Harr_L")) & is.na(match(CU_Name, "Widgeon_RT")) & is.na(match(CU_Name, "Taseko_ES")) )
                CU_Data$EFS <- CU_Data[,CU]
           }# end else
           if(CU_Name == "Taseko_ES"){ CU_Data[,"EFS"][CU_Data[,"Year"]%in%1969:1992]<- NA  # added Jan 2024 - these years were previously removed manually for calculating WSP metrics
           } # end if Taseko
        } # End if loop         
    } # End if Data == "EFS"
    
    #GPEdit: for some reason, this causes all kinds of problems, do outside of this fn
    #CU_Data <- cbind(CU_Data,CU_ID = CU_ID, row.names = NULL)) #as.character(CU_ID) 
    write.csv(CU_Data, file= (paste0(out.folder,CU_Name,"_",Data,".csv")), row.names=FALSE)
    
    

   
     # GP Edits
    #browser()
    #print(dim(CU_Data))
    #print(names(CU_Data))
    #print(head(CU_Data))
    #stream_data <- select(CU_Data, -EFS, -Year) 

    
    
    # build dataframe parts
    #New_Year<- rep(CU_Data$Year, times=ncol(stream_data))
    #CU_ID <- rep(CU_ID, times=length(New_Year))
    #metric=rep(as.factor(Data), times=length(New_Year))
    #long_data <- tidyr::gather(stream_data, key=stream, value=value, factor_key=T) 
    
    #if(length(New_Year) != length(long_data[,1])) print("Error! data frames mismatch in length")
    
    #Longform_Table <- cbind(CU_ID=CU_ID,year=New_Year, long_data, metric)
    #write.csv(Longform_Table, file= (paste0(out.folder,CU_Name,Data,"_Longform.csv")))
  
    # GP added
    #print(head(CU_Data))


   out.obj <- CU_Data[,c("Year",Data)]
                        
      
    return(out.obj)
    
    
    
  } # End function GetCUData
  





Run_Long <- function(CU="Bowron_ES"){ 
 
}

