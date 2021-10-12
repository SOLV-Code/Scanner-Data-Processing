# SCRIPT TO MATCH UP VARIOUS POPULATION IDENTIFIERS AND GENERATE A 
# CONSOLIDATED DECODER FILE FOR THE FRASER SOCKEYE DATA

# -----------------------------------------------------------------------------------------
# General Notes: 
#	- using the annacis decoder file as the basis, then pulling in other info
#	- column labels are AF = Annacis File, NF = nuSEDS file, SF = State of the Salmon File 


# -----------------------------------------------------------------------------------------
# Data Notes

#  DFO_STR_ID in Annacis file corresponds to GFE_ID in nuSEDS file 
#        (have a match up in "nuSEDS_LookupFile.csv", which is just a renamed version of "open_maps_mv.xls"
#          provided by Bruce Baxter)


# records without POP_ID in Annacis file: checks without sockeye obs -> not entered into nuSEDS

# records with "None Observed?" or "Not inspected?" are not expected to have any sockeye, but have not yet been confirmed
#			through field notes etc. (most of these are in the 1930s and 1940s)

 




# read in source files
annacis.file <- read.csv("DATA_IN/SOURCES/SPN_Sockeye_Fraser_ByPop.csv",stringsAsFactors=FALSE)
annacis.decoder <- read.csv("DATA_IN/SOURCES/Fraser Sockeye Matching/COMPUCOD-2019.csv",stringsAsFactors=FALSE)
sos.decoder <-  read.csv("DATA_IN/SOURCES/Fraser Sockeye Matching/FRSK_Streams_Info.csv",stringsAsFactors=FALSE)
nuseds.file <- read.csv("DATA_IN/SOURCES/nuSEDS_Raw_Sockeye_Fraser.csv",stringsAsFactors=FALSE)


# **************BLM added Brigittes Lookup file*********************************************
setwd("/")
mapdata <- read.csv("github/Shiny-App-for-Synoptic/data/PopLookup.csv",stringsAsFactors=FALSE)
#********************************************************************************************



# verify that Stock.Group is a unique identifier
if(max(table(annacis.decoder$Stock.Group))>1 | sum(is.na(annacis.decoder$Stock.Group)) >0 ){
		warning("Annacis Decoder file has duplicate or missing ID in the Stock.Group column! Need to fix.")
		stop()
}


# generate the template for the merged decoder
decoder.fr.sox <- annacis.decoder[,c("Stock.Group","Stock.Name.stream.","Stream.Alias","Timing.Group" ,"wsp.cu.id","Notes","POP_ID")]  
names(decoder.fr.sox) <- c("AF_ID","AF_PopName","AF_AltPopName","AF_Timing","AF_CU_ID","AF_Notes","AF_POP_ID")
decoder.fr.sox <- cbind(decoder.fr.sox,
				NF_POP_ID = NA,NF_PopName =  NA,NF_AltPopName =  NA,NF_CU_ID = NA, NF_CUName = NA,
				SF_ID =  NA,SF_PopName =  NA, SF_CU_ID= NA, SF_CUName= NA,
				
				#***************************** bMAC ADDED THIS ***************************
				MF_POP_ID = NA,MF_PopName =  NA,MF_AltPopName =  NA, MF_TSName=NA, MF_CU_ID = NA   
				)

options(warn=2) # turn warnings into errors that stop the loop

for(af.id in annacis.decoder$Stock.Group){
  print("----------------------------------")
  print(af.id)
  
  decoder.idx <- decoder.fr.sox$AF_ID == af.id
  print(decoder.fr.sox$AF_PopName[decoder.idx])
  
  
  # this finds all the nuseds records with the same POP_ID
  
  
  nf.idx <-   nuseds.file$POP_ID == decoder.fr.sox$AF_POP_ID[decoder.idx]
  
  # *********************************** ADDED *******************************
  map.idx <- mapdata$Pop_ID == decoder.fr.sox$AF_POP_ID[decoder.idx]
  # ***********************************************************************
  
  
  # If POPIDs are the same between the AF and NF:
    if(sum(nf.idx,na.rm=TRUE)>0){
    
    	print("found nuSEDS match")
    
    	# these will alway match because using AF pop id for the match
    	decoder.fr.sox$NF_POP_ID[decoder.idx] <- decoder.fr.sox$AF_POP_ID[decoder.idx] 
    
    	# these might have multiple matches, so checking for it
    	nf.pop.name <- unique(nuseds.file$GEOGRAPHICAL_EXTNT_OF_ESTIMATE[nf.idx])	
    	nf.alt.pop.name <- unique(nuseds.file$PopName_Mod[nf.idx])
    	nf.cu.name <- unique(nuseds.file$CU_NAME[nf.idx])
    	nf.cu.id<- unique(nuseds.file$CU_INDEX[nf.idx])                  
    
    	if(length(nf.pop.name)==1){decoder.fr.sox$NF_PopName[decoder.idx] <- nf.pop.name}
    	if(length(nf.pop.name)> 1){decoder.fr.sox$NF_PopName[decoder.idx] <- length(nf.pop.name)}
    
    	if(length(nf.alt.pop.name)==1){decoder.fr.sox$NF_AltPopName[decoder.idx] <- nf.alt.pop.name}
    	if(length(nf.alt.pop.name)> 1){decoder.fr.sox$NF_AltPopName[decoder.idx] <- length(nf.alt.pop.name)}
    
    	if(length(nf.cu.name)==1){decoder.fr.sox$NF_CUName[decoder.idx] <- nf.cu.name}
    	if(length(nf.cu.name)> 1){decoder.fr.sox$NF_CUName[decoder.idx] <- length(nf.cu.name)}
    
    	if(length(nf.cu.id)==1){decoder.fr.sox$NF_CU_ID[decoder.idx] <- nf.cu.id}
    	if(length(nf.cu.id)> 1){decoder.fr.sox$NF_CU_ID[decoder.idx] <- length(nf.cu.id)}
    	
    } # end if have a match in nuSEDS file
  
  

    # ****************************** Bmac added this to do the same to the mapfile **********************************************
    if(sum(map.idx,na.rm=TRUE)>0){
    
      print("found mapfile match")
    
      # these will alway match because using AF pop id for the match
      decoder.fr.sox$MF_POP_ID[decoder.idx] <- mapdata$Pop_ID[map.idx] 
      
      # these might have multiple matches, so checking for it
      mf.pop.name <- unique(mapdata$MapData_Pop_Name[map.idx])	
      mf.alt.pop.name <- unique(mapdata$Pop_Name[map.idx])
      mf.ts.name <- unique(mapdata$TimeSeriesData_Pop_Name[map.idx])
      mf.cu.id<- unique(mapdata$MapData_CU_ID[map.idx])   
      
      
      if(length(mf.pop.name)==1){decoder.fr.sox$MF_PopName[decoder.idx] <- mf.pop.name}
      if(length(mf.pop.name)> 1){decoder.fr.sox$MF_PopName[decoder.idx] <- length(mf.pop.name)}
      
      if(length(mf.alt.pop.name)==1){decoder.fr.sox$MF_AltPopName[decoder.idx] <- mf.alt.pop.name}
      if(length(mf.alt.pop.name)> 1){decoder.fr.sox$MF_AltPopName[decoder.idx] <- length(mf.alt.pop.name)}
      
      if(length(nf.cu.name)==1){decoder.fr.sox$MF_TSName[decoder.idx] <- mf.ts.name}
      if(length(nf.cu.name)> 1){decoder.fr.sox$MF_TSName[decoder.idx] <- length(mf.ts.name)}
      
      if(length(mf.cu.id)==1){decoder.fr.sox$MF_CU_ID[decoder.idx] <- mf.cu.id}
      if(length(mf.cu.id)> 1){decoder.fr.sox$MF_CU_ID[decoder.idx] <- length(mf.cu.id)}  
      
    }
  # ********************************************************************************************************************** END
  
  
    sos.idx <-  sos.decoder$BaseUnit.Name == decoder.fr.sox$AF_PopName[decoder.idx]
    
    # Where pop names are the same between SOS file and decoder file aka Annacis file COPUCOD
    if(sum(sos.idx,na.rm=TRUE)>0){
    
    	print("found SOS match")
    
    	# these will alway match because using AF pop  name for the match
    	decoder.fr.sox$SF_PopName[decoder.idx] <- decoder.fr.sox$AF_PopName[decoder.idx] 
    
    	# these might have multiple matches, so checking for it
    	sf.id <- unique(sos.decoder$BaseUnit.ID[sos.idx])	
    	sf.cu.name <- unique(sos.decoder$Base.Unit.CU.ShortName[sos.idx])
    	sf.cu.id<- unique(sos.decoder$Base.Unit.CU.ID[sos.idx])   
    
    
    	if(length(sf.id)==1){decoder.fr.sox$SF_ID[decoder.idx] <- sf.id}
    	if(length(sf.id)> 1){decoder.fr.sox$SF_ID[decoder.idx] <- length(sf.id)}
    
    
    	if(length(sf.cu.name)==1){decoder.fr.sox$SF_CUName[decoder.idx] <- sf.cu.name}
    	if(length(sf.cu.name)> 1){decoder.fr.sox$SF_CUName[decoder.idx] <- length(sf.cu.name)}
    
    	if(length(sf.cu.id)==1){decoder.fr.sox$SF_CU_ID[decoder.idx] <- sf.cu.id}
    	if(length(sf.cu.id)> 1){decoder.fr.sox$SF_CU_ID[decoder.idx] <- length(sf.cu.id)}

      }  # end if have match in SOS file




}  # end looping through Annacis pop id




options(warn=1) # change warnings back to NOT stopping things


head(decoder.fr.sox)

decoder.fr.sox <- decoder.fr.sox[order(decoder.fr.sox$AF_PopName), ]
setwd("/")
write.csv(decoder.fr.sox,"github/GPs-Code-Packages/SOS-Data-Processing/DATA_LOOKUP_FILES/Site_Info_GeneratedDecoder_Fraser_Sk.csv",row.names=FALSE)



# CHECK CU MATCHES
cu.match<- unique(decoder.fr.sox[,c("AF_CU_ID","NF_CU_ID","MF_CU_ID","SF_CU_ID","NF_CUName","SF_CUName")])
cu.match <- cu.match[order(cu.match$AF_CU_ID),]

write.csv(cu.match,"github/GPs-Code-Packages/SOS-Data-Processing/DATA_TRACKING/FrSox_CU_Match_Check.csv",row.names=FALSE)



# IDENTIFY POPS NOT IN THE ANNACIS DECODER FILE


# Annacis decoder that's not in nuSEDS  - we have POPIDs bot no spatial data
af.not.nf.idx <- !(annacis.decoder$POP_ID %in% nuseds.file$POP_ID )
sum(af.not.nf.idx)
write.csv(annacis.decoder[af.not.nf.idx,],"github/GPs-Code-Packages/SOS-Data-Processing/DATA_TRACKING/FrSox_PopMatch_AFNotNF.csv",row.names=FALSE)
# 105


# ********************************************************************************ADDED
# Annacis file not in Map file  - we have POPIDs bot no spatial data
af.not.map.idx <- !(annacis.decoder$POP_ID %in% mapdata$Pop_ID )
sum(af.not.map.idx)
write.csv(annacis.decoder[af.not.map.idx,],"github/GPs-Code-Packages/SOS-Data-Processing/DATA_TRACKING/FrSox_PopMatch_AFNotMAP.csv",row.names=FALSE)
#103
# ************************************************************************************END


# in nuSEDS but not in Annacis file
nf.not.af.idx <- !(nuseds.file$POP_ID %in% annacis.decoder$POP_ID)	
unique(nuseds.file$POP_ID[nf.not.af.idx]) 
write.csv(nuseds.file[nf.not.af.idx,],"github/GPs-Code-Packages/SOS-Data-Processing/DATA_TRACKING/FrSox_PopMatch_NFNotAF.csv",row.names=FALSE)


# in Annacis Decoder but not in SOS decoder
af.not.sf.idx <- !(annacis.decoder$Stock.Name.stream. %in% sos.decoder$BaseUnit.Name )
sum(af.not.sf.idx)
write.csv(annacis.decoder[af.not.sf.idx,],"DATA_TRACKING/FrSox_PopMatch_AFNotSF.csv",row.names=FALSE)


# in SOS decoder but not in Annacis Decoder  --- No POPIDS
sf.not.af.idx <- !( sos.decoder$BaseUnit.Name  %in% annacis.decoder$Stock.Name.stream.  |
				sos.decoder$BaseUnit.Name  %in% annacis.decoder$Stream.Alias )	
unique(sos.decoder$BaseUnit.Name[sf.not.af.idx]) 
write.csv(sos.decoder[sf.not.af.idx,],"DATA_TRACKING/FrSox_PopMatch_SFNotAF.csv",row.names=FALSE)
# 7 populations



# ********************************************************************************************************* ADDED
# Are any of the missing sites part of CUs?
CU_sites <- read.csv("github/GPs-Code-Packages/SOS-Data-Processing/DATA_IN/Sockeye_Fraser_CU_Streams.csv")
CU_info <- read.csv("github/GPs-Code-Packages/SOS-Data-Processing/DATA_LOOKUP_FILES/CU_Info_MasterFile.csv")

library("tidyr")

CU.list<- CU_sites %>% pivot_longer(everything(),names_to="CU_SubName",values_to = "Sites")
CU.list$CU_ID<- NA

CU.list$CU_ID <-  CU_info$Base.Unit.CU.ID[ match(CU.list$CU_SubName, CU_info$Base.Unit.CU.ShortName) ]

 
 
   
missing.mapdata <- annacis.decoder[af.not.map.idx,]

# Data that we have POP_ID for but no spatial data and is in the CU list
no_spat <- missing.mapdata$Stock.Name.stream.[missing.mapdata$Stock.Name.stream.%in% CU.list$Sites]   # 8 sites

# Data that we don't have a POP_ID for but is in the CU list
no_pop <- sos.decoder$BaseUnit.Name[sf.not.af.idx][sos.decoder$BaseUnit.Name[sf.not.af.idx] %in% CU.list$Sites] # 4 sites on Quesnel Lake

prior_sites <- cbind(c(no_spat, no_pop), c( missing.mapdata$POP_ID[missing.mapdata$Stock.Name.stream.%in% CU.list$Sites], rep(NA, times=length(no_pop))))



write.csv(prior_sites,"github/GPs-Code-Packages/SOS-Data-Processing/DATA_TRACKING/Priority_Sites.csv")
#********************************************************************************************************** END










