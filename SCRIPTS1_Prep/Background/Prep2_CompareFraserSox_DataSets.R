######################
# IMPORTANT:
# this sceript originally used to create lookup files. 
# Script has not been updated since, and currently doesn't run "as-is" 


# THIS SCRIPT CREATES COMPARISON TABLES AND PLOTS FOR THE 
# FRASER SOCKEYE DATA FROM ANNACIS AND nuSEDS
# (data is matched based on the decoder file generated in script Prep1_*)



# read in files
decoder.fr.sox <- read.csv("DATA_LOOKUP_FILES/Site_Info_GeneratedDecoder_Fraser_Sk.csv",stringsAsFactors = FALSE)
annacis.data <-    read.csv("DATA_IN/SOURCES/SPN_Sockeye_Fraser_ByPop.csv",stringsAsFactors=FALSE)
nuseds.data <- read.csv("DATA_IN/SOURCES/nuSEDS_Raw_Sockeye_Fraser.csv",stringsAsFactors=FALSE)


all.yrs.range <- range(annacis.data$Year,nuseds.data$ANALYSIS_YR)

# NOTE:
# stream ID used in Annacis decoder assigns a unique number to each record, so using that as the basic unit
# for organizing all the comparisons. However, the Annacis data set does not include that decoder ID, 
# so using "Stock.Name.stream." to get the data from there. For nuSEDS, using POP_ID match.


# ------------------------------------------------------
# Build a comparison table by site (just adding columns  to decoder file)
# and generate a comparison plot by site at the same time

# extract annacis ID and sort by CU id
af.id.list <- decoder.fr.sox$AF_ID
af.id.list <- af.id.list[order(decoder.fr.sox$AF_CU_ID)]



out.df <- cbind(decoder.fr.sox[order(decoder.fr.sox$AF_CU_ID),],
				AF_NumRec = NA,
				AF_NumRecLgZero = NA,
				AF_FirstYear = NA,
				AF_LastYear = NA,
				AF_Min= NA,
				AF_Med = NA,
				AF_Max = NA,
				NF_NumRec = NA,
				NF_NumRecLgZero = NA,
				NF_FirstYear = NA,
				NF_LastYear = NA,
				NF_Min= NA,
				NF_Med = NA,
				NF_Max = NA
				)




pdf("DATA_TRACKING/FrSox_RecordCompPlots.pdf",onefile=TRUE,height=8.5, width=11)

# loop through stream id

for(af.id.use in af.id.list){
print("--------------")
print(af.id.use)

par(mfrow=c(2,2))

out.idx <- out.df$AF_ID ==  af.id.use

# clear out the data vectors (so they don't stick around and get plotted later for a pop with no data
af.data.vec <- NA
nf.data.vec <- NA

af.name.use <- out.df$AF_PopName[out.idx]
af.timing.use <- out.df$AF_Timing[out.idx]
print(af.name.use)
print(af.timing.use)

af.data.idx <-  annacis.data$Stock.Name.stream. == af.name.use  & annacis.data$Timing.Group == af.timing.use
out.df[out.idx,"AF_NumRec"] <- sum(af.data.idx,na.rm=TRUE)



if(sum(af.data.idx,na.rm=TRUE)>0){
	af.data.vec <- annacis.data$Total[af.data.idx]
	out.df[out.idx,"AF_NumRecLgZero"] <- sum(af.data.vec>0,na.rm=TRUE)	
	out.df[out.idx,c("AF_FirstYear","AF_LastYear")] <- range(annacis.data$Year[af.data.idx])
	out.df[out.idx,c("AF_Min","AF_Med","AF_Max")] <- quantile(af.data.vec,probs=c(0,0.5,1),na.rm=TRUE)
	}



nf.data.idx <-  nuseds.data$POP_ID == out.df$AF_POP_ID[out.idx]
out.df[out.idx,"NF_NumRec"] <- sum(nf.data.idx,na.rm=TRUE)
if(sum(nf.data.idx,na.rm=TRUE)>0){
	nf.data.vec <- nuseds.data$MAX_ESTIMATE[nf.data.idx]  # or should this use NATURAL_ADULT_SPAWNERS?
	out.df[out.idx,"NF_NumRecLgZero"] <- sum(nf.data.vec>0,na.rm=TRUE)	
	out.df[out.idx,c("NF_FirstYear","NF_LastYear")] <- range(nuseds.data$ANALYSIS_YR[nf.data.idx])
	out.df[out.idx,c("NF_Min","NF_Med","NF_Max")] <- quantile(nf.data.vec,probs=c(0,0.5,1),na.rm=TRUE)
	}


# create the plots 

if(sum(af.data.idx,na.rm=TRUE)>0 | sum(nf.data.idx,na.rm=TRUE)>0 ){  # if have any data in either file

y.lim <- c(0,max(af.data.vec,nf.data.vec,na.rm=TRUE))

plot(1:5,1:5,xlim= all.yrs.range, ylim= y.lim,xlab="Return Year",ylab="Spawners",type="n",bty="n")
points(nuseds.data$ANALYSIS_YR[nf.data.idx],nuseds.data$MAX_ESTIMATE[nf.data.idx],pch=21,cex=1.5,col="darkblue")
lines(annacis.data$Year[af.data.idx],annacis.data$Total[af.data.idx],type="o",pch=19,col="red")
title(main="Esc")

legend("topleft",bty="n", legend = c("Annacis - Total","nuSEDS - MaxEst"),col=c("red","darkblue"), pch=c(19,21))

plot(1:5,1:5,xlim= all.yrs.range, ylim= c(0,max(1,log(y.lim[2]))),xlab="Return Year",ylab="Spawners",type="n",bty="n")
points(nuseds.data$ANALYSIS_YR[nf.data.idx],log(nuseds.data$MAX_ESTIMATE[nf.data.idx]),pch=21,cex=1.5,col="darkblue")
lines(annacis.data$Year[af.data.idx],log(annacis.data$Total[af.data.idx]),type="o",pch=19,col="red")
title(main="Log(Esc)")


title(main=af.name.use,outer=TRUE, line=-2, col.main="darkblue",cex.main=1.4)

} # end if creating a plot



} # end looping through pops

dev.off()


head(out.df)

out.df <- out.df[order(out.df$AF_PopName),]

write.csv(out.df,"DATA_TRACKING/FrSox_RecordComparison.csv",row.names=FALSE)








# ------------------------------------------------------
# compare the Estimate classifications

# NOT YET WORKING, LOOKING FOR A DATA FILE WITH ANNACIS ID COLUMN.

if(FALSE){
est.class.check <- cbind(annacis.data[,c("Year","Watershed.Group.Name", "Stock.Name.stream." ,"est_type")],NF_EstClass = NA)

# loop through records

pops.list <- decoder.fr.sox$AF_POP_ID 
pops.list <- sort(pops.list[pops.list>0 & !is.na(pops.list)])


for(pop.check in pops.list  ){ 
 

af.idx <- annacis.decoder$POP_ID %in% pop.check
if(sum(af.idx)>1){stop()}
af.id.check  <- annacis.decoder[af.idx,"Stock.Group"]

af.name <- annacis.decoder$Stock.Name.stream.[af.idx]
af.data.idx <- est.class.check
yrs.check <-  est.class.check[    ,"Year"]


print("--------------------")
print(annacis.decoder$Stock.Name.stream.[af.idx])
print(pop.check)
print(yrs.check)

#nuseds.idx <- nuseds.data$POP_ID == pop.check & nuseds.data$ANALYSIS_YR == yr.check 
#nuseds.est.class <- nuseds.data$ESTIMATE_CLASSIFICATION[nuseds.idx]
#if(length(nuseds.est.class)==1){ est.class.check[i,"NF_EstClass"] <- nuseds.est.class}
#if(length(nuseds.est.class)>1){ est.class.check[i,"NF_EstClass"] <- length(nuseds.est.class)}

}






decoder.fr.sox 

nuseds.data



} # end skipping








