library(tidyverse)


ck.df.old <- read_csv("DATA_IN/Old_Backup_Chinook_SBC_ByCU.csv")
ck.df.new <- read_csv("DATA_IN/Chinook_SBC_ByCU.csv")
  
  

cu.list <- sort(unique(c(ck.df.old$CU_ID,ck.df.new$CU_ID)))
cu.list

yrs.lim <- range(ck.df.old$Year,ck.df.new$Year)



pdf(paste0("DATA_TRACKING/DataCheck_CK_Inputs.pdf"),onefile=TRUE,height=8.5, width=11)


for(cu.plot in cu.list){
print("-----------------")
print(cu.plot)  
  
par(mfrow=c(1,2))
  
data.old <- ck.df.old %>% dplyr::filter(CU_ID == cu.plot)
data.new <- ck.df.new %>% dplyr::filter(CU_ID == cu.plot)

ylim.use <- c(0,max(data.old$AllSitesInfilledAdj,data.new$AllSitesInfilledAdj,na.rm=TRUE))
ylim.use

if(!is.finite(ylim.use[2])){ ylim.use[2] <-5 }

plot(1:5,1:5, type="n",ylim=ylim.use, xlim = yrs.lim,bty="n",xlab = "Year",ylab = "Spn",las=1)  
  
if(sum(!is.na(data.old$AllSitesInfilledAdj))>0) {lines(data.old$Year, data.old$AllSitesInfilledAdj ,pch=21, 
                                                   type="o",cex=1.5,bg="white",col="darkblue")   }
if(sum(!is.na(data.old$AllSitesInfilledAdj))>0) {lines(data.new$Year, data.new$AllSitesInfilledAdj ,pch=19, 
                                                    type="o",cex=1,col="darkblue") }

title(main = paste(cu.plot, "- All Sites"))



ylim.use <- c(0,max(data.old$LowUnkSitesInfilledAdj,data.new$LowUnkSitesInfilledAdj,na.rm=TRUE))
ylim.use


if(!is.finite(ylim.use[2])){ ylim.use[2] <-5 }
  

plot(1:5,1:5, type="n",ylim=ylim.use, xlim = yrs.lim,bty="n",xlab = "Year",ylab = "Spn",las=1)  

if(sum(!is.na(data.old$AllSitesInfilled))>0) {lines(data.old$Year, data.old$LowUnkSitesInfilledAdj ,pch=21, 
                                                    type="o",cex=1.5,bg="white",col="darkblue")   }
if(sum(!is.na(data.old$AllSitesInfilled))>0) {lines(data.new$Year, data.new$LowUnkSitesInfilledAdj,pch=19, 
                                                    type="o",cex=1,col="darkblue") }

title(main = paste(cu.plot, "- Wild Sites"))

title(main = unique(data.old$CU_Name),cex.main=1.5, col.main = "darkblue",outer=TRUE,line=-2)



} # end looping through CUs


dev.off()