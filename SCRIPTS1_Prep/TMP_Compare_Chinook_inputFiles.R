library(tidyverse)



ck.sites.info <- read_csv("DATA_LOOKUP_FILES/SBC_Chinook_VerifiedSiteLookup.csv") 


site.count.df <- ck.sites.info %>% dplyr::filter(Pop_Category == "P") %>%
  group_by(CU_findex) %>% 
  summarize(TotalSites = n(), WildSites = sum(Enhancement_Rank %in% c("LOW","UNK")))

write.csv(site.count.df,"DATA_TRACKING/Site_Count_by_EnhRank.csv")


ck.df.old <- read_csv("DATA_IN/Old_Backup_Chinook_SBC_ByCU.csv") 
ck.df.new <- read_csv("DATA_IN/Chinook_SBC_ByCU.csv") 
ck.merged.output <-   read_csv("DATA_OUT/MERGED_FLAT_FILE_BY_CU.csv") %>%
                            dplyr::filter(Species == "Chinook")
ck.retro.out <- read_csv("DATA_OUT/Retrospective_Metrics_Values.csv")%>%
                      dplyr::filter(Species == "Chinook")



cu.list <- sort(unique(c(ck.df.old$CU_ID,ck.df.new$CU_ID)))
cu.list




yrs.lim <- c(1970,2020) #range(ck.df.old$Year,ck.df.new$Year) 



pdf(paste0("DATA_TRACKING/DataCheck_CK_Inputs.pdf"),onefile=TRUE,height=8.5, width=11)


for(cu.plot in cu.list){
print("-----------------")
print(cu.plot)  
  
par(mfrow=c(2,2))
  
data.old <- ck.df.old %>% dplyr::filter(CU_ID == cu.plot)
data.new <- ck.df.new %>% dplyr::filter(CU_ID == cu.plot)
data.out <- ck.merged.output %>% dplyr::filter(CU_ID == cu.plot)
metrics.out <- ck.retro.out %>% dplyr::filter(CU_ID == cu.plot) 

metrics.out$RelLBM[!is.na(metrics.out$RelLBM)] <- 4
metrics.out$AbsLBM[!is.na(metrics.out$AbsLBM)] <- 3
metrics.out$LongTrend[!is.na(metrics.out$LongTrend)] <- 2
metrics.out$PercChange[!is.na(metrics.out$PercChange)] <- 1
metrics.out



ylim.use <- c(0,max(data.old$AllSitesInfilled,data.new$AllSitesInfilled,na.rm=TRUE))
ylim.use

if(!is.finite(ylim.use[2])){ ylim.use[2] <-5 }

plot(1:5,1:5, type="n",ylim=ylim.use, xlim = yrs.lim,bty="n",xlab = "Year",ylab = "Spn",las=1) 
abline(v=1995,col="red",lty=2)
title(main = paste(unique(data.old$CU_Name),"(",cu.plot,")"),cex.main=1.5, col.main = "darkblue",outer=TRUE,line=-1)



if(sum(!is.na(data.old$AllSitesInfilled))>0) {lines(data.old$Year, data.old$AllSitesInfilled ,pch=21, 
                                                   type="o",cex=1.8,bg="white",col="darkblue")   }
if(sum(!is.na(data.old$AllSitesInfilled))>0) {lines(data.new$Year, data.new$AllSitesInfilled ,pch=19, 
                                                    type="o",cex=1,col="darkblue") }

title(main = "Input File - All Sites")

legend("topleft",legend=c("Old Data File","New Data File"),pch=c(21,19),col="darkblue",bg="White",bty="n")

ylim.use <- c(0,max(data.old$LowUnkSitesInfilled,data.new$LowUnkSitesInfilled,na.rm=TRUE))
ylim.use


if(!is.finite(ylim.use[2])){ ylim.use[2] <-5 }
  

plot(1:5,1:5, type="n",ylim=ylim.use, xlim = yrs.lim,bty="n",xlab = "Year",ylab = "Spn",las=1)  
abline(v=1995,col="red",lty=2)

if(sum(!is.na(data.old$LowUnkSitesInfilled))>0) {lines(data.old$Year, data.old$LowUnkSitesInfilled ,pch=21, 
                                                    type="o",cex=1.8,bg="white",col="darkblue")   }
if(sum(!is.na(data.old$LowUnkSitesInfilled))>0) {lines(data.new$Year, data.new$LowUnkSitesInfilled,pch=19, 
                                                    type="o",cex=1,col="darkblue") }

title(main = "Input File - Wild Sites")
legend("topleft",legend=c("Old Data File","New Data File"),pch=c(21,19),col="darkblue",bg="White",bty="n")




plot(1:5,1:5, type="n",ylim=ylim.use, xlim = yrs.lim,bty="n",xlab = "Year",ylab = "Spn",las=1)  
abline(v=1995,col="red",lty=2)


if(sum(!is.na(data.new$LowUnkSitesInfilled))>0) {lines(data.new$Year, data.new$LowUnkSitesInfilled ,pch=21, 
                                                          type="o",cex=1.8,bg="white",col="darkblue")   }

if(sum(!is.na(data.out$SpnForAbd_Wild))>0) {lines(data.out$Year, data.out$SpnForAbd_Wild ,pch=4, 
                                                    type="p",cex=1.5,col="red")   }


title(main = "Output File - SpnForAbd_Wild")
legend("topleft",legend=c("New Data File","Merged Output File"),pch=c(21,4),col=c("darkblue","red"),bg="White",bty="n")





plot(1:5,1:5, type="n",ylim=c(0,5), xlim = yrs.lim,bty="n",xlab = "Year",ylab = "",las=1,axes=FALSE)  
axis(1)
abline(v=1995,col="red",lty=2)
abline(h=c(1:4),col="darkgrey")
lines(metrics.out$Year, metrics.out$RelLBM ,pch=19, type="o",cex=1,bg="white",col="darkblue")
lines(metrics.out$Year, metrics.out$AbsLBM ,pch=19, type="o",cex=1,bg="white",col="darkblue")
lines(metrics.out$Year, metrics.out$LongTrend ,pch=19, type="o",cex=1,bg="white",col="darkblue")
lines(metrics.out$Year, metrics.out$PercChange,pch=19, type="o",cex=1,bg="white",col="darkblue")
text(rep(par("usr")[1],4),4:1,c("Rel Abd","Abs Abd","Long Trend","Perc Change"), xpd=NA,adj=1)



} # end looping through CUs


dev.off()