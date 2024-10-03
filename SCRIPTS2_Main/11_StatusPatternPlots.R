# generate plot of num CU over time
# (like in methods paper)
# for now only doing Skeena for the tech report
# need to generalize and do 1 for "approved", and "all", etc

if(!dir.exists("OUTPUT/STATUS_PATTERNS")){dir.create("OUTPUT/STATUS_PATTERNS")}

library(tidyverse)
# for now exclude proxy statuses and extirpated CUs from summary counts
# - > DISCUSS

src.df <- read_csv("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_SkeenaMODS_Explore.csv") %>%
          dplyr::filter(grepl("Skeena",Group)) 
# this works, because "Group" is not yet filled in for the proxy and extirpated CUs
# need to fix in the step creating this file, then fix here.


status.counts.df <- src.df %>% group_by(Year,RapidStatus) %>% summarize(n = n()) %>% 
  ungroup() %>% # otherwise Year sticks around as grouping variable and mucks up later
  pivot_wider(id_cols = Year, names_from = RapidStatus, values_from = n) %>%
  select(Year,Green,Amber,Red,None) %>% 
  replace_na(list(Green = 0,Amber=0,Red=0,None=0, Ext=0)) %>%
  mutate(Assessed = Green + Amber + Red) %>%
  mutate(Total = Assessed + None)

write_csv(status.counts.df,"OUTPUT/DASHBOARDS/StatusCounts_SkeenaSockeye.csv")




png(filename = paste0("OUTPUT/STATUS_PATTERNS/Status_Pattern_SkeenaSockeye.png"),
    width = 480*4.5, height = 480*2.6, units = "px", 
    pointsize = 14*3.4, bg = "white",  res = NA)

par(mfrow=c(1,2), mai=c(3.5,3.5,3,2))



# NUmber assessed and red over time

plot(status.counts.df$Year,status.counts.df$Assessed,
     bty="n",las=1,col="darkblue",xlab= "Year",ylab="Number of CUs",
     type="o",pch=21,ylim=c(0,16), bg="lightblue")
text(tail(status.counts.df$Year,1),
     tail(status.counts.df$Assessed,1),
     tail(status.counts.df$Assessed,1),
     xpd=NA,adj=c(-0.3))
     

lines(status.counts.df$Year,status.counts.df$Red,
      type="o",pch=21,ylim=c(0,16), bg="firebrick1")
text(tail(status.counts.df$Year,1),
     tail(status.counts.df$Red,1),
     tail(status.counts.df$Red,1),
     xpd=NA,adj=c(-0.8))
legend(1995,18.5,legend = c("Status Assigned","Red Status"),
       pch=21, col="darkblue", pt.bg = c("lightblue","firebrick1"),
       bty="n",xpd=NA,cex=0.85)

title(main = "A) Number of CUs",cex.main=1)


# Percent Composition of Status

props.in <- prop.table(status.counts.df %>% 
                         select(Red,Amber,Green) %>% 
                         as.matrix() %>% t() ,
                       margin=2)
props.in
colSums(props.in)

cols.use <- c("firebrick1","orange","palegreen")

tmp<-barplot(props.in,
             border=NA,space=0, 
             axes=FALSE,
             col=cols.use ,
             main="B) Status Composition (% of Assigned)",
             cex.main=1,
             #cex.main=p.cex.main,
             xaxt="n",
             xpd=NA)

axis(side=2,at=seq(0,1,by=0.2),labels=paste(seq(0,1,by=0.2)*100,"%",sep=""),
     #cex.axis=p.cex.axis,
     xpd=TRUE,las=1)
abline(h=seq(0.2,0.8,by=0.2),col="white",lwd=3,lty=2)


x.ticks <- pretty(status.counts.df$Year)
x.ticks

axis(side=1,at=x.ticks - min(status.counts.df$Year),
     labels=x.ticks,xpd=TRUE)

#get last column
last.year <- props.in[,ncol(props.in)]  %>% t() %>% as.data.frame()
last.year

label.y <- c(last.year$Red/2,
             last.year$Amber/2 + last.year$Red,
             last.year$Green/2 + last.year$Red + last.year$Amber)
label.y

text(par("usr")[2]-1,
     label.y, labels = c("Red","Amber","Green"),
     xpd = NA, adj=0,col="darkblue",font=2
)

dev.off()


#############################################################################

# Percent of all Current CUs
# skip this one for now.
# interpretation too messy with None vs. Data Deficient vs. Proxy
if(FALSE){
props.in <- prop.table(status.counts.df %>% 
                         select(Red,Amber,Green,None) %>% 
                         as.matrix() %>% t() ,
                       margin=2)
props.in
colSums(props.in)

cols.use <- c("firebrick1","orange","palegreen","white")


tmp<-barplot(props.in,
             border=NA,space=0, 
             axes=FALSE,
             col=cols.use ,
             main="% Composition - All Current CUs",
             #cex.main=p.cex.main,
             xaxt="n",
             xpd=NA)

axis(side=2,at=seq(0,1,by=0.2),labels=paste(seq(0,1,by=0.2)*100,"%",sep=""),
     #cex.axis=p.cex.axis,
     xpd=TRUE,las=1)
abline(h=seq(0.2,0.8,by=0.2),col="white",lwd=1.5)


x.ticks <- pretty(status.counts.df$Year)
x.ticks

axis(side=1,at=x.ticks - min(status.counts.df$Year),
     labels=x.ticks,xpd=TRUE)

#get last column
last.year <- props.in[,ncol(props.in)]  %>% t() %>% as.data.frame()
last.year

label.y <- c(last.year$Red/2,
             last.year$Amber/2 + last.year$Red,
             last.year$Green/2 + last.year$Red + last.year$Amber,
             last.year$None/2 + last.year$Green + last.year$Red + last.year$Amber)
label.y

text(par("usr")[2]-1,
     label.y, labels = c("Red","Amber","Green","No Status"),
     xpd = NA, adj=0,col="darkblue",font=2
)

}






