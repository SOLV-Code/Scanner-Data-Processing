# generate plot of num CU over time
# (like in methods paper)
# for now only doing Skeena for the tech report
# need to generalize and do 1 for "approved", and "all", etc



library(tidyverse)
# for now exclude proxy statuses and extirpated CUs from summary counts
# - > DISCUSS

src.df <- read_csv("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_SkeenaMODS_Explore.csv") %>%
          dplyr::filter(grepl("Skeena",Group)) 
# this works, because "Group" is not yet filled in for the proxy and extirpated CUs
# need to fix in the step creating this file, then fix here.


status.counts.df <- src.df %>% group_by(Year,RapidStatus) %>% summarize(n = n()) %>% 
  pivot_wider(id_cols = Year, names_from = RapidStatus, values_from = n) %>%
  select(Year,Green,Amber,Red,None) %>% 
  replace_na(list(Green = 0,Amber=0,Red=0,None=0, Ext=0)) %>%
  mutate(Assessed = Green + Amber + Red) %>%
  mutate(Total = Assessed + None)

write_csv(status.counts.df,"OUTPUT/DASHBOARDS/StatusCounts_SkeenaSockeye.csv")


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
     xpd=NA,adj=c(-0.3))
legend("topleft",legend = c("Assessed","Red Status"),
       pch=21, col="darkblue", pt.bg = c("lightblue","firebrick1"),
       bty="n")

# Percent Composition of assessed








