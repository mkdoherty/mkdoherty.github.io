#r setup, include=F, echo=FALSE, cache=FALSE}
deps = c("AUCRF","pROC", "vegan", "dplyr", 'knitr', "gridExtra", "grid", "xtable", "devtools", "pgirmess", "knitcitations", "scales", "tidyr", "ggplot2", "Hmisc", "cowplot", "mrgsolve", "magrittr", "cdcfluview", "purrr");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

pi_deaths <- cdcfluview::pi_mortality()
pi_deaths$year <- as.integer(gsub("(\\d+)-(\\d+)-(\\d+)", "\\1", pi_deaths$wk_end))
pi_deaths$seasons <- ifelse(pi_deaths$year_wk_num<40, paste(pi_deaths$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths$year), sep=""), paste(pi_deaths$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths$year+1), sep=""))
pi_deaths$seasonid <- as.numeric(pi_deaths$seasonid)

i <- as.numeric(max(pi_deaths$seasonid))
j <- as.numeric(ifelse(max(pi_deaths$year_wk_num<40), paste((max(pi_deaths$year)-1)), paste(max(pi_deaths$year))))
k <- as.numeric(ifelse(max(pi_deaths$year_wk_num<40), gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", max(pi_deaths$year)), gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", (max(pi_deaths$year)+1))))
get_season_yr <- function(seasonid){
  season_yr <- ifelse(seasonid == i, paste(j, "-", k, sep=""), paste((j-(i-seasonid)), "-", (k-(i-seasonid)), sep = ""))
  return(season_yr)
  }

pi_deaths$season <- get_season_yr(pi_deaths$seasonid)
#pi_deaths$year <- as.integer(gsub("(\\d+)-(\\d+)-(\\d+)", "\\1", pi_deaths$wk_start))
pi_deaths$season_wk <- as.factor(paste(pi_deaths$season,pi_deaths$year_wk_num, sep="_"))

#x <- c("2013-14_40", "2013-14_42")
#gsub("(\\d+)-(\\d+)_\\d+", "\\1", x), gsub("(\\d)(\\d)(\\d)(\\d)\\-(\\d+)_\\d+", "\\1\\2\\5", x))


pi_deaths$year <- ifelse(pi_deaths$year_wk_num>=40, gsub("(\\d+)-(\\d+)_\\d+", "\\1", pi_deaths$season_wk), gsub("(\\d)(\\d)(\\d)(\\d)\\-(\\d+)_\\d+", "\\1\\2\\5", pi_deaths$season_wk))

checks <- subset(pi_deaths, select=c("season","seasonid", "year", "year_wk_num","wk_start","wk_end"))
pi_deaths$year_wk <- paste(pi_deaths$year,pi_deaths$year_wk_num, sep="_")
pi_deaths_current_utd <- pi_deaths[pi_deaths$year==max(pi_deaths$year),]
pi_deaths_current_utd <- pi_deaths_current_utd[pi_deaths_current_utd$year_wk_num==max(pi_deaths_current_utd$year_wk_num),]
pi_deaths_current_utd$season <- "2017-18*"
pi_deaths <- data.frame(subset(pi_deaths, select=c("year_wk", "season","seasonid", "year", "year_wk_num","wk_start","wk_end", "number_influenza", "number_pneumonia", "all_deaths", "total_pni")))


ili_nat <- cdcfluview::ilinet("national")
ili_nat$season <- ifelse(ili_nat$week<40, paste(ili_nat$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year), sep=""), paste(ili_nat$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year+1), sep=""))
ili_nat$year_wk <- paste(ili_nat$year,ili_nat$week, sep="_")
ili_nat <- data.frame(subset(ili_nat, select=c("year_wk", "season", "week_start", "year", "week", "ilitotal", "total_patients")))
ili_nat_current_utd <- ili_nat[ili_nat$year==max(ili_nat$year),]
ili_nat_current_utd <- ili_nat_current_utd[ili_nat_current_utd$week==max(ili_nat_current_utd$week),]
ili_nat_current_utd$season <- "2017-18*"

ili_pi <- merge(pi_deaths, ili_nat, by="year_wk", all=F)
ili_pi$season <- ifelse(!is.na(ili_pi$season.x), ili_pi$season.x, ili_pi$season.y)

ili_pi_season <- subset(ili_pi, select=c("season", "week","year_wk", "week_start", "wk_start", "wk_end","ilitotal", "total_patients", "number_influenza", "number_pneumonia", "all_deaths", "total_pni"))

#ili_pi_season[is.na(ili_pi_season)] <- 0
ili_pi_since09 <- ili_pi_season[ili_pi_season$total_pni>0,]

ili_pi_2008 <- ili_pi_season[ili_pi_season$season=="2008-09",]
ili_pi_since2008 <- rbind(ili_pi_since09, ili_pi_2008)
ili_pi_since2008 <- arrange(ili_pi_since2008, season)
ili_pi_since2008$rateflud <- ili_pi_since2008$number_influenza/ili_pi_since2008$all_deaths*100
ili_pi_since2008$ratePId <- ili_pi_since2008$total_pni/ili_pi_since2008$all_deaths*100
ili_pi_since2008$rateILIbytot <- ili_pi_since2008$ilitotal/ili_pi_since2008$total_patients*100

ili_pi_gg <- subset(ili_pi_since2008, select=c("season", "week", "ratePId", "rateILIbytot"))



#ili_pi_since2008<-ili_pi_since2008[ili_pi_since2008$year_wk!='2016_52' & ili_pi_since2008$wk_end!="",]

ili_pi_gg %>% map_if(is.character, as.factor) %>% as_data_frame -> ili_pi_gg 

ili_pi_gg$week <- as.factor(ili_pi_gg$week)  
ili_pi_gg <- ili_pi_gg %>% drop_na(season)
#ili_pi_gg <- ili_pi_gg[ili_pi_gg$]
  
ili_pi_gg$week <- ordered(ili_pi_gg$week, levels=c(seq(40,53,1), seq(1,39,1)))


ilipi_byweek_gg <- gather(ili_pi_gg, type, rate, -c(season, week))
ilipi_byweek_gg %>% map_if(is.character, as.factor) %>% as_data_frame -> ilipi_byweek_gg
ilipi_byweek_gg$type <- relevel(ilipi_byweek_gg$type, "ratePId")
levels(ilipi_byweek_gg$type) <- c("P&I Deaths", "ILI Visits")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


pflu <- ggplot(ilipi_byweek_gg, aes(x=week, y=rate, group=interaction(season, type), color=type, lty=type))+geom_line(size=1)+facet_grid(~season)+theme_bw()+scale_y_continuous(name="% of All Healthcare Visits for ILI*", sec.axis = sec_axis(~., name = "% of All Deaths Due to P&I^"))+scale_x_discrete(name="Week",breaks=c("40", "50", "10", "20", "30"))+theme(axis.title.y.right = element_text(colour = "darkred"), axis.title.y = element_text(colour = "darkblue"), panel.spacing=unit(0, "lines"), legend.position = c(0.1,0.9), legend.background = element_rect(linetype = "solid", colour = "black"), legend.title = element_blank(), panel.grid.major=element_line("grey"), panel.grid.minor=element_line("grey"), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 1))+ scale_linetype_manual(values = c("twodash", "solid"))+scale_color_manual(values = c("darkred", "darkblue"))+labs(color="% of All", lty="% of All", caption = "* ILI Reported by the U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet).\n^ P&I Deaths Reported by P&I Mortality Surveillance from the National Center for Health Statistics Mortality Surveillance System.")+ggtitle(wrapper("Percentage of All Visits to Healthcare Providers for Influenza-like Illness (ILI) in the United States by Season and Week Since 2011-12 (blue line, left axis) and the Percentage of All Deaths Due to Pneumonia and Influenza (P&I) Since 2011-12 (red dashed line, right axis).", width=120 ))
  
pflu2 <- ggplot(ilipi_byweek_gg, aes(x=week, y=rate, group=interaction(season, type), color=type, lty=type))+geom_line(size=1)+facet_grid(~season)+theme_bw()+scale_y_continuous(name="% Total")+scale_x_discrete(name="Week",breaks=c("40", "50", "10", "20", "30"))+theme(axis.title.y.right = element_text(colour = "darkred"), panel.spacing=unit(0, "lines"), legend.position = c(0.1,0.9), legend.background = element_rect(linetype = "solid", colour = "black"), legend.title = element_blank(), panel.grid.major=element_line("grey"), panel.grid.minor=element_line("grey"), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 1))+ scale_linetype_manual(values = c("twodash", "solid"))+scale_color_manual(values = c("darkred", "darkblue"))+labs(color="% of All", lty="% of All", caption = "* ILI Reported by the U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet) since 2011-12 (blue line).\n^ P&I Deaths Reported by P&I Mortality Surveillance from the National Center for Health Statistics Mortality Surveillance System since 2011-12 (red dashed line).")+ggtitle("Percentage of All Visits to Healthcare Providers for Influenza-like Illness (ILI)* and \nPercentage of All Deaths Due to Pneumonia and Influenza (P&I)^ \nin the United States by Season and Week.")

jpeg(file="ilipi_by_seasonANDweek.jpeg", width = 12, height = 9, units = "in", res=100)
pflu2
dev.off()
