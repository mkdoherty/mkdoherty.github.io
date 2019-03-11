#r setup, include=F, echo=FALSE, cache=FALSE}
deps = c("AUCRF","pROC", "vegan", "dplyr", 'knitr', "gridExtra", "grid", "xtable", "devtools", "pgirmess", "knitcitations", "scales", "tidyr", "ggplot2", "Hmisc", "cowplot", "mrgsolve", "magrittr", "cdcfluview");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

pi_deaths <- cdcfluview::pi_mortality()
pi_deaths$year <- as.integer(gsub("(\\d+)-(\\d+)-(\\d+)", "\\1", pi_deaths$wk_end))
pi_deaths$season <- ifelse(pi_deaths$year_wk_num<40, paste(pi_deaths$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths$year), sep=""), paste(pi_deaths$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths$year+1), sep=""))
pi_deaths$year_wk <- paste(pi_deaths$year,pi_deaths$year_wk_num, sep="_")
pi_deaths_current_utd <- pi_deaths[pi_deaths$year==max(pi_deaths$year),]
pi_deaths_current_utd <- pi_deaths_current_utd[pi_deaths_current_utd$year_wk_num==max(pi_deaths_current_utd$year_wk_num),]
pi_deaths_current_utd$season <- paste(ifelse(pi_deaths_current_utd$year_wk_num<40, paste(pi_deaths_current_utd$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths_current_utd$year), sep=""), paste(pi_deaths_current_utd$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", pi_deaths_current_utd$year+1), sep="")), "*", sep = "")
pi_deaths <- data.frame(subset(pi_deaths, select=c("year_wk", "season","seasonid", "year", "year_wk_num", "number_influenza", "number_pneumonia", "all_deaths", "total_pni")))


ili_nat <- cdcfluview::ilinet("national")
ili_nat$season <- ifelse(ili_nat$week<40, paste(ili_nat$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year), sep=""), paste(ili_nat$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year+1), sep=""))
ili_nat$year_wk <- paste(ili_nat$year,ili_nat$week, sep="_")
ili_nat <- data.frame(subset(ili_nat, select=c("year_wk", "season", "week_start", "year", "week", "ilitotal", "total_patients")))
ili_nat_current_utd <- ili_nat[ili_nat$year==max(ili_nat$year),]
ili_nat_current_utd <- ili_nat_current_utd[ili_nat_current_utd$week==max(ili_nat_current_utd$week),]
ili_nat_current_utd$season <- paste(ifelse(ili_nat_current_utd$week<40, paste(ili_nat_current_utd$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat_current_utd$year), sep=""), paste(ili_nat_current_utd$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat_current_utd$year+1), sep="")), '*', sep = "")


ili_pi <- merge(pi_deaths, ili_nat, by="year_wk", all=T)
ili_pi$season <- ifelse(!is.na(ili_pi$season.x), ili_pi$season.x, ili_pi$season.y)

ili_pi_season <- subset(ili_pi, select=c("season", "ilitotal", "total_patients", "number_influenza", "number_pneumonia", "all_deaths", "total_pni"))

ili_pi_season[is.na(ili_pi_season)] <- 0

ilipi_byseason <- aggregate(ili_pi_season[,-1], by=list(Season = ili_pi_season$season), FUN=sum)
ilipi_since09 <- ilipi_byseason[ilipi_byseason$total_pni>0,]
ilipi_2008 <- ilipi_byseason[ilipi_byseason$Season=="2008-09",]
ilipi_since2008 <- rbind(ilipi_since09, ilipi_2008)
ilipi_since2008 <- arrange(ilipi_since2008, Season)
ilipi_since2008$rateflud <- ilipi_since2008$number_influenza/ilipi_since2008$all_deaths*100
ilipi_since2008$ratePId <- ilipi_since2008$total_pni/ilipi_since2008$all_deaths*100
ilipi_since2008$rateILIbytot <- ilipi_since2008$ilitotal/ilipi_since2008$total_patients*100


caption <- paste("Figure 1: Total Visits to Healthcare Providers for Influenza-like Illness (ILI) in the United States by Season Since 2008-09 (grey bars, left axis) and the Percentage of All Deaths Due to Influenza Since 2009-2010 (red line, right axis).", "ILI Reported by the U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet) with Influenza Deaths Reported by Pneumonia and Influenza Mortality Surveillance from the National Center for Health Statistics Mortality Surveillance System.", "* ILI Through", format(ili_nat_current_utd$week_start, format="%B %d, %Y"), "with Influenza Deaths Through", format(pi_deaths_current_utd$wk_end, format="%B %d, %Y"))

seasons <- c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19")
seasons_lab <- c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-2018", expression(bold("2018-19*")))


ilipi_byseason_gg <- gather(ilipi_byseason, type, Incidents, -c(Season))
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
#iliflu_byyear_gg <- gather(ili_byyear, Year, cases, -c(Year))
pflu <- ggplot(ilipi_since2008, aes(x=Season, y=ilitotal))+
  geom_bar(stat = "identity", position = "dodge",  fill = "darkgrey")+
  ylab("Healthcare Visits for Influenza-Like-Illness")+ xlab("Flu Season")+
  scale_x_discrete(limits=seasons, breaks = ilipi_byseason$Season, labels=seasons_lab)+theme_bw()+ theme(panel.grid.major.y=element_line("grey"), panel.grid.minor.y=element_line("grey"), legend.position = "topleft", plot.caption = element_text(hjust=0, size=rel(1.2)))+
  labs(caption=wrapper(caption, width=136))+ theme(plot.subtitle = element_text(hjust = 0.5))+geom_point(aes(x=Season, y=rateflud*1000000), group=1, subset(ilipi_since2008, Season!="2008-09"), colour="red", size=3)+geom_line(aes(x=Season, y=rateflud*1000000), group=1, subset(ilipi_since2008, Season!="2008-09"), colour="red", lty=2, size=1)+scale_y_continuous(limits=c(0, 1100000), expand = c(0, 0), sec.axis = sec_axis(~./1000000, name="% All Deaths Due to Influenza"))+geom_text(aes(label=round(rateflud, 2), x=Season, y=rateflud*1000000+50000), colour="black", subset(ilipi_since2008, Season!="2008-09"))

pflu

#jpeg(file="ili_by_season.jpeg", width = 12, height = 9, units = "in", res=100)
pflu
#dev.off()


pflu <- ggplot(ilipi_since2008, aes(x=Season, y=ilitotal))+
  geom_bar(stat = "identity", position = "dodge",  fill = "darkgrey")+
  ylab("Healthcare Visits for Influenza-Like-Illness")+ xlab("Flu Season")+
  scale_x_discrete(limits=seasons, breaks = ilipi_byseason$Season, labels=seasons_lab)+
  ggtitle("Figure 1: Total Visits to Healthcare Providers for Influenza-like Illness (ILI) in the United States by Season since 2008-09 \nand deaths due to pneumonuia or influenza (PI) since 2009-2010")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y=element_line("grey"), panel.grid.minor.y=element_line("grey"), legend.position = "topleft", plot.caption = element_text(face="bold", hjust=0, size=rel(1.2)))+
  labs(caption=wrapper(caption, width=150), subtitle="ILI Reported by the U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet) with Influenza deaths reported by Pneumonia and Influenza Mortality Surveillance from the National Center for Health Statistics Mortality Surveillance System")+ theme(plot.subtitle = element_text(hjust = 0.5))+geom_point(aes(x=Season, y=rateflud*1000000), group=1, subset(ilipi_since2008, Season!="2008-09"), colour="red", size=3)+geom_line(aes(x=Season, y=rateflud*1000000), group=1, subset(ilipi_since2008, Season!="2008-09"), colour="red", lty=2, size=1)+scale_y_continuous(limits=c(0, 1100000), expand = c(0, 0), sec.axis = sec_axis(~./1000000, name="% Influenza Deaths/Total Deaths"))+geom_text(aes(label=round(rateflud, 2), x=Season, y=rateflud*1000000+50000), colour="black", subset(ilipi_since2008, Season!="2008-09"))
