#r setup, include=F, echo=FALSE, cache=FALSE}
deps = c("AUCRF","pROC", "vegan", "dplyr", 'knitr', "gridExtra", "grid", "xtable", "devtools", "pgirmess", "knitcitations", "scales", "tidyr", "ggplot2", "Hmisc", "cowplot", "mrgsolve", "magrittr", "cdcfluview");
for (dep in deps){
	if (dep %in% installed.packages()[,"Package"] == FALSE){
		install.packages(as.character(dep), quiet=TRUE);
	}
	library(dep, verbose=FALSE, character.only=TRUE)
}

pi_deaths <- cdcfluview::pi_mortality()
ili_nat <- cdcfluview::ilinet("national")
hospFlu <- cdcfluview::hospitalizations()

ili_nat$season <- ifelse(ili_nat$week<40, paste(ili_nat$year-1,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year), sep=""), paste(ili_nat$year,"-", gsub("(\\d)(\\d)(\\d)(\\d)", "\\3\\4", ili_nat$year+1), sep=""))


ili_nat_current_utd <- ili_nat[ili_nat$year==max(ili_nat$year),]
ili_nat_current_utd <- ili_nat_current_utd[ili_nat_current_utd$week==max(ili_nat_current_utd$week),]
ili_nat_current_utd$season <- "2017-18*"

caption <- paste("* Through", format(ili_nat_current_utd$week_start, format="%B %d, %Y"))

pi_deaths <- data.frame(subset(pi_deaths, select=c("wk_start","wk_end","year_wk_num","seasonid", "number_influenza", "number_pneumonia", "all_deaths", "total_pni")))
ili_nat <- data.frame(subset(ili_nat, select=c("season", "week_start", "year", "week", "ilitotal")))
    

ili_nat_last10seasons <- ili_nat[ili_nat$year>=2008 & ili_nat$season!="2007-08",]

seasons <- c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18*")

ili_year <- data.frame(subset(ili_nat, select=c("year", "ilitotal")))
ili_season <- data.frame(subset(ili_nat_last10seasons, select=c("season", "ilitotal")))

ili_byseason <- aggregate(ili_season[,-1], by=list(Season = ili_season$season), FUN=sum)



#iliflu_byyear_gg <- gather(ili_byyear, Year, cases, -c(Year))
pflu <- ggplot(ili_byseason, aes(Season, x))+
  geom_bar(stat = "identity", position = "dodge",  fill = "darkgrey")+
  ylab("Visits")+ xlab("Flu Season")+
  scale_x_discrete(breaks = ili_byseason$Season, labels=seasons)+
  ggtitle("Figure 1: Total Visits to Healthcare Providers for Influenza-like Illness in the United States by Season since 2008-09")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(caption=caption, subtitle="Reported by the U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet).")+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(limits=c(0, 1100000), expand = c(0, 0))

jpeg(file="ili_by_season.jpeg", width = 12, height = 9, units = "in", res=100)
pflu
dev.off()

hospFlu <- data.frame(subset(hospFlu, select=c("sea_label", "year", "year_wk_num", "rate", "age_label", "wk_end")))
hospFlu_overall <- hospFlu[hospFlu$age_label=="Overall",]
hospFlu_overall_wk17 <- hospFlu_overall[hospFlu_overall$year_wk_num=="17",]
hospFlu_overall_wk10_2018 <- hospFlu_overall[hospFlu_overall$year_wk_num=="10" & hospFlu_overall$year=="2018",]
hospFlu_overall_current_utd <- hospFlu_overall[hospFlu_overall$year==max(hospFlu_overall$year),]
hospFlu_overall_current_utd <- hospFlu_overall_current_utd[hospFlu_overall_current_utd$year_wk_num==max(hospFlu_overall_current_utd$year_wk_num),]
hospFlu_overall_current_utd$sea_label <- "2017-18*"

hospFlu_cumrate <- rbind(hospFlu_overall_wk17, hospFlu_overall_current_utd)
hospFlu_cumrate_gg <- subset(hospFlu_cumrate, select=c("sea_label", "rate"))

caption <- paste("* Through", format(hospFlu_overall_current_utd$wk_end, format="%B %d, %Y"))

theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

hosp_flu_plot <- ggplot(hospFlu_cumrate_gg, aes(sea_label, rate))+
	geom_bar(stat = "identity", position = "dodge", fill = "red")+
	ylab("Cumulative Incidence Rate per 100,000 Population")+ xlab("Flu Season")+
	ggtitle("Figure 1: Laboratory-Confirmed Influenza-Related Hospitalizations by Season")+	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5))+
	labs(caption=caption, subtitle="Reported by The Influenza Hospitalization Surveillence Network (FluSurv-NET).")+ 
	theme(plot.subtitle = element_text(hjust = 0.5))+
	scale_x_discrete(breaks = hospFlu_cumrate_gg$sea_label)+
	expand_limits(y=c(0,100))+
	scale_y_continuous(breaks = seq(0,100,10))


pdf(file = "fluhospitalizationsbyseason.pdf", width = 8, height = 6)
hosp_flu_plot
dev.off()

jpeg(file = "fluhospitalizationsbyseason.jpeg", width = 8, height = 6, units = "in", res = 300)
hosp_flu_plot
dev.off()


	
	



pi_deaths$year <- gsub("(\\d+)-(\\d+)-(\\d+)", "\\1", pi_deaths$wk_end)
pi_deaths$year_wk <- paste(pi_deaths$year,pi_deaths$year_wk_num, sep="_")
pi_deaths$season <- as.factor(get_season_yr(pi_deaths$seasonid))


ili_nat$year_wk <- paste(ili_nat$year,ili_nat$week, sep="_")
ili_pi <- merge(pi_deaths, ili_nat, by="season", all=T)

ili_pi <- subset(ili_pi, select=c("season", "year.y", "year_wk", "seasonid", "number_influenza", "number_pneumonia", "total_pni", "ilitotal"))


get_season_yr <- function(seasonid){
	season_yr <- ifelse(seasonid == "49", "09-10", 
										ifelse(seasonid == "50", "10-11", 
														ifelse(seasonid == "51", "11-12", 
														 			ifelse(seasonid == "52", "12-13", 
														 			 			ifelse(seasonid == "53", "13-14" , 
														 			 			 			ifelse(seasonid == "54", "14-15", 
														 			 			 			 			ifelse(seasonid == "55", "15-16", 
														 			 			 			 			 			ifelse(seasonid == "56", "16-17", "17-18"))))))))
	return(season_yr)
}

ili_pi$season_yrs <- as.factor(get_season_yr(ili_pi$seasonid))
str(ili_pi)
colnames(ili_pi) <- c("year.y", "year_wk", "seasonid","influenza_deaths", "pneumonia_deaths", "FluPneumonia_deaths", "Influenza-Like-Illness", "Season")


ili_pi_seasonyrs <- subset(ili_pi, select=c("year.y", "Season", "FluPneumonia_deaths", "Influenza-Like-Illness"))
str(ili_pi_seasonyrs)

colnames(ili_pi_seasonyrs)

ili_pi_byseason <- aggregate(ili_pi_seasonyrs[,-c(1:2)], by=list(Year = ili_pi_seasonyrs$year.y), FUN=sum)

ili_byseason_gg <- gather(ili_pi_byseason, type, cases, -c(Season))
str(ili_byseason_gg)
p <- ggplot(ili_byseason_gg, aes(Season, cases))+
	geom_bar(stat = "identity", aes(fill=type), position = "dodge")+
	ylab("Individuals")
p					 

ann_text <- data.frame(RESPONSEwk6 = c("No", "Yes") , relabund = 95,
											 phylum = "Bacteroidetes")
phyRabRESPONSEwk6_figure2 <- ggplot(trtd.phyRabRESPONSEwk6_ggplot, aes(x=RESPONSEwk6, y=relabund, fill=RESPONSEwk6)) +
	geom_boxplot(position=position_dodge(width = 1), alpha = .4, outlier.shape = NA) +facet_grid(. ~ phylum) +
	ylab("Week 0 Relative Abundance (%)") + xlab("Week 6 Response") + theme(legend.position="none") + theme(strip.text = element_text(face = "italic")) +
	stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1))	 #geom_text(data = ann_text, label = "*", size=10) 




ili_flu_seasonyrs <- subset(ili_pi, select=c("season_yrs", "number_influenza", "ilitotal"))
str(ili_flu_seasonyrs)

ili_flu_byseason <- aggregate(ili_flu_seasonyrs[,-1], by=list(Season = ili_flu_seasonyrs$season_yrs), FUN=sum)

iliflu_byseason_gg <- gather(ili_flu_byseason, type, cases, -c(Season))
pflu <- ggplot(iliflu_byseason_gg, aes(Season, cases))+
	geom_bar(stat = "identity", aes(fill=type), position = "dodge")
pflu					


