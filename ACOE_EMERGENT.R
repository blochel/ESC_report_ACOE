#EMERGENT
#nr stems - Exceedance curves 
#nr stems monthly - bargrapth w.SE
#shoot ratio - Exceedance curves
#shoot ratio trendlines annual - scatter plot
#shoot ratio trendlines dry/wet - scatter plot

library('tidyverse')
library('readxl')
library("dplyr")
library("plyr")
library("lubridate")
library("ggplot2")
library("scales")
library("ggrepel")
library("ggpubr")





#df %>%
#dplyr::group_by(city) %>%
#  dplyr::summarise(
#    n = dplyr::n()
#    ,mean_pos = mean(as.integer(df$mean_daily_temp))
#    ) 
#%>% dplyr::filter( as.integer(df$date) > "2020/12/01")


#choose what hydro year it is - change green text to the HY of interest. 
this_report<-  "22-23"            #change this nr to current HY! (YYYY-YY)
remove_report<-"23-24"           #if hy greater than report year

#emergent data is in the seperate folders, meaning we need a code that can grab data from several WD folders...


TR<- read_xls("/PLANTS/TR/TREMERGENT.xls", skip = 2) %>% 
  dplyr::select(c("Date","Year","Month","Hydro. Year","QUAD #",
         "SHOOTS","STEMS","Canopy","Max.","Notes"))
HC<- read_xls("/PLANTS/HC/HCEMERGENT.xls", skip = 2)%>% 
  dplyr::select(c("Date","Year","Month","Hydro. Year","QUAD #",
                  "SHOOTS","STEMS","Canopy","Max.","Notes"))
JB<- read_xls("/PLANTS/JB/JBEMERGENT.xls", skip = 2)%>% 
  dplyr::select(c("Date","Year","Month","Hydro. Year","QUAD #",
                  "SHOOTS","STEMS","Canopy","Max.","Notes"))

#make what we need numeric
TR$SHOOTS<- as.numeric(TR$SHOOTS)
TR$STEMS<- as.numeric(TR$STEMS)
HC$SHOOTS<- as.numeric(HC$SHOOTS)
HC$STEMS<- as.numeric(HC$STEMS)
JB$SHOOTS<- as.numeric(JB$SHOOTS)
JB$STEMS<- as.numeric(JB$STEMS)

#create a site name column
TR$site<- as.factor(rep("TR", nrow(TR)))
HC$site<- as.factor(rep("HC", nrow(HC)))
JB$site<- as.factor(rep("JB", nrow(JB)))

#merge all data frames 
emergent.df<- rbind(TR,JB)
emergent.df<- rbind(emergent.df,HC)
#change HY column name
names(emergent.df)[names(emergent.df) == "Hydro. Year"] <- "HY"
#makesure data is Date
emergent.df$Date<- as.Date(emergent.df$Date, "%d-%b-%y")
#season as words
emergent.df <- 
  emergent.df %>% 
  mutate(Season = if_else(month(emergent.df$Date) >= 6 &
                            month(emergent.df$Date) <= 11, 
                          'Wet Season', 
                          'Dry Season'),
         Season = factor(Season, levels = c('Wet Season','Dry Season')))

#add a MonthDay column
emergent.df$MonthDay <- paste( month(emergent.df$Date), day(emergent.df$Date), sep="-" )
#shoots to stems, ratio - shoot:stem, stems/shoots
emergent.df$ratio<- emergent.df$STEMS/emergent.df$SHOOTS

#remove to early year
remv.hy <- which(emergent.df$HY == remove_report)
emergent.df <- emergent.df[-c(remv.hy),]

emergent.df$site <- factor(emergent.df$site, levels = c("TR","JB","HC"))
emergent.df$Month <- factor(emergent.df$Month, levels = c("JUL","SEP","NOV","JAN","MAR","MAY"))

#remove first year (incopmlete year)
emergent.df <- 
  emergent.df %>% filter(HY != '95-96') %>% 
  dplyr::select(-Notes)


#this year and POR data frame
hy.now<- which(emergent.df$HY == this_report)
hy.POR<- which(emergent.df$HY != this_report)
emergent.POR<- emergent.df[-c(hy.now),]
emergent.now<- emergent.df[c(hy.now),]


# Emergent plots ####
#stems 
Emergent.stems_HY<-  ggplot(emergent.now, aes(x=site, y=STEMS, group=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of Stems" (~m^2)))

Emergent.stems_month<-  ggplot(emergent.now, aes(x=Month, y=STEMS, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of Stems" (~m^2)))  

Emergent.stems_season<-  ggplot(emergent.now, aes(x=Season, y=STEMS, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of Stems" (~m^2))) 


#SHOOTS 
Emergent.SHOOTS_HY<-  ggplot(emergent.now, aes(x=site, y=SHOOTS, group=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of shoots" (~m^2)))

Emergent.SHOOTS_month<-  ggplot(emergent.now, aes(x=Month, y=SHOOTS, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of shoots" (~m^2)))  

Emergent.SHOOTS_season<-  ggplot(emergent.now, aes(x=Season, y=SHOOTS, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  labs(x = "", y = expression ("Number of shoots" (~m^2))) 

#ratio 
Emergent.ratio_HY<-  ggplot(emergent.now, aes(x=site, y=ratio, group=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Ratio of Shoots to Stems") 

Emergent.ratio_month<-  ggplot(emergent.now, aes(x=Month, y=ratio, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Ratio of Shoots to Stems")   

Emergent.ratio_season<-  ggplot(emergent.now, aes(x=Season, y=ratio, fill=site)) + 
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","blue1","green1"))+
  scale_color_manual(values=c("red1","blue1","green1"))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Ratio of Shoots to Stems")  

#graph dump
Emergent.stems_HY
Emergent.stems_month
Emergent.stems_season
Emergent.ratio_HY
Emergent.ratio_month
Emergent.ratio_season

### Annual, wet/dry season - Exceedance curves  STEMS ####

TR.shoots_this_year_ann<- mean(emergent.now[emergent.now$site == "TR", ]$SHOOTS)
TR.ratio_this_year_ann<- mean(emergent.now[emergent.now$site == "TR", ]$ratio)
JB.shoots_this_year_ann<- mean(emergent.now[emergent.now$site == "JB", ]$SHOOTS)
JB.ratio_this_year_ann<- mean(emergent.now[emergent.now$site == "JB", ]$ratio)
HC.shoots_this_year_ann<- mean(emergent.now[emergent.now$site == "HC", ]$SHOOTS)
HC.ratio_this_year_ann<- mean(emergent.now[emergent.now$site == "HC", ]$ratio)
#TR POR shoots/ratio ####
#TR shoots
shoots.TR.ann <- ddply(emergent.df[emergent.df$site == "TR", ], .(HY, site), summarise, 
                       shoots = mean(SHOOTS, na.rm=TRUE ))
shoots.season.ex<- ddply(emergent.df[emergent.df$site == "TR", ], .(HY, Season, site), 
                         summarise, shoots = mean(SHOOTS, na.rm=TRUE ))
dry.ex.xyz <- which(shoots.season.ex$Season == "Dry Season")
shoots.TR.dry <- shoots.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(shoots.season.ex$Season == "Wet Season")
shoots.TR.wet <- shoots.season.ex[c(wet.ex.xyz),]

#TR ratio
ratio.TR.ann <- ddply(emergent.df[emergent.df$site == "TR", ], .(HY, site), summarise, 
                      ratio = mean(ratio, na.rm=TRUE ))
ratio.season.ex<- ddply(emergent.df[emergent.df$site == "TR", ], .(HY, Season, site), 
                        summarise, ratio = mean(ratio, na.rm=TRUE ))
dry.ex.xyz <- which(ratio.season.ex$Season == "Dry Season")
ratio.TR.dry <- ratio.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(ratio.season.ex$Season == "Wet Season")
ratio.TR.wet <- ratio.season.ex[c(wet.ex.xyz),]

#JB POR shoots/ratio ####
#JB shoots
shoots.JB.ann <- ddply(emergent.df[emergent.df$site == "JB", ], .(HY, site), summarise, 
                       shoots = mean(SHOOTS, na.rm=TRUE ))
shoots.season.ex<- ddply(emergent.df[emergent.df$site == "JB", ], .(HY, Season, site), 
                         summarise, shoots = mean(SHOOTS, na.rm=TRUE ))
dry.ex.xyz <- which(shoots.season.ex$Season == "Dry Season")
shoots.JB.dry <- shoots.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(shoots.season.ex$Season == "Wet Season")
shoots.JB.wet <- shoots.season.ex[c(wet.ex.xyz),]

#JB ratio
ratio.JB.ann <- ddply(emergent.df[emergent.df$site == "JB", ], .(HY, site), summarise, 
                      ratio = mean(ratio, na.rm=TRUE ))
ratio.season.ex<- ddply(emergent.df[emergent.df$site == "JB", ], .(HY, Season, site), 
                        summarise, ratio = mean(ratio, na.rm=TRUE ))
dry.ex.xyz <- which(ratio.season.ex$Season == "Dry Season")
ratio.JB.dry <- ratio.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(ratio.season.ex$Season == "Wet Season")
ratio.JB.wet <- ratio.season.ex[c(wet.ex.xyz),]

#HC POR shoots/ratio ####
#HC shoots
shoots.HC.ann <- ddply(emergent.df[emergent.df$site == "HC", ], .(HY, site), summarise, 
                       shoots = mean(SHOOTS, na.rm=TRUE ))
shoots.season.ex<- ddply(emergent.df[emergent.df$site == "HC", ], .(HY, Season, site), 
                         summarise, shoots = mean(SHOOTS, na.rm=TRUE ))
dry.ex.xyz <- which(shoots.season.ex$Season == "Dry Season")
shoots.HC.dry <- shoots.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(shoots.season.ex$Season == "Wet Season")
shoots.HC.wet <- shoots.season.ex[c(wet.ex.xyz),]

#HC ratio
ratio.HC.ann <- ddply(emergent.df[emergent.df$site == "HC", ], .(HY, site), summarise, 
                      ratio = mean(ratio, na.rm=TRUE ))
ratio.season.ex<- ddply(emergent.df[emergent.df$site == "HC", ], .(HY, Season, site), 
                        summarise, ratio = mean(ratio, na.rm=TRUE ))
dry.ex.xyz <- which(ratio.season.ex$Season == "Dry Season")
ratio.HC.dry <- ratio.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(ratio.season.ex$Season == "Wet Season")
ratio.HC.wet <- ratio.season.ex[c(wet.ex.xyz),]

#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(shoots.TR.ann[shoots.TR.ann$site == "TR", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(shoots.JB.ann[shoots.JB.ann$site == "JB", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(shoots.HC.ann[shoots.HC.ann$site == "HC", ]$HY)-1))
#sort data
TR.shoots.wet.ind<-order(-shoots.TR.wet$shoots)
TR.shoots.dry.ind<-order(-shoots.TR.dry$shoots)
TR.shoots.ann.ind<-order(-shoots.TR.ann$shoots)
TR.ratio.wet.ind<-order(-ratio.TR.wet$ratio)
TR.ratio.dry.ind<-order(-ratio.TR.dry$ratio)
TR.ratio.ann.ind<-order(-ratio.TR.ann$ratio)

JB.shoots.wet.ind<-order(-shoots.JB.wet$shoots)
JB.shoots.dry.ind<-order(-shoots.JB.dry$shoots)
JB.shoots.ann.ind<-order(-shoots.JB.ann$shoots)
JB.ratio.wet.ind<-order(-ratio.JB.wet$ratio)
JB.ratio.dry.ind<-order(-ratio.JB.dry$ratio)
JB.ratio.ann.ind<-order(-ratio.JB.ann$ratio)

HC.shoots.wet.ind<-order(-shoots.HC.wet$shoots)
HC.shoots.dry.ind<-order(-shoots.HC.dry$shoots)
HC.shoots.ann.ind<-order(-shoots.HC.ann$shoots)
HC.ratio.wet.ind<-order(-ratio.HC.wet$ratio)
HC.ratio.dry.ind<-order(-ratio.HC.dry$ratio)
HC.ratio.ann.ind<-order(-ratio.HC.ann$ratio)
#create dataframes for ggplot####
#ex.dat shoots TR

TR.shoots.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                                length(shoots.TR.wet$HY)),
                            HY=as.factor(shoots.TR.wet$HY[TR.shoots.wet.ind]), 
                            shoots=shoots.TR.wet$shoots[TR.shoots.wet.ind] )
TR.shoots.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                                length(shoots.TR.dry$HY)),
                            HY=as.factor(shoots.TR.dry$HY[TR.shoots.dry.ind]), 
                            shoots=shoots.TR.dry$shoots[TR.shoots.dry.ind] )
TR.shoots.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                                length(shoots.TR.ann$HY)),
                            HY=as.factor(shoots.TR.ann$HY[TR.shoots.ann.ind]), 
                            shoots=shoots.TR.ann$shoots[TR.shoots.ann.ind] )
TR.shoots.exceedance_df<- data.frame(bind_rows(TR.shoots.ann.df, TR.shoots.wet.df, TR.shoots.dry.df) )
TR.shoots.exceedance_df$Percent<- factor(TR.shoots.exceedance_df$Percent, 
                                         levels=label_percent(accuracy=1)(y.ex.TR))
TR.shoots.exceedance_df$What<- as.factor(TR.shoots.exceedance_df$What)
this_year_TR.shoots_df <- as.data.frame(TR.shoots.exceedance_df[TR.shoots.exceedance_df$HY == this_report, ])

#ex.dat shoots JB
JB.shoots.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                                length(shoots.JB.wet$HY)),
                            HY=as.factor(shoots.JB.wet$HY[JB.shoots.wet.ind]), 
                            shoots=shoots.JB.wet$shoots[JB.shoots.wet.ind] )
JB.shoots.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                                length(shoots.JB.dry$HY)),
                            HY=as.factor(shoots.JB.dry$HY[JB.shoots.dry.ind]), 
                            shoots=shoots.JB.dry$shoots[JB.shoots.dry.ind] )
JB.shoots.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                                length(shoots.JB.ann$HY)),
                            HY=as.factor(shoots.JB.ann$HY[JB.shoots.ann.ind]), 
                            shoots=shoots.JB.ann$shoots[JB.shoots.ann.ind] )
JB.shoots.exceedance_df<- data.frame(bind_rows(JB.shoots.ann.df, JB.shoots.wet.df, JB.shoots.dry.df) )
JB.shoots.exceedance_df$Percent<- factor(JB.shoots.exceedance_df$Percent, 
                                         levels=label_percent(accuracy=1)(y.ex.JB))
JB.shoots.exceedance_df$What<- as.factor(JB.shoots.exceedance_df$What)
this_year_JB.shoots_df <- as.data.frame(JB.shoots.exceedance_df[JB.shoots.exceedance_df$HY == this_report, ])

#ex.dat shoots HC
HC.shoots.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                                length(shoots.HC.wet$HY)),
                            HY=as.factor(shoots.HC.wet$HY[HC.shoots.wet.ind]), 
                            shoots=shoots.HC.wet$shoots[HC.shoots.wet.ind] )
HC.shoots.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                                length(shoots.HC.dry$HY)),
                            HY=as.factor(shoots.HC.dry$HY[HC.shoots.dry.ind]), 
                            shoots=shoots.HC.dry$shoots[HC.shoots.dry.ind] )
HC.shoots.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                                length(shoots.HC.ann$HY)),
                            HY=as.factor(shoots.HC.ann$HY[HC.shoots.ann.ind]), 
                            shoots=shoots.HC.ann$shoots[HC.shoots.ann.ind] )
HC.shoots.exceedance_df<- data.frame(bind_rows(HC.shoots.ann.df, HC.shoots.wet.df, HC.shoots.dry.df) )
HC.shoots.exceedance_df$Percent<- factor(HC.shoots.exceedance_df$Percent, 
                                         levels=label_percent(accuracy=1)(y.ex.HC))
HC.shoots.exceedance_df$What<- as.factor(HC.shoots.exceedance_df$What)
this_year_HC.shoots_df <- as.data.frame(HC.shoots.exceedance_df[HC.shoots.exceedance_df$HY == this_report, ])


#ex.dat ratio TR
TR.ratio.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                               length(ratio.TR.wet$HY)),
                           HY=as.factor(ratio.TR.wet$HY[TR.ratio.wet.ind]), 
                           ratio=ratio.TR.wet$ratio[TR.ratio.wet.ind] )
TR.ratio.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                               length(ratio.TR.dry$HY)),
                           HY=as.factor(ratio.TR.dry$HY[TR.ratio.dry.ind]), 
                           ratio=ratio.TR.dry$ratio[TR.ratio.dry.ind] )
TR.ratio.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                               length(ratio.TR.ann$HY)),
                           HY=as.factor(ratio.TR.ann$HY[TR.ratio.ann.ind]), 
                           ratio=ratio.TR.ann$ratio[TR.ratio.ann.ind] )
TR.ratio.exceedance_df<- data.frame(bind_rows(TR.ratio.ann.df, TR.ratio.wet.df, TR.ratio.dry.df) )
TR.ratio.exceedance_df$Percent<- factor(TR.ratio.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TR))
TR.ratio.exceedance_df$What<- as.factor(TR.ratio.exceedance_df$What)
this_year_TR.ratio_df <- as.data.frame(TR.ratio.exceedance_df[TR.ratio.exceedance_df$HY == this_report, ])

#ex.dat ratio JB
JB.ratio.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                               length(ratio.JB.wet$HY)),
                           HY=as.factor(ratio.JB.wet$HY[JB.ratio.wet.ind]), 
                           ratio=ratio.JB.wet$ratio[JB.ratio.wet.ind] )
JB.ratio.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                               length(ratio.JB.dry$HY)),
                           HY=as.factor(ratio.JB.dry$HY[JB.ratio.dry.ind]), 
                           ratio=ratio.JB.dry$ratio[JB.ratio.dry.ind] )
JB.ratio.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                               length(ratio.JB.ann$HY)),
                           HY=as.factor(ratio.JB.ann$HY[JB.ratio.ann.ind]), 
                           ratio=ratio.JB.ann$ratio[JB.ratio.ann.ind] )
JB.ratio.exceedance_df<- data.frame(bind_rows(JB.ratio.ann.df, JB.ratio.wet.df, JB.ratio.dry.df) )
JB.ratio.exceedance_df$Percent<- factor(JB.ratio.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.JB))
JB.ratio.exceedance_df$What<- as.factor(JB.ratio.exceedance_df$What)
this_year_JB.ratio_df <- as.data.frame(JB.ratio.exceedance_df[JB.ratio.exceedance_df$HY == this_report, ])

#ex.dat ratio HC
HC.ratio.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                               length(ratio.HC.wet$HY)),
                           HY=as.factor(ratio.HC.wet$HY[HC.ratio.wet.ind]), 
                           ratio=ratio.HC.wet$ratio[HC.ratio.wet.ind] )
HC.ratio.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                               length(ratio.HC.dry$HY)),
                           HY=as.factor(ratio.HC.dry$HY[HC.ratio.dry.ind]), 
                           ratio=ratio.HC.dry$ratio[HC.ratio.dry.ind] )
HC.ratio.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                               length(ratio.HC.ann$HY)),
                           HY=as.factor(ratio.HC.ann$HY[HC.ratio.ann.ind]), 
                           ratio=ratio.HC.ann$ratio[HC.ratio.ann.ind] )
HC.ratio.exceedance_df<- data.frame(bind_rows(HC.ratio.ann.df, HC.ratio.wet.df, HC.ratio.dry.df) )
HC.ratio.exceedance_df$Percent<- factor(HC.ratio.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.HC))
HC.ratio.exceedance_df$What<- as.factor(HC.ratio.exceedance_df$What)
this_year_HC.ratio_df <- as.data.frame(HC.ratio.exceedance_df[HC.ratio.exceedance_df$HY == this_report, ])


#EXCEEDANCE PLOTS####
#TR  
ex.TR.shoots <- ggplot(TR.shoots.exceedance_df, aes(x=Percent, y= shoots)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.shoots_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.shoots_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression ("Number of Shoots" (~m^2)))

ex.TR.ratio <- ggplot(TR.ratio.exceedance_df, aes(x=Percent, y= ratio)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.ratio_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.ratio_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Ratio of Shoots to Stems")

#JB  
ex.JB.shoots <- ggplot(JB.shoots.exceedance_df, aes(x=Percent, y= shoots)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.shoots_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.shoots_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression ("Number of Shoots" (~m^2)))

ex.JB.ratio <- ggplot(JB.ratio.exceedance_df, aes(x=Percent, y= ratio)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.ratio_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.ratio_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Ratio of Shoots to Stems")

#HC  
ex.HC.shoots <- ggplot(HC.shoots.exceedance_df, aes(x=Percent, y= shoots)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.shoots_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.shoots_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression ("Number of Shoots" (~m^2)))

ex.HC.ratio <- ggplot(HC.ratio.exceedance_df, aes(x=Percent, y= ratio)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.ratio_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.ratio_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Ratio of Shoots to Stems")

#exceedance dump
ex.TR.shoots
ex.TR.ratio
ex.JB.shoots
ex.JB.ratio
ex.HC.shoots
ex.HC.ratio



#graphs ####
ggarrange(Emergent.stems_HY, ggarrange(Emergent.stems_month, Emergent.stems_season, 
                                       ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(Emergent.ratio_HY, ggarrange(Emergent.ratio_month, Emergent.ratio_season, 
                                       ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(Emergent.SHOOTS_HY, ggarrange(Emergent.SHOOTS_month, Emergent.SHOOTS_season, 
                                       ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)



ggarrange(ex.TR.shoots, ex.JB.shoots,ex.HC.shoots, nrow = 1, ncol = 3, 
          labels=c("       A","       B","       C"), 
          vjust= 2, hjust = -0.5,  common.legend = TRUE )

ggarrange(ex.TR.ratio, ex.JB.ratio,ex.HC.ratio, nrow = 1, ncol = 3, 
          labels=c("      A","      B","      C"), 
          vjust= 2, hjust = -0.5,  common.legend = TRUE )


# data to send ------------------------------------------------------------

write.csv(
  emergent.now, 
  paste('/Databases/tidy/Reports/ACOE/Data to send/AUDUBON_EHV_data_20', 
        this_report, 
        '.csv',
        sep = ''))
