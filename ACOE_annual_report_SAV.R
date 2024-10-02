#SAV

#Warning, as SAV is qaqcd every 2months there might be next years HY included, eg. a report for 
#HY 2019-20, has HY 2020-21 included in the data, this needs to be removed for exceedance curves to work

library("readr") 
library("arsenal")
library("tidyverse")
library("kableExtra")
library("huxtable") 
library("gtools")
library("ggtext")
library("ggplot2")
library("plyr")
library("dplyr")
library("data.table")
library("scales")
library("ggrepel")
library("Hmisc")
library("ggpmisc")
library("hablar")
library("ggpubr")
library("Rmisc")
library('lubridate')
library('readxl')

#chose HY, remove any successive HY  
this_report<- "2022-23" 
remv.hy<-"2023-24"         #line 197 if active 





#Upload sav data ####
TR1<- read_excel('/PLANTS/TR/TR1.xls', skip = 2) %>% 
  mutate(site = 'TR1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR2<- read_excel('/PLANTS/TR/TR2.xls', skip = 2) %>% 
  mutate(site = 'TR2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR3<- read_excel('/PLANTS/TR/TR3.xls', skip = 2) %>% 
  mutate(site = 'TR3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR4A<- read_excel('/PLANTS/TR/TR4A.xls', skip = 2) %>% 
  mutate(site = 'TR4A', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR5<- read_excel('/PLANTS/TR/TR5.xls', skip = 2) %>% 
  mutate(site = 'TR5', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR6<- read_excel('/PLANTS/TR/TR6.xls', skip = 2)%>% 
  mutate(site = 'TR6', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
TR_SAV<- bind_rows(TR1,TR2,TR3,TR4A,TR5,TR6) %>% 
  mutate(area = 'TR', 
         region = 'LMB') 


EC1<- read_excel('/PLANTS/EC/EC1.xls', skip = 2) %>% 
  mutate(site = 'EC1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
EC2<- read_excel('/PLANTS/EC/EC2.xls', skip = 2) %>% 
  mutate(site = 'EC2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
EC3<- read_excel('/PLANTS/EC/EC3.xls', skip = 2) %>% 
  mutate(site = 'EC3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
EC_SAV<- bind_rows(EC1,EC2,EC3)%>% 
  mutate(area = 'EC', 
         region = 'LMB') 


WJ1<- read_excel('/PLANTS/WJ/WJ1.xls', skip = 2) %>% 
  mutate(site = 'WJ1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
WJ2<- read_excel('/PLANTS/WJ/WJ2.xls', skip = 2) %>% 
  mutate(site = 'WJ2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
WJ_SAV<- bind_rows(WJ1,WJ2) %>% 
  mutate(area = 'WJ', 
         region = 'TC')


JB1<- read_excel('/PLANTS/JB/JB1.xls', skip = 2) %>% 
  mutate(site = 'JB1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
JB2<- read_excel('/PLANTS/JB/JB2.xls', skip = 2) %>% 
  mutate(site = 'JB2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
JB3<- read_excel('/PLANTS/JB/JB3.xls', skip = 2) %>% 
  mutate(site = 'JB3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
JB4<- read_excel('/PLANTS/JB/JB4.xls', skip = 2) %>% 
  mutate(site = 'JB4', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
JB5<- read_excel('/PLANTS/JB/JB5.xls', skip = 2) %>% 
  mutate(site = 'JB5', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH)) %>% 
  dplyr::select(-"...19")
JB6<- read_excel('/PLANTS/JB/JB6.xls', skip = 2) %>% 
  mutate(site = 'JB6', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
JB_SAV<- bind_rows(JB1,JB2,JB3,JB4,JB5,JB6) %>% 
  mutate(area = 'JB', 
         region = 'TC')


SB1<- read_excel('/PLANTS/SB/SB1.xls', skip = 2) %>% 
  mutate(site = 'SB1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
SB2<- read_excel('/PLANTS/SB/SB2.xls', skip = 2) %>% 
  mutate(site = 'SB2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
SB3<- read_excel('/PLANTS/SB/SB3.xls', skip = 2) %>% 
  mutate(site = 'SB3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
SB_SAV<- bind_rows(SB1,SB2,SB3) %>% 
  mutate(area = 'SB', 
         region = 'LS')


HC1<- read_excel('/PLANTS/HC/HC1.xls', skip = 2) %>% 
  mutate(site = 'HC1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC1A<- read_excel('/PLANTS/HC/HC1A.xls', skip = 2) %>% 
  mutate(site = 'HC1A', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC2<- read_excel('/PLANTS/HC/HC2.xls', skip = 2) %>% 
  mutate(site = 'HC2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC3<- read_excel('/PLANTS/HC/HC3.xls', skip = 2) %>% 
  mutate(site = 'HC3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC4A<- read_excel('/PLANTS/HC/HC4A.xls', skip = 2) %>% 
  mutate(site = 'HC4A', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC5<- read_excel('/PLANTS/HC/HC5.xls', skip = 2) %>% 
  mutate(site = 'HC5', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC6<- read_excel('/PLANTS/HC/HC6.xls', skip = 2) %>% 
  mutate(site = 'HC6', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
HC_SAV<- bind_rows(HC1, HC1A,HC2,HC3,HC4A,HC5,HC6) %>% 
  mutate(area = 'HC', 
         region = 'LS')


MB1<- read_excel('/PLANTS/MB/MB1.xls', skip = 2) %>% 
  mutate(site = 'MB1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
MB2<- read_excel('/PLANTS/MB/MB2.xls', skip = 2) %>% 
  mutate(site = 'MB2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
MB3<- read_excel('/PLANTS/MB/MB3.xls', skip = 2) %>% 
  mutate(site = 'MB3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
MB_SAV<- bind_rows(MB1,MB2, MB3) %>% 
  mutate(area = 'MB', 
         region = 'SBB')

BS1<- read_excel('/PLANTS/BS/BS1.xls', skip = 2) %>% 
  mutate(site = 'BS1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
BS2<- read_excel('/PLANTS/BS/BS2.xls', skip = 2) %>% 
  mutate(site = 'BS2', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
BS3<- read_excel('/PLANTS/BS/BS3.xls', skip = 2) %>% 
  mutate(site = 'BS3', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
BS4<- read_excel('/PLANTS/BS/BS4.xls', skip = 2) %>% 
  mutate(site = 'BS4', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH))
BS_SAV<- bind_rows(BS1,BS2, BS3, BS4) %>% 
  mutate(area = 'BS', 
         region = 'SBB')


CS_SAV<- read_excel('/PLANTS/CS/CS1.xls', skip = 2) %>% 
  mutate(site = 'CS1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH), 
         area = 'CS', 
         region = 'SBB')


TP_SAV<- read_excel('/PLANTS/TP/TP1.xls', skip = 2) %>% 
  mutate(site = 'TP1', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH), 
         area = 'TP', 
         region = 'TP')


SevenP_SAV<- read_excel('/PLANTS/7P/7P.xls', skip = 2) %>% 
  mutate(site = '7P', 
         TEMP = as.numeric(TEMP),
         SALINITY = as.numeric(SALINITY),
         DEPTH = as.numeric(DEPTH), 
         area = '7P', 
         region = '7P')

#structure new data frame ####

all_SAV<- bind_rows(TR_SAV,EC_SAV,WJ_SAV,
                    JB_SAV,SB_SAV,HC_SAV,
                    MB_SAV,BS_SAV %>% 
                      filter(`Hydro. Year` != '1996-97'),
                    CS_SAV,TP_SAV) %>% 
  dplyr::select(-c(notes, `...26`)) %>% 
  dplyr::rename(HY = `Hydro. Year`,
                Utric = `Utr sp.`,
                Rup = `Rup mar`,
                Chara = `Cha hor`, 
                Halo = `Hal wri`,
                Naja = `Naj mar`,
                Bat = `Bat sp.`,
                Clad = `Cla sp.`,
                Nit = `Nit sp.`,
                Spiro = `Spi sp.`,
                Ace = `Ace sp.`,
                Sara = `Sar sp.`,
                Day = `Das sp.`,
                Thal = `Tha tes`,
                Poly = `Pol sp.`,
                Lau = `Lau sp.`,
                Pen = `Pen sp.`,
                Udo = `Udo sp.`,
                Hali = `Hali sp.`,
                Ulv = `Ulv sp.`,
                Cau = `Cau sp.`,
                Rhi = `Rhi sp.`) %>% 
                #Unk = `Unk sp.` 
  relocate(c(site,area,region), .after = HY) %>% 
  mutate_at(vars(TOTAL:Rhi), as.numeric) %>%  
  mutate_at(vars(TOTAL:Rhi), ~ . * 4) %>% 
  mutate(HY = if_else(ymd(Date)> ymd('2000-06-01'), 
                      paste('20', HY, sep = ""), 
                      paste('19', HY, sep = "") ),
         HY = if_else(HY == '1900-01', 
                      '2000-01', 
                      HY ),
         area = factor(area, levels = c("TR","EC","WJ","JB","SB",
                                        "HC","MB","BS","CS", "TP")), 
         site = factor(site, 
                       levels = c("TR1","TR2","TR3","TR4A","TR5","TR6", 
                                  "EC1","EC2","EC3",
                                  "WJ1","WJ2","JB1","JB2","JB3","JB4","JB5","JB6",
                                  "SB1","SB2","SB3",
                                  "HC1","HC1A","HC2","HC3","HC4A","HC5","HC6",
                                  "MB1","MB2","MB3","BS1","BS2","BS3","BS4","CS1",
                                  'TP1')),
         Month = factor(toupper(month.abb[month(Date)]),
                        levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                   'DEC',"JAN","FEB","MAR",'APR',"MAY")),
         region = factor(region, levels = c("LMB","TC","LS","SBB", "TP")),
         Season = 
           if_else(month(Date) >= 6 & month(Date) <= 11, 
                   'Wet Season',
                   'Dry Season'),
         Season = factor(Season, levels = c('Wet Season', 'Dry Season'))) %>% 
           as.data.frame() %>% 
           filter(HY != '1995-96') %>% 
  filter(HY != '1996-97') %>% 
  filter(HY != '1997-98') %>% 
  mutate(Month = if_else(Month == 'DEC', 'NOV', Month))



#will need this if next years hydro year data has started to come in
#remv.hy <- which(all_SAV$HY == remove_report)
all_SAV <- all_SAV %>% 
  filter(HY != remv.hy) %>% 
  filter(HY != '2024-25')

SAV.now<- all_SAV %>% 
  filter(HY == this_report)
SAV.POR<- all_SAV %>% 
  filter(HY != this_report)

#ddply(SAV.now, .(Year, Month, site), summarise, total = mean(TOTAL, na.rm=TRUE))

#SAV graphs ####
#TOTAL ####
SAV.all_total.cover_HY<-  ggplot(SAV.now, aes(x=site, y=TOTAL, group=site)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4" ,"orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow3","yellow1","yellow4" ,"orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage") 

SAV.all_total.cover_month<-  ggplot(SAV.now, aes(x=Month, y=TOTAL, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4" ,"orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4" ,"orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage")   

SAV.all_total.cover_season<-  ggplot(SAV.now, aes(x=Season, y=TOTAL, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4" ,"orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4" ,"orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage")   

#Utric ####
SAV.all_Utric.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Utric, group=site)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4" ,"orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.all_Utric.cover_month<-  ggplot(SAV.now, aes(x=Month, y=Utric, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

SAV.all_Utric.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Utric, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

#Rupia ####
SAV.all_Rupia.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Rup, group=site)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.all_Rupia.cover_month<-  ggplot(SAV.now, aes(x=Month, y=Rup, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())   

SAV.all_Rupia.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Rup, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())


#Chara ####
SAV.all_Chara.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Chara, group=site)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.all_Chara.cover_month<-  ggplot(SAV.now, aes(x=Month, y=Chara, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

SAV.all_Chara.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Chara, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown()) 

#halo ####
SAV.all_Halo.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Halo, group=site)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4","orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4","orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown()) 

SAV.all_Halo.cover_month<-  ggplot(SAV.now, aes(x=Month, y=Halo, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())    

SAV.all_Halo.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Halo, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4", "orange4"))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4", "orange4"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  



#graph dup ####
SAV.all_total.cover_HY
SAV.all_total.cover_month
SAV.all_total.cover_season

SAV.all_Utric.cover_HY
SAV.all_Utric.cover_month
SAV.all_Utric.cover_season

SAV.all_Rupia.cover_HY
SAV.all_Rupia.cover_month
SAV.all_Rupia.cover_season

SAV.all_Chara.cover_HY
SAV.all_Chara.cover_month
SAV.all_Chara.cover_season

SAV.all_Halo.cover_HY
SAV.all_Halo.cover_month
SAV.all_Halo.cover_season
#####
#exceedance curves are a bitch ####
#Exceedance Curves Depth and Sal

SAV.now.wet<- SAV.now[SAV.now$Season == "Wet Season", ]
SAV.now.dry<- SAV.now[SAV.now$Season == "Dry Season", ]

# this year mean annual and seasonal TOTAL NUMBERS
#LMB TOTAL ####
#TR
TR.total_this_year_ann<- mean(SAV.now[SAV.now$site == "TR1", ]$TOTAL)
TR.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "TR1", ]$TOTAL)
TR.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "TR1", ]$TOTAL)
#EC
EC.total_this_year_ann<- mean(SAV.now[SAV.now$site == "EC1", ]$TOTAL)
EC.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "EC1", ]$TOTAL)
EC.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "EC1", ]$TOTAL)
#TC total####
#WJ
WJ.total_this_year_ann<- mean(SAV.now[SAV.now$site == "WJ1", ]$TOTAL)
WJ.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "WJ1", ]$TOTAL)
WJ.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "WJ1", ]$TOTAL)
#JB
JB.total_this_year_ann<- mean(SAV.now[SAV.now$site == "JB1", ]$TOTAL)
JB.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "JB1", ]$TOTAL)
JB.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "JB1", ]$TOTAL)
#LS total####
#SB
SB.total_this_year_ann<- mean(SAV.now[SAV.now$site == "SB1", ]$TOTAL)
SB.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "SB1", ]$TOTAL)
SB.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "SB1", ]$TOTAL)
#HC
HC.total_this_year_ann<- mean(SAV.now[SAV.now$site == "HC1A", ]$TOTAL)
HC.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "HC1A", ]$TOTAL)
HC.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "HC1A", ]$TOTAL)
#SBB total####
#MB
MB.total_this_year_ann<- mean(SAV.now[SAV.now$site == "MB1", ]$TOTAL)
MB.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "MB1", ]$TOTAL)
MB.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "MB1", ]$TOTAL)
#BS
BS.total_this_year_ann<- mean(SAV.now[SAV.now$site == "BS1", ]$TOTAL)
BS.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "BS1", ]$TOTAL)
BS.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "BS1", ]$TOTAL)
#CS
CS.total_this_year_ann<- mean(SAV.now[SAV.now$site == "CS1", ]$TOTAL)
CS.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "CS1", ]$TOTAL)
CS.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "CS1", ]$TOTAL)

#mean all other years mean annaul and seasons total sav ####
#LMB POR TOTAL ####
#TR 
total.TR.ann <- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.TR.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.TR.wet <- total.season.ex[c(wet.ex.xyz),]
#EC 
total.EC.ann <- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.EC.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.EC.wet <- total.season.ex[c(wet.ex.xyz),]
#TC POR total####
#JB
total.JB.ann <- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.JB.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.JB.wet <- total.season.ex[c(wet.ex.xyz),]
#WJ
total.WJ.ann <- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.WJ.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.WJ.wet <- total.season.ex[c(wet.ex.xyz),]
#LS POR total ####
#SB
total.SB.ann <- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.SB.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.SB.wet <- total.season.ex[c(wet.ex.xyz),]
#HC
total.HC.ann <- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.HC.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.HC.wet <- total.season.ex[c(wet.ex.xyz),]
#SBB POR total####
#MB
total.MB.ann <- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.MB.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.MB.wet <- total.season.ex[c(wet.ex.xyz),]
#BS
total.BS.ann <- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.BS.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.BS.wet <- total.season.ex[c(wet.ex.xyz),]
#CS
total.CS.ann <- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.CS.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.CS.wet <- total.season.ex[c(wet.ex.xyz),]

#data file exceedance depth ####

#drop HY?
#remove unused years 
#drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
#long sites
#depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
#depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
#depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]

ann.total.ex<- rbind(total.TR.ann, total.EC.ann,total.JB.ann, total.WJ.ann, 
                     total.SB.ann, total.HC.ann,total.MB.ann, total.BS.ann, total.CS.ann)
wet.total.ex<- rbind(total.TR.wet, total.EC.wet,total.JB.wet, total.WJ.wet, 
                     total.SB.wet, total.HC.wet,total.MB.wet, total.BS.wet, total.CS.wet)
dry.total.ex<- rbind(total.TR.dry, total.EC.dry,total.JB.dry, total.WJ.dry, 
                     total.SB.dry, total.HC.dry,total.MB.dry, total.BS.dry, total.CS.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "CS1", ]$HY)-1))

#create an ind by sorting total; largest to smallest for wet, dry and annual variable ####
#LMB total####
#TR
TR.total.wet.ind<-order(-total.TR.wet$total)
TR.total.dry.ind<-order(-total.TR.dry$total)
TR.total.ann.ind<-order(-total.TR.ann$total)
#EC
EC.total.wet.ind<-order(-total.EC.wet$total)
EC.total.dry.ind<-order(-total.EC.dry$total)
EC.total.ann.ind<-order(-total.EC.ann$total)
#TC total####
#WJ
WJ.total.wet.ind<-order(-total.WJ.wet$total)
WJ.total.dry.ind<-order(-total.WJ.dry$total)
WJ.total.ann.ind<-order(-total.WJ.ann$total)
#JB
JB.total.wet.ind<-order(-total.JB.wet$total)
JB.total.dry.ind<-order(-total.JB.dry$total)
JB.total.ann.ind<-order(-total.JB.ann$total)
#LS total####
#SB
SB.total.wet.ind<-order(-total.SB.wet$total)
SB.total.dry.ind<-order(-total.SB.dry$total)
SB.total.ann.ind<-order(-total.SB.ann$total)
#HC
HC.total.wet.ind<-order(-total.HC.wet$total)
HC.total.dry.ind<-order(-total.HC.dry$total)
HC.total.ann.ind<-order(-total.HC.ann$total)
#SBB total####
#MB
MB.total.wet.ind<-order(-total.MB.wet$total)
MB.total.dry.ind<-order(-total.MB.dry$total)
MB.total.ann.ind<-order(-total.MB.ann$total)
#BS
BS.total.wet.ind<-order(-total.BS.wet$total)
BS.total.dry.ind<-order(-total.BS.dry$total)
BS.total.ann.ind<-order(-total.BS.ann$total)
#CS
CS.total.wet.ind<-order(-total.CS.wet$total)
CS.total.dry.ind<-order(-total.CS.dry$total)
CS.total.ann.ind<-order(-total.CS.ann$total)
#create dataframes for ggplot####
#ex.dat total LMB####
#TR
TR.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                               length(total.TR.wet$HY)),
                           HY=as.factor(total.TR.wet$HY[TR.total.wet.ind]), 
                           total=total.TR.wet$total[TR.total.wet.ind] )
TR.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                               length(total.TR.dry$HY)),
                           HY=as.factor(total.TR.dry$HY[TR.total.dry.ind]), 
                           total=total.TR.dry$total[TR.total.dry.ind] )
TR.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                               length(total.TR.ann$HY)),
                           HY=as.factor(total.TR.ann$HY[TR.total.ann.ind]), 
                           total=total.TR.ann$total[TR.total.ann.ind] )
TR.total.exceedance_df<- data.frame(bind_rows(TR.total.ann.df, TR.total.wet.df, TR.total.dry.df) )
TR.total.exceedance_df$Percent<- factor(TR.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TR))
TR.total.exceedance_df$What<- as.factor(TR.total.exceedance_df$What)
this_year_TR.total_df <- as.data.frame(TR.total.exceedance_df[TR.total.exceedance_df$HY == this_report, ])
#EC
EC.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                               length(total.EC.wet$HY)),
                           HY=as.factor(total.EC.wet$HY[EC.total.wet.ind]), 
                           total=total.EC.wet$total[EC.total.wet.ind] )
EC.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                               length(total.EC.dry$HY)),
                           HY=as.factor(total.EC.dry$HY[EC.total.dry.ind]), 
                           total=total.EC.dry$total[EC.total.dry.ind] )
EC.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                               length(total.EC.ann$HY)),
                           HY=as.factor(total.EC.ann$HY[EC.total.ann.ind]), 
                           total=total.EC.ann$total[EC.total.ann.ind] )
EC.total.exceedance_df<- data.frame(bind_rows(EC.total.ann.df, EC.total.wet.df, EC.total.dry.df) )
EC.total.exceedance_df$Percent<- factor(EC.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.EC))
EC.total.exceedance_df$What<- as.factor(EC.total.exceedance_df$What)
this_year_EC.total_df <- as.data.frame(EC.total.exceedance_df[EC.total.exceedance_df$HY == this_report, ])
#ex.dat total TC####
#WJ
WJ.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                               length(total.WJ.wet$HY)),
                           HY=as.factor(total.WJ.wet$HY[WJ.total.wet.ind]), 
                           total=total.WJ.wet$total[WJ.total.wet.ind] )
WJ.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                               length(total.WJ.dry$HY)),
                           HY=as.factor(total.WJ.dry$HY[WJ.total.dry.ind]), 
                           total=total.WJ.dry$total[WJ.total.dry.ind] )
WJ.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                               length(total.WJ.ann$HY)),
                           HY=as.factor(total.WJ.ann$HY[WJ.total.ann.ind]), 
                           total=total.WJ.ann$total[WJ.total.ann.ind] )
WJ.total.exceedance_df<- data.frame(bind_rows(WJ.total.ann.df, WJ.total.wet.df, WJ.total.dry.df) )
WJ.total.exceedance_df$Percent<- factor(WJ.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.total.exceedance_df$What<- as.factor(WJ.total.exceedance_df$What)
this_year_WJ.total_df <- as.data.frame(WJ.total.exceedance_df[WJ.total.exceedance_df$HY == this_report, ])
#JB
JB.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                               length(total.JB.wet$HY)),
                           HY=as.factor(total.JB.wet$HY[JB.total.wet.ind]), 
                           total=total.JB.wet$total[JB.total.wet.ind] )
JB.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                               length(total.JB.dry$HY)),
                           HY=as.factor(total.JB.dry$HY[JB.total.dry.ind]), 
                           total=total.JB.dry$total[JB.total.dry.ind] )
JB.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                               length(total.JB.ann$HY)),
                           HY=as.factor(total.JB.ann$HY[JB.total.ann.ind]), 
                           total=total.JB.ann$total[JB.total.ann.ind] )
JB.total.exceedance_df<- data.frame(bind_rows(JB.total.ann.df, JB.total.wet.df, JB.total.dry.df) )
JB.total.exceedance_df$Percent<- factor(JB.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.JB))
JB.total.exceedance_df$What<- as.factor(JB.total.exceedance_df$What)
this_year_JB.total_df <- as.data.frame(JB.total.exceedance_df[JB.total.exceedance_df$HY == this_report, ])
#ex.dat total LS####
#SB
SB.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                               length(total.SB.wet$HY)),
                           HY=as.factor(total.SB.wet$HY[SB.total.wet.ind]), 
                           total=total.SB.wet$total[SB.total.wet.ind] )
SB.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                               length(total.SB.dry$HY)),
                           HY=as.factor(total.SB.dry$HY[SB.total.dry.ind]), 
                           total=total.SB.dry$total[SB.total.dry.ind] )
SB.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                               length(total.SB.ann$HY)),
                           HY=as.factor(total.SB.ann$HY[SB.total.ann.ind]), 
                           total=total.SB.ann$total[SB.total.ann.ind] )
SB.total.exceedance_df<- data.frame(bind_rows(SB.total.ann.df, SB.total.wet.df, SB.total.dry.df) )
SB.total.exceedance_df$Percent<- factor(SB.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.SB))
SB.total.exceedance_df$What<- as.factor(SB.total.exceedance_df$What)
this_year_SB.total_df <- as.data.frame(SB.total.exceedance_df[SB.total.exceedance_df$HY == this_report, ])
#HC
HC.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                               length(total.HC.wet$HY)),
                           HY=as.factor(total.HC.wet$HY[HC.total.wet.ind]), 
                           total=total.HC.wet$total[HC.total.wet.ind] )
HC.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                               length(total.HC.dry$HY)),
                           HY=as.factor(total.HC.dry$HY[HC.total.dry.ind]), 
                           total=total.HC.dry$total[HC.total.dry.ind] )
HC.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                               length(total.HC.ann$HY)),
                           HY=as.factor(total.HC.ann$HY[HC.total.ann.ind]), 
                           total=total.HC.ann$total[HC.total.ann.ind] )
HC.total.exceedance_df<- data.frame(bind_rows(HC.total.ann.df, HC.total.wet.df, HC.total.dry.df) )
HC.total.exceedance_df$Percent<- factor(HC.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.HC))
HC.total.exceedance_df$What<- as.factor(HC.total.exceedance_df$What)
this_year_HC.total_df <- as.data.frame(HC.total.exceedance_df[HC.total.exceedance_df$HY == this_report, ])
#ex.dat total SBB####
#MB
MB.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                               length(total.MB.wet$HY)),
                           HY=as.factor(total.MB.wet$HY[MB.total.wet.ind]), 
                           total=total.MB.wet$total[MB.total.wet.ind] )
MB.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                               length(total.MB.dry$HY)),
                           HY=as.factor(total.MB.dry$HY[MB.total.dry.ind]), 
                           total=total.MB.dry$total[MB.total.dry.ind] )
MB.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                               length(total.MB.ann$HY)),
                           HY=as.factor(total.MB.ann$HY[MB.total.ann.ind]), 
                           total=total.MB.ann$total[MB.total.ann.ind] )
MB.total.exceedance_df<- data.frame(bind_rows(MB.total.ann.df, MB.total.wet.df, MB.total.dry.df) )
MB.total.exceedance_df$Percent<- factor(MB.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.MB))
MB.total.exceedance_df$What<- as.factor(MB.total.exceedance_df$What)
this_year_MB.total_df <- as.data.frame(MB.total.exceedance_df[MB.total.exceedance_df$HY == this_report, ])
#BS
BS.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                               length(total.BS.wet$HY)),
                           HY=as.factor(total.BS.wet$HY[BS.total.wet.ind]), 
                           total=total.BS.wet$total[BS.total.wet.ind] )
BS.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                               length(total.BS.dry$HY)),
                           HY=as.factor(total.BS.dry$HY[BS.total.dry.ind]), 
                           total=total.BS.dry$total[BS.total.dry.ind] )
BS.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                               length(total.BS.ann$HY)),
                           HY=as.factor(total.BS.ann$HY[BS.total.ann.ind]), 
                           total=total.BS.ann$total[BS.total.ann.ind] )
BS.total.exceedance_df<- data.frame(bind_rows(BS.total.ann.df, BS.total.wet.df, BS.total.dry.df) )
BS.total.exceedance_df$Percent<- factor(BS.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.BS))
BS.total.exceedance_df$What<- as.factor(BS.total.exceedance_df$What)
this_year_BS.total_df <- as.data.frame(BS.total.exceedance_df[BS.total.exceedance_df$HY == this_report, ])
#CS
CS.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                               length(total.CS.wet$HY)),
                           HY=as.factor(total.CS.wet$HY[CS.total.wet.ind]), 
                           total=total.CS.wet$total[CS.total.wet.ind] )
CS.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                               length(total.CS.dry$HY)),
                           HY=as.factor(total.CS.dry$HY[CS.total.dry.ind]), 
                           total=total.CS.dry$total[CS.total.dry.ind] )
CS.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                               length(total.CS.ann$HY)),
                           HY=as.factor(total.CS.ann$HY[CS.total.ann.ind]), 
                           total=total.CS.ann$total[CS.total.ann.ind] )
CS.total.exceedance_df<- data.frame(bind_rows(CS.total.ann.df, CS.total.wet.df, CS.total.dry.df) )
CS.total.exceedance_df$Percent<- factor(CS.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.CS))
CS.total.exceedance_df$What<- as.factor(CS.total.exceedance_df$What)
this_year_CS.total_df <- as.data.frame(CS.total.exceedance_df[CS.total.exceedance_df$HY == this_report, ])


# this year mean annual and seasonal UTRIC NUMBERS
#LMB utric ####
#TR
TR.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "TR1", ]$Utric)
TR.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "TR1", ]$Utric)
TR.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "TR1", ]$Utric)
#EC
EC.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "EC1", ]$Utric)
EC.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "EC1", ]$Utric)
EC.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "EC1", ]$Utric)
#TC utric####
#WJ
WJ.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "WJ1", ]$Utric)
WJ.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "WJ1", ]$Utric)
WJ.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "WJ1", ]$Utric)
#JB
JB.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "JB1", ]$Utric)
JB.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "JB1", ]$Utric)
JB.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "JB1", ]$Utric)
#LS utric####
#SB
SB.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "SB1", ]$Utric)
SB.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "SB1", ]$Utric)
SB.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "SB1", ]$Utric)
#HC
HC.depth_this_year_ann<- mean(SAV.now[SAV.now$site == "HC1A", ]$Utric)
HC.depth_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "HC1A", ]$Utric)
HC.depth_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "HC1A", ]$Utric)
#SBB utric####
#MB
MB.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "MB1", ]$Utric)
MB.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "MB1", ]$Utric)
MB.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "MB1", ]$Utric)
#BS
BS.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "BS1", ]$Utric)
BS.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "BS1", ]$Utric)
BS.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "BS1", ]$Utric)
#CS
CS.utric_this_year_ann<- mean(SAV.now[SAV.now$site == "CS1", ]$Utric)
CS.utric_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "CS1", ]$Utric)
CS.utric_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "CS1", ]$Utric)

#mean all other years mean annaul and seasons utric sav ####
#LMB POR utric ####
#TR 
utric.TR.ann <- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.TR.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.TR.wet <- utric.season.ex[c(wet.ex.xyz),]
#EC 
utric.EC.ann <- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.EC.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.EC.wet <- utric.season.ex[c(wet.ex.xyz),]
#TC POR utric####
#JB
utric.JB.ann <- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.JB.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.JB.wet <- utric.season.ex[c(wet.ex.xyz),]
#WJ
utric.WJ.ann <- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.WJ.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.WJ.wet <- utric.season.ex[c(wet.ex.xyz),]
#LS POR utric ####
#SB
utric.SB.ann <- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.SB.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.SB.wet <- utric.season.ex[c(wet.ex.xyz),]
#HC
utric.HC.ann <- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.HC.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.HC.wet <- utric.season.ex[c(wet.ex.xyz),]
#SBB POR utric####
#MB
utric.MB.ann <- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.MB.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.MB.wet <- utric.season.ex[c(wet.ex.xyz),]
#BS
utric.BS.ann <- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.BS.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.BS.wet <- utric.season.ex[c(wet.ex.xyz),]
#CS
utric.CS.ann <- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, site), summarise, utric = mean(Utric))
utric.season.ex<- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, Season, site), summarise, utric = mean(Utric))
dry.ex.xyz <- which(utric.season.ex$Season == "Dry Season")
utric.CS.dry <- utric.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(utric.season.ex$Season == "Wet Season")
utric.CS.wet <- utric.season.ex[c(wet.ex.xyz),]

#data file exceedance utric ####

#drop HY?
#remove unused years 
#drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
#long sites
#depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
#depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
#depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]

ann.utric.ex<- rbind(utric.TR.ann, utric.EC.ann,utric.JB.ann, utric.WJ.ann, 
                     utric.SB.ann, utric.HC.ann,utric.MB.ann, utric.BS.ann, utric.CS.ann)
wet.utric.ex<- rbind(utric.TR.wet, utric.EC.wet,utric.JB.wet, utric.WJ.wet, 
                     utric.SB.wet, utric.HC.wet,utric.MB.wet, utric.BS.wet, utric.CS.wet)
dry.utric.ex<- rbind(utric.TR.dry, utric.EC.dry,utric.JB.dry, utric.WJ.dry, 
                     utric.SB.dry, utric.HC.dry,utric.MB.dry, utric.BS.dry, utric.CS.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.utric.ex[ann.utric.ex$site == "CS1", ]$HY)-1))

#create an ind by sorting utric; largest to smallest for wet, dry and annual variable ####
#LMB utric####
#TR
TR.utric.wet.ind<-order(-utric.TR.wet$utric)
TR.utric.dry.ind<-order(-utric.TR.dry$utric)
TR.utric.ann.ind<-order(-utric.TR.ann$utric)
#EC
EC.utric.wet.ind<-order(-utric.EC.wet$utric)
EC.utric.dry.ind<-order(-utric.EC.dry$utric)
EC.utric.ann.ind<-order(-utric.EC.ann$utric)
#TC utric####
#WJ
WJ.utric.wet.ind<-order(-utric.WJ.wet$utric)
WJ.utric.dry.ind<-order(-utric.WJ.dry$utric)
WJ.utric.ann.ind<-order(-utric.WJ.ann$utric)
#JB
JB.utric.wet.ind<-order(-utric.JB.wet$utric)
JB.utric.dry.ind<-order(-utric.JB.dry$utric)
JB.utric.ann.ind<-order(-utric.JB.ann$utric)
#LS utric####
#SB
SB.utric.wet.ind<-order(-utric.SB.wet$utric)
SB.utric.dry.ind<-order(-utric.SB.dry$utric)
SB.utric.ann.ind<-order(-utric.SB.ann$utric)
#HC
HC.utric.wet.ind<-order(-utric.HC.wet$utric)
HC.utric.dry.ind<-order(-utric.HC.dry$utric)
HC.utric.ann.ind<-order(-utric.HC.ann$utric)
#SBB utric####
#MB
MB.utric.wet.ind<-order(-utric.MB.wet$utric)
MB.utric.dry.ind<-order(-utric.MB.dry$utric)
MB.utric.ann.ind<-order(-utric.MB.ann$utric)
#BS
BS.utric.wet.ind<-order(-utric.BS.wet$utric)
BS.utric.dry.ind<-order(-utric.BS.dry$utric)
BS.utric.ann.ind<-order(-utric.BS.ann$utric)
#CS
CS.utric.wet.ind<-order(-utric.CS.wet$utric)
CS.utric.dry.ind<-order(-utric.CS.dry$utric)
CS.utric.ann.ind<-order(-utric.CS.ann$utric)
#create dataframes for ggplot####
#ex.dat utric LMB####
#TR
TR.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                               length(utric.TR.wet$HY)),
                           HY=as.factor(utric.TR.wet$HY[TR.utric.wet.ind]), 
                           utric=utric.TR.wet$utric[TR.utric.wet.ind] )
TR.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                               length(utric.TR.dry$HY)),
                           HY=as.factor(utric.TR.dry$HY[TR.utric.dry.ind]), 
                           utric=utric.TR.dry$utric[TR.utric.dry.ind] )
TR.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                               length(utric.TR.ann$HY)),
                           HY=as.factor(utric.TR.ann$HY[TR.utric.ann.ind]), 
                           utric=utric.TR.ann$utric[TR.utric.ann.ind] )
TR.utric.exceedance_df<- data.frame(bind_rows(TR.utric.ann.df, TR.utric.wet.df, TR.utric.dry.df) )
TR.utric.exceedance_df$Percent<- factor(TR.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TR))
TR.utric.exceedance_df$What<- as.factor(TR.utric.exceedance_df$What)
this_year_TR.utric_df <- as.data.frame(TR.utric.exceedance_df[TR.utric.exceedance_df$HY == this_report, ])
#EC
EC.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                               length(utric.EC.wet$HY)),
                           HY=as.factor(utric.EC.wet$HY[EC.utric.wet.ind]), 
                           utric=utric.EC.wet$utric[EC.utric.wet.ind] )
EC.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                               length(utric.EC.dry$HY)),
                           HY=as.factor(utric.EC.dry$HY[EC.utric.dry.ind]), 
                           utric=utric.EC.dry$utric[EC.utric.dry.ind] )
EC.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                               length(utric.EC.ann$HY)),
                           HY=as.factor(utric.EC.ann$HY[EC.utric.ann.ind]), 
                           utric=utric.EC.ann$utric[EC.utric.ann.ind] )
EC.utric.exceedance_df<- data.frame(bind_rows(EC.utric.ann.df, EC.utric.wet.df, EC.utric.dry.df) )
EC.utric.exceedance_df$Percent<- factor(EC.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.EC))
EC.utric.exceedance_df$What<- as.factor(EC.utric.exceedance_df$What)
this_year_EC.utric_df <- as.data.frame(EC.utric.exceedance_df[EC.utric.exceedance_df$HY == this_report, ])
#ex.dat utric TC####
#WJ
WJ.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                               length(utric.WJ.wet$HY)),
                           HY=as.factor(utric.WJ.wet$HY[WJ.utric.wet.ind]), 
                           utric=utric.WJ.wet$utric[WJ.utric.wet.ind] )
WJ.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                               length(utric.WJ.dry$HY)),
                           HY=as.factor(utric.WJ.dry$HY[WJ.utric.dry.ind]), 
                           utric=utric.WJ.dry$utric[WJ.utric.dry.ind] )
WJ.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                               length(utric.WJ.ann$HY)),
                           HY=as.factor(utric.WJ.ann$HY[WJ.utric.ann.ind]), 
                           utric=utric.WJ.ann$utric[WJ.utric.ann.ind] )
WJ.utric.exceedance_df<- data.frame(bind_rows(WJ.utric.ann.df, WJ.utric.wet.df, WJ.utric.dry.df) )
WJ.utric.exceedance_df$Percent<- factor(WJ.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.utric.exceedance_df$What<- as.factor(WJ.utric.exceedance_df$What)
this_year_WJ.utric_df <- as.data.frame(WJ.utric.exceedance_df[WJ.utric.exceedance_df$HY == this_report, ])
#JB
JB.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                               length(utric.JB.wet$HY)),
                           HY=as.factor(utric.JB.wet$HY[JB.utric.wet.ind]), 
                           utric=utric.JB.wet$utric[JB.utric.wet.ind] )
JB.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                               length(utric.JB.dry$HY)),
                           HY=as.factor(utric.JB.dry$HY[JB.utric.dry.ind]), 
                           utric=utric.JB.dry$utric[JB.utric.dry.ind] )
JB.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                               length(utric.JB.ann$HY)),
                           HY=as.factor(utric.JB.ann$HY[JB.utric.ann.ind]), 
                           utric=utric.JB.ann$utric[JB.utric.ann.ind] )
JB.utric.exceedance_df<- data.frame(bind_rows(JB.utric.ann.df, JB.utric.wet.df, JB.utric.dry.df) )
JB.utric.exceedance_df$Percent<- factor(JB.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.JB))
JB.utric.exceedance_df$What<- as.factor(JB.utric.exceedance_df$What)
this_year_JB.utric_df <- as.data.frame(JB.utric.exceedance_df[JB.utric.exceedance_df$HY == this_report, ])
#ex.dat utric LS####
#SB
SB.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                               length(utric.SB.wet$HY)),
                           HY=as.factor(utric.SB.wet$HY[SB.utric.wet.ind]), 
                           utric=utric.SB.wet$utric[SB.utric.wet.ind] )
SB.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                               length(utric.SB.dry$HY)),
                           HY=as.factor(utric.SB.dry$HY[SB.utric.dry.ind]), 
                           utric=utric.SB.dry$utric[SB.utric.dry.ind] )
SB.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                               length(utric.SB.ann$HY)),
                           HY=as.factor(utric.SB.ann$HY[SB.utric.ann.ind]), 
                           utric=utric.SB.ann$utric[SB.utric.ann.ind] )
SB.utric.exceedance_df<- data.frame(bind_rows(SB.utric.ann.df, SB.utric.wet.df, SB.utric.dry.df) )
SB.utric.exceedance_df$Percent<- factor(SB.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.SB))
SB.utric.exceedance_df$What<- as.factor(SB.utric.exceedance_df$What)
this_year_SB.utric_df <- as.data.frame(SB.utric.exceedance_df[SB.utric.exceedance_df$HY == this_report, ])
#HC
HC.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                               length(utric.HC.wet$HY)),
                           HY=as.factor(utric.HC.wet$HY[HC.utric.wet.ind]), 
                           utric=utric.HC.wet$utric[HC.utric.wet.ind] )
HC.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                               length(utric.HC.dry$HY)),
                           HY=as.factor(utric.HC.dry$HY[HC.utric.dry.ind]), 
                           utric=utric.HC.dry$utric[HC.utric.dry.ind] )
HC.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                               length(utric.HC.ann$HY)),
                           HY=as.factor(utric.HC.ann$HY[HC.utric.ann.ind]), 
                           utric=utric.HC.ann$utric[HC.utric.ann.ind] )
HC.utric.exceedance_df<- data.frame(bind_rows(HC.utric.ann.df, HC.utric.wet.df, HC.utric.dry.df) )
HC.utric.exceedance_df$Percent<- factor(HC.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.HC))
HC.utric.exceedance_df$What<- as.factor(HC.utric.exceedance_df$What)
this_year_HC.utric_df <- as.data.frame(HC.utric.exceedance_df[HC.utric.exceedance_df$HY == this_report, ])
#ex.dat utric SBB####
#MB
MB.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                               length(utric.MB.wet$HY)),
                           HY=as.factor(utric.MB.wet$HY[MB.utric.wet.ind]), 
                           utric=utric.MB.wet$utric[MB.utric.wet.ind] )
MB.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                               length(utric.MB.dry$HY)),
                           HY=as.factor(utric.MB.dry$HY[MB.utric.dry.ind]), 
                           utric=utric.MB.dry$utric[MB.utric.dry.ind] )
MB.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                               length(utric.MB.ann$HY)),
                           HY=as.factor(utric.MB.ann$HY[MB.utric.ann.ind]), 
                           utric=utric.MB.ann$utric[MB.utric.ann.ind] )
MB.utric.exceedance_df<- data.frame(bind_rows(MB.utric.ann.df, MB.utric.wet.df, MB.utric.dry.df) )
MB.utric.exceedance_df$Percent<- factor(MB.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.MB))
MB.utric.exceedance_df$What<- as.factor(MB.utric.exceedance_df$What)
this_year_MB.utric_df <- as.data.frame(MB.utric.exceedance_df[MB.utric.exceedance_df$HY == this_report, ])
#BS
BS.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                               length(utric.BS.wet$HY)),
                           HY=as.factor(utric.BS.wet$HY[BS.utric.wet.ind]), 
                           utric=utric.BS.wet$utric[BS.utric.wet.ind] )
BS.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                               length(utric.BS.dry$HY)),
                           HY=as.factor(utric.BS.dry$HY[BS.utric.dry.ind]), 
                           utric=utric.BS.dry$utric[BS.utric.dry.ind] )
BS.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                               length(utric.BS.ann$HY)),
                           HY=as.factor(utric.BS.ann$HY[BS.utric.ann.ind]), 
                           utric=utric.BS.ann$utric[BS.utric.ann.ind] )
BS.utric.exceedance_df<- data.frame(bind_rows(BS.utric.ann.df, BS.utric.wet.df, BS.utric.dry.df) )
BS.utric.exceedance_df$Percent<- factor(BS.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.BS))
BS.utric.exceedance_df$What<- as.factor(BS.utric.exceedance_df$What)
this_year_BS.utric_df <- as.data.frame(BS.utric.exceedance_df[BS.utric.exceedance_df$HY == this_report, ])
#CS
CS.utric.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                               length(utric.CS.wet$HY)),
                           HY=as.factor(utric.CS.wet$HY[CS.utric.wet.ind]), 
                           utric=utric.CS.wet$utric[CS.utric.wet.ind] )
CS.utric.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                               length(utric.CS.dry$HY)),
                           HY=as.factor(utric.CS.dry$HY[CS.utric.dry.ind]), 
                           utric=utric.CS.dry$utric[CS.utric.dry.ind] )
CS.utric.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                               length(utric.CS.ann$HY)),
                           HY=as.factor(utric.CS.ann$HY[CS.utric.ann.ind]), 
                           utric=utric.CS.ann$utric[CS.utric.ann.ind] )
CS.utric.exceedance_df<- data.frame(bind_rows(CS.utric.ann.df, CS.utric.wet.df, CS.utric.dry.df) )
CS.utric.exceedance_df$Percent<- factor(CS.utric.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.CS))
CS.utric.exceedance_df$What<- as.factor(CS.utric.exceedance_df$What)
this_year_CS.utric_df <- as.data.frame(CS.utric.exceedance_df[CS.utric.exceedance_df$HY == this_report, ])



# this year mean annual and seasonal CHARA NUMBERS
#LMB chara ####
#TR
TR.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "TR1", ]$Chara)
TR.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "TR1", ]$Chara)
TR.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "TR1", ]$Chara)
#EC
EC.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "EC1", ]$Chara)
EC.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "EC1", ]$Chara)
EC.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "EC1", ]$Chara)
#TC chara####
#WJ
WJ.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "WJ1", ]$Chara)
WJ.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "WJ1", ]$Chara)
WJ.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "WJ1", ]$Chara)
#JB
JB.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "JB1", ]$Chara)
JB.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "JB1", ]$Chara)
JB.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "JB1", ]$Chara)
#LS chara####
#SB
SB.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "SB1", ]$Chara)
SB.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "SB1", ]$Chara)
SB.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "SB1", ]$Chara)
#HC
HC.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "HC1A", ]$Chara)
HC.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "HC1A", ]$Chara)
HC.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "HC1A", ]$Chara)
#SBB chara####
#MB
MB.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "MB1", ]$Chara)
MB.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "MB1", ]$Chara)
MB.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "MB1", ]$Chara)
#BS
BS.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "BS1", ]$Chara)
BS.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "BS1", ]$Chara)
BS.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "BS1", ]$Chara)
#CS
CS.chara_this_year_ann<- mean(SAV.now[SAV.now$site == "CS1", ]$Chara)
CS.chara_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "CS1", ]$Chara)
CS.chara_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "CS1", ]$Chara)

#mean all other years mean annaul and seasons chara sav ####
#LMB POR chara ####
#TR 
chara.TR.ann <- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.TR.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.TR.wet <- chara.season.ex[c(wet.ex.xyz),]
#EC 
chara.EC.ann <- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.EC.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.EC.wet <- chara.season.ex[c(wet.ex.xyz),]
#TC POR chara####
#JB
chara.JB.ann <- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.JB.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.JB.wet <- chara.season.ex[c(wet.ex.xyz),]
#WJ
chara.WJ.ann <- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.WJ.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.WJ.wet <- chara.season.ex[c(wet.ex.xyz),]
#LS POR chara ####
#SB
chara.SB.ann <- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.SB.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.SB.wet <- chara.season.ex[c(wet.ex.xyz),]
#HC
chara.HC.ann <- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.HC.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.HC.wet <- chara.season.ex[c(wet.ex.xyz),]
#SBB POR chara####
#MB
chara.MB.ann <- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.MB.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.MB.wet <- chara.season.ex[c(wet.ex.xyz),]
#BS
chara.BS.ann <- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.BS.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.BS.wet <- chara.season.ex[c(wet.ex.xyz),]
#CS
chara.CS.ann <- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, site), summarise, chara = mean(Chara))
chara.season.ex<- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, Season, site), summarise, chara = mean(Chara))
dry.ex.xyz <- which(chara.season.ex$Season == "Dry Season")
chara.CS.dry <- chara.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(chara.season.ex$Season == "Wet Season")
chara.CS.wet <- chara.season.ex[c(wet.ex.xyz),]

#data file exceedance depth ####

#drop HY?
#remove unused years 
#drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
#long sites
#depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
#depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
#depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]

ann.chara.ex<- rbind(chara.TR.ann, chara.EC.ann,chara.JB.ann, chara.WJ.ann, 
                     chara.SB.ann, chara.HC.ann,chara.MB.ann, chara.BS.ann, chara.CS.ann)
wet.chara.ex<- rbind(chara.TR.wet, chara.EC.wet,chara.JB.wet, chara.WJ.wet, 
                     chara.SB.wet, chara.HC.wet,chara.MB.wet, chara.BS.wet, chara.CS.wet)
dry.chara.ex<- rbind(chara.TR.dry, chara.EC.dry,chara.JB.dry, chara.WJ.dry, 
                     chara.SB.dry, chara.HC.dry,chara.MB.dry, chara.BS.dry, chara.CS.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.chara.ex[ann.chara.ex$site == "CS1", ]$HY)-1))

#create an ind by sorting chara; largest to smallest for wet, dry and annual variable ####
#LMB chara####
#TR
TR.chara.wet.ind<-order(-chara.TR.wet$chara)
TR.chara.dry.ind<-order(-chara.TR.dry$chara)
TR.chara.ann.ind<-order(-chara.TR.ann$chara)
#EC
EC.chara.wet.ind<-order(-chara.EC.wet$chara)
EC.chara.dry.ind<-order(-chara.EC.dry$chara)
EC.chara.ann.ind<-order(-chara.EC.ann$chara)
#TC chara####
#WJ
WJ.chara.wet.ind<-order(-chara.WJ.wet$chara)
WJ.chara.dry.ind<-order(-chara.WJ.dry$chara)
WJ.chara.ann.ind<-order(-chara.WJ.ann$chara)
#JB
JB.chara.wet.ind<-order(-chara.JB.wet$chara)
JB.chara.dry.ind<-order(-chara.JB.dry$chara)
JB.chara.ann.ind<-order(-chara.JB.ann$chara)
#LS chara####
#SB
SB.chara.wet.ind<-order(-chara.SB.wet$chara)
SB.chara.dry.ind<-order(-chara.SB.dry$chara)
SB.chara.ann.ind<-order(-chara.SB.ann$chara)
#HC
HC.chara.wet.ind<-order(-chara.HC.wet$chara)
HC.chara.dry.ind<-order(-chara.HC.dry$chara)
HC.chara.ann.ind<-order(-chara.HC.ann$chara)
#SBB chara####
#MB
MB.chara.wet.ind<-order(-chara.MB.wet$chara)
MB.chara.dry.ind<-order(-chara.MB.dry$chara)
MB.chara.ann.ind<-order(-chara.MB.ann$chara)
#BS
BS.chara.wet.ind<-order(-chara.BS.wet$chara)
BS.chara.dry.ind<-order(-chara.BS.dry$chara)
BS.chara.ann.ind<-order(-chara.BS.ann$chara)
#CS
CS.chara.wet.ind<-order(-chara.CS.wet$chara)
CS.chara.dry.ind<-order(-chara.CS.dry$chara)
CS.chara.ann.ind<-order(-chara.CS.ann$chara)
#create dataframes for ggplot####
#ex.dat chara LMB####
#TR
TR.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                               length(chara.TR.wet$HY)),
                           HY=as.factor(chara.TR.wet$HY[TR.chara.wet.ind]), 
                           chara=chara.TR.wet$chara[TR.chara.wet.ind] )
TR.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                               length(chara.TR.dry$HY)),
                           HY=as.factor(chara.TR.dry$HY[TR.chara.dry.ind]), 
                           chara=chara.TR.dry$chara[TR.chara.dry.ind] )
TR.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                               length(chara.TR.ann$HY)),
                           HY=as.factor(chara.TR.ann$HY[TR.chara.ann.ind]), 
                           chara=chara.TR.ann$chara[TR.chara.ann.ind] )
TR.chara.exceedance_df<- data.frame(bind_rows(TR.chara.ann.df, TR.chara.wet.df, TR.chara.dry.df) )
TR.chara.exceedance_df$Percent<- factor(TR.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TR))
TR.chara.exceedance_df$What<- as.factor(TR.chara.exceedance_df$What)
this_year_TR.chara_df <- as.data.frame(TR.chara.exceedance_df[TR.chara.exceedance_df$HY == this_report, ])
#EC
EC.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                               length(chara.EC.wet$HY)),
                           HY=as.factor(chara.EC.wet$HY[EC.chara.wet.ind]), 
                           chara=chara.EC.wet$chara[EC.chara.wet.ind] )
EC.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                               length(chara.EC.dry$HY)),
                           HY=as.factor(chara.EC.dry$HY[EC.chara.dry.ind]), 
                           chara=chara.EC.dry$chara[EC.chara.dry.ind] )
EC.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                               length(chara.EC.ann$HY)),
                           HY=as.factor(chara.EC.ann$HY[EC.chara.ann.ind]), 
                           chara=chara.EC.ann$chara[EC.chara.ann.ind] )
EC.chara.exceedance_df<- data.frame(bind_rows(EC.chara.ann.df, EC.chara.wet.df, EC.chara.dry.df) )
EC.chara.exceedance_df$Percent<- factor(EC.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.EC))
EC.chara.exceedance_df$What<- as.factor(EC.chara.exceedance_df$What)
this_year_EC.chara_df <- as.data.frame(EC.chara.exceedance_df[EC.chara.exceedance_df$HY == this_report, ])
#ex.dat chara TC####
#WJ
WJ.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                               length(chara.WJ.wet$HY)),
                           HY=as.factor(chara.WJ.wet$HY[WJ.chara.wet.ind]), 
                           chara=chara.WJ.wet$chara[WJ.chara.wet.ind] )
WJ.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                               length(chara.WJ.dry$HY)),
                           HY=as.factor(chara.WJ.dry$HY[WJ.chara.dry.ind]), 
                           chara=chara.WJ.dry$chara[WJ.chara.dry.ind] )
WJ.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                               length(chara.WJ.ann$HY)),
                           HY=as.factor(chara.WJ.ann$HY[WJ.chara.ann.ind]), 
                           chara=chara.WJ.ann$chara[WJ.chara.ann.ind] )
WJ.chara.exceedance_df<- data.frame(bind_rows(WJ.chara.ann.df, WJ.chara.wet.df, WJ.chara.dry.df) )
WJ.chara.exceedance_df$Percent<- factor(WJ.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.chara.exceedance_df$What<- as.factor(WJ.chara.exceedance_df$What)
this_year_WJ.chara_df <- as.data.frame(WJ.chara.exceedance_df[WJ.chara.exceedance_df$HY == this_report, ])
#JB
JB.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                               length(chara.JB.wet$HY)),
                           HY=as.factor(chara.JB.wet$HY[JB.chara.wet.ind]), 
                           chara=chara.JB.wet$chara[JB.chara.wet.ind] )
JB.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                               length(chara.JB.dry$HY)),
                           HY=as.factor(chara.JB.dry$HY[JB.chara.dry.ind]), 
                           chara=chara.JB.dry$chara[JB.chara.dry.ind] )
JB.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                               length(chara.JB.ann$HY)),
                           HY=as.factor(chara.JB.ann$HY[JB.chara.ann.ind]), 
                           chara=chara.JB.ann$chara[JB.chara.ann.ind] )
JB.chara.exceedance_df<- data.frame(bind_rows(JB.chara.ann.df, JB.chara.wet.df, JB.chara.dry.df) )
JB.chara.exceedance_df$Percent<- factor(JB.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.JB))
JB.chara.exceedance_df$What<- as.factor(JB.chara.exceedance_df$What)
this_year_JB.chara_df <- as.data.frame(JB.chara.exceedance_df[JB.chara.exceedance_df$HY == this_report, ])
#ex.dat chara LS####
#SB
SB.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                               length(chara.SB.wet$HY)),
                           HY=as.factor(chara.SB.wet$HY[SB.chara.wet.ind]), 
                           chara=chara.SB.wet$chara[SB.chara.wet.ind] )
SB.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                               length(chara.SB.dry$HY)),
                           HY=as.factor(chara.SB.dry$HY[SB.chara.dry.ind]), 
                           chara=chara.SB.dry$chara[SB.chara.dry.ind] )
SB.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                               length(chara.SB.ann$HY)),
                           HY=as.factor(chara.SB.ann$HY[SB.chara.ann.ind]), 
                           chara=chara.SB.ann$chara[SB.chara.ann.ind] )
SB.chara.exceedance_df<- data.frame(bind_rows(SB.chara.ann.df, SB.chara.wet.df, SB.chara.dry.df) )
SB.chara.exceedance_df$Percent<- factor(SB.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.SB))
SB.chara.exceedance_df$What<- as.factor(SB.chara.exceedance_df$What)
this_year_SB.chara_df <- as.data.frame(SB.chara.exceedance_df[SB.chara.exceedance_df$HY == this_report, ])
#HC
HC.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                               length(chara.HC.wet$HY)),
                           HY=as.factor(chara.HC.wet$HY[HC.chara.wet.ind]), 
                           chara=chara.HC.wet$chara[HC.chara.wet.ind] )
HC.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                               length(chara.HC.dry$HY)),
                           HY=as.factor(chara.HC.dry$HY[HC.chara.dry.ind]), 
                           chara=chara.HC.dry$chara[HC.chara.dry.ind] )
HC.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                               length(chara.HC.ann$HY)),
                           HY=as.factor(chara.HC.ann$HY[HC.chara.ann.ind]), 
                           chara=chara.HC.ann$chara[HC.chara.ann.ind] )
HC.chara.exceedance_df<- data.frame(bind_rows(HC.chara.ann.df, HC.chara.wet.df, HC.chara.dry.df) )
HC.chara.exceedance_df$Percent<- factor(HC.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.HC))
HC.chara.exceedance_df$What<- as.factor(HC.chara.exceedance_df$What)
this_year_HC.chara_df <- as.data.frame(HC.chara.exceedance_df[HC.chara.exceedance_df$HY == this_report, ])
#ex.dat chara SBB####
#MB
MB.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                               length(chara.MB.wet$HY)),
                           HY=as.factor(chara.MB.wet$HY[MB.chara.wet.ind]), 
                           chara=chara.MB.wet$chara[MB.chara.wet.ind] )
MB.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                               length(chara.MB.dry$HY)),
                           HY=as.factor(chara.MB.dry$HY[MB.chara.dry.ind]), 
                           chara=chara.MB.dry$chara[MB.chara.dry.ind] )
MB.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                               length(chara.MB.ann$HY)),
                           HY=as.factor(chara.MB.ann$HY[MB.chara.ann.ind]), 
                           chara=chara.MB.ann$chara[MB.chara.ann.ind] )
MB.chara.exceedance_df<- data.frame(bind_rows(MB.chara.ann.df, MB.chara.wet.df, MB.chara.dry.df) )
MB.chara.exceedance_df$Percent<- factor(MB.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.MB))
MB.chara.exceedance_df$What<- as.factor(MB.chara.exceedance_df$What)
this_year_MB.chara_df <- as.data.frame(MB.chara.exceedance_df[MB.chara.exceedance_df$HY == this_report, ])
#BS
BS.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                               length(chara.BS.wet$HY)),
                           HY=as.factor(chara.BS.wet$HY[BS.chara.wet.ind]), 
                           chara=chara.BS.wet$chara[BS.chara.wet.ind] )
BS.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                               length(chara.BS.dry$HY)),
                           HY=as.factor(chara.BS.dry$HY[BS.chara.dry.ind]), 
                           chara=chara.BS.dry$chara[BS.chara.dry.ind] )
BS.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                               length(chara.BS.ann$HY)),
                           HY=as.factor(chara.BS.ann$HY[BS.chara.ann.ind]), 
                           chara=chara.BS.ann$chara[BS.chara.ann.ind] )
BS.chara.exceedance_df<- data.frame(bind_rows(BS.chara.ann.df, BS.chara.wet.df, BS.chara.dry.df) )
BS.chara.exceedance_df$Percent<- factor(BS.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.BS))
BS.chara.exceedance_df$What<- as.factor(BS.chara.exceedance_df$What)
this_year_BS.chara_df <- as.data.frame(BS.chara.exceedance_df[BS.chara.exceedance_df$HY == this_report, ])
#CS
CS.chara.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                               length(chara.CS.wet$HY)),
                           HY=as.factor(chara.CS.wet$HY[CS.chara.wet.ind]), 
                           chara=chara.CS.wet$chara[CS.chara.wet.ind] )
CS.chara.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                               length(chara.CS.dry$HY)),
                           HY=as.factor(chara.CS.dry$HY[CS.chara.dry.ind]), 
                           chara=chara.CS.dry$chara[CS.chara.dry.ind] )
CS.chara.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                               length(chara.CS.ann$HY)),
                           HY=as.factor(chara.CS.ann$HY[CS.chara.ann.ind]), 
                           chara=chara.CS.ann$chara[CS.chara.ann.ind] )
CS.chara.exceedance_df<- data.frame(bind_rows(CS.chara.ann.df, CS.chara.wet.df, CS.chara.dry.df) )
CS.chara.exceedance_df$Percent<- factor(CS.chara.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.CS))
CS.chara.exceedance_df$What<- as.factor(CS.chara.exceedance_df$What)
this_year_CS.chara_df <- as.data.frame(CS.chara.exceedance_df[CS.chara.exceedance_df$HY == this_report, ])




# this year mean annual and seasonal rup NUMBERS
#LMB rup ####
#TR
TR.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "TR1", ]$Rup)
TR.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "TR1", ]$Rup)
TR.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "TR1", ]$Rup)
#EC
EC.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "EC1", ]$Rup)
EC.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "EC1", ]$Rup)
EC.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "EC1", ]$Rup)
#TC rup####
#WJ
WJ.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "WJ1", ]$Rup)
WJ.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "WJ1", ]$Rup)
WJ.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "WJ1", ]$Rup)
#JB
JB.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "JB1", ]$Rup)
JB.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "JB1", ]$Rup)
JB.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "JB1", ]$Rup)
#LS rup####
#SB
SB.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "SB1", ]$Rup)
SB.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "SB1", ]$Rup)
SB.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "SB1", ]$Rup)
#HC
HC.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "HC1A", ]$Rup)
HC.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "HC1A", ]$Rup)
HC.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "HC1A", ]$Rup)
#SBB rup####
#MB
MB.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "MB1", ]$Rup)
MB.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "MB1", ]$Rup)
MB.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "MB1", ]$Rup)
#BS
BS.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "BS1", ]$Rup)
BS.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "BS1", ]$Rup)
BS.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "BS1", ]$Rup)
#CS
CS.rup_this_year_ann<- mean(SAV.now[SAV.now$site == "CS1", ]$Rup)
CS.rup_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "CS1", ]$Rup)
CS.rup_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "CS1", ]$Rup)

#mean all other years mean annaul and seasons rup sav ####
#LMB POR rup ####
#TR 
rup.TR.ann <- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.TR.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.TR.wet <- rup.season.ex[c(wet.ex.xyz),]
#EC 
rup.EC.ann <- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.EC.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.EC.wet <- rup.season.ex[c(wet.ex.xyz),]
#TC POR rup####
#JB
rup.JB.ann <- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.JB.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.JB.wet <- rup.season.ex[c(wet.ex.xyz),]
#WJ
rup.WJ.ann <- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.WJ.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.WJ.wet <- rup.season.ex[c(wet.ex.xyz),]
#LS POR rup ####
#SB
rup.SB.ann <- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.SB.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.SB.wet <- rup.season.ex[c(wet.ex.xyz),]
#HC
rup.HC.ann <- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.HC.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.HC.wet <- rup.season.ex[c(wet.ex.xyz),]
#SBB POR rup####
#MB
rup.MB.ann <- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.MB.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.MB.wet <- rup.season.ex[c(wet.ex.xyz),]
#BS
rup.BS.ann <- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.BS.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.BS.wet <- rup.season.ex[c(wet.ex.xyz),]
#CS
rup.CS.ann <- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, site), summarise, rup = mean(Rup))
rup.season.ex<- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, Season, site), summarise, rup = mean(Rup))
dry.ex.xyz <- which(rup.season.ex$Season == "Dry Season")
rup.CS.dry <- rup.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(rup.season.ex$Season == "Wet Season")
rup.CS.wet <- rup.season.ex[c(wet.ex.xyz),]

#data file exceedance rup ####

#drop HY?
#remove unused years 
#drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
#long sites
#depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
#depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
#depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]

ann.rup.ex<- rbind(rup.TR.ann, rup.EC.ann,rup.JB.ann, rup.WJ.ann, 
                   rup.SB.ann, rup.HC.ann,rup.MB.ann, rup.BS.ann, rup.CS.ann)
wet.rup.ex<- rbind(rup.TR.wet, rup.EC.wet,rup.JB.wet, rup.WJ.wet, 
                   rup.SB.wet, rup.HC.wet,rup.MB.wet, rup.BS.wet, rup.CS.wet)
dry.rup.ex<- rbind(rup.TR.dry, rup.EC.dry,rup.JB.dry, rup.WJ.dry, 
                   rup.SB.dry, rup.HC.dry,rup.MB.dry, rup.BS.dry, rup.CS.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.rup.ex[ann.rup.ex$site == "CS1", ]$HY)-1))

#create an ind by sorting rup; largest to smallest for wet, dry and annual variable ####
#LMB rup####
#TR
TR.rup.wet.ind<-order(-rup.TR.wet$rup)
TR.rup.dry.ind<-order(-rup.TR.dry$rup)
TR.rup.ann.ind<-order(-rup.TR.ann$rup)
#EC
EC.rup.wet.ind<-order(-rup.EC.wet$rup)
EC.rup.dry.ind<-order(-rup.EC.dry$rup)
EC.rup.ann.ind<-order(-rup.EC.ann$rup)
#TC rup####
#WJ
WJ.rup.wet.ind<-order(-rup.WJ.wet$rup)
WJ.rup.dry.ind<-order(-rup.WJ.dry$rup)
WJ.rup.ann.ind<-order(-rup.WJ.ann$rup)
#JB
JB.rup.wet.ind<-order(-rup.JB.wet$rup)
JB.rup.dry.ind<-order(-rup.JB.dry$rup)
JB.rup.ann.ind<-order(-rup.JB.ann$rup)
#LS rup####
#SB
SB.rup.wet.ind<-order(-rup.SB.wet$rup)
SB.rup.dry.ind<-order(-rup.SB.dry$rup)
SB.rup.ann.ind<-order(-rup.SB.ann$rup)
#HC
HC.rup.wet.ind<-order(-rup.HC.wet$rup)
HC.rup.dry.ind<-order(-rup.HC.dry$rup)
HC.rup.ann.ind<-order(-rup.HC.ann$rup)
#SBB rup####
#MB
MB.rup.wet.ind<-order(-rup.MB.wet$rup)
MB.rup.dry.ind<-order(-rup.MB.dry$rup)
MB.rup.ann.ind<-order(-rup.MB.ann$rup)
#BS
BS.rup.wet.ind<-order(-rup.BS.wet$rup)
BS.rup.dry.ind<-order(-rup.BS.dry$rup)
BS.rup.ann.ind<-order(-rup.BS.ann$rup)
#CS
CS.rup.wet.ind<-order(-rup.CS.wet$rup)
CS.rup.dry.ind<-order(-rup.CS.dry$rup)
CS.rup.ann.ind<-order(-rup.CS.ann$rup)
#create dataframes for ggplot####
#ex.dat rup LMB####
#TR
TR.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                             length(rup.TR.wet$HY)),
                         HY=as.factor(rup.TR.wet$HY[TR.rup.wet.ind]), 
                         rup=rup.TR.wet$rup[TR.rup.wet.ind] )
TR.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                             length(rup.TR.dry$HY)),
                         HY=as.factor(rup.TR.dry$HY[TR.rup.dry.ind]), 
                         rup=rup.TR.dry$rup[TR.rup.dry.ind] )
TR.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                             length(rup.TR.ann$HY)),
                         HY=as.factor(rup.TR.ann$HY[TR.rup.ann.ind]), 
                         rup=rup.TR.ann$rup[TR.rup.ann.ind] )
TR.rup.exceedance_df<- data.frame(bind_rows(TR.rup.ann.df, TR.rup.wet.df, TR.rup.dry.df) )
TR.rup.exceedance_df$Percent<- factor(TR.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.TR))
TR.rup.exceedance_df$What<- as.factor(TR.rup.exceedance_df$What)
this_year_TR.rup_df <- as.data.frame(TR.rup.exceedance_df[TR.rup.exceedance_df$HY == this_report, ])
#EC
EC.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                             length(rup.EC.wet$HY)),
                         HY=as.factor(rup.EC.wet$HY[EC.rup.wet.ind]), 
                         rup=rup.EC.wet$rup[EC.rup.wet.ind] )
EC.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                             length(rup.EC.dry$HY)),
                         HY=as.factor(rup.EC.dry$HY[EC.rup.dry.ind]), 
                         rup=rup.EC.dry$rup[EC.rup.dry.ind] )
EC.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                             length(rup.EC.ann$HY)),
                         HY=as.factor(rup.EC.ann$HY[EC.rup.ann.ind]), 
                         rup=rup.EC.ann$rup[EC.rup.ann.ind] )
EC.rup.exceedance_df<- data.frame(bind_rows(EC.rup.ann.df, EC.rup.wet.df, EC.rup.dry.df) )
EC.rup.exceedance_df$Percent<- factor(EC.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.EC))
EC.rup.exceedance_df$What<- as.factor(EC.rup.exceedance_df$What)
this_year_EC.rup_df <- as.data.frame(EC.rup.exceedance_df[EC.rup.exceedance_df$HY == this_report, ])
#ex.dat rup TC####
#WJ
WJ.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                             length(rup.WJ.wet$HY)),
                         HY=as.factor(rup.WJ.wet$HY[WJ.rup.wet.ind]), 
                         rup=rup.WJ.wet$rup[WJ.rup.wet.ind] )
WJ.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                             length(rup.WJ.dry$HY)),
                         HY=as.factor(rup.WJ.dry$HY[WJ.rup.dry.ind]), 
                         rup=rup.WJ.dry$rup[WJ.rup.dry.ind] )
WJ.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                             length(rup.WJ.ann$HY)),
                         HY=as.factor(rup.WJ.ann$HY[WJ.rup.ann.ind]), 
                         rup=rup.WJ.ann$rup[WJ.rup.ann.ind] )
WJ.rup.exceedance_df<- data.frame(bind_rows(WJ.rup.ann.df, WJ.rup.wet.df, WJ.rup.dry.df) )
WJ.rup.exceedance_df$Percent<- factor(WJ.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.rup.exceedance_df$What<- as.factor(WJ.rup.exceedance_df$What)
this_year_WJ.rup_df <- as.data.frame(WJ.rup.exceedance_df[WJ.rup.exceedance_df$HY == this_report, ])
#JB
JB.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                             length(rup.JB.wet$HY)),
                         HY=as.factor(rup.JB.wet$HY[JB.rup.wet.ind]), 
                         rup=rup.JB.wet$rup[JB.rup.wet.ind] )
JB.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                             length(rup.JB.dry$HY)),
                         HY=as.factor(rup.JB.dry$HY[JB.rup.dry.ind]), 
                         rup=rup.JB.dry$rup[JB.rup.dry.ind] )
JB.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                             length(rup.JB.ann$HY)),
                         HY=as.factor(rup.JB.ann$HY[JB.rup.ann.ind]), 
                         rup=rup.JB.ann$rup[JB.rup.ann.ind] )
JB.rup.exceedance_df<- data.frame(bind_rows(JB.rup.ann.df, JB.rup.wet.df, JB.rup.dry.df) )
JB.rup.exceedance_df$Percent<- factor(JB.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.JB))
JB.rup.exceedance_df$What<- as.factor(JB.rup.exceedance_df$What)
this_year_JB.rup_df <- as.data.frame(JB.rup.exceedance_df[JB.rup.exceedance_df$HY == this_report, ])
#ex.dat rup LS####
#SB
SB.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                             length(rup.SB.wet$HY)),
                         HY=as.factor(rup.SB.wet$HY[SB.rup.wet.ind]), 
                         rup=rup.SB.wet$rup[SB.rup.wet.ind] )
SB.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                             length(rup.SB.dry$HY)),
                         HY=as.factor(rup.SB.dry$HY[SB.rup.dry.ind]), 
                         rup=rup.SB.dry$rup[SB.rup.dry.ind] )
SB.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                             length(rup.SB.ann$HY)),
                         HY=as.factor(rup.SB.ann$HY[SB.rup.ann.ind]), 
                         rup=rup.SB.ann$rup[SB.rup.ann.ind] )
SB.rup.exceedance_df<- data.frame(bind_rows(SB.rup.ann.df, SB.rup.wet.df, SB.rup.dry.df) )
SB.rup.exceedance_df$Percent<- factor(SB.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.SB))
SB.rup.exceedance_df$What<- as.factor(SB.rup.exceedance_df$What)
this_year_SB.rup_df <- as.data.frame(SB.rup.exceedance_df[SB.rup.exceedance_df$HY == this_report, ])
#HC
HC.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                             length(rup.HC.wet$HY)),
                         HY=as.factor(rup.HC.wet$HY[HC.rup.wet.ind]), 
                         rup=rup.HC.wet$rup[HC.rup.wet.ind] )
HC.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                             length(rup.HC.dry$HY)),
                         HY=as.factor(rup.HC.dry$HY[HC.rup.dry.ind]), 
                         rup=rup.HC.dry$rup[HC.rup.dry.ind] )
HC.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                             length(rup.HC.ann$HY)),
                         HY=as.factor(rup.HC.ann$HY[HC.rup.ann.ind]), 
                         rup=rup.HC.ann$rup[HC.rup.ann.ind] )
HC.rup.exceedance_df<- data.frame(bind_rows(HC.rup.ann.df, HC.rup.wet.df, HC.rup.dry.df) )
HC.rup.exceedance_df$Percent<- factor(HC.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.HC))
HC.rup.exceedance_df$What<- as.factor(HC.rup.exceedance_df$What)
this_year_HC.rup_df <- as.data.frame(HC.rup.exceedance_df[HC.rup.exceedance_df$HY == this_report, ])
#ex.dat rup SBB####
#MB
MB.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                             length(rup.MB.wet$HY)),
                         HY=as.factor(rup.MB.wet$HY[MB.rup.wet.ind]), 
                         rup=rup.MB.wet$rup[MB.rup.wet.ind] )
MB.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                             length(rup.MB.dry$HY)),
                         HY=as.factor(rup.MB.dry$HY[MB.rup.dry.ind]), 
                         rup=rup.MB.dry$rup[MB.rup.dry.ind] )
MB.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                             length(rup.MB.ann$HY)),
                         HY=as.factor(rup.MB.ann$HY[MB.rup.ann.ind]), 
                         rup=rup.MB.ann$rup[MB.rup.ann.ind] )
MB.rup.exceedance_df<- data.frame(bind_rows(MB.rup.ann.df, MB.rup.wet.df, MB.rup.dry.df) )
MB.rup.exceedance_df$Percent<- factor(MB.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.MB))
MB.rup.exceedance_df$What<- as.factor(MB.rup.exceedance_df$What)
this_year_MB.rup_df <- as.data.frame(MB.rup.exceedance_df[MB.rup.exceedance_df$HY == this_report, ])
#BS
BS.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                             length(rup.BS.wet$HY)),
                         HY=as.factor(rup.BS.wet$HY[BS.rup.wet.ind]), 
                         rup=rup.BS.wet$rup[BS.rup.wet.ind] )
BS.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                             length(rup.BS.dry$HY)),
                         HY=as.factor(rup.BS.dry$HY[BS.rup.dry.ind]), 
                         rup=rup.BS.dry$rup[BS.rup.dry.ind] )
BS.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                             length(rup.BS.ann$HY)),
                         HY=as.factor(rup.BS.ann$HY[BS.rup.ann.ind]), 
                         rup=rup.BS.ann$rup[BS.rup.ann.ind] )
BS.rup.exceedance_df<- data.frame(bind_rows(BS.rup.ann.df, BS.rup.wet.df, BS.rup.dry.df) )
BS.rup.exceedance_df$Percent<- factor(BS.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.BS))
BS.rup.exceedance_df$What<- as.factor(BS.rup.exceedance_df$What)
this_year_BS.rup_df <- as.data.frame(BS.rup.exceedance_df[BS.rup.exceedance_df$HY == this_report, ])
#CS
CS.rup.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                             length(rup.CS.wet$HY)),
                         HY=as.factor(rup.CS.wet$HY[CS.rup.wet.ind]), 
                         rup=rup.CS.wet$rup[CS.rup.wet.ind] )
CS.rup.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                             length(rup.CS.dry$HY)),
                         HY=as.factor(rup.CS.dry$HY[CS.rup.dry.ind]), 
                         rup=rup.CS.dry$rup[CS.rup.dry.ind] )
CS.rup.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                             length(rup.CS.ann$HY)),
                         HY=as.factor(rup.CS.ann$HY[CS.rup.ann.ind]), 
                         rup=rup.CS.ann$rup[CS.rup.ann.ind] )
CS.rup.exceedance_df<- data.frame(bind_rows(CS.rup.ann.df, CS.rup.wet.df, CS.rup.dry.df) )
CS.rup.exceedance_df$Percent<- factor(CS.rup.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.CS))
CS.rup.exceedance_df$What<- as.factor(CS.rup.exceedance_df$What)
this_year_CS.rup_df <- as.data.frame(CS.rup.exceedance_df[CS.rup.exceedance_df$HY == this_report, ])



# this year mean annual and seasonal halo NUMBERS
#LMB halo ####
#TR
TR.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "TR1", ]$Halo)
TR.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "TR1", ]$Halo)
TR.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "TR1", ]$Halo)
#EC
EC.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "EC1", ]$Halo)
EC.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "EC1", ]$Halo)
EC.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "EC1", ]$Halo)
#TC halo####
#WJ
WJ.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "WJ1", ]$Halo)
WJ.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "WJ1", ]$Halo)
WJ.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "WJ1", ]$Halo)
#JB
JB.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "JB1", ]$Halo)
JB.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "JB1", ]$Halo)
JB.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "JB1", ]$Halo)
#LS halo####
#SB
SB.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "SB1", ]$Halo)
SB.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "SB1", ]$Halo)
SB.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "SB1", ]$Halo)
#HC
HC.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "HC1A", ]$Halo)
HC.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "HC1A", ]$Halo)
HC.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "HC1A", ]$Halo)
#SBB halo####
#MB
MB.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "MB1", ]$Halo)
MB.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "MB1", ]$Halo)
MB.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "MB1", ]$Halo)
#BS
BS.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "BS1", ]$Halo)
BS.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "BS1", ]$Halo)
BS.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "BS1", ]$Halo)
#CS
CS.halo_this_year_ann<- mean(SAV.now[SAV.now$site == "CS1", ]$Halo)
CS.halo_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$site == "CS1", ]$Halo)
CS.halo_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$site == "CS1", ]$Halo)

#mean all other years mean annaul and seasons halo sav ####
#LMB POR halo ####
#TR 
halo.TR.ann <- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "TR1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.TR.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.TR.wet <- halo.season.ex[c(wet.ex.xyz),]
#EC 
halo.EC.ann <- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "EC1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.EC.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.EC.wet <- halo.season.ex[c(wet.ex.xyz),]
#TC POR halo####
#JB
halo.JB.ann <- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "JB1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.JB.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.JB.wet <- halo.season.ex[c(wet.ex.xyz),]
#WJ
halo.WJ.ann <- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "WJ1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.WJ.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.WJ.wet <- halo.season.ex[c(wet.ex.xyz),]
#LS POR halo ####
#SB
halo.SB.ann <- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "SB1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.SB.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.SB.wet <- halo.season.ex[c(wet.ex.xyz),]
#HC
halo.HC.ann <- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "HC1A", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.HC.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.HC.wet <- halo.season.ex[c(wet.ex.xyz),]
#SBB POR halo####
#MB
halo.MB.ann <- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "MB1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.MB.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.MB.wet <- halo.season.ex[c(wet.ex.xyz),]
#BS
halo.BS.ann <- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "BS1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.BS.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.BS.wet <- halo.season.ex[c(wet.ex.xyz),]
#CS
halo.CS.ann <- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, site), summarise, halo = mean(Halo))
halo.season.ex<- ddply(all_SAV[all_SAV$site == "CS1", ], .(HY, Season, site), summarise, halo = mean(Halo))
dry.ex.xyz <- which(halo.season.ex$Season == "Dry Season")
halo.CS.dry <- halo.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(halo.season.ex$Season == "Wet Season")
halo.CS.wet <- halo.season.ex[c(wet.ex.xyz),]

#data file exceedance depth ####

#drop HY?
#remove unused years 
#drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
#long sites
#depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
#depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
#depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]

ann.halo.ex<- rbind(halo.TR.ann, halo.EC.ann,halo.JB.ann, halo.WJ.ann, 
                    halo.SB.ann, halo.HC.ann,halo.MB.ann, halo.BS.ann, halo.CS.ann)
wet.halo.ex<- rbind(halo.TR.wet, halo.EC.wet,halo.JB.wet, halo.WJ.wet, 
                    halo.SB.wet, halo.HC.wet,halo.MB.wet, halo.BS.wet, halo.CS.wet)
dry.halo.ex<- rbind(halo.TR.dry, halo.EC.dry,halo.JB.dry, halo.WJ.dry, 
                    halo.SB.dry, halo.HC.dry,halo.MB.dry, halo.BS.dry, halo.CS.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.halo.ex[ann.halo.ex$site == "CS1", ]$HY)-1))

#create an ind by sorting halo; largest to smallest for wet, dry and annual variable ####
#LMB halo####
#TR
TR.halo.wet.ind<-order(-halo.TR.wet$halo)
TR.halo.dry.ind<-order(-halo.TR.dry$halo)
TR.halo.ann.ind<-order(-halo.TR.ann$halo)
#EC
EC.halo.wet.ind<-order(-halo.EC.wet$halo)
EC.halo.dry.ind<-order(-halo.EC.dry$halo)
EC.halo.ann.ind<-order(-halo.EC.ann$halo)
#TC halo####
#WJ
WJ.halo.wet.ind<-order(-halo.WJ.wet$halo)
WJ.halo.dry.ind<-order(-halo.WJ.dry$halo)
WJ.halo.ann.ind<-order(-halo.WJ.ann$halo)
#JB
JB.halo.wet.ind<-order(-halo.JB.wet$halo)
JB.halo.dry.ind<-order(-halo.JB.dry$halo)
JB.halo.ann.ind<-order(-halo.JB.ann$halo)
#LS halo####
#SB
SB.halo.wet.ind<-order(-halo.SB.wet$halo)
SB.halo.dry.ind<-order(-halo.SB.dry$halo)
SB.halo.ann.ind<-order(-halo.SB.ann$halo)
#HC
HC.halo.wet.ind<-order(-halo.HC.wet$halo)
HC.halo.dry.ind<-order(-halo.HC.dry$halo)
HC.halo.ann.ind<-order(-halo.HC.ann$halo)
#SBB halo####
#MB
MB.halo.wet.ind<-order(-halo.MB.wet$halo)
MB.halo.dry.ind<-order(-halo.MB.dry$halo)
MB.halo.ann.ind<-order(-halo.MB.ann$halo)
#BS
BS.halo.wet.ind<-order(-halo.BS.wet$halo)
BS.halo.dry.ind<-order(-halo.BS.dry$halo)
BS.halo.ann.ind<-order(-halo.BS.ann$halo)
#CS
CS.halo.wet.ind<-order(-halo.CS.wet$halo)
CS.halo.dry.ind<-order(-halo.CS.dry$halo)
CS.halo.ann.ind<-order(-halo.CS.ann$halo)
#create dataframes for ggplot####
#ex.dat halo LMB####
#TR
TR.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                              length(halo.TR.wet$HY)),
                          HY=as.factor(halo.TR.wet$HY[TR.halo.wet.ind]), 
                          halo=halo.TR.wet$halo[TR.halo.wet.ind] )
TR.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                              length(halo.TR.dry$HY)),
                          HY=as.factor(halo.TR.dry$HY[TR.halo.dry.ind]), 
                          halo=halo.TR.dry$halo[TR.halo.dry.ind] )
TR.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                              length(halo.TR.ann$HY)),
                          HY=as.factor(halo.TR.ann$HY[TR.halo.ann.ind]), 
                          halo=halo.TR.ann$halo[TR.halo.ann.ind] )
TR.halo.exceedance_df<- data.frame(bind_rows(TR.halo.ann.df, TR.halo.wet.df, TR.halo.dry.df) )
TR.halo.exceedance_df$Percent<- factor(TR.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.TR))
TR.halo.exceedance_df$What<- as.factor(TR.halo.exceedance_df$What)
this_year_TR.halo_df <- as.data.frame(TR.halo.exceedance_df[TR.halo.exceedance_df$HY == this_report, ])
#EC
EC.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                              length(halo.EC.wet$HY)),
                          HY=as.factor(halo.EC.wet$HY[EC.halo.wet.ind]), 
                          halo=halo.EC.wet$halo[EC.halo.wet.ind] )
EC.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                              length(halo.EC.dry$HY)),
                          HY=as.factor(halo.EC.dry$HY[EC.halo.dry.ind]), 
                          halo=halo.EC.dry$halo[EC.halo.dry.ind] )
EC.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                              length(halo.EC.ann$HY)),
                          HY=as.factor(halo.EC.ann$HY[EC.halo.ann.ind]), 
                          halo=halo.EC.ann$halo[EC.halo.ann.ind] )
EC.halo.exceedance_df<- data.frame(bind_rows(EC.halo.ann.df, EC.halo.wet.df, EC.halo.dry.df) )
EC.halo.exceedance_df$Percent<- factor(EC.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.EC))
EC.halo.exceedance_df$What<- as.factor(EC.halo.exceedance_df$What)
this_year_EC.halo_df <- as.data.frame(EC.halo.exceedance_df[EC.halo.exceedance_df$HY == this_report, ])
#ex.dat halo TC####
#WJ
WJ.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                              length(halo.WJ.wet$HY)),
                          HY=as.factor(halo.WJ.wet$HY[WJ.halo.wet.ind]), 
                          halo=halo.WJ.wet$halo[WJ.halo.wet.ind] )
WJ.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                              length(halo.WJ.dry$HY)),
                          HY=as.factor(halo.WJ.dry$HY[WJ.halo.dry.ind]), 
                          halo=halo.WJ.dry$halo[WJ.halo.dry.ind] )
WJ.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                              length(halo.WJ.ann$HY)),
                          HY=as.factor(halo.WJ.ann$HY[WJ.halo.ann.ind]), 
                          halo=halo.WJ.ann$halo[WJ.halo.ann.ind] )
WJ.halo.exceedance_df<- data.frame(bind_rows(WJ.halo.ann.df, WJ.halo.wet.df, WJ.halo.dry.df) )
WJ.halo.exceedance_df$Percent<- factor(WJ.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.halo.exceedance_df$What<- as.factor(WJ.halo.exceedance_df$What)
this_year_WJ.halo_df <- as.data.frame(WJ.halo.exceedance_df[WJ.halo.exceedance_df$HY == this_report, ])
#JB
JB.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                              length(halo.JB.wet$HY)),
                          HY=as.factor(halo.JB.wet$HY[JB.halo.wet.ind]), 
                          halo=halo.JB.wet$halo[JB.halo.wet.ind] )
JB.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                              length(halo.JB.dry$HY)),
                          HY=as.factor(halo.JB.dry$HY[JB.halo.dry.ind]), 
                          halo=halo.JB.dry$halo[JB.halo.dry.ind] )
JB.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                              length(halo.JB.ann$HY)),
                          HY=as.factor(halo.JB.ann$HY[JB.halo.ann.ind]), 
                          halo=halo.JB.ann$halo[JB.halo.ann.ind] )
JB.halo.exceedance_df<- data.frame(bind_rows(JB.halo.ann.df, JB.halo.wet.df, JB.halo.dry.df) )
JB.halo.exceedance_df$Percent<- factor(JB.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.JB))
JB.halo.exceedance_df$What<- as.factor(JB.halo.exceedance_df$What)
this_year_JB.halo_df <- as.data.frame(JB.halo.exceedance_df[JB.halo.exceedance_df$HY == this_report, ])
#ex.dat halo LS####
#SB
SB.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                              length(halo.SB.wet$HY)),
                          HY=as.factor(halo.SB.wet$HY[SB.halo.wet.ind]), 
                          halo=halo.SB.wet$halo[SB.halo.wet.ind] )
SB.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                              length(halo.SB.dry$HY)),
                          HY=as.factor(halo.SB.dry$HY[SB.halo.dry.ind]), 
                          halo=halo.SB.dry$halo[SB.halo.dry.ind] )
SB.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                              length(halo.SB.ann$HY)),
                          HY=as.factor(halo.SB.ann$HY[SB.halo.ann.ind]), 
                          halo=halo.SB.ann$halo[SB.halo.ann.ind] )
SB.halo.exceedance_df<- data.frame(bind_rows(SB.halo.ann.df, SB.halo.wet.df, SB.halo.dry.df) )
SB.halo.exceedance_df$Percent<- factor(SB.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.SB))
SB.halo.exceedance_df$What<- as.factor(SB.halo.exceedance_df$What)
this_year_SB.halo_df <- as.data.frame(SB.halo.exceedance_df[SB.halo.exceedance_df$HY == this_report, ])
#HC
HC.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                              length(halo.HC.wet$HY)),
                          HY=as.factor(halo.HC.wet$HY[HC.halo.wet.ind]), 
                          halo=halo.HC.wet$halo[HC.halo.wet.ind] )
HC.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                              length(halo.HC.dry$HY)),
                          HY=as.factor(halo.HC.dry$HY[HC.halo.dry.ind]), 
                          halo=halo.HC.dry$halo[HC.halo.dry.ind] )
HC.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                              length(halo.HC.ann$HY)),
                          HY=as.factor(halo.HC.ann$HY[HC.halo.ann.ind]), 
                          halo=halo.HC.ann$halo[HC.halo.ann.ind] )
HC.halo.exceedance_df<- data.frame(bind_rows(HC.halo.ann.df, HC.halo.wet.df, HC.halo.dry.df) )
HC.halo.exceedance_df$Percent<- factor(HC.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.HC))
HC.halo.exceedance_df$What<- as.factor(HC.halo.exceedance_df$What)
this_year_HC.halo_df <- as.data.frame(HC.halo.exceedance_df[HC.halo.exceedance_df$HY == this_report, ])
#ex.dat halo SBB####
#MB
MB.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                              length(halo.MB.wet$HY)),
                          HY=as.factor(halo.MB.wet$HY[MB.halo.wet.ind]), 
                          halo=halo.MB.wet$halo[MB.halo.wet.ind] )
MB.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                              length(halo.MB.dry$HY)),
                          HY=as.factor(halo.MB.dry$HY[MB.halo.dry.ind]), 
                          halo=halo.MB.dry$halo[MB.halo.dry.ind] )
MB.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                              length(halo.MB.ann$HY)),
                          HY=as.factor(halo.MB.ann$HY[MB.halo.ann.ind]), 
                          halo=halo.MB.ann$halo[MB.halo.ann.ind] )
MB.halo.exceedance_df<- data.frame(bind_rows(MB.halo.ann.df, MB.halo.wet.df, MB.halo.dry.df) )
MB.halo.exceedance_df$Percent<- factor(MB.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.MB))
MB.halo.exceedance_df$What<- as.factor(MB.halo.exceedance_df$What)
this_year_MB.halo_df <- as.data.frame(MB.halo.exceedance_df[MB.halo.exceedance_df$HY == this_report, ])
#BS
BS.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                              length(halo.BS.wet$HY)),
                          HY=as.factor(halo.BS.wet$HY[BS.halo.wet.ind]), 
                          halo=halo.BS.wet$halo[BS.halo.wet.ind] )
BS.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                              length(halo.BS.dry$HY)),
                          HY=as.factor(halo.BS.dry$HY[BS.halo.dry.ind]), 
                          halo=halo.BS.dry$halo[BS.halo.dry.ind] )
BS.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                              length(halo.BS.ann$HY)),
                          HY=as.factor(halo.BS.ann$HY[BS.halo.ann.ind]), 
                          halo=halo.BS.ann$halo[BS.halo.ann.ind] )
BS.halo.exceedance_df<- data.frame(bind_rows(BS.halo.ann.df, BS.halo.wet.df, BS.halo.dry.df) )
BS.halo.exceedance_df$Percent<- factor(BS.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.BS))
BS.halo.exceedance_df$What<- as.factor(BS.halo.exceedance_df$What)
this_year_BS.halo_df <- as.data.frame(BS.halo.exceedance_df[BS.halo.exceedance_df$HY == this_report, ])
#CS
CS.halo.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                              length(halo.CS.wet$HY)),
                          HY=as.factor(halo.CS.wet$HY[CS.halo.wet.ind]), 
                          halo=halo.CS.wet$halo[CS.halo.wet.ind] )
CS.halo.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                              length(halo.CS.dry$HY)),
                          HY=as.factor(halo.CS.dry$HY[CS.halo.dry.ind]), 
                          halo=halo.CS.dry$halo[CS.halo.dry.ind] )
CS.halo.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                              length(halo.CS.ann$HY)),
                          HY=as.factor(halo.CS.ann$HY[CS.halo.ann.ind]), 
                          halo=halo.CS.ann$halo[CS.halo.ann.ind] )
CS.halo.exceedance_df<- data.frame(bind_rows(CS.halo.ann.df, CS.halo.wet.df, CS.halo.dry.df) )
CS.halo.exceedance_df$Percent<- factor(CS.halo.exceedance_df$Percent, 
                                       levels=label_percent(accuracy=1)(y.ex.CS))
CS.halo.exceedance_df$What<- as.factor(CS.halo.exceedance_df$What)
this_year_CS.halo_df <- as.data.frame(CS.halo.exceedance_df[CS.halo.exceedance_df$HY == this_report, ])



# REGION EXCEEDANCE TOTAL -------------------------------------------------



#TR
LMB.total_this_year_ann<- mean(SAV.now[SAV.now$region == "LMB", ]$TOTAL, na.rm  = TRUE)
LMB.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$region == "LMB", ]$TOTAL, na.rm  = TRUE)
LMB.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$region == "LMB", ]$TOTAL, na.rm  = TRUE)

total.LMB.ann <- ddply(all_SAV[all_SAV$region == "LMB", ], .(HY, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
total.season.ex<- ddply(all_SAV[all_SAV$region == "LMB", ], .(HY, Season, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.LMB.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.LMB.wet <- total.season.ex[c(wet.ex.xyz),]



y.ex.LMB<-seq(from = 0, to = 1, by = 1/(length(total.LMB.ann[total.LMB.ann$region == "LMB", ]$HY)-1))


LMB.total.wet.ind<-order(-total.LMB.wet$total)
LMB.total.dry.ind<-order(-total.LMB.dry$total)
LMB.total.ann.ind<-order(-total.LMB.ann$total)



LMB.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LMB),What=rep("Wet Season", 
                                                                               length(total.LMB.wet$HY)),
                           HY=as.factor(total.LMB.wet$HY[LMB.total.wet.ind]), 
                           total=total.LMB.wet$total[LMB.total.wet.ind] )
LMB.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LMB),What=rep("Dry Season", 
                                                                               length(total.LMB.dry$HY)),
                           HY=as.factor(total.LMB.dry$HY[LMB.total.dry.ind]), 
                           total=total.LMB.dry$total[LMB.total.dry.ind] )
LMB.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LMB),What=rep("Annual", 
                                                                               length(total.LMB.ann$HY)),
                           HY=as.factor(total.LMB.ann$HY[LMB.total.ann.ind]), 
                           total=total.LMB.ann$total[LMB.total.ann.ind] )
LMB.total.exceedance_df<- data.frame(bind_rows(LMB.total.ann.df, LMB.total.wet.df, LMB.total.dry.df) )
LMB.total.exceedance_df$Percent<- factor(LMB.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.LMB))
LMB.total.exceedance_df$What<- as.factor(LMB.total.exceedance_df$What)
this_year_LMB.total_df <- as.data.frame(LMB.total.exceedance_df[LMB.total.exceedance_df$HY == this_report, ])


#LMB
ex.LMB.total <- ggplot(LMB.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_LMB.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_LMB.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")


#TC
TC.total_this_year_ann<- mean(SAV.now[SAV.now$region == "TC", ]$TOTAL, na.rm  = TRUE)
TC.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$region == "TC", ]$TOTAL, na.rm  = TRUE)
TC.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$region == "TC", ]$TOTAL, na.rm  = TRUE)

total.TC.ann <- ddply(all_SAV[all_SAV$region == "TC", ], .(HY, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
total.season.ex<- ddply(all_SAV[all_SAV$region == "TC", ], .(HY, Season, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.TC.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.TC.wet <- total.season.ex[c(wet.ex.xyz),]



y.ex.TC<-seq(from = 0, to = 1, by = 1/(length(total.TC.ann[total.TC.ann$region == "TC", ]$HY)-1))


TC.total.wet.ind<-order(-total.TC.wet$total)
TC.total.dry.ind<-order(-total.TC.dry$total)
TC.total.ann.ind<-order(-total.TC.ann$total)



TC.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TC),What=rep("Wet Season", 
                                                                                 length(total.TC.wet$HY)),
                            HY=as.factor(total.TC.wet$HY[TC.total.wet.ind]), 
                            total=total.TC.wet$total[TC.total.wet.ind] )
TC.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TC),What=rep("Dry Season", 
                                                                                 length(total.TC.dry$HY)),
                            HY=as.factor(total.TC.dry$HY[TC.total.dry.ind]), 
                            total=total.TC.dry$total[TC.total.dry.ind] )
TC.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TC),What=rep("Annual", 
                                                                                 length(total.TC.ann$HY)),
                            HY=as.factor(total.TC.ann$HY[TC.total.ann.ind]), 
                            total=total.TC.ann$total[TC.total.ann.ind] )
TC.total.exceedance_df<- data.frame(bind_rows(TC.total.ann.df, TC.total.wet.df, TC.total.dry.df) )
TC.total.exceedance_df$Percent<- factor(TC.total.exceedance_df$Percent, 
                                         levels=label_percent(accuracy=1)(y.ex.TC))
TC.total.exceedance_df$What<- as.factor(TC.total.exceedance_df$What)
this_year_TC.total_df <- as.data.frame(TC.total.exceedance_df[TC.total.exceedance_df$HY == this_report, ])


#TC
ex.TC.total <- ggplot(TC.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TC.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TC.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")


#LS

LS.total_this_year_ann<- mean(SAV.now[SAV.now$region == "LS", ]$TOTAL, na.rm  = TRUE)
LS.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$region == "LS", ]$TOTAL, na.rm  = TRUE)
LS.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$region == "LS", ]$TOTAL, na.rm  = TRUE)

total.LS.ann <- ddply(all_SAV[all_SAV$region == "LS", ], .(HY, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
total.season.ex<- ddply(all_SAV[all_SAV$region == "LS", ], .(HY, Season, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.LS.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.LS.wet <- total.season.ex[c(wet.ex.xyz),]



y.ex.LS<-seq(from = 0, to = 1, by = 1/(length(total.LS.ann[total.LS.ann$region == "LS", ]$HY)-1))


LS.total.wet.ind<-order(-total.LS.wet$total)
LS.total.dry.ind<-order(-total.LS.dry$total)
LS.total.ann.ind<-order(-total.LS.ann$total)



LS.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LS[-1]),What=rep("Wet Season", 
                                                                                 length(total.LS.wet$HY)),
                            HY=as.factor(total.LS.wet$HY[LS.total.wet.ind]), 
                            total=total.LS.wet$total[LS.total.wet.ind] )
LS.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LS),What=rep("Dry Season", 
                                                                                 length(total.LS.dry$HY)),
                            HY=as.factor(total.LS.dry$HY[LS.total.dry.ind]), 
                            total=total.LS.dry$total[LS.total.dry.ind] )
LS.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.LS),What=rep("Annual", 
                                                                                 length(total.LS.ann$HY)),
                            HY=as.factor(total.LS.ann$HY[LS.total.ann.ind]), 
                            total=total.LS.ann$total[LS.total.ann.ind] )
LS.total.exceedance_df<- data.frame(bind_rows(LS.total.ann.df, LS.total.wet.df, LS.total.dry.df) )
LS.total.exceedance_df$Percent<- factor(LS.total.exceedance_df$Percent, 
                                         levels=label_percent(accuracy=1)(y.ex.LS))
LS.total.exceedance_df$What<- as.factor(LS.total.exceedance_df$What)
this_year_LS.total_df <- as.data.frame(LS.total.exceedance_df[LS.total.exceedance_df$HY == this_report, ])


#LS
ex.LS.total <- ggplot(LS.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_LS.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_LS.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")




#SBB

SBB.total_this_year_ann<- mean(SAV.now[SAV.now$region == "SBB", ]$TOTAL, na.rm  = TRUE)
SBB.total_this_year_wet<- mean(SAV.now.wet[SAV.now.wet$region == "SBB", ]$TOTAL, na.rm  = TRUE)
SBB.total_this_year_dry<- mean(SAV.now.dry[SAV.now.dry$region == "SBB", ]$TOTAL, na.rm  = TRUE)

total.SBB.ann <- ddply(all_SAV[all_SAV$region == "SBB", ], .(HY, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
total.season.ex<- ddply(all_SAV[all_SAV$region == "SBB", ], .(HY, Season, region), summarise, total = mean(TOTAL, na.rm  = TRUE))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.SBB.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.SBB.wet <- total.season.ex[c(wet.ex.xyz),]



y.ex.SBB<-seq(from = 0, to = 1, by = 1/(length(total.SBB.ann[total.SBB.ann$region == "SBB", ]$HY)-1))


SBB.total.wet.ind<-order(-total.SBB.wet$total)
SBB.total.dry.ind<-order(-total.SBB.dry$total)
SBB.total.ann.ind<-order(-total.SBB.ann$total)



SBB.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SBB),What=rep("Wet Season", 
                                                                                   length(total.SBB.wet$HY)),
                           HY=as.factor(total.SBB.wet$HY[SBB.total.wet.ind]), 
                           total=total.SBB.wet$total[SBB.total.wet.ind] )
SBB.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SBB),What=rep("Dry Season", 
                                                                               length(total.SBB.dry$HY)),
                           HY=as.factor(total.SBB.dry$HY[SBB.total.dry.ind]), 
                           total=total.SBB.dry$total[SBB.total.dry.ind] )
SBB.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SBB),What=rep("Annual", 
                                                                               length(total.SBB.ann$HY)),
                           HY=as.factor(total.SBB.ann$HY[SBB.total.ann.ind]), 
                           total=total.SBB.ann$total[SBB.total.ann.ind] )
SBB.total.exceedance_df<- data.frame(bind_rows(SBB.total.ann.df, SBB.total.wet.df, SBB.total.dry.df) )
SBB.total.exceedance_df$Percent<- factor(SBB.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.SBB))
SBB.total.exceedance_df$What<- as.factor(SBB.total.exceedance_df$What)
this_year_SBB.total_df <- as.data.frame(SBB.total.exceedance_df[SBB.total.exceedance_df$HY == this_report, ])


#SBB
ex.SBB.total <- ggplot(SBB.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SBB.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SBB.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")







#EXCEEDANCE PLOTS####
#LMB total####
#TR
ex.TR.total <- ggplot(TR.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#EC
ex.EC.total <- ggplot(EC.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")

#TC total####
#JB
ex.JB.total <- ggplot(JB.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")

#WJ
ex.WJ.total <- ggplot(WJ.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#LS total####
#SB
ex.SB.total <- ggplot(SB.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#HC
ex.HC.total <- ggplot(HC.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#SBB total####
#MB
ex.MB.total <- ggplot(MB.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#BS
ex.BS.total <- ggplot(BS.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")
#CS
ex.CS.total <- ggplot(CS.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") +ylab("Total SAV %")




#LMB utric####
#TR
ex.TR.utric <- ggplot(TR.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#EC
ex.EC.utric <- ggplot(EC.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#TC utric####
#JB
ex.JB.utric <- ggplot(JB.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#WJ
ex.WJ.utric <- ggplot(WJ.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#LS utric####
#SB
ex.SB.utric <- ggplot(SB.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#HC
ex.HC.utric <- ggplot(HC.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#SBB utric####
#MB
ex.MB.utric <- ggplot(MB.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#BS
ex.BS.utric <- ggplot(BS.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#CS
ex.CS.utric <- ggplot(CS.utric.exceedance_df, aes(x=Percent, y= utric)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.utric_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.utric_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())



#LMB chara####
#TR
ex.TR.chara <- ggplot(TR.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#EC
ex.EC.chara <- ggplot(EC.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#TC chara####
#JB
ex.JB.chara <- ggplot(JB.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#WJ
ex.WJ.chara <- ggplot(WJ.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#LS chara####
#SB
ex.SB.chara <- ggplot(SB.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#HC
ex.HC.chara <- ggplot(HC.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#SBB chara####
#MB
ex.MB.chara <- ggplot(MB.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#BS
ex.BS.chara <- ggplot(BS.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#CS
ex.CS.chara <- ggplot(CS.chara.exceedance_df, aes(x=Percent, y= chara)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.chara_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.chara_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())




#LMB rup####
#TR
ex.TR.rup <- ggplot(TR.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#EC
ex.EC.rup <- ggplot(EC.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#TC rup####
#JB
ex.JB.rup <- ggplot(JB.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#WJ
ex.WJ.rup <- ggplot(WJ.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#LS rup####
#SB
ex.SB.rup <- ggplot(SB.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#HC
ex.HC.rup <- ggplot(HC.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#SBB rup####
#MB
ex.MB.rup <- ggplot(MB.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#BS
ex.BS.rup <- ggplot(BS.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#CS
ex.CS.rup <- ggplot(CS.rup.exceedance_df, aes(x=Percent, y= rup)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.rup_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.rup_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())





#LMB halo####
#TR
ex.TR.halo <- ggplot(TR.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#EC
ex.EC.halo <- ggplot(EC.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#TC halo####
#JB
ex.JB.halo <- ggplot(JB.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

#WJ
ex.WJ.halo <- ggplot(WJ.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#LS halo####
#SB
ex.SB.halo <- ggplot(SB.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#HC
ex.HC.halo <- ggplot(HC.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#SBB halo####
#MB
ex.MB.halo <- ggplot(MB.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#BS
ex.BS.halo <- ggplot(BS.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())
#CS
ex.CS.halo <- ggplot(CS.halo.exceedance_df, aes(x=Percent, y= halo)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.halo_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.halo_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())




#exceedance dump ####
ex.TR.total
ex.EC.total
ex.WJ.total
ex.JB.total
ex.SB.total
ex.HC.total
ex.MB.total
ex.BS.total
ex.CS.total

ex.TR.utric
ex.EC.utric
ex.WJ.utric
ex.JB.utric
ex.SB.utric
ex.HC.utric
ex.MB.utric
ex.BS.utric
ex.CS.utric

ex.TR.chara
ex.EC.chara
ex.WJ.chara
ex.JB.chara
ex.SB.chara
ex.HC.chara
ex.MB.chara
ex.BS.chara
ex.CS.chara

ex.TR.rup
ex.EC.rup
ex.WJ.rup
ex.JB.rup
ex.SB.rup
ex.HC.rup
ex.MB.rup
ex.BS.rup
ex.CS.rup

ex.TR.halo
ex.EC.halo
ex.WJ.halo
ex.JB.halo
ex.SB.halo
ex.HC.halo
ex.MB.halo
ex.BS.halo
ex.CS.halo

#all SAV graphs####

SAV.all_total.cover_HY
SAV.all_total.cover_month
SAV.all_total.cover_season

SAV.all_Utric.cover_HY
SAV.all_Utric.cover_month
SAV.all_Utric.cover_season

SAV.all_Rupia.cover_HY
SAV.all_Rupia.cover_month
SAV.all_Rupia.cover_season

SAV.all_Chara.cover_HY
SAV.all_Chara.cover_month
SAV.all_Chara.cover_season

SAV.all_Halo.cover_HY
SAV.all_Halo.cover_month
SAV.all_Halo.cover_season



ggarrange(SAV.all_total.cover_HY, ggarrange(SAV.all_total.cover_month, SAV.all_total.cover_season, 
                                            ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.all_Utric.cover_HY, ggarrange(SAV.all_Utric.cover_month, SAV.all_Utric.cover_season, 
                                            ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.all_Chara.cover_HY, ggarrange(SAV.all_Chara.cover_month, SAV.all_Chara.cover_season, 
                                            ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.all_Rupia.cover_HY, ggarrange(SAV.all_Rupia.cover_month, SAV.all_Rupia.cover_season, 
                                            ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.all_Halo.cover_HY, ggarrange(SAV.all_Halo.cover_month, SAV.all_Halo.cover_season, 
                                           ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)



annotate_figure(ggarrange(ex.TR.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("TR;LMB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.TR.utric, ex.TR.chara,ex.TR.rup, ex.TR.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("TR;LMB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.EC.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("EC", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.EC.utric, ex.EC.chara,ex.EC.rup, ex.EC.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("EC", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.WJ.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("WJ", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.WJ.utric, ex.WJ.chara,ex.WJ.rup, ex.WJ.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("WJ", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.JB.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("JB;TC", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.JB.utric, ex.JB.chara,ex.JB.rup, ex.JB.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("JB;TC", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.SB.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("SB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.SB.utric, ex.SB.chara,ex.SB.rup, ex.SB.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("SB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.HC.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("HC;LS", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.HC.utric, ex.HC.chara,ex.HC.rup, ex.HC.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("HC;LS", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.MB.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("MB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.MB.utric, ex.MB.chara,ex.MB.rup, ex.MB.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("MB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.BS.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("BS;SBB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.BS.utric, ex.BS.chara,ex.BS.rup, ex.BS.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("BS;SBB", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.CS.total, vjust= 2, hjust = -0.5,  common.legend = TRUE),
                bottom = text_grob("CS", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(ex.CS.utric, ex.CS.chara,ex.CS.rup, ex.CS.halo, nrow = 2, ncol = 2, 
                          labels=c("       A","       B","       C","       D"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("CS", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))



# grouped sites into REGIONS####
#SAV graphs ####
#TOTAL ####
SAV.region_total.cover_HY<-  ggplot(SAV.now, aes(x=site, y=TOTAL,group=site)) + 
  geom_boxplot(aes(fill=region, color =region))+
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1", "orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1"  ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage") 

SAV.region_total.cover_month<-  ggplot(SAV.now %>% 
                                         mutate(Month = str_replace(Month,'DEC', 'NOV'), 
                                                Month = factor(Month,
                                                       levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                                                  'DEC',"JAN","FEB","MAR",'APR',"MAY"))), 
                                       
                                       
                                       aes(x=Month, y=TOTAL, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage")   

SAV.region_total.cover_season<-  ggplot(SAV.now, aes(x=Season, y=TOTAL, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV % Coverage")   

#Utric ####
SAV.region_Utric.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Utric, group=site)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.region_Utric.cover_month<-  ggplot(SAV.now%>% 
                                         mutate(Month = str_replace(Month,'DEC', 'NOV'), 
                                                Month = factor(Month,
                                                               levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                                                          'DEC',"JAN","FEB","MAR",'APR',"MAY"))),
                                       aes(x=Month, y=Utric, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

SAV.region_Utric.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Utric, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*Utricularia spp.* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

#Rupia ####
SAV.region_Rupia.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Rup, group=site)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.region_Rupia.cover_month<-  ggplot(SAV.now%>% 
                                         mutate(Month = str_replace(Month,'DEC', 'NOV'), 
                                                Month = factor(Month,
                                                               levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                                                          'DEC',"JAN","FEB","MAR",'APR',"MAY"))),
                                       aes(x=Month, y=Rup, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())   

SAV.region_Rupia.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Rup, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*R. maritima* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())


#Chara ####
SAV.region_Chara.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Chara, group=site)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1" ,"orange1"))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())

SAV.region_Chara.cover_month<-  ggplot(SAV.now%>% 
                                         mutate(Month = str_replace(Month,'DEC', 'NOV'), 
                                                Month = factor(Month,
                                                               levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                                                          'DEC',"JAN","FEB","MAR",'APR',"MAY"))),
                                       aes(x=Month, y=Chara, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  

SAV.region_Chara.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Chara, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*C. hornemannii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown()) 

#halo ####
SAV.region_Halo.cover_HY<-  ggplot(SAV.now, aes(x=site, y=Halo, group=site)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown()) 

SAV.region_Halo.cover_month<-  ggplot(SAV.now%>% 
                                        mutate(Month = str_replace(Month,'DEC', 'NOV'), 
                                               Month = factor(Month,
                                                              levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                                                         'DEC',"JAN","FEB","MAR",'APR',"MAY"))),
                                      aes(x=Month, y=Halo, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())    

SAV.region_Halo.cover_season<-  ggplot(SAV.now, aes(x=Season, y=Halo, fill=region)) + 
  geom_boxplot(aes(fill=region, color =region)) +
  scale_fill_manual(values=c("red1","blue1","green1",
                             "yellow1","orange1" ))+
  scale_color_manual(values=c("red1","blue1","green1",
                              "yellow1","orange1" ))+
  geom_boxplot(aes(fill=region ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("*H. wrightii* % Coverage")+
  theme(axis.title.y = ggtext::element_markdown())  



#graph dup ####
SAV.region_total.cover_HY
SAV.region_total.cover_month
SAV.region_total.cover_season

SAV.region_Utric.cover_HY
SAV.region_Utric.cover_month
SAV.region_Utric.cover_season

SAV.region_Rupia.cover_HY
SAV.region_Rupia.cover_month
SAV.region_Rupia.cover_season

SAV.region_Chara.cover_HY
SAV.region_Chara.cover_month
SAV.region_Chara.cover_season

SAV.region_Halo.cover_HY
SAV.region_Halo.cover_month
SAV.region_Halo.cover_season

#region graphs ####


ggarrange(SAV.region_total.cover_HY, ggarrange(SAV.region_total.cover_month, SAV.region_total.cover_season, 
                                               ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.region_Utric.cover_HY, ggarrange(SAV.region_Utric.cover_month, SAV.region_Utric.cover_season, 
                                               ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.region_Chara.cover_HY, ggarrange(SAV.region_Chara.cover_month, SAV.region_Chara.cover_season, 
                                               ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.region_Rupia.cover_HY, ggarrange(SAV.region_Rupia.cover_month, SAV.region_Rupia.cover_season, 
                                               ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)
ggarrange(SAV.region_Halo.cover_HY, ggarrange(SAV.region_Halo.cover_month, SAV.region_Halo.cover_season, 
                                              ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)



ggarrange(ex.TR.total, ex.EC.total,
          labels=c("TR", "EC"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)


ggarrange(ex.WJ.total, ex.JB.total,
          labels=c("WJ", "JB"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)


ggarrange(ex.SB.total, ex.HC.total,
          labels=c("SB", "HC"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(ex.MB.total, ex.BS.total, ex.CS.total,
          labels=c("MB", "BS", "CS"), nrow= 3, vjust= 2, hjust = -0.5, common.legend = TRUE)

#tables####


#NEED TO ADD na.rm = TRUE in the ddply code

#secci to percent 
SAV.now$SECCHI<-ifelse(SAV.now$SECCHI == "bottom",  SAV.now$DEPTH, SAV.now$SECCHI)
SAV.now$SECCHI<-ifelse(SAV.now$SECCHI == "Bottom",  SAV.now$DEPTH, SAV.now$SECCHI)
SAV.now$SECCHI<- as.numeric(SAV.now$SECCHI)
SAV.now$DEPTH<- as.numeric(SAV.now$DEPTH)
SAV.now$secci_per<- as.numeric(SAV.now$SECCHI)/as.numeric(SAV.now$DEPTH)

#set site to make table ####
df_SAV_sitesTR1<- SAV.now[SAV.now$site == "TR1", ]

#create data frame for table 
SAV_for_tableTR1<-ddply(df_SAV_sitesTR1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR1$Date<-format(SAV_for_tableTR1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR1<- SAV_for_tableTR1[,which(unlist(lapply(SAV_for_tableTR1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR1[,1:7]
second_df<-SAV_for_tableTR1[,8:ncol(SAV_for_tableTR1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR1<-cbind(first_df,second_df)

#create the table####
SAV_TABLETR1 <-knitr::kable(SAV_for_tableTR1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR1[,1])), caption = SAV_for_tableTR1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR1

#set site to make table ####
df_SAV_sitesTR2<- SAV.now[SAV.now$site == "TR2", ]

#create data frame for table 
SAV_for_tableTR2<-ddply(df_SAV_sitesTR2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR2$Date<-format(SAV_for_tableTR2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR2<- SAV_for_tableTR2[,which(unlist(lapply(SAV_for_tableTR2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR2[,1:7]
second_df<-SAV_for_tableTR2[,8:ncol(SAV_for_tableTR2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR2<-cbind(first_df,second_df)

#create the table####
SAV_TABLETR2 <-knitr::kable(SAV_for_tableTR2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR2[,1])), caption = SAV_for_tableTR2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR2


#set site to make table ####
df_SAV_sitesTR3<- SAV.now[SAV.now$site == "TR3", ]

#create data frame for table 
SAV_for_tableTR3<-ddply(df_SAV_sitesTR3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR3$Date<-format(SAV_for_tableTR3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR3<- SAV_for_tableTR3[,which(unlist(lapply(SAV_for_tableTR3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR3[,1:7]
second_df<-SAV_for_tableTR3[,8:ncol(SAV_for_tableTR3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR3<-cbind(first_df,second_df)
names(SAV_for_tableTR3)[names(SAV_for_tableTR3) == "second_df" ] <- "<i>Batophora spp.<i>"

#WARNING THIS NEEDS TO CHANGE IF MORE SPECIES... 

#create the table####
SAV_TABLETR3 <-knitr::kable(SAV_for_tableTR3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR3[,1])), caption = SAV_for_tableTR3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR3

#set site to make table ####
df_SAV_sitesTR4A<- SAV.now[SAV.now$site == "TR4A", ]

#create data frame for table 
SAV_for_tableTR4A<-ddply(df_SAV_sitesTR4A, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                         'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                         "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                         "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                         '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                         "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                         "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                         "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                         "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                         "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                         "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                         "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                         "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                         "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR4A$Date<-format(SAV_for_tableTR4A$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR4A<- SAV_for_tableTR4A[,which(unlist(lapply(SAV_for_tableTR4A, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR4A[,1:7]
second_df<-SAV_for_tableTR4A[,8:ncol(SAV_for_tableTR4A)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR4A<-cbind(first_df,second_df)

#create the table####
SAV_TABLETR4A <-knitr::kable(SAV_for_tableTR4A[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR4A[,1])), caption = SAV_for_tableTR4A[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR4A

#set site to make table ####
df_SAV_sitesTR5<- SAV.now[SAV.now$site == "TR5", ]

#df_SAV_sitesTR5$DEPTH<- as.numeric(df_SAV_sitesTR5$DEPTH)
#create data frame for table 
SAV_for_tableTR5<-ddply(df_SAV_sitesTR5, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR5$Date<-format(SAV_for_tableTR5$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR5<- SAV_for_tableTR5[,which(unlist(lapply(SAV_for_tableTR5, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR5[,1:7]
second_df<-SAV_for_tableTR5[,8:ncol(SAV_for_tableTR5)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR5<- cbind(first_df, second_df)
names(SAV_for_tableTR5)[names(SAV_for_tableTR5) == "second_df" ] <- "<i>Chara<i> <br> \n <i>hornemanii<i>"

#WARNING. THIS WILL NEED TO BE CHECKED EACH YEAR

#create the table####
SAV_TABLETR5 <-knitr::kable(SAV_for_tableTR5[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR5[,1])), caption = SAV_for_tableTR5[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR5

#set site to make table ####
df_SAV_sitesTR6<- SAV.now[SAV.now$site == "TR6", ]

#create data frame for table 
SAV_for_tableTR6<-ddply(df_SAV_sitesTR6, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTR6$Date<-format(SAV_for_tableTR6$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTR6<- SAV_for_tableTR6[,which(unlist(lapply(SAV_for_tableTR6, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTR6[,1:7]
second_df<-SAV_for_tableTR6[,8:ncol(SAV_for_tableTR6)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTR6<-cbind(first_df,second_df)

#create the table####
SAV_TABLETR6 <-knitr::kable(SAV_for_tableTR6[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTR6[,1])), caption = SAV_for_tableTR6[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETR6

#set site to make table ####
df_SAV_sitesEC1<- SAV.now[SAV.now$site == "EC1", ]

#create data frame for table 
SAV_for_tableEC1<-ddply(df_SAV_sitesEC1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableEC1$Date<-format(SAV_for_tableEC1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableEC1<- SAV_for_tableEC1[,which(unlist(lapply(SAV_for_tableEC1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableEC1[,1:7]
second_df<-SAV_for_tableEC1[,8:ncol(SAV_for_tableEC1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableEC1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEEC1 <-knitr::kable(SAV_for_tableEC1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableEC1[,1])), caption = SAV_for_tableEC1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEEC1

#set site to make table ####
df_SAV_sitesEC2<- SAV.now[SAV.now$site == "EC2", ]

#create data frame for table 
SAV_for_tableEC2<-ddply(df_SAV_sitesEC2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableEC2$Date<-format(SAV_for_tableEC2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableEC2<- SAV_for_tableEC2[,which(unlist(lapply(SAV_for_tableEC2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableEC2[,1:7]
second_df<-SAV_for_tableEC2[,8:ncol(SAV_for_tableEC2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableEC2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEEC2 <-knitr::kable(SAV_for_tableEC2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableEC2[,1])), caption = SAV_for_tableEC2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEEC2

#set site to make table ####
df_SAV_sitesEC3<- SAV.now[SAV.now$site == "EC3", ]

#create data frame for table 
SAV_for_tableEC3<-ddply(df_SAV_sitesEC3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableEC3$Date<-format(SAV_for_tableEC3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableEC3<- SAV_for_tableEC3[,which(unlist(lapply(SAV_for_tableEC3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableEC3[,1:7]
second_df<-SAV_for_tableEC3[,8:ncol(SAV_for_tableEC3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableEC3<-cbind(first_df,second_df)

#create the table####
SAV_TABLEEC3 <-knitr::kable(SAV_for_tableEC3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableEC3[,1])), caption = SAV_for_tableEC3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEEC3

#set site to make table ####
df_SAV_sitesWJ1<- SAV.now[SAV.now$site == "WJ1", ]

#create data frame for table 
SAV_for_tableWJ1<-ddply(df_SAV_sitesWJ1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableWJ1$Date<-format(SAV_for_tableWJ1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableWJ1<- SAV_for_tableWJ1[,which(unlist(lapply(SAV_for_tableWJ1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableWJ1[,1:7]
second_df<-SAV_for_tableWJ1[,8:ncol(SAV_for_tableWJ1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableWJ1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEWJ1 <-knitr::kable(SAV_for_tableWJ1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableWJ1[,1])), caption = SAV_for_tableWJ1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEWJ1

#set site to make table ####
df_SAV_sitesWJ2<- SAV.now[SAV.now$site == "WJ2", ]

#create data frame for table 
SAV_for_tableWJ2<-ddply(df_SAV_sitesWJ2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableWJ2$Date<-format(SAV_for_tableWJ2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableWJ2<- SAV_for_tableWJ2[,which(unlist(lapply(SAV_for_tableWJ2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableWJ2[,1:7]
second_df<-SAV_for_tableWJ2[,8:ncol(SAV_for_tableWJ2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableWJ2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEWJ2 <-knitr::kable(SAV_for_tableWJ2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableWJ2[,1])), caption = SAV_for_tableWJ2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEWJ2

#set site to make table ####
df_SAV_sitesJB1<- SAV.now[SAV.now$site == "JB1", ]

#create data frame for table 
SAV_for_tableJB1<-ddply(df_SAV_sitesJB1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableJB1$Date<-format(SAV_for_tableJB1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB1<- SAV_for_tableJB1[,which(unlist(lapply(SAV_for_tableJB1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB1[,1:7]
second_df<-SAV_for_tableJB1[,8:ncol(SAV_for_tableJB1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB1 <-knitr::kable(SAV_for_tableJB1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB1[,1])), caption = SAV_for_tableJB1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB1

#set site to make table ####
df_SAV_sitesJB2<- SAV.now[SAV.now$site == "JB2", ]

#create data frame for table 
SAV_for_tableJB2<-ddply(df_SAV_sitesJB2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableJB2$Date<-format(SAV_for_tableJB2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB2<- SAV_for_tableJB2[,which(unlist(lapply(SAV_for_tableJB2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB2[,1:7]
second_df<-SAV_for_tableJB2[,8:ncol(SAV_for_tableJB2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB2 <-knitr::kable(SAV_for_tableJB2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB2[,1])), caption = SAV_for_tableJB2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB2

#set site to make table ####
df_SAV_sitesJB3<- SAV.now[SAV.now$site == "JB3", ]

#create data frame for table 
SAV_for_tableJB3<-ddply(df_SAV_sitesJB3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableJB3$Date<-format(SAV_for_tableJB3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB3<- SAV_for_tableJB3[,which(unlist(lapply(SAV_for_tableJB3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB3[,1:7]
second_df<-SAV_for_tableJB3[,8:ncol(SAV_for_tableJB3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB3<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB3 <-knitr::kable(SAV_for_tableJB3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB3[,1])), caption = SAV_for_tableJB3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB3

#set site to make table ####
df_SAV_sitesJB4<- SAV.now[SAV.now$site == "JB4", ]

#create data frame for table 
SAV_for_tableJB4<-ddply(df_SAV_sitesJB4, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableJB4$Date<-format(SAV_for_tableJB4$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB4<- SAV_for_tableJB4[,which(unlist(lapply(SAV_for_tableJB4, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB4[,1:7]
second_df<-SAV_for_tableJB4[,8:ncol(SAV_for_tableJB4)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB4<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB4 <-knitr::kable(SAV_for_tableJB4[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB4[,1])), caption = SAV_for_tableJB4[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB4

#set site to make table ####
df_SAV_sitesJB5<- SAV.now[SAV.now$site == "JB5", ]


#create data frame for table 
SAV_for_tableJB5<-ddply(df_SAV_sitesJB5, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY, na.rm = T ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH, na.rm = T ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per, na.rm = T)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP, na.rm = T) ),2),'<b>Total<b>'= round(mean(TOTAL, na.rm = T),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric, na.rm = T ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup, na.rm = T ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit, na.rm = T ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara, na.rm = T ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja, na.rm = T ),2),"<i>Batophora spp.<i>"  = round(mean(Bat, na.rm = T  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad, na.rm = T),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro, na.rm = T),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace, na.rm = T ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara, na.rm = T  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo, na.rm = T  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal, na.rm = T  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly, na.rm = T  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau, na.rm = T  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen, na.rm = T   ),2),"<i>Dasya spp.<i>"   =round(mean(Day, na.rm = T   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo, na.rm = T ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali, na.rm = T    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv, na.rm = T  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau, na.rm = T     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi, na.rm = T   ),2)) 

#set date as abriviation
SAV_for_tableJB5$Date<-format(SAV_for_tableJB5$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB5<- SAV_for_tableJB5[,which(unlist(lapply(SAV_for_tableJB5, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB5[,1:7]
second_df<-SAV_for_tableJB5[,8:ncol(SAV_for_tableJB5)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB5<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB5 <-knitr::kable(SAV_for_tableJB5[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB5[,1])), caption = SAV_for_tableJB5[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB5

#set site to make table ####
df_SAV_sitesJB6<- SAV.now[SAV.now$site == "JB6", ]

#create data frame for table 
SAV_for_tableJB6<-ddply(df_SAV_sitesJB6, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableJB6$Date<-format(SAV_for_tableJB6$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableJB6<- SAV_for_tableJB6[,which(unlist(lapply(SAV_for_tableJB6, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableJB6[,1:7]
second_df<-SAV_for_tableJB6[,8:ncol(SAV_for_tableJB6)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableJB6<-cbind(first_df,second_df)

#create the table####
SAV_TABLEJB6 <-knitr::kable(SAV_for_tableJB6[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableJB6[,1])), caption = SAV_for_tableJB6[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEJB6

#set site to make table ####
df_SAV_sitesSB1<- SAV.now[SAV.now$site == "SB1", ]

#create data frame for table 
SAV_for_tableSB1<-ddply(df_SAV_sitesSB1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableSB1$Date<-format(SAV_for_tableSB1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableSB1<- SAV_for_tableSB1[,which(unlist(lapply(SAV_for_tableSB1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableSB1[,1:7]
second_df<-SAV_for_tableSB1[,8:ncol(SAV_for_tableSB1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableSB1<-cbind(first_df,second_df)

#create the table####
SAV_TABLESB1 <-knitr::kable(SAV_for_tableSB1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableSB1[,1])), caption = SAV_for_tableSB1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLESB1

#set site to make table ####
df_SAV_sitesSB2<- SAV.now[SAV.now$site == "SB2", ]

#create data frame for table 
SAV_for_tableSB2<-ddply(df_SAV_sitesSB2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableSB2$Date<-format(SAV_for_tableSB2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableSB2<- SAV_for_tableSB2[,which(unlist(lapply(SAV_for_tableSB2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableSB2[,1:7]
second_df<-SAV_for_tableSB2[,8:ncol(SAV_for_tableSB2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableSB2<-cbind(first_df,second_df)

#create the table####
SAV_TABLESB2 <-knitr::kable(SAV_for_tableSB2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableSB2[,1])), caption = SAV_for_tableSB2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLESB2

#set site to make table ####
df_SAV_sitesSB3<- SAV.now[SAV.now$site == "SB3", ]

#create data frame for table 
SAV_for_tableSB3<-ddply(df_SAV_sitesSB3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableSB3$Date<-format(SAV_for_tableSB3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableSB3<- SAV_for_tableSB3[,which(unlist(lapply(SAV_for_tableSB3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableSB3[,1:7]
second_df<-SAV_for_tableSB3[,8:ncol(SAV_for_tableSB3)]
#second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableSB3<-cbind(first_df,second_df)

SAV_for_tableSB3<-SAV_for_tableSB3[, colSums(na.omit(SAV_for_tableSB3) != 0) > 0]


#create the table####
SAV_TABLESB3 <-knitr::kable(SAV_for_tableSB3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableSB3[,1])), caption = SAV_for_tableSB3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLESB3


#set site to make table ####
df_SAV_sitesHC1<- SAV.now[SAV.now$site == "HC1", ]

#create data frame for table 
SAV_for_tableHC1<-ddply(df_SAV_sitesHC1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC1$Date<-format(SAV_for_tableHC1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC1<- SAV_for_tableHC1[,which(unlist(lapply(SAV_for_tableHC1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC1[,1:7]
second_df<-SAV_for_tableHC1[,8:ncol(SAV_for_tableHC1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC1 <-knitr::kable(SAV_for_tableHC1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC1[,1])), caption = SAV_for_tableHC1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC1

#set site to make table ####
df_SAV_sitesHC1A<- SAV.now[SAV.now$site == "HC1A", ]

#create data frame for table 
SAV_for_tableHC1A<-ddply(df_SAV_sitesHC1A, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                         'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                         "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                         "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                         '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                         "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                         "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                         "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                         "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                         "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                         "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                         "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                         "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                         "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC1A$Date<-format(SAV_for_tableHC1A$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC1A<- SAV_for_tableHC1A[,which(unlist(lapply(SAV_for_tableHC1A, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC1A[,1:7]
second_df<-SAV_for_tableHC1A[,8:ncol(SAV_for_tableHC1A)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC1A<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC1A <-knitr::kable(SAV_for_tableHC1A[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC1A[,1])), caption = SAV_for_tableHC1A[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC1A

#set site to make table ####
df_SAV_sitesHC2<- SAV.now[SAV.now$site == "HC2", ]

#create data frame for table 
SAV_for_tableHC2<-ddply(df_SAV_sitesHC2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC2$Date<-format(SAV_for_tableHC2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC2<- SAV_for_tableHC2[,which(unlist(lapply(SAV_for_tableHC2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC2[,1:7]
second_df<-SAV_for_tableHC2[,8:ncol(SAV_for_tableHC2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC2 <-knitr::kable(SAV_for_tableHC2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC2[,1])), caption = SAV_for_tableHC2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC2

#set site to make table ####
df_SAV_sitesHC3<- SAV.now[SAV.now$site == "HC3", ]

#create data frame for table 
SAV_for_tableHC3<-ddply(df_SAV_sitesHC3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC3$Date<-format(SAV_for_tableHC3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC3<- SAV_for_tableHC3[,which(unlist(lapply(SAV_for_tableHC3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC3[,1:7]
second_df<-SAV_for_tableHC3[,8:ncol(SAV_for_tableHC3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC3<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC3 <-knitr::kable(SAV_for_tableHC3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC3[,1])), caption = SAV_for_tableHC3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC3

#set site to make table ####
df_SAV_sitesHC4A<- SAV.now[SAV.now$site == "HC4A", ]

#create data frame for table 
SAV_for_tableHC4A<-ddply(df_SAV_sitesHC4A, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                         'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                         "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                         "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                         '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                         "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                         "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                         "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                         "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                         "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                         "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                         "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                         "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                         "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC4A$Date<-format(SAV_for_tableHC4A$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC4A<- SAV_for_tableHC4A[,which(unlist(lapply(SAV_for_tableHC4A, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC4A[,1:7]
second_df<-SAV_for_tableHC4A[,8:ncol(SAV_for_tableHC4A)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC4A<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC4A <-knitr::kable(SAV_for_tableHC4A[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC4A[,1])), caption = SAV_for_tableHC4A[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC4A

#set site to make table ####
df_SAV_sitesHC5<- SAV.now[SAV.now$site == "HC5", ]

#create data frame for table 
SAV_for_tableHC5<-ddply(df_SAV_sitesHC5, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC5$Date<-format(SAV_for_tableHC5$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC5<- SAV_for_tableHC5[,which(unlist(lapply(SAV_for_tableHC5, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC5[,1:7]
second_df<-SAV_for_tableHC5[,8:ncol(SAV_for_tableHC5)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC5<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC5 <-knitr::kable(SAV_for_tableHC5[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC5[,1])), caption = SAV_for_tableHC5[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC5

#set site to make table ####
df_SAV_sitesHC6<- SAV.now[SAV.now$site == "HC6", ]

#create data frame for table 
SAV_for_tableHC6<-ddply(df_SAV_sitesHC6, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableHC6$Date<-format(SAV_for_tableHC6$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableHC6<- SAV_for_tableHC6[,which(unlist(lapply(SAV_for_tableHC6, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableHC6[,1:7]
second_df<-SAV_for_tableHC6[,8:ncol(SAV_for_tableHC6)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableHC6<-cbind(first_df,second_df)

#create the table####
SAV_TABLEHC6 <-knitr::kable(SAV_for_tableHC6[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableHC6[,1])), caption = SAV_for_tableHC6[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEHC6

#set site to make table ####
df_SAV_sitesMB1<- SAV.now[SAV.now$site == "MB1", ]

#create data frame for table 
SAV_for_tableMB1<-ddply(df_SAV_sitesMB1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableMB1$Date<-format(SAV_for_tableMB1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableMB1<- SAV_for_tableMB1[,which(unlist(lapply(SAV_for_tableMB1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableMB1[,1:7]
second_df<-SAV_for_tableMB1[,8:ncol(SAV_for_tableMB1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableMB1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEMB1 <-knitr::kable(SAV_for_tableMB1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableMB1[,1])), caption = SAV_for_tableMB1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEMB1

#set site to make table ####
df_SAV_sitesMB2<- SAV.now[SAV.now$site == "MB2", ]

#create data frame for table 
SAV_for_tableMB2<-ddply(df_SAV_sitesMB2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableMB2$Date<-format(SAV_for_tableMB2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableMB2<- SAV_for_tableMB2[,which(unlist(lapply(SAV_for_tableMB2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableMB2[,1:7]
second_df<-SAV_for_tableMB2[,8:ncol(SAV_for_tableMB2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableMB2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEMB2 <-knitr::kable(SAV_for_tableMB2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableMB2[,1])), caption = SAV_for_tableMB2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEMB2

#set site to make table ####
df_SAV_sitesMB3<- SAV.now[SAV.now$site == "MB3", ]

#create data frame for table 
SAV_for_tableMB3<-ddply(df_SAV_sitesMB3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableMB3$Date<-format(SAV_for_tableMB3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableMB3<- SAV_for_tableMB3[,which(unlist(lapply(SAV_for_tableMB3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableMB3[,1:7]
second_df<-SAV_for_tableMB3[,8:ncol(SAV_for_tableMB3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableMB3<-cbind(first_df,second_df)

#create the table####
SAV_TABLEMB3 <-knitr::kable(SAV_for_tableMB3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableMB3[,1])), caption = SAV_for_tableMB3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEMB3

#set site to make table ####
df_SAV_sitesBS1<- SAV.now[SAV.now$site == "BS1", ]

#create data frame for table 
SAV_for_tableBS1<-ddply(df_SAV_sitesBS1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableBS1$Date<-format(SAV_for_tableBS1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableBS1<- SAV_for_tableBS1[,which(unlist(lapply(SAV_for_tableBS1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableBS1[,1:7]
second_df<-SAV_for_tableBS1[,8:ncol(SAV_for_tableBS1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableBS1<-cbind(first_df,second_df)

#create the table####
SAV_TABLEBS1 <-knitr::kable(SAV_for_tableBS1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableBS1[,1])), caption = SAV_for_tableBS1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEBS1

#set site to make table ####
df_SAV_sitesBS2<- SAV.now[SAV.now$site == "BS2", ]

#create data frame for table 
SAV_for_tableBS2<-ddply(df_SAV_sitesBS2, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableBS2$Date<-format(SAV_for_tableBS2$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableBS2<- SAV_for_tableBS2[,which(unlist(lapply(SAV_for_tableBS2, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableBS2[,1:7]
second_df<-SAV_for_tableBS2[,8:ncol(SAV_for_tableBS2)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableBS2<-cbind(first_df,second_df)

#create the table####
SAV_TABLEBS2 <-knitr::kable(SAV_for_tableBS2[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableBS2[,1])), caption = SAV_for_tableBS2[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEBS2

#set site to make table ####
df_SAV_sitesBS3<- SAV.now[SAV.now$site == "BS3", ]

#create data frame for table 
SAV_for_tableBS3<-ddply(df_SAV_sitesBS3, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableBS3$Date<-format(SAV_for_tableBS3$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableBS3<- SAV_for_tableBS3[,which(unlist(lapply(SAV_for_tableBS3, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableBS3[,1:7]
second_df<-SAV_for_tableBS3[,8:ncol(SAV_for_tableBS3)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableBS3<-cbind(first_df,second_df)

#create the table####
SAV_TABLEBS3 <-knitr::kable(SAV_for_tableBS3[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableBS3[,1])), caption = SAV_for_tableBS3[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEBS3

#set site to make table ####
df_SAV_sitesBS4<- SAV.now[SAV.now$site == "BS4", ]

#create data frame for table 
SAV_for_tableBS4<-ddply(df_SAV_sitesBS4, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableBS4$Date<-format(SAV_for_tableBS4$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableBS4<- SAV_for_tableBS4[,which(unlist(lapply(SAV_for_tableBS4, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableBS4[,1:7]
second_df<-SAV_for_tableBS4[,8:ncol(SAV_for_tableBS4)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableBS4<-cbind(first_df,second_df)

#create the table####
SAV_TABLEBS4 <-knitr::kable(SAV_for_tableBS4[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableBS4[,1])), caption = SAV_for_tableBS4[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLEBS4

#set site to make table ####
df_SAV_sitesCS1<- SAV.now[SAV.now$site == "CS1", ]

#create data frame for table 
SAV_for_tableCS1<-ddply(df_SAV_sitesCS1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableCS1$Date<-format(SAV_for_tableCS1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableCS1<- SAV_for_tableCS1[,which(unlist(lapply(SAV_for_tableCS1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableCS1[,1:7]
second_df<-SAV_for_tableCS1[,8:ncol(SAV_for_tableCS1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableCS1<-cbind(first_df,second_df)

#create the table####
SAV_TABLECS1 <-knitr::kable(SAV_for_tableCS1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableCS1[,1])), 
                            caption = SAV_for_tableCS1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLECS1


#set site to make table ####
df_SAV_sitesTP1<- SAV.now[SAV.now$site == "TP1", ]

#create data frame for table 
SAV_for_tableTP1<-ddply(df_SAV_sitesTP1, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2),'<i>Ruppia<i> <br> \n <i>maritima</i>' = round(mean(Rup ),2),
                        '<i>Nitella spp.<i>' = round(mean(Nit ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Najas<i> <br> \n <i>marina<i>"  = round(mean(Naja ),2),"<i>Batophora spp.<i>"  = round(mean(Bat  ),2),
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2), 
                        "<i>Acetabularia spp.<i>" = round(mean(Ace ),2), "<i>Sargassum spp.<i>"  =round(mean(Sara  ),2), 
                        "<i>Halodule<i> <br> \n <i>wrightii<i>"   = round(mean(Halo  ),2),"<i>Thalassia<i> <br> \n <i>testudinum<i>"  = round(mean(Thal  ),2),
                        "<i>Polysiphonia spp.<i>"  =round(mean(Poly  ),2), "<i>Laurencia spp.<i>"   = round(mean(Lau  ),2),
                        "<i>Penicillus spp.<i>"   = round(mean(Pen   ),2),"<i>Dasya spp.<i>"   =round(mean(Day   ),2), 
                        "<i>Udotea spp.<i>" = round(mean(Udo ),2), "<i>Halimeda spp.<i>"    =round(mean(Hali    ),2), 
                        "<i>Ulva spp.<i>"  = round(mean(Ulv  ),2),"<i>Caulerpa spp.<i>"     =round(mean(Cau     ),2), 
                        "<i>Rhipocephalus<i> <br> \n <i>phoenix<i>"   = round(mean(Rhi   ),2)) 

#set date as abriviation
SAV_for_tableTP1$Date<-format(SAV_for_tableTP1$Date,"%d-%b-%y")
#remove na variables 
SAV_for_tableTP1<- SAV_for_tableTP1[,which(unlist(lapply(SAV_for_tableTP1, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_tableTP1[,1:7]
second_df<-SAV_for_tableTP1[,8:ncol(SAV_for_tableTP1)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_tableTP1<-cbind(first_df,second_df)

#create the table####
SAV_TABLETP1 <-knitr::kable(SAV_for_tableTP1[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_tableTP1[,1])), 
                            caption = SAV_for_tableTP1[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLETP1

#### #7P table ####
df_SAV_sites7P<- SevenP_SAV %>%
  dplyr::rename(HY = `Hydro. Year`,
                Utric = `Utr sp.`,
                Chara = `Cha hor`, 
                Spiro = `Spi sp.`,
                Clad = `Cla sp.`) %>% 
  mutate(HY = if_else(ymd(Date)> ymd('2000-06-01'), 
                      paste('20', HY, sep = ""), 
                      paste('19', HY, sep = "") ),
         HY = if_else(HY == '1900-01', 
                      '2000-01', 
                      HY ),
         SECCHI = ifelse(SECCHI == "Bottom",  DEPTH, SECCHI),
         SECCHI = ifelse(SECCHI == "bottom",  DEPTH, SECCHI),
        secci_per = as.numeric(SECCHI)/as.numeric(DEPTH)) %>% 
  dplyr::select(-c(notes)) %>% 
  filter(HY == this_report) 



#create data frame for table 
SAV_for_table7P<-ddply(df_SAV_sites7P, .(site, Date), summarise, 'Salinity <br> \n (PSU)'= round(mean(SALINITY ),2), 
                        'Water <br> \n Depth (cm)'= round(mean(DEPTH ),2),'Secchi <br> \n Depth (%)' = round(mean(secci_per)*100), 
                        "Water <br> \n Temp (\u00B0C)"= round(mean(as.numeric(TEMP) ),2),'<b>Total<b>'= round(mean(TOTAL),2), 
                        "<i>Utricularia spp.<i>" =round(mean(Utric ),2), '<i>Chara<i> <br> \n <i>hornemanii<i>' =round(mean(Chara ),2), 
                        "<i>Cladophora spp.<i>"  =round(mean(Clad),2), "<i>Spirogyra spp.<i>"= round(mean(Spiro),2)) 

#set date as abriviation
SAV_for_table7P$Date<-format(SAV_for_table7P$Date,"%d-%b-%y")
#remove na variables 
SAV_for_table7P<- SAV_for_table7P[,which(unlist(lapply(SAV_for_table7P, function(x) !all(is.na(x)))))]
#remove columns with only genus zeros 
first_df<-SAV_for_table7P[,1:7]
second_df<-SAV_for_table7P[,8:ncol(SAV_for_table7P)]
second_df<-second_df[, colSums(na.omit(second_df) != 0) > 0]
SAV_for_table7P<-cbind(first_df,second_df)

#create the table####
SAV_TABLE7P <-knitr::kable(SAV_for_table7P[,-1] , format="html", escape=FALSE, align=rep('c', length(SAV_for_table7P[,1])), 
                            caption = SAV_for_table7P[1,1]) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size=12)
SAV_TABLE7P


#sample dates ####

ddply(SAV.now, .(Month, area), summarise, sav_date = mean(Date)) %>% 
  mutate(what = paste0(area, "_SAV"),
         sav_date = as.character(sav_date)) %>% 
write_csv( paste('/Databases/tidy/Reports/',
                           this_report,
                           '_sav_sample_dates.csv',
                           sep = ""))
#analysis ####

#model1 = lme(TOTAL ~ Month, random=~1|region,
#             data=SAV.now,
#             method="REML",
#             na.action=na.exclude)
#anova.lme(model1,
#          type="sequential",
#          adjustSigma = FALSE)
#library("multcomp")
#posthoc = glht(model1,
#               linfct = mcp(Month="Tukey"))
#mcs = summary(posthoc,
#              test=adjusted("single-step"))
#mcs
#cld(mcs,
#    level=0.05,
#    decreasing=TRUE)
#
#
#summary(glm(all_SAV$TOTAL~all_SAV$DEPTH*all_SAV$SALINITY, family = "poisson"))
#TukeyHSD(glm(all_SAV$TOTAL~all_SAV$DEPTH*all_SAV$SALINITY, family = "poisson"))


# data to send ------------------------------------------------------------

write.csv(
  all_SAV %>% 
    filter(HY == this_report), 
  paste('/Databases/tidy/Reports/ACOE/Data to send/AUDUBON_SAV_data_', 
        this_report, 
        '.csv',
        sep = ''))

