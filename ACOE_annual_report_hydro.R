
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
library("nlme")
library("multcomp")
library("digest")
library("tidyverse")
library('readxl')



#FOR NEXT YEAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#AVEARGE DAILY RANGE FOR MONTH!!!!!!!!!!!!!!!!!!!!!!!!!
#TEMPERATURE MEAN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





# #annual report ----------------------------------------------------------




#HYDRO YEARS TO USE and remove
this_report<- "2022-23" 

#HYDRO YEAR TO REMOVE if some newer data that this HY is included, check tail() for NA values
remove_HY<- "2023-24"


#check columns, change names and order

#columns to remove; structured after regions, sites
drops <- c("X","X.1","X.2","X.3", "Sp..Cond...mS.cm.", "Sp..Cond.","Water.Level..ft.", "Stage..ft..NAVD88",
           "DO...sat.", "DO..mg.L.", "pH", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", "X.11", "X.12",
           "ORP", "X.13", "X.14", "X.15", "X.16", "X.17", "X.18", "Rain..in.." )



# date data ---------------------------------------------------------------

sample_dates.fish<-read.csv(paste('/Databases/tidy/Reports/',
                                  this_report,
                                  '_fish_sample_dates.csv',
                                  sep = ""), header = T)
sample_dates.SAV<-read.csv(paste('/Databases/tidy/Reports/',
                                 this_report,
                                 '_sav_sample_dates.csv',
                                 sep = ""), header = T)

# daily data --------------------------------------------------------------

raw_df.hydro.day <- 
  lapply(file.path(
    '/Hydrology/Quality Checked Data_all years/Hydro Files',
    pattern = 
      #grab all .xls files
      list.files(
        path = '/Hydrology/Quality Checked Data_all years/Hydro Files',
        pattern='*.xls')) %>% 
      str_remove_all('[~$]'), 
    #need tp have first sheet as data in .xlx files
    function(x) read_excel(x,
                           #make sure no columns are logical
                           guess_max = Inf)) %>% 
  #dplyr::rename each element after list names
  purrr::set_names(
    list.files(
      path = '/Hydrology/Quality Checked Data_all years/Hydro Files') %>% 
      str_replace(
        pattern = '.xlsx?', 
        replacement = '_DAY'))


# read excel sheets - hourlies --------------------------------------------


read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- paste(filename %>% 
                      str_remove(
                        '/Hydrology/Quality Checked Data_all years/'),
                    sheets)
  x}

#LMB ####
#TR

TR_day<- raw_df.hydro.day$TR_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity, 
         rain = `Rain (in.)`)


TR_h <- 
read_excel_allsheets('/Hydrology/Quality Checked Data_all years/TR hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)", 
                   'Rain')) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(TR_h) <- NULL






#EC

EC_day<- raw_df.hydro.day$EC_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH","ORP")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity #, rain = `Rain (in.)`
         )


EC_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/EC hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88 (Benchmark name = ENP-BM9)",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(EC_h) <- NULL

#TC ####

#WJ

WJ_day<- raw_df.hydro.day$WJ_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity #, rain = `Rain (in.)`
  )


WJ_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/WJ hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(WJ_h) <- NULL

#JB

JB_day<- raw_df.hydro.day$JB_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH","ORP")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity , 
         rain = `Rain (in.)`  )

JB_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/JB hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(JB_h) <- NULL

#LS ####
#SB

SB_day<- raw_df.hydro.day$SB_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH","ORP")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity #, rain = `Rain (in.)`
  )

SB_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/SB hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(SB_h) <- NULL


#HC

HC_day<- raw_df.hydro.day$HC_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity , rain = `Rain (in.)`
  )


HC_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/HC hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(HC_h) <- NULL

#SBB####
#MB

MB_day<- raw_df.hydro.day$MB_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity # , rain = `Rain (in.)`
         )

MB_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/MB hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(MB_h) <- NULL

#BS

BS_day<- raw_df.hydro.day$BS_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH","ORP")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity, 
         rain = `Rain (in.)`)

BS_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/BS hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Water Level (ft)",
                   "Stage (ft) NAVD88",
                   "Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(BS_h) <- NULL

#CS

CS_day<- raw_df.hydro.day$CS_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "pH")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity # , rain = `Rain (in.)`
         )


CS_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/CS hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(CS_h) <- NULL

#extra sites####
#TP

TP_day<- raw_df.hydro.day$TP_HYDRO_DAY %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.","DO (%sat)",
                   "DO (mg/L)","pH","ORP")) %>% 
  dplyr::rename(depth = Depth, 
         temp = `Water Temp`, 
         sal = Salinity # , rain = `Rain (in.)`
         )

TP_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/TP hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
         depth = `Water Level (cm)`, 
         temp = `Temp (°C)`, 
         sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(TP_h) <- NULL

#sevenP
sevenP_day<- raw_df.hydro.day$`7P_HYDRO_DAY` %>% 
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond.")) %>% 
  dplyr::rename(depth = Depth, 
                temp = `Water Temp`, 
                sal = Salinity # , rain = `Rain (in.)`
  )

sevenP_h <- 
  read_excel_allsheets('/Hydrology/Quality Checked Data_all years/7P hourlies.xlsx') %>% 
  rlist::list.rbind()%>%  
  as.data.frame() %>% 
  dplyr::select(-c("Sp. Cond. (mS/cm)")) %>% 
  dplyr::rename(Date_Time = `Date    Time`, 
                depth = `Water Level (cm)`, 
                temp = `Temp (°C)`, 
                sal = `Salinity (psu)`) %>% 
  mutate(Date_Time = as.POSIXct(Date_Time, "%m/%d/%Y %H:%M", tz = "UTC") %>% 
           format(format='%Y-%m-%d %H:%M'))
rownames(sevenP_h) <- NULL

#HYDRO YEARS TO USE and remove####


TR_day<- subset(TR_day, !(HY == remove_HY))
TR_h<-subset(TR_h, !(HY == remove_HY))
EC_day<-subset(EC_day, !(HY == remove_HY))
EC_h<-subset(EC_h, !(HY == remove_HY))
WJ_day<-subset(WJ_day, !(HY == remove_HY))
WJ_h<-subset(WJ_h, !(HY == remove_HY))
JB_day<-subset(JB_day, !(HY == remove_HY))
JB_h<-subset(JB_h, !(HY == remove_HY))
SB_day<-subset(SB_day, !(HY == remove_HY))
SB_h<-subset(SB_h, !(HY == remove_HY))
HC_day<-subset(HC_day, !(HY == remove_HY))
HC_h<-subset(HC_h, !(HY == remove_HY))
MB_day<-subset(MB_day, !(HY == remove_HY))
MB_h<-subset(MB_h, !(HY == remove_HY))
BS_day<-subset(BS_day, !(HY == remove_HY))
BS_h<-subset(BS_h, !(HY == remove_HY))
CS_day<-subset(CS_day, !(HY == remove_HY))
CS_day<- subset(CS_day, !(YEAR == "NA"))
CS_h<-subset(CS_h, !(HY == remove_HY))
TP_day<-subset(TP_day, !(HY == remove_HY))
TP_h<-subset(TP_h, !(HY == remove_HY))
sevenP_h<-subset(sevenP_h, !(HY == remove_HY))
sevenP_day<-subset(sevenP_day, !(HY == remove_HY))
sevenP_day<- subset(sevenP_day, !(YEAR == "NA"))


#THIS YEARS ANNUAL MEAN WITH POR
#LMB####

#TR
TR_day.now<- which(TR_day$HY == this_report)
TR_hour.now<- which(TR_h$HY == this_report)
#create this HY and POR data frames
TRday.POR<- TR_day[-c(TR_day.now),]
TRday.now<- TR_day[c(TR_day.now),]
TRhour.POR<- TR_h[-c(TR_hour.now),]
TRhour.now<- TR_h[c(TR_hour.now),]


#TR depth
TRhour.POR$MonthDay <- paste( TRhour.POR$MONTH, TRhour.POR$DAY, sep="-" )
TRhour.now$MonthDay <- paste( TRhour.now$MONTH, TRhour.now$DAY, sep="-" )
depthTR.POR.df<- ddply(TRhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthTR.POR.df<-subset(depthTR.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthTR.POR.df<-subset(depthTR.POR.df, !(MonthDay == "NA-NA" ))
depthTR.POR.df<-subset(depthTR.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthTR.POR.df), ncol = 2, nrow = nrow(depthTR.POR.df)))
ydf<- unique(TRhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthTR.POR.df<- merge(xydf,depthTR.POR.df, by = "MonthDay")
depthTR.POR<- depthTR.POR.df[order(depthTR.POR.df$ind),]
#TR sal
salTR.POR.df<- ddply(TRhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salTR.POR.df<-subset(salTR.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salTR.POR.df<-subset(salTR.POR.df, !(MonthDay == "NA-NA" ))
salTR.POR.df<-subset(salTR.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salTR.POR.df), ncol = 2, nrow = nrow(salTR.POR.df)))
ydf<- unique(TRhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salTR.POR.df<- merge(xydf,salTR.POR.df, by = "MonthDay")
salTR.POR<- salTR.POR.df[order(salTR.POR.df$ind),]

#EC
EC_day.now<- which(EC_day$HY == this_report)
EC_hour.now<- which(EC_h$HY == this_report)
#create this HY and POR data frames
ECday.POR<- EC_day[-c(EC_day.now),]
ECday.now<- EC_day[c(EC_day.now),]
EChour.POR<- EC_h[-c(EC_hour.now),]
EChour.now<- EC_h[c(EC_hour.now),]

#EC depth
EChour.POR$MonthDay <- paste( EChour.POR$MONTH, EChour.POR$DAY, sep="-" )
EChour.now$MonthDay <- paste( EChour.now$MONTH, EChour.now$DAY, sep="-" )
depthEC.POR.df<- ddply(EChour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthEC.POR.df<-subset(depthEC.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthEC.POR.df<-subset(depthEC.POR.df, !(MonthDay == "NA-NA" ))
depthEC.POR.df<-subset(depthEC.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthEC.POR.df), ncol = 2, nrow = nrow(depthEC.POR.df)))
ydf<- unique(EChour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthEC.POR.df<- merge(xydf,depthEC.POR.df, by = "MonthDay")
depthEC.POR<- depthEC.POR.df[order(depthEC.POR.df$ind),]
#EC sal
salEC.POR.df<- ddply(EChour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salEC.POR.df<-subset(salEC.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salEC.POR.df<-subset(salEC.POR.df, !(MonthDay == "NA-NA" ))
salEC.POR.df<-subset(salEC.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salEC.POR.df), ncol = 2, nrow = nrow(salEC.POR.df)))
ydf<- unique(EChour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salEC.POR.df<- merge(xydf,salEC.POR.df, by = "MonthDay")
salEC.POR<- salEC.POR.df[order(salEC.POR.df$ind),]


#TC####

#JB
JB_day.now<- which(JB_day$HY == this_report)
JB_hour.now<- which(JB_h$HY == this_report)
#create this HY and POR data frames
JBday.POR<- JB_day[-c(JB_day.now),]
JBday.now<- JB_day[c(JB_day.now),]
JBhour.POR<- JB_h[-c(JB_hour.now),]
JBhour.now<- JB_h[c(JB_hour.now),]

#JB depth
JBhour.POR$MonthDay <- paste( JBhour.POR$MONTH, JBhour.POR$DAY, sep="-" )
JBhour.now$MonthDay <- paste( JBhour.now$MONTH, JBhour.now$DAY, sep="-" )
depthJB.POR.df<- ddply(JBhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthJB.POR.df<-subset(depthJB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthJB.POR.df<-subset(depthJB.POR.df, !(MonthDay == "1-0" ))
depthJB.POR.df<-subset(depthJB.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthJB.POR.df), ncol = 2, nrow = nrow(depthJB.POR.df)))
ydf<- unique(JBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthJB.POR.df<- merge(xydf,depthJB.POR.df, by = "MonthDay")
depthJB.POR<- depthJB.POR.df[order(depthJB.POR.df$ind),]
#JB sal
salJB.POR.df<- ddply(JBhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salJB.POR.df<-subset(salJB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salJB.POR.df<-subset(salJB.POR.df, !(MonthDay == "1-0" ))
salJB.POR.df<-subset(salJB.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salJB.POR.df), ncol = 2, nrow = nrow(salJB.POR.df)))
ydf<- unique(JBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salJB.POR.df<- merge(xydf,salJB.POR.df, by = "MonthDay")
salJB.POR<- salJB.POR.df[order(salJB.POR.df$ind),]

#WJ
WJ_day.now<- which(WJ_day$HY == this_report)
WJ_hour.now<- which(WJ_h$HY == this_report)
#create this HY and POR data frames
WJday.POR<- WJ_day[-c(WJ_day.now),]
WJday.now<- WJ_day[c(WJ_day.now),]
WJhour.POR<- WJ_h[-c(WJ_hour.now),]
WJhour.now<- WJ_h[c(WJ_hour.now),]

#WJ depth
WJhour.POR$MonthDay <- paste( WJhour.POR$MONTH, WJhour.POR$DAY, sep="-" )
WJhour.now$MonthDay <- paste( WJhour.now$MONTH, WJhour.now$DAY, sep="-" )
depthWJ.POR.df<- ddply(WJhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthWJ.POR.df<-subset(depthWJ.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthWJ.POR.df<-subset(depthWJ.POR.df, !(MonthDay == "1-0" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthWJ.POR.df), ncol = 2, nrow = nrow(depthWJ.POR.df)))
ydf<- unique(WJhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthWJ.POR.df<- merge(xydf,depthWJ.POR.df, by = "MonthDay")
depthWJ.POR<- depthWJ.POR.df[order(depthWJ.POR.df$ind),]
#WJ sal
salWJ.POR.df<- ddply(WJhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salWJ.POR.df<-subset(salWJ.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salWJ.POR.df<-subset(salWJ.POR.df, !(MonthDay == "1-0" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salWJ.POR.df), ncol = 2, nrow = nrow(salWJ.POR.df)))
ydf<- unique(WJhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salWJ.POR.df<- merge(xydf,salWJ.POR.df, by = "MonthDay")
salWJ.POR<- salWJ.POR.df[order(salWJ.POR.df$ind),]

#LS####

#HC
HC_day.now<- which(HC_day$HY == this_report)
HC_hour.now<- which(HC_h$HY == this_report)
#create this HY and POR data frames
HCday.POR<- HC_day[-c(HC_day.now),]
HCday.now<- HC_day[c(HC_day.now),]
HChour.POR<- HC_h[-c(HC_hour.now),]
HChour.now<- HC_h[c(HC_hour.now),]

#HC depth
HChour.POR$MonthDay <- paste( HChour.POR$MONTH, HChour.POR$DAY, sep="-" )
HChour.now$MonthDay <- paste( HChour.now$MONTH, HChour.now$DAY, sep="-" )
depthHC.POR.df<- ddply(HChour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthHC.POR.df<-subset(depthHC.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthHC.POR.df<-subset(depthHC.POR.df, !(MonthDay == "1-0" ))
depthHC.POR.df<-subset(depthHC.POR.df, !(MonthDay == "-" ))
depthHC.POR.df<-subset(depthHC.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthHC.POR.df), ncol = 2, nrow = nrow(depthHC.POR.df)))
ydf<- unique(HChour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthHC.POR.df<- merge(xydf,depthHC.POR.df, by = "MonthDay")
depthHC.POR<- depthHC.POR.df[order(depthHC.POR.df$ind),]
#HC sal
salHC.POR.df<- ddply(HChour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salHC.POR.df<-subset(salHC.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salHC.POR.df<-subset(salHC.POR.df, !(MonthDay == "1-0" ))
salHC.POR.df<-subset(salHC.POR.df, !(MonthDay == "-" ))
salHC.POR.df<-subset(salHC.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salHC.POR.df), ncol = 2, nrow = nrow(salHC.POR.df)))
ydf<- unique(HChour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salHC.POR.df<- merge(xydf,salHC.POR.df, by = "MonthDay")
salHC.POR<- salHC.POR.df[order(salHC.POR.df$ind),]


#SB
SB_day.now<- which(SB_day$HY == this_report)
SB_hour.now<- which(SB_h$HY == this_report)
#create this HY and POR data frames
SBday.POR<- SB_day[-c(SB_day.now),]
SBday.now<- SB_day[c(SB_day.now),]
SBhour.POR<- SB_h[-c(SB_hour.now),]
SBhour.now<- SB_h[c(SB_hour.now),]

#SB depth
SBhour.POR$MonthDay <- paste( SBhour.POR$MONTH, SBhour.POR$DAY, sep="-" )
SBhour.now$MonthDay <- paste( SBhour.now$MONTH, SBhour.now$DAY, sep="-" )
depthSB.POR.df<- ddply(SBhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthSB.POR.df<-subset(depthSB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthSB.POR.df<-subset(depthSB.POR.df, !(MonthDay == "1-0" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthSB.POR.df), ncol = 2, nrow = nrow(depthSB.POR.df)))
ydf<- unique(SBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthSB.POR.df<- merge(xydf,depthSB.POR.df, by = "MonthDay")
depthSB.POR<- depthSB.POR.df[order(depthSB.POR.df$ind),]
#SB sal
salSB.POR.df<- ddply(SBhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salSB.POR.df<-subset(salSB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salSB.POR.df<-subset(salSB.POR.df, !(MonthDay == "1-0" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salSB.POR.df), ncol = 2, nrow = nrow(salSB.POR.df)))
ydf<- unique(SBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salSB.POR.df<- merge(xydf,salSB.POR.df, by = "MonthDay")
salSB.POR<- salSB.POR.df[order(salSB.POR.df$ind),]




#SBB####

#MB
MB_day.now<- which(MB_day$HY == this_report)
MB_hour.now<- which(MB_h$HY == this_report)
#create this HY and POR data frames
MBday.POR<- MB_day[-c(MB_day.now),]
MBday.now<- MB_day[c(MB_day.now),]
MBhour.POR<- MB_h[-c(MB_hour.now),]
MBhour.now<- MB_h[c(MB_hour.now),]

#MB depth
MBhour.POR$MonthDay <- paste( MBhour.POR$MONTH, MBhour.POR$DAY, sep="-" )
MBhour.now$MonthDay <- paste( MBhour.now$MONTH, MBhour.now$DAY, sep="-" )
depthMB.POR.df<- ddply(MBhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthMB.POR.df<-subset(depthMB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthMB.POR.df<-subset(depthMB.POR.df, !(MonthDay == "1-0" ))
depthMB.POR.df<-subset(depthMB.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthMB.POR.df), ncol = 2, nrow = nrow(depthMB.POR.df)))
ydf<- unique(MBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthMB.POR.df<- merge(xydf,depthMB.POR.df, by = "MonthDay")
depthMB.POR<- depthMB.POR.df[order(depthMB.POR.df$ind),]
#MB sal
salMB.POR.df<- ddply(MBhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salMB.POR.df<-subset(salMB.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salMB.POR.df<-subset(salMB.POR.df, !(MonthDay == "1-0" ))
salMB.POR.df<-subset(salMB.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salMB.POR.df), ncol = 2, nrow = nrow(salMB.POR.df)))
ydf<- unique(MBhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salMB.POR.df<- merge(xydf,salMB.POR.df, by = "MonthDay")
salMB.POR<- salMB.POR.df[order(salMB.POR.df$ind),]


#BS
BS_day.now<- which(BS_day$HY == this_report)
BS_hour.now<- which(BS_h$HY == this_report)
#create this HY and POR data frames
BSday.POR<- BS_day[-c(BS_day.now),]
BSday.now<- BS_day[c(BS_day.now),]
BShour.POR<- BS_h[-c(BS_hour.now),]
BShour.now<- BS_h[c(BS_hour.now),]

#BS depth
BShour.POR$MonthDay <- paste( BShour.POR$MONTH, BShour.POR$DAY, sep="-" )
BShour.now$MonthDay <- paste( BShour.now$MONTH, BShour.now$DAY, sep="-" )
depthBS.POR.df<- ddply(BShour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthBS.POR.df<-subset(depthBS.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthBS.POR.df<-subset(depthBS.POR.df, !(MonthDay == "1-0" ))
depthBS.POR.df<-subset(depthBS.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthBS.POR.df), ncol = 2, nrow = nrow(depthBS.POR.df)))
ydf<- unique(BShour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthBS.POR.df<- merge(xydf,depthBS.POR.df, by = "MonthDay")
depthBS.POR<- depthBS.POR.df[order(depthBS.POR.df$ind),]
#BS sal
salBS.POR.df<- ddply(BShour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salBS.POR.df<-subset(salBS.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salBS.POR.df<-subset(salBS.POR.df, !(MonthDay == "1-0" ))
salBS.POR.df<-subset(salBS.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salBS.POR.df), ncol = 2, nrow = nrow(salBS.POR.df)))
ydf<- unique(BShour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salBS.POR.df<- merge(xydf,salBS.POR.df, by = "MonthDay")
salBS.POR<- salBS.POR.df[order(salBS.POR.df$ind),]

#CS
CS_day.now<- which(CS_day$HY == this_report)
CS_hour.now<- which(CS_h$HY == this_report)
#create this HY and POR data frames
CSday.POR<- CS_day[-c(CS_day.now),]
CSday.now<- CS_day[c(CS_day.now),]
CShour.POR<- CS_h[-c(CS_hour.now),]
CShour.now<- CS_h[c(CS_hour.now),]

#CS depth
CShour.POR$MonthDay <- paste( CShour.POR$MONTH, CShour.POR$DAY, sep="-" )
CShour.now$MonthDay <- paste( CShour.now$MONTH, CShour.now$DAY, sep="-" )
depthCS.POR.df<- ddply(CShour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE),
                       depthPOR.sd = sd(depth, na.rm=TRUE))
depthCS.POR.df<-subset(depthCS.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthCS.POR.df<-subset(depthCS.POR.df, !(MonthDay == "1-0" ))
depthCS.POR.df<-subset(depthCS.POR.df, !(MonthDay == "NA-NA" ))
depthCS.POR.df<-subset(depthCS.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthCS.POR.df), ncol = 2, nrow = nrow(depthCS.POR.df)))
ydf<- unique(CShour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthCS.POR.df<- merge(xydf,depthCS.POR.df, by = "MonthDay")
depthCS.POR<- depthCS.POR.df[order(depthCS.POR.df$ind),]
#CS sal
salCS.POR.df<- ddply(CShour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE),
                     salPOR.sd = sd(sal, na.rm=TRUE))
salCS.POR.df<-subset(salCS.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salCS.POR.df<-subset(salCS.POR.df, !(MonthDay == "1-0" ))
salCS.POR.df<-subset(salCS.POR.df, !(MonthDay == "NA-NA" ))
salCS.POR.df<-subset(salCS.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salCS.POR.df), ncol = 2, nrow = nrow(salCS.POR.df)))
ydf<- unique(CShour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salCS.POR.df<- merge(xydf,salCS.POR.df, by = "MonthDay")
salCS.POR<- salCS.POR.df[order(salCS.POR.df$ind),]

#extra sites 

#TP
TP_day.now<- which(TP_day$HY == this_report)
TP_hour.now<- which(TP_h$HY == this_report)
#create this HY and POR data frames
TPday.POR<- TP_day[-c(TP_day.now),]
TPday.now<- TP_day[c(TP_day.now),]
TPhour.POR<- TP_h[-c(TP_hour.now),]
TPhour.now<- TP_h[c(TP_hour.now),]

#TP depth
TPhour.POR$MonthDay <- paste( TPhour.POR$MONTH, TPhour.POR$DAY, sep="-" )
TPhour.now$MonthDay <- paste( TPhour.now$MONTH, TPhour.now$DAY, sep="-" )
depthTP.POR.df<- ddply(TPhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE))
depthTP.POR.df<-subset(depthTP.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthTP.POR.df<-subset(depthTP.POR.df, !(MonthDay == "1-0" ))
depthTP.POR.df<-subset(depthTP.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthTP.POR.df), ncol = 2, nrow = nrow(depthTP.POR.df)))
ydf<- unique(TPhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthTP.POR.df<- merge(xydf,depthTP.POR.df, by = "MonthDay")
depthTP.POR<- depthTP.POR.df[order(depthTP.POR.df$ind),]
#TP sal
salTP.POR.df<- ddply(TPhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE))
salTP.POR.df<-subset(salTP.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salTP.POR.df<-subset(salTP.POR.df, !(MonthDay == "1-0" ))
salTP.POR.df<-subset(salTP.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salTP.POR.df), ncol = 2, nrow = nrow(salTP.POR.df)))
ydf<- unique(TPhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salTP.POR.df<- merge(xydf,salTP.POR.df, by = "MonthDay")
salTP.POR<- salTP.POR.df[order(salTP.POR.df$ind),]

#sevenP
sevenP_day.now<- which(sevenP_day$HY == this_report)
sevenP_hour.now<- which(sevenP_h$HY == this_report)
#create this HY and POR data frames
sevenPday.POR<- sevenP_day[-c(sevenP_day.now),]
sevenPday.now<- sevenP_day[c(sevenP_day.now),]
sevenPhour.POR<- sevenP_h[-c(sevenP_hour.now),]
sevenPhour.now<- sevenP_h[c(sevenP_hour.now),]

#sevenP depth
sevenPhour.POR$MonthDay <- paste( sevenPhour.POR$MONTH, sevenPhour.POR$DAY, sep="-" )
sevenPhour.now$MonthDay <- paste( sevenPhour.now$MONTH, sevenPhour.now$DAY, sep="-" )
depthsevenP.POR.df<- ddply(sevenPhour.POR, .(MonthDay), summarise, depthPOR = mean(depth, na.rm=TRUE))
depthsevenP.POR.df<-subset(depthsevenP.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
depthsevenP.POR.df<-subset(depthsevenP.POR.df, !(MonthDay == "1-0" ))
depthsevenP.POR.df<-subset(depthsevenP.POR.df, !(MonthDay == "-" ))
depthsevenP.POR.df<-subset(depthsevenP.POR.df, !(MonthDay == "NA-NA" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(depthsevenP.POR.df), ncol = 2, nrow = nrow(depthsevenP.POR.df)))
ydf<- unique(sevenPhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthsevenP.POR.df<- merge(xydf,depthsevenP.POR.df, by = "MonthDay")
depthsevenP.POR<- depthsevenP.POR.df[order(depthsevenP.POR.df$ind),]
#sevenP sal
salsevenP.POR.df<- ddply(sevenPhour.POR, .(MonthDay), summarise, SalPOR = mean(sal, na.rm=TRUE))
salsevenP.POR.df<-subset(salsevenP.POR.df, !(MonthDay == "#VALUE!-#VALUE!" ))
salsevenP.POR.df<-subset(salsevenP.POR.df, !(MonthDay == "1-0" ))
salsevenP.POR.df<-subset(salsevenP.POR.df, !(MonthDay == "-" ))
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 next year
xdf<-data.frame(matrix( data = 1:nrow(salsevenP.POR.df), ncol = 2, nrow = nrow(salsevenP.POR.df)))
ydf<- unique(sevenPhour.now$MonthDay)
xdf<- xdf[1:length(ydf),]
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salsevenP.POR.df<- merge(xydf,salsevenP.POR.df, by = "MonthDay")
salsevenP.POR<- salsevenP.POR.df[order(salsevenP.POR.df$ind),]



#create data set for grapth
#sample dates, fish/sav####

#havent included TP and 7P here



TR_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "TR")),]
TR_sampledate.fish$LABEL<- as.Date(TR_sampledate.fish$LABEL, "%d-%b-%y") 
TR_sampledate.fish$fish_date<- as.Date(TR_sampledate.fish$fish_date, "%m/%d/%Y")
EC_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "EC")),]
EC_sampledate.fish$LABEL<- as.Date(EC_sampledate.fish$LABEL, "%d-%b-%y") 
EC_sampledate.fish$fish_date<- as.Date(EC_sampledate.fish$fish_date, "%m/%d/%Y")
JB_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "JB")),]
JB_sampledate.fish$LABEL<- as.Date(JB_sampledate.fish$LABEL, "%d-%b-%y") 
JB_sampledate.fish$fish_date<- as.Date(JB_sampledate.fish$fish_date, "%m/%d/%Y")
WJ_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "WJ")),]
WJ_sampledate.fish$LABEL<- as.Date(WJ_sampledate.fish$LABEL, "%d-%b-%y") 
WJ_sampledate.fish$fish_date<- as.Date(WJ_sampledate.fish$fish_date, "%m/%d/%Y") 
SB_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "SB")),]
SB_sampledate.fish$LABEL<- as.Date(SB_sampledate.fish$LABEL, "%d-%b-%y") 
SB_sampledate.fish$fish_date<- as.Date(SB_sampledate.fish$fish_date, "%m/%d/%Y") 
HC_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "HC")),]
HC_sampledate.fish$LABEL<- as.Date(HC_sampledate.fish$LABEL, "%d-%b-%y") 
HC_sampledate.fish$fish_date<- as.Date(HC_sampledate.fish$fish_date, "%m/%d/%Y") 
MB_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "MB")),]
MB_sampledate.fish$LABEL<- as.Date(MB_sampledate.fish$LABEL, "%d-%b-%y") 
MB_sampledate.fish$fish_date<- as.Date(MB_sampledate.fish$fish_date, "%m/%d/%Y") 
BS_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "BS")),]
BS_sampledate.fish$LABEL<- as.Date(BS_sampledate.fish$LABEL, "%d-%b-%y") 
BS_sampledate.fish$fish_date<- as.Date(BS_sampledate.fish$fish_date, "%m/%d/%Y") 
CS_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "CS")),]
CS_sampledate.fish$LABEL<- as.Date(CS_sampledate.fish$LABEL, "%d-%b-%y") 
CS_sampledate.fish$fish_date<- as.Date(CS_sampledate.fish$fish_date, "%m/%d/%Y") 
TP_sampledate.fish <- sample_dates.fish[c(which(sample_dates.fish$site == "TP")),]
TP_sampledate.fish$LABEL<- as.Date(TP_sampledate.fish$LABEL, "%d-%b-%y") 
TP_sampledate.fish$fish_date<- as.Date(TP_sampledate.fish$fish_date, "%m/%d/%Y") 

sample_dates.SAV<- sample_dates.SAV %>% dplyr::rename(site = area) %>% 
  mutate(LABEL = sav_date)

TR_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "TR")),]
TR_sampledate.SAV$LABEL<- as.Date(TR_sampledate.SAV$LABEL) 
TR_sampledate.SAV$sav_date<- as.Date(TR_sampledate.SAV$sav_date) 
EC_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "EC")),]
EC_sampledate.SAV$LABEL<- as.Date(EC_sampledate.SAV$LABEL,) 
EC_sampledate.SAV$sav_date<- as.Date(EC_sampledate.SAV$sav_date) 
JB_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "JB")),]
JB_sampledate.SAV$LABEL<- as.Date(JB_sampledate.SAV$LABEL) 
JB_sampledate.SAV$sav_date<- as.Date(JB_sampledate.SAV$sav_date)
WJ_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "WJ")),]
WJ_sampledate.SAV$LABEL<- as.Date(WJ_sampledate.SAV$LABEL) 
WJ_sampledate.SAV$sav_date<- as.Date(WJ_sampledate.SAV$sav_date) 
SB_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "SB")),]
SB_sampledate.SAV$LABEL<- as.Date(SB_sampledate.SAV$LABEL) 
SB_sampledate.SAV$sav_date<- as.Date(SB_sampledate.SAV$sav_date)
HC_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "HC")),]
HC_sampledate.SAV$LABEL<- as.Date(HC_sampledate.SAV$LABEL) 
HC_sampledate.SAV$sav_date<- as.Date(HC_sampledate.SAV$sav_date) 
MB_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "MB")),]
MB_sampledate.SAV$LABEL<- as.Date(MB_sampledate.SAV$LABEL) 
MB_sampledate.SAV$sav_date<- as.Date(MB_sampledate.SAV$sav_date) 
BS_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "BS")),]
BS_sampledate.SAV$LABEL<- as.Date(BS_sampledate.SAV$LABEL) 
BS_sampledate.SAV$sav_date<- as.Date(BS_sampledate.SAV$sav_date) 
CS_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "CS")),]
CS_sampledate.SAV$LABEL<- as.Date(CS_sampledate.SAV$LABEL) 
CS_sampledate.SAV$sav_date<- as.Date(CS_sampledate.SAV$sav_date) 
TP_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "TP")),]
TP_sampledate.SAV$LABEL<- as.Date(TP_sampledate.SAV$LABEL) 
TP_sampledate.SAV$sav_date<- as.Date(TP_sampledate.SAV$sav_date) 


TP_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "TP")),]
TP_sampledate.SAV$LABEL<- as.Date(TP_sampledate.SAV$LABEL) 
TP_sampledate.SAV$sav_date<- as.Date(TP_sampledate.SAV$sav_date) 

sevenP_sampledate.SAV <- sample_dates.SAV[c(which(sample_dates.SAV$site == "7P")),]
sevenP_sampledate.SAV$LABEL<- as.Date(sevenP_sampledate.SAV$LABEL) 
sevenP_sampledate.SAV$sav_date<- as.Date(sevenP_sampledate.SAV$sav_date) 

#test this when i get the data from Devon
#TEST.fish.dates<- read.csv("INCOMPLETE_sample_dates_incomplete.csv", header = T)
#TEST.fish.dates$LABEL<-as.Date(CS_sampledate.SAV$LABEL, "%d-%b-%Y") 
#TEST.fish.dates<-TEST.fish.dates[,5:7]
#names(TEST.fish.dates)[names(TEST.fish.dates) == "TSC_site_code"] <- "site"

#LMB####
#TR
daily.TR.data<-cbind(TRday.now, depthTR.POR)                                      
daily.TR.data<-cbind(daily.TR.data, salTR.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.TR.data<-daily.TR.data[ , !(names(daily.TR.data) %in% drops)]

TR.DF1<-daily.TR.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
TR.DF1$what<-sprintf("NOW", 1:nrow(TR.DF1))
TR.DF2<-daily.TR.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
TR.DF2$what<- sprintf("POR", 1:nrow(TR.DF2)) 
names(TR.DF2)[names(TR.DF2) == "depthPOR"] <- "depth"
names(TR.DF2)[names(TR.DF2) == "SalPOR"] <- "sal"
daily.TR.DF<- rbind(TR.DF1, TR.DF2)
daily.TR.DF<- daily.TR.DF[!(format(daily.TR.DF$LABEL,"%m") == "02" & 
                              format(daily.TR.DF$LABEL, "%d") == "29"), , drop = FALSE]
#EC
daily.EC.data<-cbind(ECday.now, depthEC.POR)                                      
daily.EC.data<-cbind(daily.EC.data, salEC.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.EC.data<-daily.EC.data[ , !(names(daily.EC.data) %in% drops)]

EC.DF1<-daily.EC.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
EC.DF1$what<-sprintf("NOW", 1:nrow(EC.DF1))
EC.DF2<-daily.EC.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
EC.DF2$what<- sprintf("POR", 1:nrow(EC.DF2)) 
names(EC.DF2)[names(EC.DF2) == "depthPOR"] <- "depth"
names(EC.DF2)[names(EC.DF2) == "SalPOR"] <- "sal"
daily.EC.DF<- rbind(EC.DF1, EC.DF2)
daily.EC.DF<- daily.EC.DF[!(format(daily.EC.DF$LABEL,"%m") == "02" & 
                                          format(daily.EC.DF$LABEL, "%d") == "29"), , drop = FALSE]
#TC####
#JB
daily.JB.data<-cbind(JBday.now, depthJB.POR)                                      
daily.JB.data<-cbind(daily.JB.data, salJB.POR)                                 
drops <- c("MonthDay","exJBa", "ind")
daily.JB.data<-daily.JB.data[ , !(names(daily.JB.data) %in% drops)]

JB.DF1<-daily.JB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
JB.DF1$what<-sprintf("NOW", 1:nrow(JB.DF1))
JB.DF2<-daily.JB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
JB.DF2$what<- sprintf("POR", 1:nrow(JB.DF2)) 
names(JB.DF2)[names(JB.DF2) == "depthPOR"] <- "depth"
names(JB.DF2)[names(JB.DF2) == "SalPOR"] <- "sal"
daily.JB.DF<- rbind(JB.DF1, JB.DF2)
daily.JB.DF<- daily.JB.DF[!(format(daily.JB.DF$LABEL,"%m") == "02" & 
                              format(daily.JB.DF$LABEL, "%d") == "29"), , drop = FALSE]
#WJ
daily.WJ.data <-cbind(WJday.now, depthWJ.POR)                                      
daily.WJ.data<-cbind(daily.WJ.data, salWJ.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.WJ.data<-daily.WJ.data[ , !(names(daily.WJ.data) %in% drops)]

WJ.DF1<-daily.WJ.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
WJ.DF1$what<-sprintf("NOW", 1:nrow(WJ.DF1))
WJ.DF2<-daily.WJ.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
WJ.DF2$what<- sprintf("POR", 1:nrow(WJ.DF2)) 
names(WJ.DF2)[names(WJ.DF2) == "depthPOR"] <- "depth"
names(WJ.DF2)[names(WJ.DF2) == "SalPOR"] <- "sal"
daily.WJ.DF<- rbind(WJ.DF1, WJ.DF2)
daily.WJ.DF<- daily.WJ.DF[!(format(daily.WJ.DF$LABEL,"%m") == "02" & 
                              format(daily.WJ.DF$LABEL, "%d") == "29"), , drop = FALSE]

#LS####
#SB
daily.SB.data <-cbind(SBday.now, depthSB.POR)                                      
daily.SB.data<-cbind(daily.SB.data, salSB.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.SB.data<-daily.SB.data[ , !(names(daily.SB.data) %in% drops)]

SB.DF1<-daily.SB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
SB.DF1$what<-sprintf("NOW", 1:nrow(SB.DF1))
SB.DF2<-daily.SB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
SB.DF2$what<- sprintf("POR", 1:nrow(SB.DF2)) 
names(SB.DF2)[names(SB.DF2) == "depthPOR"] <- "depth"
names(SB.DF2)[names(SB.DF2) == "SalPOR"] <- "sal"
daily.SB.DF<- rbind(SB.DF1, SB.DF2)
daily.SB.DF<- daily.SB.DF[!(format(daily.SB.DF$LABEL,"%m") == "02" & 
                              format(daily.SB.DF$LABEL, "%d") == "29"), , drop = FALSE]
#HC
daily.HC.data<-cbind(HCday.now, depthHC.POR)                                      
daily.HC.data<-cbind(daily.HC.data, salHC.POR)                                 
drops <- c("MonthDay","exHCa", "ind")
daily.HC.data<-daily.HC.data[ , !(names(daily.HC.data) %in% drops)]

HC.DF1<-daily.HC.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
HC.DF1$what<-sprintf("NOW", 1:nrow(HC.DF1))
HC.DF2<-daily.HC.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
HC.DF2$what<- sprintf("POR", 1:nrow(HC.DF2)) 
names(HC.DF2)[names(HC.DF2) == "depthPOR"] <- "depth"
names(HC.DF2)[names(HC.DF2) == "SalPOR"] <- "sal"
daily.HC.DF<- rbind(HC.DF1, HC.DF2)
daily.HC.DF<- daily.HC.DF[!(format(daily.HC.DF$LABEL,"%m") == "02" & 
                              format(daily.HC.DF$LABEL, "%d") == "29"), , drop = FALSE]


#SBB####
#MB
daily.MB.data<-cbind(MBday.now, depthMB.POR)                                      
daily.MB.data<-cbind(daily.MB.data, salMB.POR)                                 
drops <- c("MonthDay","exMBa", "ind")
daily.MB.data<-daily.MB.data[ , !(names(daily.MB.data) %in% drops)]

MB.DF1<-daily.MB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
MB.DF1$what<-sprintf("NOW", 1:nrow(MB.DF1))
MB.DF2<-daily.MB.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
MB.DF2$what<- sprintf("POR", 1:nrow(MB.DF2)) 
names(MB.DF2)[names(MB.DF2) == "depthPOR"] <- "depth"
names(MB.DF2)[names(MB.DF2) == "SalPOR"] <- "sal"
daily.MB.DF<- rbind(MB.DF1, MB.DF2)
daily.MB.DF<- daily.MB.DF[!(format(daily.MB.DF$LABEL,"%m") == "02" & 
                              format(daily.MB.DF$LABEL, "%d") == "29"), , drop = FALSE]
#BS
daily.BS.data<-cbind(BSday.now, depthBS.POR)                                      
daily.BS.data<-cbind(daily.BS.data, salBS.POR)                                 
drops <- c("MonthDay","exBSa", "ind")
daily.BS.data<-daily.BS.data[ , !(names(daily.BS.data) %in% drops)]

BS.DF1<-daily.BS.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
BS.DF1$what<-sprintf("NOW", 1:nrow(BS.DF1))
BS.DF2<-daily.BS.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
BS.DF2$what<- sprintf("POR", 1:nrow(BS.DF2)) 
names(BS.DF2)[names(BS.DF2) == "depthPOR"] <- "depth"
names(BS.DF2)[names(BS.DF2) == "SalPOR"] <- "sal"
daily.BS.DF<- rbind(BS.DF1, BS.DF2)
daily.BS.DF<- daily.BS.DF[!(format(daily.BS.DF$LABEL,"%m") == "02" & 
                              format(daily.BS.DF$LABEL, "%d") == "29"), , drop = FALSE]
#CS
daily.CS.data<-cbind(CSday.now, depthCS.POR)                                      
daily.CS.data<-cbind(daily.CS.data, salCS.POR)                                 
drops <- c("MonthDay","exCSa", "ind")
daily.CS.data<-daily.CS.data[ , !(names(daily.CS.data) %in% drops)]

CS.DF1<-daily.CS.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
CS.DF1$what<-sprintf("NOW", 1:nrow(CS.DF1))
CS.DF2<-daily.CS.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
CS.DF2$what<- sprintf("POR", 1:nrow(CS.DF2)) 
names(CS.DF2)[names(CS.DF2) == "depthPOR"] <- "depth"
names(CS.DF2)[names(CS.DF2) == "SalPOR"] <- "sal"
daily.CS.DF<- rbind(CS.DF1, CS.DF2)
daily.CS.DF<- daily.CS.DF[!(format(daily.CS.DF$LABEL,"%m") == "02" & 
                              format(daily.CS.DF$LABEL, "%d") == "29"), , drop = FALSE]

#TP
daily.TP.data<-cbind(TPday.now, depthTP.POR)                                      
daily.TP.data<-cbind(daily.TP.data, salTP.POR)                                 
drops <- c("MonthDay","exTPa", "ind")
daily.TP.data<-daily.TP.data[ , !(names(daily.TP.data) %in% drops)]

TP.DF1<-daily.TP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
TP.DF1$what<-sprintf("NOW", 1:nrow(TP.DF1))
TP.DF2<-daily.TP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
TP.DF2$what<- sprintf("POR", 1:nrow(TP.DF2)) 
names(TP.DF2)[names(TP.DF2) == "depthPOR"] <- "depth"
names(TP.DF2)[names(TP.DF2) == "SalPOR"] <- "sal"
daily.TP.DF<- rbind(TP.DF1, TP.DF2)
daily.TP.DF<- daily.TP.DF[!(format(daily.TP.DF$LABEL,"%m") == "02" & 
                              format(daily.TP.DF$LABEL, "%d") == "29"), , drop = FALSE]

#extra sites####
#TP
daily.TP.data<-cbind(TPday.now, depthTP.POR)                                      
daily.TP.data<-cbind(daily.TP.data, salTP.POR)                                 
drops <- c("MonthDay","exTPa", "ind")
daily.TP.data<-daily.TP.data[ , !(names(daily.TP.data) %in% drops)]

TP.DF1<-daily.TP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
TP.DF1$what<-sprintf("NOW", 1:nrow(TP.DF1))
TP.DF2<-daily.TP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
TP.DF2$what<- sprintf("POR", 1:nrow(TP.DF2)) 
names(TP.DF2)[names(TP.DF2) == "depthPOR"] <- "depth"
names(TP.DF2)[names(TP.DF2) == "SalPOR"] <- "sal"
daily.TP.DF<- rbind(TP.DF1, TP.DF2)
#sevenP
daily.sevenP.data<-cbind(sevenPday.now, depthsevenP.POR)                                      
daily.sevenP.data<-cbind(daily.sevenP.data, salsevenP.POR)                                 
drops <- c("MonthDay","exsevenPa", "ind")
daily.sevenP.data<-daily.sevenP.data[ , !(names(daily.sevenP.data) %in% drops)]

sevenP.DF1<-daily.sevenP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depth, sal)
sevenP.DF1$what<-sprintf("NOW", 1:nrow(sevenP.DF1))
sevenP.DF2<-daily.sevenP.data %>% dplyr::select(YEAR, MONTH, DAY, LABEL, Season, HY, depthPOR, SalPOR)
sevenP.DF2$what<- sprintf("POR", 1:nrow(sevenP.DF2)) 
names(sevenP.DF2)[names(sevenP.DF2) == "depthPOR"] <- "depth"
names(sevenP.DF2)[names(sevenP.DF2) == "SalPOR"] <- "sal"
daily.sevenP.DF<- rbind(sevenP.DF1, sevenP.DF2)




#line graph - daily sal and depth with POR and sample dates
#                            DAILY 

#MERGE SAMPLE DATES AND HYDRO DATA####

daily.TR.DF.fish<- left_join(x = daily.TR.DF, y = TR_sampledate.fish, by = "LABEL")
daily.TR.DF.fish1<- daily.TR.DF.fish %>% filter(what == 'NOW')
daily.TR.DF.fish1<- daily.TR.DF.fish1[!(format(daily.TR.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.TR.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.EC.DF.fish<- left_join(x = daily.EC.DF, y = EC_sampledate.fish, by = "LABEL" )
daily.EC.DF.fish1<- daily.EC.DF.fish %>% filter(what == 'NOW')
daily.EC.DF.fish1<- daily.EC.DF.fish1[!(format(daily.EC.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.EC.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.JB.DF.fish<- left_join(x = daily.JB.DF, y = JB_sampledate.fish, by = "LABEL" )
daily.JB.DF.fish1<- daily.JB.DF.fish %>% filter(what == 'NOW')
daily.JB.DF.fish1<- daily.JB.DF.fish1[!(format(daily.JB.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.JB.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.WJ.DF.fish<- left_join(x = daily.WJ.DF, y = WJ_sampledate.fish, by = "LABEL" )
daily.WJ.DF.fish1<- daily.WJ.DF.fish %>% filter(what == 'NOW')
daily.WJ.DF.fish1<- daily.WJ.DF.fish1[!(format(daily.WJ.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.WJ.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.SB.DF.fish<- left_join(x = daily.SB.DF, y = SB_sampledate.fish, by = "LABEL" )
daily.SB.DF.fish1<- daily.SB.DF.fish %>% filter(what == 'NOW')
daily.SB.DF.fish1<- daily.SB.DF.fish1[!(format(daily.SB.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.SB.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.HC.DF.fish<- left_join(x = daily.HC.DF, y = HC_sampledate.fish, by = "LABEL" )
daily.HC.DF.fish1<- daily.HC.DF.fish %>% filter(what == 'NOW')
daily.HC.DF.fish1<- daily.HC.DF.fish1[!(format(daily.HC.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.HC.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.MB.DF.fish<- left_join(x = daily.MB.DF, y = MB_sampledate.fish, by = "LABEL" )
daily.MB.DF.fish1<- daily.MB.DF.fish %>% filter(what == 'NOW')
daily.MB.DF.fish1<- daily.MB.DF.fish1[!(format(daily.MB.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.MB.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.BS.DF.fish<- left_join(x = daily.BS.DF, y = BS_sampledate.fish, by = "LABEL" )
daily.BS.DF.fish1<- daily.BS.DF.fish %>% filter(what == 'NOW')
daily.BS.DF.fish1<- daily.BS.DF.fish1[!(format(daily.BS.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.BS.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.CS.DF.fish<- left_join(x = daily.CS.DF, y = CS_sampledate.fish, by = "LABEL" )
daily.CS.DF.fish1<- daily.CS.DF.fish %>% filter(what == 'NOW')
daily.CS.DF.fish1<- daily.CS.DF.fish1[!(format(daily.CS.DF.fish1$LABEL,"%m") == "02" & 
                                        format(daily.CS.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]
daily.TP.DF.fish<- left_join(x = daily.TP.DF, y = TP_sampledate.fish, by = "LABEL" )
daily.TP.DF.fish1<- daily.TP.DF.fish %>% filter(what == 'NOW')
daily.TP.DF.fish1<- daily.TP.DF.fish1[!(format(daily.TP.DF.fish1$LABEL,"%m") == "02" & 
                                          format(daily.TP.DF.fish1$LABEL, "%d") == "29"), , drop = FALSE]

daily.TR.DF.SAV<- left_join(x = daily.TR.DF, y = TR_sampledate.SAV, by = "LABEL" )
daily.TR.DF.SAV1<- daily.TR.DF.SAV[(daily.TR.DF.SAV$what.x=="NOW"),] 
daily.TR.DF.SAV1<- daily.TR.DF.SAV1[!(format(daily.TR.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.TR.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.EC.DF.SAV<- left_join(x = daily.EC.DF, y = EC_sampledate.SAV, by = "LABEL" )
daily.EC.DF.SAV1<- daily.EC.DF.SAV[(daily.EC.DF.SAV$what.x=="NOW"),] 
daily.EC.DF.SAV1<- daily.EC.DF.SAV1[!(format(daily.EC.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.EC.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.JB.DF.SAV<- left_join(x = daily.JB.DF, y = JB_sampledate.SAV, by = "LABEL" )
daily.JB.DF.SAV1<- daily.JB.DF.SAV[(daily.JB.DF.SAV$what.x=="NOW"),]
daily.JB.DF.SAV1<- daily.JB.DF.SAV1[!(format(daily.JB.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.JB.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.WJ.DF.SAV<- left_join(x = daily.WJ.DF, y = WJ_sampledate.SAV, by = "LABEL" )
daily.WJ.DF.SAV1<- daily.WJ.DF.SAV[(daily.WJ.DF.SAV$what.x=="NOW"),]
daily.WJ.DF.SAV1<- daily.WJ.DF.SAV1[!(format(daily.WJ.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.WJ.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.SB.DF.SAV<- left_join(x = daily.SB.DF, y = SB_sampledate.SAV, by = "LABEL" )
daily.SB.DF.SAV1<- daily.SB.DF.SAV[(daily.SB.DF.SAV$what.x=="NOW"),]
daily.SB.DF.SAV1<- daily.SB.DF.SAV1[!(format(daily.SB.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.SB.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.HC.DF.SAV<- left_join(x = daily.HC.DF, y = HC_sampledate.SAV, by = "LABEL" )
daily.HC.DF.SAV1<- daily.HC.DF.SAV[(daily.HC.DF.SAV$what.x=="NOW"),]
daily.HC.DF.SAV1<- daily.HC.DF.SAV1[!(format(daily.HC.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.HC.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.MB.DF.SAV<- left_join(x = daily.MB.DF, y = MB_sampledate.SAV, by = "LABEL" )
daily.MB.DF.SAV1<- daily.MB.DF.SAV[(daily.MB.DF.SAV$what.x=="NOW"),]
daily.MB.DF.SAV1<- daily.MB.DF.SAV1[!(format(daily.MB.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.MB.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.BS.DF.SAV<- left_join(x = daily.BS.DF, y = BS_sampledate.SAV, by = "LABEL" )
daily.BS.DF.SAV1<- daily.BS.DF.SAV[(daily.BS.DF.SAV$what.x=="NOW"),]
daily.BS.DF.SAV1<- daily.BS.DF.SAV1[!(format(daily.BS.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.BS.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.CS.DF.SAV<- left_join(x = daily.CS.DF, y = CS_sampledate.SAV, by = "LABEL" )
daily.CS.DF.SAV1<- daily.CS.DF.SAV[(daily.CS.DF.SAV$what.x=="NOW"),]
daily.CS.DF.SAV1<- daily.CS.DF.SAV1[!(format(daily.CS.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.CS.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.TP.DF.SAV<- left_join(x = daily.TP.DF, y = TP_sampledate.SAV, by = "LABEL" )
daily.TP.DF.SAV1<- daily.TP.DF.SAV[(daily.TP.DF.SAV$what.x=="NOW"),]
daily.TP.DF.SAV1<- daily.TP.DF.SAV1[!(format(daily.TP.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.TP.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]
daily.7P.DF.SAV<- left_join(x = daily.sevenP.DF, y = sevenP_sampledate.SAV, by = "LABEL" )
daily.7P.DF.SAV1<- daily.7P.DF.SAV[(daily.7P.DF.SAV$what.x=="NOW"),]
daily.7P.DF.SAV1<- daily.7P.DF.SAV1[!(format(daily.7P.DF.SAV1$LABEL,"%m") == "02" & 
                                        format(daily.7P.DF.SAV1$LABEL, "%d") == "29"), , drop = FALSE]

#LMB####
#TR
d.plot.TR.DAILY<- ggplot(data = daily.TR.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.TR.DF.fish1, aes(x= as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.TR.DF.SAV1, aes(x= as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("TR") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.TR.DF$depth)/5)*5, 
                                  round(max(daily.TR.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.EC.DF$depth)/5)*5, round(max(daily.TR.DF$depth)/5)*5))+
  #ylim(0, 55)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.TR.DAILY <- ggplot(data = daily.TR.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.TR.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.TR.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("TR") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.TR.data$sal)/5)*5, ceiling(max(daily.TR.data$sal)), 5))+
  #ylim(0, 25)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#EC
d.plot.EC.DAILY<- ggplot(data = daily.EC.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.EC.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.EC.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("EC") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.TR.DF$depth)/5)*5, 
                                  round(max(daily.TR.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.EC.DF$depth)/5)*5, round(max(daily.TR.DF$depth)/5)*5))+
  #ylim(0, 55)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.EC.DAILY <- ggplot(data = daily.EC.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.EC.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.EC.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("EC") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.EC.data$sal)/5)*5, ceiling(max(daily.EC.data$sal)), 5))+
  #ylim(0, 25)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


#TC####
#WJ
d.plot.WJ.DAILY<- ggplot(data = daily.WJ.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.WJ.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.WJ.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("WJ") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.WJ.DF$depth)/5)*5, 
                                  round(max(daily.WJ.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.WJ.DF$depth)/5)*5, round(max(daily.WJ.DF$depth)/5)*5))+
  #ylim(round(min(daily.JB.data$depth)/5)*5, round(max(daily.JB.data$depth)/5)*5)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.WJ.DAILY <- ggplot(data = daily.WJ.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.WJ.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.WJ.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("WJ") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.WJ.data$sal)/5)*5, ceiling(max(daily.WJ.data$sal)), 5))+
  #ylim(0, 30)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#JB
d.plot.JB.DAILY<- ggplot(data = daily.JB.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.JB.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.JB.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("JB") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.JB.DF$depth)/5)*5, 
                                round_any(max(daily.JB.DF$depth),5), 
                                5),
                     limits = c(round(min(daily.WJ.DF$depth)/5)*5, round(max(daily.JB.DF$depth)/5)*5))+
  #ylim(0, 55)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.JB.DAILY <- ggplot(data = daily.JB.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.JB.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.JB.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("JB") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.JB.data$sal)/5)*5, ceiling(max(daily.JB.data$sal)), 5))+
  #ylim(0, 30)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


#LS####
#SB
d.plot.SB.DAILY<- ggplot(data = daily.SB.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.SB.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.SB.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("SB") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.SB.DF$depth)/5)*5, 
                                  round(max(daily.SB.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.HC.DF$depth)/5)*5, round(max(daily.SB.DF$depth)/5)*5))+
  #ylim(0, 55)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.SB.DAILY <- ggplot(data = daily.SB.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.SB.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.SB.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("SB") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.SB.data$sal)/5)*5, ceiling(max(daily.SB.data$sal)), 5))+
  #ylim(0, 35)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#HC
d.plot.HC.DAILY<- ggplot(data = daily.HC.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.HC.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.HC.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("HC") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.SB.DF$depth)/5)*5, 
                                  round(max(daily.SB.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.HC.DF$depth)/5)*5, round(max(daily.SB.DF$depth)/5)*5))+
  #ylim(0, 55)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.HC.DAILY <- ggplot(data = daily.HC.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.HC.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.HC.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("HC") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.HC.data$sal)/5)*5, ceiling(max(daily.HC.data$sal)), 5))+
  #ylim(0, 35)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#SBB####
#MB


d.plot.MB.DAILY<- ggplot(data = daily.MB.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.MB.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.MB.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("MB") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.MB.DF$depth)/5)*5, 
                                  round(max(daily.MB.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.BS.DF$depth)/5)*5, round(max(daily.MB.DF$depth)/5)*5))+
  #ylim(0, 80)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.MB.DAILY <- ggplot(data = daily.MB.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.MB.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.MB.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("MB") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.MB.data$sal)/5)*5, ceiling(max(daily.MB.data$sal)), 5))+
  #ylim(0, 45)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#BS
d.plot.BS.DAILY<- ggplot(data = daily.BS.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.BS.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.BS.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("BS") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.MB.DF$depth)/5)*5, 
                                  round(max(daily.MB.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.BS.DF$depth)/5)*5, round(max(daily.MB.DF$depth)/5)*5))+
  #ylim(0, 80)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.BS.DAILY <- ggplot(data = daily.BS.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.BS.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.BS.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("BS") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.BS.data$sal)/5)*5, ceiling(max(daily.BS.data$sal)), 5))+
  #ylim(0, 45)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#CS
d.plot.CS.DAILY<- ggplot(data = daily.CS.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.CS.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.CS.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("CS") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks = seq(round(min(daily.MB.DF$depth)/5)*5, 
                                  round(max(daily.MB.DF$depth)/5)*5, 
                                  5),
                     limits = c(round(min(daily.BS.DF$depth)/5)*5, round(max(daily.MB.DF$depth)/5)*5))+
  #ylim(0, 80)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.CS.DAILY <- ggplot(data = daily.CS.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.CS.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.CS.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("CS") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.CS.data$sal)/5)*5, ceiling(max(daily.CS.data$sal)), 5))+
  #ylim(0, 45)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


#TP
d.plot.TP.DAILY<- ggplot(data = daily.TP.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  geom_point(data = daily.TP.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.TP.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("TP") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.TP.data$depth)/5)*5, ceiling(max(daily.TP.data$depth)), 5))+
  ##ylim(0, 100)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.TP.DAILY <- ggplot(data = daily.TP.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.TP.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.TP.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("TP") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.TP.data$sal)/5)*5, ceiling(max(daily.TP.data$sal)), 5))+
  ##ylim(0, 75)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())




#7P
d.plot.sevenP.DAILY<- ggplot(data = daily.sevenP.DF, aes(x=as.Date(LABEL), y=depth, colour=what ))+
  geom_line()+
  #geom_point(data = daily.sevenP.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour="red", shape=16, size=5)+
  geom_point(data = daily.7P.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
 scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", color = "black")+
  theme_bw() +
  xlab("7P") + ylab("Water Level (cm)") + 
 scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(floor(min(daily.sevenP.data$depth)/5)*5, ceiling(max(daily.sevenP.data$depth)), 5))+
  #ylim(0, 100)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

s.plot.sevenP.DAILY <- ggplot(data = daily.sevenP.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  #geom_point(data = daily.sevenP.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.7P.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("7P") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  #scale_y_continuous(breaks=seq(round(min(daily.sevenP.data$sal)/0.5)*5, max(daily.sevenP.data$sal), 5))+
  #ylim(0, 75)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#plot dump - daily and POR with sample dates ####
d.plot.TR.DAILY
s.plot.TR.DAILY
d.plot.EC.DAILY
s.plot.EC.DAILY
d.plot.WJ.DAILY
s.plot.WJ.DAILY
d.plot.JB.DAILY
s.plot.JB.DAILY
d.plot.SB.DAILY
s.plot.SB.DAILY
d.plot.HC.DAILY
s.plot.HC.DAILY
d.plot.MB.DAILY
s.plot.MB.DAILY
d.plot.BS.DAILY
s.plot.BS.DAILY
d.plot.CS.DAILY
s.plot.CS.DAILY
d.plot.TP.DAILY
s.plot.TP.DAILY
d.plot.sevenP.DAILY
s.plot.sevenP.DAILY



#Exceedance Curves Depth and Sal
#LMB ####
TR_day1<-TR_day %>% 
  mutate(site = "TR", 
         area = "LMB") %>% 
  dplyr::select(-rain)
EC_day1<-EC_day %>% 
  mutate(site = "EC", 
         area = "LMB")

#TC ####
WJ_day1<-WJ_day%>% 
  mutate(site = "WJ", 
         area = "TC")
JB_day1<-JB_day%>% 
  mutate(site = "JB", 
         area = "TC")%>% 
  dplyr::select(-rain)

#LS ####
SB_day1<-SB_day%>% 
  mutate(site = "SB", 
         area = "LS")
HC_day1<-HC_day%>% 
  mutate(site = "HC", 
         area = "LS")%>% 
  dplyr::select(-rain)

#SBB ####
MB_day1<-MB_day%>% 
  mutate(site = "MB", 
         area = "SBB")
BS_day1<-BS_day%>% 
  mutate(site = "BS", 
         area = "SBB")%>% 
  dplyr::select(-rain)
CS_day1<-CS_day%>% 
  mutate(site = "CS", 
         area = "SBB")

#extra SBB ####
TP_day1<-TP_day%>% 
  mutate(site = "TP", 
         area = "N_SBB")

# bind all sites ####
all.sites<- rbind(TR_day1, EC_day1, 
                  WJ_day1 %>% dplyr::select(-`Rain (in.)`), 
                  JB_day1, 
                  SB_day1, HC_day1, 
                  MB_day1%>% dplyr::select(-`Rain (in.)`),
                  BS_day1, CS_day1,
                  TP_day1)



all.sites$Season<- mapvalues(all.sites$Season, from = c(1,2), to = c("Dry Season", "Wet Season"))
hy.now.xyz<- which(all.sites$HY == this_report)
hy.now<- all.sites[c(hy.now.xyz),]

write.csv(all.sites, '/Databases/tidy/Reports/ACOE/Data to send/hydro_all_sites_for_fish.csv')
# this year seasons
hy.now.wet.xyz <- which(hy.now$Season == "Wet Season")
hy.now.wet <- hy.now[c(hy.now.wet.xyz),]
hy.now.dry.xyz <- which(hy.now$Season == "Dry Season")
hy.now.dry <- hy.now[c(hy.now.dry.xyz),]


# this year mean annual and seasonal depth and sal NUMBERS
#LMB depth ####
#TR
TR.depth_this_year_ann<- mean(hy.now[hy.now$site == "TR", ]$depth)
TR.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "TR", ]$depth)
TR.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "TR", ]$depth)
#EC
EC.depth_this_year_ann<- mean(hy.now[hy.now$site == "EC", ]$depth)
EC.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "EC", ]$depth)
EC.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "EC", ]$depth)
#TC depth####
#WJ
WJ.depth_this_year_ann<- mean(hy.now[hy.now$site == "WJ", ]$depth)
WJ.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "WJ", ]$depth)
WJ.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "WJ", ]$depth)
#JB
JB.depth_this_year_ann<- mean(hy.now[hy.now$site == "JB", ]$depth)
JB.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "JB", ]$depth)
JB.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "JB", ]$depth)
#LS depth####
#SB
SB.depth_this_year_ann<- mean(hy.now[hy.now$site == "SB", ]$depth)
SB.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "SB", ]$depth)
SB.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "SB", ]$depth)
#HC
HC.depth_this_year_ann<- mean(hy.now[hy.now$site == "HC", ]$depth)
HC.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "HC", ]$depth)
HC.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "HC", ]$depth)
#SBB depth####
#MB
MB.depth_this_year_ann<- mean(hy.now[hy.now$site == "MB", ]$depth)
MB.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "MB", ]$depth)
MB.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "MB", ]$depth)
#BS
BS.depth_this_year_ann<- mean(hy.now[hy.now$site == "BS", ]$depth)
BS.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "BS", ]$depth)
BS.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "BS", ]$depth)
#CS
CS.depth_this_year_ann<- mean(hy.now[hy.now$site == "CS", ]$depth)
CS.depth_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "CS", ]$depth)
CS.depth_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "CS", ]$depth)
#LMB sal####
#TR
TR.sal_this_year_ann<- mean(hy.now[hy.now$site == "TR", ]$sal)
TR.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "TR", ]$sal)
TR.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "TR", ]$sal)
#EC
EC.sal_this_year_ann<- mean(hy.now[hy.now$site == "EC", ]$sal)
EC.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "EC", ]$sal)
EC.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "EC", ]$sal)
#TC sal####
#JB
JB.sal_this_year_ann<- mean(hy.now[hy.now$site == "JB", ]$sal)
JB.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "JB", ]$sal)
JB.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "JB", ]$sal)
#WJ
WJ.sal_this_year_ann<- mean(hy.now[hy.now$site == "WJ", ]$sal)
WJ.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "WJ", ]$sal)
WJ.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "WJ", ]$sal)
#LS sal####
#SB
SB.sal_this_year_ann<- mean(hy.now[hy.now$site == "SB", ]$sal)
SB.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "SB", ]$sal)
SB.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "SB", ]$sal)
#HC
HC.sal_this_year_ann<- mean(hy.now[hy.now$site == "HC", ]$sal)
HC.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "HC", ]$sal)
HC.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "HC", ]$sal)
#SBB sal####
#MB
MB.sal_this_year_ann<- mean(hy.now[hy.now$site == "MB", ]$sal)
MB.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "MB", ]$sal)
MB.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "MB", ]$sal)
#BS
BS.sal_this_year_ann<- mean(hy.now[hy.now$site == "BS", ]$sal)
BS.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "BS", ]$sal)
BS.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "BS", ]$sal)
#CS
CS.sal_this_year_ann<- mean(hy.now[hy.now$site == "CS", ]$sal)
CS.sal_this_year_wet<- mean(hy.now.wet[hy.now.wet$site == "CS", ]$sal)
CS.sal_this_year_dry<- mean(hy.now.dry[hy.now.dry$site == "CS", ]$sal)

#mean all other years mean annaul and seasons depth and sal ####
#LMB POR depth ####
#TR 
depth.TR.ann <- ddply(all.sites[all.sites$site == "TR", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "TR", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.TR.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.TR.wet <- depth.season.ex[c(wet.ex.xyz),]
#EC 
depth.EC.ann <- ddply(all.sites[all.sites$site == "EC", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "EC", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.EC.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.EC.wet <- depth.season.ex[c(wet.ex.xyz),]
#TC POR depth####
#JB
depth.JB.ann <- ddply(all.sites[all.sites$site == "JB", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "JB", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.JB.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.JB.wet <- depth.season.ex[c(wet.ex.xyz),]
#WJ
depth.WJ.ann <- ddply(all.sites[all.sites$site == "WJ", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "WJ", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.WJ.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.WJ.wet <- depth.season.ex[c(wet.ex.xyz),]
#LS POR depth ####
#SB
depth.SB.ann <- ddply(all.sites[all.sites$site == "SB", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "SB", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.SB.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.SB.wet <- depth.season.ex[c(wet.ex.xyz),]
#HC
depth.HC.ann <- ddply(all.sites[all.sites$site == "HC", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "HC", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.HC.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.HC.wet <- depth.season.ex[c(wet.ex.xyz),]
#SBB POR depth####
#MB
depth.MB.ann <- ddply(all.sites[all.sites$site == "MB", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "MB", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.MB.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.MB.wet <- depth.season.ex[c(wet.ex.xyz),]
#BS
depth.BS.ann <- ddply(all.sites[all.sites$site == "BS", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "BS", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.BS.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.BS.wet <- depth.season.ex[c(wet.ex.xyz),]
#CS
depth.CS.ann <- ddply(all.sites[all.sites$site == "CS", ], .(HY, site), summarise, depth = mean(depth))
depth.season.ex<- ddply(all.sites[all.sites$site == "CS", ], .(HY, Season, site), summarise, depth = mean(depth))
dry.ex.xyz <- which(depth.season.ex$Season == "Dry Season")
depth.CS.dry <- depth.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(depth.season.ex$Season == "Wet Season")
depth.CS.wet <- depth.season.ex[c(wet.ex.xyz),]

#data file exceedance depth ####

#remove unused years 
drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "<NA>")
drops.longsite3 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "1990-91", "<NA>")
#long sites
depth.TR.ann2<-depth.TR.ann[ ! depth.TR.ann$HY %in% drops.longsite, ]
depth.TR.wet2<-depth.TR.wet[ ! depth.TR.wet$HY %in% drops.longsite, ]
depth.TR.dry2<-depth.TR.dry[ ! depth.TR.dry$HY %in% drops.longsite, ]
depth.JB.ann2<-depth.JB.ann[ ! depth.JB.ann$HY %in% drops.longsite, ]
depth.JB.wet2<-depth.JB.wet[ ! depth.JB.wet$HY %in% drops.longsite, ]
depth.JB.dry2<-depth.JB.dry[ ! depth.JB.dry$HY %in% drops.longsite, ]
depth.HC.ann2<-depth.HC.ann[ ! depth.HC.ann$HY %in% drops.longsite, ]
depth.HC.wet2<-depth.HC.wet[ ! depth.HC.wet$HY %in% drops.longsite, ]
depth.HC.dry2<-depth.HC.dry[ ! depth.HC.dry$HY %in% drops.longsite, ]
depth.BS.ann2<-depth.BS.ann[ ! depth.BS.ann$HY %in% drops.longsite3, ]
depth.BS.wet2<-depth.BS.wet[ ! depth.BS.wet$HY %in% drops.longsite3, ]
depth.BS.dry2<-depth.BS.dry[ ! depth.BS.dry$HY %in% drops.longsite3, ]
#short sites 
drops.longsite2 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "2001-02", "2002-03",
                     "2003-04")
depth.CS.ann2<-depth.CS.ann[ ! depth.CS.ann$HY %in% drops.longsite2, ]
depth.CS.wet2<-depth.CS.wet[ ! depth.CS.wet$HY %in% drops.longsite2, ]
depth.CS.dry2<-depth.CS.dry[ ! depth.CS.dry$HY %in% drops.longsite2, ]
depth.MB.ann2<-depth.MB.ann[ ! depth.MB.ann$HY %in% drops.longsite2, ]
depth.MB.wet2<-depth.MB.wet[ ! depth.MB.wet$HY %in% drops.longsite2, ]
depth.MB.dry2<-depth.MB.dry[ ! depth.MB.dry$HY %in% drops.longsite2, ]
depth.SB.ann2<-depth.SB.ann[ ! depth.SB.ann$HY %in% drops.longsite2, ]
depth.SB.wet2<-depth.SB.wet[ ! depth.SB.wet$HY %in% drops.longsite2, ]
depth.SB.dry2<-depth.SB.dry[ ! depth.SB.dry$HY %in% drops.longsite2, ]
depth.WJ.ann2<-depth.WJ.ann[ ! depth.WJ.ann$HY %in% drops.longsite2, ]
depth.WJ.wet2<-depth.WJ.wet[ ! depth.WJ.wet$HY %in% drops.longsite2, ]
depth.WJ.dry2<-depth.WJ.dry[ ! depth.WJ.dry$HY %in% drops.longsite2, ]
depth.EC.ann2<-depth.EC.ann[ ! depth.EC.ann$HY %in% drops.longsite2, ]
depth.EC.wet2<-depth.EC.wet[ ! depth.EC.wet$HY %in% drops.longsite2, ]
depth.EC.dry2<-depth.EC.dry[ ! depth.EC.dry$HY %in% drops.longsite2, ]

ann.depth.ex<- rbind(depth.TR.ann2, depth.EC.ann2,depth.JB.ann2, depth.WJ.ann2, 
                     depth.SB.ann2, depth.HC.ann2, depth.MB.ann2, depth.BS.ann2, depth.CS.ann2)
wet.depth.ex<- rbind(depth.TR.wet2, depth.EC.wet2,depth.JB.wet2, depth.WJ.wet2, 
                     depth.SB.wet2, depth.HC.wet2, depth.MB.wet2, depth.BS.wet2, depth.CS.wet2)
dry.depth.ex<- rbind(depth.TR.dry2, depth.EC.dry2,depth.JB.dry2, depth.WJ.dry2, 
                     depth.SB.dry2, depth.HC.dry2, depth.MB.dry2, depth.BS.dry2, depth.CS.dry2)


#LMB POR sal ####
#TR
sal.TR.ann <- ddply(all.sites[all.sites$site == "TR", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "TR", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.TR.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.TR.wet <- sal.season.ex[c(wet.ex.xyz),]
#EC
sal.EC.ann <- ddply(all.sites[all.sites$site == "EC", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "EC", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.EC.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.EC.wet <- sal.season.ex[c(wet.ex.xyz),]
#TC POR sal####
#WJ
sal.WJ.ann <- ddply(all.sites[all.sites$site == "WJ", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "WJ", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.WJ.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.WJ.wet <- sal.season.ex[c(wet.ex.xyz),]
#JB
sal.JB.ann <- ddply(all.sites[all.sites$site == "JB", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "JB", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.JB.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.JB.wet <- sal.season.ex[c(wet.ex.xyz),]
#LS POR sal####
#SB
sal.SB.ann <- ddply(all.sites[all.sites$site == "SB", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "SB", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.SB.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.SB.wet <- sal.season.ex[c(wet.ex.xyz),]
#HC
sal.HC.ann <- ddply(all.sites[all.sites$site == "HC", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "HC", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.HC.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.HC.wet <- sal.season.ex[c(wet.ex.xyz),]
#SBB POR sal####
#MB
sal.MB.ann <- ddply(all.sites[all.sites$site == "MB", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "MB", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.MB.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.MB.wet <- sal.season.ex[c(wet.ex.xyz),]
#BS
sal.BS.ann <- ddply(all.sites[all.sites$site == "BS", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "BS", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.BS.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.BS.wet <- sal.season.ex[c(wet.ex.xyz),]
#CS
sal.CS.ann <- ddply(all.sites[all.sites$site == "CS", ], .(HY, site), summarise, sal = mean(sal))
sal.season.ex<- ddply(all.sites[all.sites$site == "CS", ], .(HY, Season, site), summarise, sal = mean(sal))
dry.ex.xyz <- which(sal.season.ex$Season == "Dry Season")
sal.CS.dry <- sal.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(sal.season.ex$Season == "Wet Season")
sal.CS.wet <- sal.season.ex[c(wet.ex.xyz),]

#data file exceedance sal####

#remove unused years (this can be worked on another time)
#long sites
drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "NA")
drops.longsite3 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "1990-91", "NA")
sal.TR.ann2<-sal.TR.ann[ ! sal.TR.ann$HY %in% drops.longsite, ]
sal.TR.wet2<-sal.TR.wet[ ! sal.TR.wet$HY %in% drops.longsite, ]
sal.TR.dry2<-sal.TR.dry[ ! sal.TR.dry$HY %in% drops.longsite, ]
sal.JB.ann2<-sal.JB.ann[ ! sal.JB.ann$HY %in% drops.longsite, ]
sal.JB.wet2<-sal.JB.wet[ ! sal.JB.wet$HY %in% drops.longsite, ]
sal.JB.dry2<-sal.JB.dry[ ! sal.JB.dry$HY %in% drops.longsite, ]
sal.HC.ann2<-sal.HC.ann[ ! sal.HC.ann$HY %in% drops.longsite, ]
sal.HC.wet2<-sal.HC.wet[ ! sal.HC.wet$HY %in% drops.longsite, ]
sal.HC.dry2<-sal.HC.dry[ ! sal.HC.dry$HY %in% drops.longsite, ]
sal.BS.ann2<-sal.BS.ann[ ! sal.BS.ann$HY %in% drops.longsite3, ]
sal.BS.wet2<-sal.BS.wet[ ! sal.BS.wet$HY %in% drops.longsite3, ]
sal.BS.dry2<-sal.BS.dry[ ! sal.BS.dry$HY %in% drops.longsite3, ]
#short sites 
drops.longsite2 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "2001-02", "2002-03",
                     "2003-04", "NA")
sal.CS.ann2<-sal.CS.ann[ ! sal.CS.ann$HY %in% drops.longsite2, ]
sal.CS.wet2<-sal.CS.wet[ ! sal.CS.wet$HY %in% drops.longsite2, ]
sal.CS.dry2<-sal.CS.dry[ ! sal.CS.dry$HY %in% drops.longsite2, ]
sal.MB.ann2<-sal.MB.ann[ ! sal.MB.ann$HY %in% drops.longsite2, ]
sal.MB.wet2<-sal.MB.wet[ ! sal.MB.wet$HY %in% drops.longsite2, ]
sal.MB.dry2<-sal.MB.dry[ ! sal.MB.dry$HY %in% drops.longsite2, ]
sal.SB.ann2<-sal.SB.ann[ ! sal.SB.ann$HY %in% drops.longsite2, ]
sal.SB.wet2<-sal.SB.wet[ ! sal.SB.wet$HY %in% drops.longsite2, ]
sal.SB.dry2<-sal.SB.dry[ ! sal.SB.dry$HY %in% drops.longsite2, ]
sal.WJ.ann2<-sal.WJ.ann[ ! sal.WJ.ann$HY %in% drops.longsite2, ]
sal.WJ.wet2<-sal.WJ.wet[ ! sal.WJ.wet$HY %in% drops.longsite2, ]
sal.WJ.dry2<-sal.WJ.dry[ ! sal.WJ.dry$HY %in% drops.longsite2, ]
sal.EC.ann2<-sal.EC.ann[ ! sal.EC.ann$HY %in% drops.longsite2, ]
sal.EC.wet2<-sal.EC.wet[ ! sal.EC.wet$HY %in% drops.longsite2, ]
sal.EC.dry2<-sal.EC.dry[ ! sal.EC.dry$HY %in% drops.longsite2, ]

ann.sal.ex<- rbind(sal.TR.ann2, sal.EC.ann2,sal.JB.ann2, sal.WJ.ann2, 
                  sal.SB.ann2, sal.HC.ann2, sal.MB.ann2, sal.BS.ann2, sal.CS.ann2)
wet.sal.ex<- rbind(sal.TR.wet2, sal.EC.wet2,sal.JB.wet2, sal.WJ.wet2, 
                  sal.SB.wet2, sal.HC.wet2, sal.MB.wet2, sal.BS.wet2, sal.CS.wet2)
dry.sal.ex<- rbind(sal.TR.dry2, sal.EC.dry2,sal.JB.dry2, sal.WJ.dry2, 
                  sal.SB.dry2, sal.HC.dry2, sal.MB.dry2, sal.BS.dry2, sal.CS.dry2)



           
           
#PERCENT ON ex.Y-AXIS ####
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "TR", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "EC", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "WJ", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "JB", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "SB", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "HC", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "MB", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "BS", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.sal.ex[ann.sal.ex$site == "CS", ]$HY)-1))
#Remove first HY from each site####


#create an ind by sorting depth and sal; largest to smallest for wet, dry and annual variable ####
#LMB depth####
#TR
TR.depth.wet.ind<-order(-depth.TR.wet2$depth)
TR.depth.dry.ind<-order(-depth.TR.dry2$depth)
TR.depth.ann.ind<-order(-depth.TR.ann2$depth)
#EC
EC.depth.wet.ind<-order(-depth.EC.wet2$depth)
EC.depth.dry.ind<-order(-depth.EC.dry2$depth)
EC.depth.ann.ind<-order(-depth.EC.ann2$depth)
#TC depth####
#WJ
WJ.depth.wet.ind<-order(-depth.WJ.wet2$depth)
WJ.depth.dry.ind<-order(-depth.WJ.dry2$depth)
WJ.depth.ann.ind<-order(-depth.WJ.ann2$depth)
#JB
JB.depth.wet.ind<-order(-depth.JB.wet2$depth)
JB.depth.dry.ind<-order(-depth.JB.dry2$depth)
JB.depth.ann.ind<-order(-depth.JB.ann2$depth)
#LS depth####
#SB
SB.depth.wet.ind<-order(-depth.SB.wet2$depth)
SB.depth.dry.ind<-order(-depth.SB.dry2$depth)
SB.depth.ann.ind<-order(-depth.SB.ann2$depth)
#HC
HC.depth.wet.ind<-order(-depth.HC.wet2$depth)
HC.depth.dry.ind<-order(-depth.HC.dry2$depth)
HC.depth.ann.ind<-order(-depth.HC.ann2$depth)
#SBB depth####
#MB
MB.depth.wet.ind<-order(-depth.MB.wet2$depth)
MB.depth.dry.ind<-order(-depth.MB.dry2$depth)
MB.depth.ann.ind<-order(-depth.MB.ann2$depth)
#BS
BS.depth.wet.ind<-order(-depth.BS.wet2$depth)
BS.depth.dry.ind<-order(-depth.BS.dry2$depth)
BS.depth.ann.ind<-order(-depth.BS.ann2$depth)
#CS
CS.depth.wet.ind<-order(-depth.CS.wet2$depth)
CS.depth.dry.ind<-order(-depth.CS.dry2$depth)
CS.depth.ann.ind<-order(-depth.CS.ann2$depth)
#LMB sal####
#TR
TR.sal.wet.ind<-order(-sal.TR.wet2$sal)
TR.sal.dry.ind<-order(-sal.TR.dry2$sal)
TR.sal.ann.ind<-order(-sal.TR.ann2$sal)
#EC
EC.sal.wet.ind<-order(-sal.EC.wet2$sal)
EC.sal.dry.ind<-order(-sal.EC.dry2$sal)
EC.sal.ann.ind<-order(-sal.EC.ann2$sal)
#TC sal####
#WJ
WJ.sal.wet.ind<-order(-sal.WJ.wet2$sal)
WJ.sal.dry.ind<-order(-sal.WJ.dry2$sal)
WJ.sal.ann.ind<-order(-sal.WJ.ann2$sal)
#JB
JB.sal.wet.ind<-order(-sal.JB.wet2$sal)
JB.sal.dry.ind<-order(-sal.JB.dry2$sal)
JB.sal.ann.ind<-order(-sal.JB.ann2$sal)
#LS sal####
#SB
SB.sal.wet.ind<-order(-sal.SB.wet2$sal)
SB.sal.dry.ind<-order(-sal.SB.dry2$sal)
SB.sal.ann.ind<-order(-sal.SB.ann2$sal)
#HC
HC.sal.wet.ind<-order(-sal.HC.wet2$sal)
HC.sal.dry.ind<-order(-sal.HC.dry2$sal)
HC.sal.ann.ind<-order(-sal.HC.ann2$sal)
#SBB sal####
#MB
MB.sal.wet.ind<-order(-sal.MB.wet2$sal)
MB.sal.dry.ind<-order(-sal.MB.dry2$sal)
MB.sal.ann.ind<-order(-sal.MB.ann2$sal)
#BS
BS.sal.wet.ind<-order(-sal.BS.wet2$sal)
BS.sal.dry.ind<-order(-sal.BS.dry2$sal)
BS.sal.ann.ind<-order(-sal.BS.ann2$sal)
#CS
CS.sal.wet.ind<-order(-sal.CS.wet2$sal)
CS.sal.dry.ind<-order(-sal.CS.dry2$sal)
CS.sal.ann.ind<-order(-sal.CS.ann2$sal)

#create dataframes for ggplot####
#ex.dat depth LMB####
#TR
TR.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                            length(depth.TR.wet2$HY)),
                           HY=as.factor(depth.TR.wet2$HY[TR.depth.wet.ind]), 
                           Depth=depth.TR.wet2$depth[TR.depth.wet.ind] )
TR.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                            length(depth.TR.dry2$HY)),
                           HY=as.factor(depth.TR.dry2$HY[TR.depth.dry.ind]), 
                           Depth=depth.TR.dry2$depth[TR.depth.dry.ind] )
TR.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                            length(depth.TR.ann2$HY)),
                           HY=as.factor(depth.TR.ann2$HY[TR.depth.ann.ind]), 
                           Depth=depth.TR.ann2$depth[TR.depth.ann.ind] )
TR.depth.exceedance_df<- data.frame(bind_rows(TR.depth.ann.df, TR.depth.wet.df, TR.depth.dry.df) )
TR.depth.exceedance_df$Percent<- factor(TR.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TR))
TR.depth.exceedance_df$What<- as.factor(TR.depth.exceedance_df$What)
this_year_TR.depth_df <- as.data.frame(TR.depth.exceedance_df[TR.depth.exceedance_df$HY == this_report, ])
#EC
EC.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                            length(depth.EC.wet2$HY)),
                           HY=as.factor(depth.EC.wet2$HY[EC.depth.wet.ind]), 
                           Depth=depth.EC.wet2$depth[EC.depth.wet.ind] )
EC.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                            length(depth.EC.dry2$HY)),
                           HY=as.factor(depth.EC.dry2$HY[EC.depth.dry.ind]), 
                           Depth=depth.EC.dry2$depth[EC.depth.dry.ind] )
EC.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                            length(depth.EC.ann2$HY)),
                           HY=as.factor(depth.EC.ann2$HY[EC.depth.ann.ind]), 
                           Depth=depth.EC.ann2$depth[EC.depth.ann.ind] )
EC.depth.exceedance_df<- data.frame(bind_rows(EC.depth.ann.df, EC.depth.wet.df, EC.depth.dry.df) )
EC.depth.exceedance_df$Percent<- factor(EC.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.EC))
EC.depth.exceedance_df$What<- as.factor(EC.depth.exceedance_df$What)
this_year_EC.depth_df <- as.data.frame(EC.depth.exceedance_df[EC.depth.exceedance_df$HY == this_report, ])
#ex.dat depth TC####
#WJ
WJ.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                            length(depth.WJ.wet2$HY)),
                           HY=as.factor(depth.WJ.wet2$HY[WJ.depth.wet.ind]), 
                           Depth=depth.WJ.wet2$depth[WJ.depth.wet.ind] )
WJ.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                            length(depth.WJ.dry2$HY)),
                           HY=as.factor(depth.WJ.dry2$HY[WJ.depth.dry.ind]), 
                           Depth=depth.WJ.dry2$depth[WJ.depth.dry.ind] )
WJ.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                            length(depth.WJ.ann2$HY)),
                           HY=as.factor(depth.WJ.ann2$HY[WJ.depth.ann.ind]), 
                           Depth=depth.WJ.ann2$depth[WJ.depth.ann.ind] )
WJ.depth.exceedance_df<- data.frame(bind_rows(WJ.depth.ann.df, WJ.depth.wet.df, WJ.depth.dry.df) )
WJ.depth.exceedance_df$Percent<- factor(WJ.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.depth.exceedance_df$What<- as.factor(WJ.depth.exceedance_df$What)
this_year_WJ.depth_df <- as.data.frame(WJ.depth.exceedance_df[WJ.depth.exceedance_df$HY == this_report, ])
#JB
JB.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                            length(depth.JB.wet2$HY)),
                           HY=as.factor(depth.JB.wet2$HY[JB.depth.wet.ind]), 
                           Depth=depth.JB.wet2$depth[JB.depth.wet.ind] )
JB.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                            length(depth.JB.dry2$HY)),
                           HY=as.factor(depth.JB.dry2$HY[JB.depth.dry.ind]), 
                           Depth=depth.JB.dry2$depth[JB.depth.dry.ind] )
JB.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                            length(depth.JB.ann2$HY)),
                           HY=as.factor(depth.JB.ann2$HY[JB.depth.ann.ind]), 
                           Depth=depth.JB.ann2$depth[JB.depth.ann.ind] )
JB.depth.exceedance_df<- data.frame(bind_rows(JB.depth.ann.df, JB.depth.wet.df, JB.depth.dry.df) )
JB.depth.exceedance_df$Percent<- factor(JB.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.JB))
JB.depth.exceedance_df$What<- as.factor(JB.depth.exceedance_df$What)
this_year_JB.depth_df <- as.data.frame(JB.depth.exceedance_df[JB.depth.exceedance_df$HY == this_report, ])
#ex.dat depth LS####
#SB
SB.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                            length(depth.SB.wet2$HY)),
                           HY=as.factor(depth.SB.wet2$HY[SB.depth.wet.ind]), 
                           Depth=depth.SB.wet2$depth[SB.depth.wet.ind] )
SB.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                            length(depth.SB.dry2$HY)),
                           HY=as.factor(depth.SB.dry2$HY[SB.depth.dry.ind]), 
                           Depth=depth.SB.dry2$depth[SB.depth.dry.ind] )
SB.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                            length(depth.SB.ann2$HY)),
                           HY=as.factor(depth.SB.ann2$HY[SB.depth.ann.ind]), 
                           Depth=depth.SB.ann2$depth[SB.depth.ann.ind] )
SB.depth.exceedance_df<- data.frame(bind_rows(SB.depth.ann.df, SB.depth.wet.df, SB.depth.dry.df) )
SB.depth.exceedance_df$Percent<- factor(SB.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.SB))
SB.depth.exceedance_df$What<- as.factor(SB.depth.exceedance_df$What)
this_year_SB.depth_df <- as.data.frame(SB.depth.exceedance_df[SB.depth.exceedance_df$HY == this_report, ])
#HC
HC.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                            length(depth.HC.wet2$HY)),
                           HY=as.factor(depth.HC.wet2$HY[HC.depth.wet.ind]), 
                           Depth=depth.HC.wet2$depth[HC.depth.wet.ind] )
HC.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                            length(depth.HC.dry2$HY)),
                           HY=as.factor(depth.HC.dry2$HY[HC.depth.dry.ind]), 
                           Depth=depth.HC.dry2$depth[HC.depth.dry.ind] )
HC.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                            length(depth.HC.ann2$HY)),
                           HY=as.factor(depth.HC.ann2$HY[HC.depth.ann.ind]), 
                           Depth=depth.HC.ann2$depth[HC.depth.ann.ind] )
HC.depth.exceedance_df<- data.frame(bind_rows(HC.depth.ann.df, HC.depth.wet.df, HC.depth.dry.df) )
HC.depth.exceedance_df$Percent<- factor(HC.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.HC))
HC.depth.exceedance_df$What<- as.factor(HC.depth.exceedance_df$What)
this_year_HC.depth_df <- as.data.frame(HC.depth.exceedance_df[HC.depth.exceedance_df$HY == this_report, ])
#ex.dat depth SBB####
#MB
MB.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                            length(depth.MB.wet2$HY)),
                           HY=as.factor(depth.MB.wet2$HY[MB.depth.wet.ind]), 
                           Depth=depth.MB.wet2$depth[MB.depth.wet.ind] )
MB.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                            length(depth.MB.dry2$HY)),
                           HY=as.factor(depth.MB.dry2$HY[MB.depth.dry.ind]), 
                           Depth=depth.MB.dry2$depth[MB.depth.dry.ind] )
MB.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                            length(depth.MB.ann2$HY)),
                           HY=as.factor(depth.MB.ann2$HY[MB.depth.ann.ind]), 
                           Depth=depth.MB.ann2$depth[MB.depth.ann.ind] )
MB.depth.exceedance_df<- data.frame(bind_rows(MB.depth.ann.df, MB.depth.wet.df, MB.depth.dry.df) )
MB.depth.exceedance_df$Percent<- factor(MB.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.MB))
MB.depth.exceedance_df$What<- as.factor(MB.depth.exceedance_df$What)
this_year_MB.depth_df <- as.data.frame(MB.depth.exceedance_df[MB.depth.exceedance_df$HY == this_report, ])
#BS
BS.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                            length(depth.BS.wet2$HY)),
                           HY=as.factor(depth.BS.wet2$HY[BS.depth.wet.ind]), 
                           Depth=depth.BS.wet2$depth[BS.depth.wet.ind] )
BS.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                            length(depth.BS.dry2$HY)),
                           HY=as.factor(depth.BS.dry2$HY[BS.depth.dry.ind]), 
                           Depth=depth.BS.dry2$depth[BS.depth.dry.ind] )
BS.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                            length(depth.BS.ann2$HY)),
                           HY=as.factor(depth.BS.ann2$HY[BS.depth.ann.ind]), 
                           Depth=depth.BS.ann2$depth[BS.depth.ann.ind] )
BS.depth.exceedance_df<- data.frame(bind_rows(BS.depth.ann.df, BS.depth.wet.df, BS.depth.dry.df) )
BS.depth.exceedance_df$Percent<- factor(BS.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.BS))
BS.depth.exceedance_df$What<- as.factor(BS.depth.exceedance_df$What)
this_year_BS.depth_df <- as.data.frame(BS.depth.exceedance_df[BS.depth.exceedance_df$HY == this_report, ])
#CS
CS.depth.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                            length(depth.CS.wet2$HY)),
                           HY=as.factor(depth.CS.wet2$HY[CS.depth.wet.ind]), 
                           Depth=depth.CS.wet2$depth[CS.depth.wet.ind] )
CS.depth.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                            length(depth.CS.dry2$HY)),
                           HY=as.factor(depth.CS.dry2$HY[CS.depth.dry.ind]), 
                           Depth=depth.CS.dry2$depth[CS.depth.dry.ind] )
CS.depth.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                            length(depth.CS.ann2$HY)),
                           HY=as.factor(depth.CS.ann2$HY[CS.depth.ann.ind]), 
                           Depth=depth.CS.ann2$depth[CS.depth.ann.ind] )
CS.depth.exceedance_df<- data.frame(bind_rows(CS.depth.ann.df, CS.depth.wet.df, CS.depth.dry.df) )
CS.depth.exceedance_df$Percent<- factor(CS.depth.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.CS))
CS.depth.exceedance_df$What<- as.factor(CS.depth.exceedance_df$What)
this_year_CS.depth_df <- as.data.frame(CS.depth.exceedance_df[CS.depth.exceedance_df$HY == this_report, ])
#ex.dat sal LMB####
#TR
TR.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Wet Season", 
                                                                          length(sal.TR.wet2$HY)),
                         HY=as.factor(sal.TR.wet2$HY[TR.sal.wet.ind]), 
                         Salinity=sal.TR.wet2$sal[TR.sal.wet.ind] )
TR.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Dry Season", 
                                                                          length(sal.TR.dry2$HY)),
                         HY=as.factor(sal.TR.dry2$HY[TR.sal.dry.ind]), 
                         Salinity=sal.TR.dry2$sal[TR.sal.dry.ind] )
TR.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TR),What=rep("Annual", 
                                                                          length(sal.TR.ann2$HY)),
                         HY=as.factor(sal.TR.ann2$HY[TR.sal.ann.ind]), 
                         Salinity=sal.TR.ann2$sal[TR.sal.ann.ind] )
TR.sal.exceedance_df<- data.frame(bind_rows(TR.sal.ann.df, TR.sal.wet.df, TR.sal.dry.df) )
TR.sal.exceedance_df$Percent<- factor(TR.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.TR))
TR.sal.exceedance_df$What<- as.factor(TR.sal.exceedance_df$What)
this_year_TR.sal_df <- as.data.frame(TR.sal.exceedance_df[TR.sal.exceedance_df$HY == this_report, ])
#EC
EC.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Wet Season", 
                                                                          length(sal.EC.wet2$HY)),
                         HY=as.factor(sal.EC.wet2$HY[EC.sal.wet.ind]), 
                         Salinity=sal.EC.wet2$sal[EC.sal.wet.ind] )
EC.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Dry Season", 
                                                                          length(sal.EC.dry2$HY)),
                         HY=as.factor(sal.EC.dry2$HY[EC.sal.dry.ind]), 
                         Salinity=sal.EC.dry2$sal[EC.sal.dry.ind] )
EC.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.EC),What=rep("Annual", 
                                                                          length(sal.EC.ann2$HY)),
                         HY=as.factor(sal.EC.ann2$HY[EC.sal.ann.ind]), 
                         Salinity=sal.EC.ann2$sal[EC.sal.ann.ind] )
EC.sal.exceedance_df<- data.frame(bind_rows(EC.sal.ann.df, EC.sal.wet.df, EC.sal.dry.df) )
EC.sal.exceedance_df$Percent<- factor(EC.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.EC))
EC.sal.exceedance_df$What<- as.factor(EC.sal.exceedance_df$What)
this_year_EC.sal_df <- as.data.frame(EC.sal.exceedance_df[EC.sal.exceedance_df$HY == this_report, ])
#ex.dat sal TC####
#WJ
WJ.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Wet Season", 
                                                                          length(sal.WJ.wet2$HY)),
                         HY=as.factor(sal.WJ.wet2$HY[WJ.sal.wet.ind]), 
                         Salinity=sal.WJ.wet2$sal[WJ.sal.wet.ind] )
WJ.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Dry Season", 
                                                                          length(sal.WJ.dry2$HY)),
                         HY=as.factor(sal.WJ.dry2$HY[WJ.sal.dry.ind]), 
                         Salinity=sal.WJ.dry2$sal[WJ.sal.dry.ind] )
WJ.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.WJ),What=rep("Annual", 
                                                                          length(sal.WJ.ann2$HY)),
                         HY=as.factor(sal.WJ.ann2$HY[WJ.sal.ann.ind]), 
                         Salinity=sal.WJ.ann2$sal[WJ.sal.ann.ind] )
WJ.sal.exceedance_df<- data.frame(bind_rows(WJ.sal.ann.df, WJ.sal.wet.df, WJ.sal.dry.df) )
WJ.sal.exceedance_df$Percent<- factor(WJ.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.WJ))
WJ.sal.exceedance_df$What<- as.factor(WJ.sal.exceedance_df$What)
this_year_WJ.sal_df <- as.data.frame(WJ.sal.exceedance_df[WJ.sal.exceedance_df$HY == this_report, ])
#JB
JB.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Wet Season", 
                                                                          length(sal.JB.wet2$HY)),
                         HY=as.factor(sal.JB.wet2$HY[JB.sal.wet.ind]), 
                         Salinity=sal.JB.wet2$sal[JB.sal.wet.ind] )
JB.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Dry Season", 
                                                                          length(sal.JB.dry2$HY)),
                         HY=as.factor(sal.JB.dry2$HY[JB.sal.dry.ind]), 
                         Salinity=sal.JB.dry2$sal[JB.sal.dry.ind] )
JB.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.JB),What=rep("Annual", 
                                                                          length(sal.JB.ann2$HY)),
                         HY=as.factor(sal.JB.ann2$HY[JB.sal.ann.ind]), 
                         Salinity=sal.JB.ann2$sal[JB.sal.ann.ind] )
JB.sal.exceedance_df<- data.frame(bind_rows(JB.sal.ann.df, JB.sal.wet.df, JB.sal.dry.df) )
JB.sal.exceedance_df$Percent<- factor(JB.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.JB))
JB.sal.exceedance_df$What<- as.factor(JB.sal.exceedance_df$What)
this_year_JB.sal_df <- as.data.frame(JB.sal.exceedance_df[JB.sal.exceedance_df$HY == this_report, ])
#ex.dat sal LS####
#SB
SB.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Wet Season", 
                                                                          length(sal.SB.wet2$HY)),
                         HY=as.factor(sal.SB.wet2$HY[SB.sal.wet.ind]), 
                         Salinity=sal.SB.wet2$sal[SB.sal.wet.ind] )
SB.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Dry Season", 
                                                                          length(sal.SB.dry2$HY)),
                         HY=as.factor(sal.SB.dry2$HY[SB.sal.dry.ind]), 
                         Salinity=sal.SB.dry2$sal[SB.sal.dry.ind] )
SB.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.SB),What=rep("Annual", 
                                                                          length(sal.SB.ann2$HY)),
                         HY=as.factor(sal.SB.ann2$HY[SB.sal.ann.ind]), 
                         Salinity=sal.SB.ann2$sal[SB.sal.ann.ind] )
SB.sal.exceedance_df<- data.frame(bind_rows(SB.sal.ann.df, SB.sal.wet.df, SB.sal.dry.df) )
SB.sal.exceedance_df$Percent<- factor(SB.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.SB))
SB.sal.exceedance_df$What<- as.factor(SB.sal.exceedance_df$What)
this_year_SB.sal_df <- as.data.frame(SB.sal.exceedance_df[SB.sal.exceedance_df$HY == this_report, ])
#HC
HC.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Wet Season", 
                                                                          length(sal.HC.wet2$HY)),
                         HY=as.factor(sal.HC.wet2$HY[HC.sal.wet.ind]), 
                         Salinity=sal.HC.wet2$sal[HC.sal.wet.ind] )
HC.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Dry Season", 
                                                                          length(sal.HC.dry2$HY)),
                         HY=as.factor(sal.HC.dry2$HY[HC.sal.dry.ind]), 
                         Salinity=sal.HC.dry2$sal[HC.sal.dry.ind] )
HC.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.HC),What=rep("Annual", 
                                                                          length(sal.HC.ann2$HY)),
                         HY=as.factor(sal.HC.ann2$HY[HC.sal.ann.ind]), 
                         Salinity=sal.HC.ann2$sal[HC.sal.ann.ind] )
HC.sal.exceedance_df<- data.frame(bind_rows(HC.sal.ann.df, HC.sal.wet.df, HC.sal.dry.df) )
HC.sal.exceedance_df$Percent<- factor(HC.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.HC))
HC.sal.exceedance_df$What<- as.factor(HC.sal.exceedance_df$What)
this_year_HC.sal_df <- as.data.frame(HC.sal.exceedance_df[HC.sal.exceedance_df$HY == this_report, ])
#ex.dat sal SBB####
#MB
MB.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Wet Season", 
                                                                          length(sal.MB.wet2$HY)),
                         HY=as.factor(sal.MB.wet2$HY[MB.sal.wet.ind]), 
                         Salinity=sal.MB.wet2$sal[MB.sal.wet.ind] )
MB.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Dry Season", 
                                                                          length(sal.MB.dry2$HY)),
                         HY=as.factor(sal.MB.dry2$HY[MB.sal.dry.ind]), 
                         Salinity=sal.MB.dry2$sal[MB.sal.dry.ind] )
MB.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.MB),What=rep("Annual", 
                                                                          length(sal.MB.ann2$HY)),
                         HY=as.factor(sal.MB.ann2$HY[MB.sal.ann.ind]), 
                         Salinity=sal.MB.ann2$sal[MB.sal.ann.ind] )
MB.sal.exceedance_df<- data.frame(bind_rows(MB.sal.ann.df, MB.sal.wet.df, MB.sal.dry.df) )
MB.sal.exceedance_df$Percent<- factor(MB.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.MB))
MB.sal.exceedance_df$What<- as.factor(MB.sal.exceedance_df$What)
this_year_MB.sal_df <- as.data.frame(MB.sal.exceedance_df[MB.sal.exceedance_df$HY == this_report, ])
#BS
BS.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Wet Season", 
                                                                          length(sal.BS.wet2$HY)),
                         HY=as.factor(sal.BS.wet2$HY[BS.sal.wet.ind]), 
                         Salinity=sal.BS.wet2$sal[BS.sal.wet.ind] )
BS.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Dry Season", 
                                                                          length(sal.BS.dry2$HY)),
                         HY=as.factor(sal.BS.dry2$HY[BS.sal.dry.ind]), 
                         Salinity=sal.BS.dry2$sal[BS.sal.dry.ind] )
BS.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.BS),What=rep("Annual", 
                                                                          length(sal.BS.ann2$HY)),
                         HY=as.factor(sal.BS.ann2$HY[BS.sal.ann.ind]), 
                         Salinity=sal.BS.ann2$sal[BS.sal.ann.ind] )
BS.sal.exceedance_df<- data.frame(bind_rows(BS.sal.ann.df, BS.sal.wet.df, BS.sal.dry.df) )
BS.sal.exceedance_df$Percent<- factor(BS.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.BS))
BS.sal.exceedance_df$What<- as.factor(BS.sal.exceedance_df$What)
this_year_BS.sal_df <- as.data.frame(BS.sal.exceedance_df[BS.sal.exceedance_df$HY == this_report, ])
#CS
CS.sal.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Wet Season", 
                                                                          length(sal.CS.wet2$HY)),
                         HY=as.factor(sal.CS.wet2$HY[CS.sal.wet.ind]), 
                         Salinity=sal.CS.wet2$sal[CS.sal.wet.ind] )
CS.sal.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Dry Season", 
                                                                          length(sal.CS.dry2$HY)),
                         HY=as.factor(sal.CS.dry2$HY[CS.sal.dry.ind]), 
                         Salinity=sal.CS.dry2$sal[CS.sal.dry.ind] )
CS.sal.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.CS),What=rep("Annual", 
                                                                          length(sal.CS.ann2$HY)),
                         HY=as.factor(sal.CS.ann2$HY[CS.sal.ann.ind]), 
                         Salinity=sal.CS.ann2$sal[CS.sal.ann.ind] )
CS.sal.exceedance_df<- data.frame(bind_rows(CS.sal.ann.df, CS.sal.wet.df, CS.sal.dry.df) )
CS.sal.exceedance_df$Percent<- factor(CS.sal.exceedance_df$Percent, 
                                      levels=label_percent(accuracy=1)(y.ex.CS))
CS.sal.exceedance_df$What<- as.factor(CS.sal.exceedance_df$What)
this_year_CS.sal_df <- as.data.frame(CS.sal.exceedance_df[CS.sal.exceedance_df$HY == this_report, ])

#EXCEEDANCE PLOTS####
#LMB depth####
#TR
ex.TR.depth <- ggplot(TR.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#EC
ex.EC.depth <- ggplot(EC.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")

#TC depth####
#JB
ex.JB.depth <- ggplot(JB.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")

#WJ
ex.WJ.depth <- ggplot(WJ.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#LS depth####
#SB
ex.SB.depth <- ggplot(SB.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#HC
ex.HC.depth <- ggplot(HC.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#SBB depth####
#MB
ex.MB.depth <- ggplot(MB.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#BS
ex.BS.depth <- ggplot(BS.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")
#CS
ex.CS.depth <- ggplot(CS.depth.exceedance_df, aes(x=Percent, y= Depth)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.depth_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.depth_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Water Level (cm)")


#LMB sal####
#TR
ex.TR.sal <- ggplot(TR.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TR.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TR.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#EC
ex.EC.sal <- ggplot(EC.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_EC.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_EC.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#TC sal####
#JB
ex.JB.sal <- ggplot(JB.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_JB.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_JB.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#WJ
ex.WJ.sal <- ggplot(WJ.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_WJ.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_WJ.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#LS sal ####
#SB
ex.SB.sal <- ggplot(SB.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_SB.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_SB.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#HC
ex.HC.sal <- ggplot(HC.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_HC.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_HC.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#SBB sal ####
#MB
ex.MB.sal <- ggplot(MB.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_MB.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_MB.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#BS
ex.BS.sal <- ggplot(BS.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_BS.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_BS.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")
#CS
ex.CS.sal <- ggplot(CS.sal.exceedance_df, aes(x=Percent, y= Salinity)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_CS.sal_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_CS.sal_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Salinity (psu)")




#exceedance dump ####
ex.TR.depth
ex.EC.depth
ex.WJ.depth
ex.JB.depth
ex.SB.depth
ex.HC.depth
ex.MB.depth
ex.BS.depth
ex.CS.depth

ex.TR.sal
ex.EC.sal
ex.WJ.sal
ex.JB.sal
ex.SB.sal
ex.HC.sal
ex.MB.sal
ex.BS.sal
ex.CS.sal



#####
#####
#box plot season depth and sal ####


hy.now.xyz <- which(hy.now$Season == "Dry Season")
hy.now.dry <- hy.now[c(hy.now.xyz),]
hy.now.abc <- which(hy.now$Season == "Wet Season")
hy.now.wet <- hy.now[c(hy.now.abc),]

hy.now$Season<-factor(hy.now$Season,levels = c("Wet Season", "Dry Season"))
hy.now$site<-factor(hy.now$site,levels = c("TR","EC","WJ","JB","SB","HC","MB","BS","CS",'TP'))
hy.now$site<-factor(hy.now$site,levels = c("TR","EC","WJ","JB","SB","HC","MB","BS","CS",'TP'))
hy.now$Month<- month.abb[hy.now$MONTH]
hy.now$Month<- factor(hy.now$Month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan",
                                              "Feb","Mar","Apr","May"))
hy.now$area<- factor(hy.now$area,levels = c("LMB","TC","LS","SBB",'N_SBB'))

#site box plot month depth and sal ####


box.season.depth.all<- ggplot(hy.now, aes(x = site, y = depth, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)")

box.season.sal.all<- ggplot(hy.now #%>% 
                              #filter(site != 'TP')
                            , aes(x = site, y = sal, fill= Season))+
  theme_bw()+
  geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)")

ggarrange(
ggplot(hy.now %>% 
       filter(area == 'LS')
       , aes(x = site, y = sal, fill= Season))+
  theme_bw()+
  geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)"),

ggplot(hy.now%>% 
         filter(area == 'LS'), aes(x = site, y = depth, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)"),
common.legend = TRUE)

monthly.mean.depth.all<- ggplot(hy.now, aes(x = factor(Month), y = depth, fill= site))+
  #geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4",'orange4'))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4",'orange4'))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)")


monthly.mean.sal.all<- ggplot(hy.now, aes(x = factor(Month), y = sal, fill= site))+
  geom_boxplot(aes(fill=site, color =site)) +
  scale_fill_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                             "yellow1","yellow3","yellow4",'orange4'))+
  scale_color_manual(values=c("red1","red4","blue1","blue4","green1","green4",
                              "yellow1","yellow3","yellow4",'orange4'))+
  geom_boxplot(aes(fill=site ), outlier.colour = NA) +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)")


monthly.mean.sal.region<- ggplot(hy.now %>% 
                                   filter(site != 'TP'), aes(x = factor(Month), y = sal, fill= area ))+
  geom_boxplot(aes(fill=area, color =area )) +
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area  ), outlier.colour = NA) +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)")


monthly.mean.depth.region<- ggplot(hy.now %>% 
                                   filter(site != 'TP'), aes(x = factor(Month), y = depth, fill= area ))+
  geom_boxplot(aes(fill=area, color =area )) +
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area  ), outlier.colour = NA) +
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)")

#BOX PLOT grapth dump####
box.season.depth.all
box.season.sal.all
monthly.mean.depth.all
monthly.mean.sal.all

monthly.mean.depth.region
monthly.mean.sal.region

#PCT ####

all.sites$nPCT<- ifelse(all.sites$depth<13,1,0)
PCT.all.sites<-ddply(all.sites, .(HY, site), summarise, PCT = sum(nPCT==1))


#sort after PCT and site 
#remove unused years  THIS NEEDS TO BE INCLUDED ####
#TR, JB, HC; drops.longsite <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "")
# EC, WJ, SB, MB, CS; drops.longsite2 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", 
#                                           "", "2001-02", "2002-03", "2003-04")
#BS; drops.longsite3 <- c("1985-86","1986-87","1987-88","1988-89", "1989-90", "", "1990-91")

TR.t.PCT<-PCT.all.sites[PCT.all.sites$site == "TR",]
TR.t.PCT<-TR.t.PCT[ ! TR.t.PCT$HY %in% drops.longsite3, ]
TR.ind.PCT<-order(-TR.t.PCT$PCT)
EC.t.PCT<-PCT.all.sites[PCT.all.sites$site == "EC",]
EC.t.PCT<-EC.t.PCT[ ! EC.t.PCT$HY %in% drops.longsite2, ]
EC.ind.PCT<-order(-EC.t.PCT$PCT)
WJ.t.PCT<-PCT.all.sites[PCT.all.sites$site == "WJ",]
WJ.t.PCT<-WJ.t.PCT[ ! WJ.t.PCT$HY %in% drops.longsite2, ]
WJ.ind.PCT<-order(-WJ.t.PCT$PCT)
JB.t.PCT<-PCT.all.sites[PCT.all.sites$site == "JB",]
JB.t.PCT<-JB.t.PCT[ ! JB.t.PCT$HY %in% drops.longsite3, ]
JB.ind.PCT<-order(-JB.t.PCT$PCT)
SB.t.PCT<-PCT.all.sites[PCT.all.sites$site == "SB",]
SB.t.PCT<-SB.t.PCT[ ! SB.t.PCT$HY %in% drops.longsite2, ]
SB.ind.PCT<-order(-SB.t.PCT$PCT)
HC.t.PCT<-PCT.all.sites[PCT.all.sites$site == "HC",]
HC.t.PCT<-HC.t.PCT[ ! HC.t.PCT$HY %in% drops.longsite3, ]
HC.ind.PCT<-order(-HC.t.PCT$PCT)
MB.t.PCT<-PCT.all.sites[PCT.all.sites$site == "MB",]
MB.t.PCT<-MB.t.PCT[ ! MB.t.PCT$HY %in% drops.longsite2, ]
MB.ind.PCT<-order(-MB.t.PCT$PCT)
BS.t.PCT<-PCT.all.sites[PCT.all.sites$site == "BS",]
BS.t.PCT<-BS.t.PCT[ ! BS.t.PCT$HY %in% drops.longsite3, ]
BS.ind.PCT<-order(-BS.t.PCT$PCT)
CS.t.PCT<-PCT.all.sites[PCT.all.sites$site == "CS",]
CS.t.PCT<-CS.t.PCT[ ! CS.t.PCT$HY %in% drops.longsite2, ]
CS.ind.PCT<-order(-CS.t.PCT$PCT)

# % for x-axis
TR.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(TR.t.PCT)-1))
EC.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(EC.t.PCT)-1))
WJ.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(WJ.t.PCT)-1))
JB.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(JB.t.PCT)-1))
SB.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(SB.t.PCT)-1))
HC.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(HC.t.PCT)-1))
MB.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(MB.t.PCT)-1))
BS.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(BS.t.PCT)-1))
CS.PCT.y.ex<-seq(from = 0, to = 1, by = 1/(nrow(CS.t.PCT)-1))

#create df for ggplot
TR.PCT<-bind_cols(Percent=label_percent(accuracy=1)(TR.PCT.y.ex),What=rep("TR", length(TR.t.PCT$HY)),
                  HY=as.factor(TR.t.PCT$HY[TR.ind.PCT]), 
                  PCT=TR.t.PCT$PCT[TR.ind.PCT] )
EC.PCT<-bind_cols(Percent=label_percent(accuracy=1)(EC.PCT.y.ex),What=rep("EC", length(EC.t.PCT$HY)),
                  HY=as.factor(EC.t.PCT$HY[EC.ind.PCT]), 
                  PCT=EC.t.PCT$PCT[EC.ind.PCT] )
WJ.PCT<-bind_cols(Percent=label_percent(accuracy=1)(WJ.PCT.y.ex),What=rep("WJ", length(WJ.t.PCT$HY)),
                  HY=as.factor(WJ.t.PCT$HY[WJ.ind.PCT]), 
                  PCT=WJ.t.PCT$PCT[WJ.ind.PCT] )
JB.PCT<-bind_cols(Percent=label_percent(accuracy=1)(JB.PCT.y.ex),What=rep("JB", length(JB.t.PCT$HY)),
                  HY=as.factor(JB.t.PCT$HY[JB.ind.PCT]), 
                  PCT=JB.t.PCT$PCT[JB.ind.PCT] )
SB.PCT<-bind_cols(Percent=label_percent(accuracy=1)(SB.PCT.y.ex),What=rep("SB", length(SB.t.PCT$HY)),
                  HY=as.factor(SB.t.PCT$HY[SB.ind.PCT]), 
                  PCT=SB.t.PCT$PCT[SB.ind.PCT] )
HC.PCT<-bind_cols(Percent=label_percent(accuracy=1)(HC.PCT.y.ex),What=rep("HC", length(HC.t.PCT$HY)),
                  HY=as.factor(HC.t.PCT$HY[HC.ind.PCT]), 
                  PCT=HC.t.PCT$PCT[HC.ind.PCT] )
MB.PCT<-bind_cols(Percent=label_percent(accuracy=1)(MB.PCT.y.ex),What=rep("MB", length(MB.t.PCT$HY)),
                  HY=as.factor(MB.t.PCT$HY[MB.ind.PCT]), 
                  PCT=MB.t.PCT$PCT[MB.ind.PCT] )
BS.PCT<-bind_cols(Percent=label_percent(accuracy=1)(BS.PCT.y.ex),What=rep("BS", length(BS.t.PCT$HY)),
                  HY=as.factor(BS.t.PCT$HY[BS.ind.PCT]), 
                  PCT=BS.t.PCT$PCT[BS.ind.PCT] )
CS.PCT<-bind_cols(Percent=label_percent(accuracy=1)(CS.PCT.y.ex),What=rep("CS", length(CS.t.PCT$HY)),
                  HY=as.factor(CS.t.PCT$HY[CS.ind.PCT]), 
                  PCT=CS.t.PCT$PCT[CS.ind.PCT] )





#bind all the individual df together, and change to factors
TR.PCT$Percent<- factor(TR.PCT$Percent, levels=label_percent(accuracy=1)(TR.PCT.y.ex))
TR.PCT$What<- as.factor(TR.PCT$What)
this_year_TR.PCT<- TR.PCT[TR.PCT$HY == this_report, ]
EC.PCT$Percent<- factor(EC.PCT$Percent, levels=label_percent(accuracy=1)(EC.PCT.y.ex))
EC.PCT$What<- as.factor(EC.PCT$What)
this_year_EC.PCT<- EC.PCT[EC.PCT$HY == this_report, ]
WJ.PCT$Percent<- factor(WJ.PCT$Percent, levels=label_percent(accuracy=1)(WJ.PCT.y.ex))
WJ.PCT$What<- as.factor(WJ.PCT$What)
this_year_WJ.PCT<- WJ.PCT[WJ.PCT$HY == this_report, ]
JB.PCT$Percent<- factor(JB.PCT$Percent, levels=label_percent(accuracy=1)(JB.PCT.y.ex))
JB.PCT$What<- as.factor(JB.PCT$What)
this_year_JB.PCT<- JB.PCT[JB.PCT$HY == this_report, ]
SB.PCT$Percent<- factor(SB.PCT$Percent, levels=label_percent(accuracy=1)(SB.PCT.y.ex))
SB.PCT$What<- as.factor(SB.PCT$What)
this_year_SB.PCT<- SB.PCT[SB.PCT$HY == this_report, ]
HC.PCT$Percent<- factor(HC.PCT$Percent, levels=label_percent(accuracy=1)(HC.PCT.y.ex))
HC.PCT$What<- as.factor(HC.PCT$What)
this_year_HC.PCT<- HC.PCT[HC.PCT$HY == this_report, ]
MB.PCT$Percent<- factor(MB.PCT$Percent, levels=label_percent(accuracy=1)(MB.PCT.y.ex))
MB.PCT$What<- as.factor(MB.PCT$What)
this_year_MB.PCT<- MB.PCT[MB.PCT$HY == this_report, ]
BS.PCT$Percent<- factor(BS.PCT$Percent, levels=label_percent(accuracy=1)(BS.PCT.y.ex))
BS.PCT$What<- as.factor(BS.PCT$What)
this_year_BS.PCT<- BS.PCT[BS.PCT$HY == this_report, ]
CS.PCT$Percent<- factor(CS.PCT$Percent, levels=label_percent(accuracy=1)(CS.PCT.y.ex))
CS.PCT$What<- as.factor(CS.PCT$What)
this_year_CS.PCT<- CS.PCT[CS.PCT$HY == this_report, ]


PCT_df.long.sites<- data.frame(bind_rows(TR.PCT,JB.PCT,HC.PCT,BS.PCT) )
this_year_PCT.long.sites<- data.frame(bind_rows(this_year_TR.PCT, this_year_JB.PCT, 
                                                this_year_HC.PCT, this_year_BS.PCT) )

PCT_df.short.sites<- data.frame(bind_rows(EC.PCT,WJ.PCT,SB.PCT,MB.PCT,CS.PCT) )
this_year_PCT.short.sites<- data.frame(bind_rows(this_year_EC.PCT, this_year_WJ.PCT, this_year_SB.PCT, 
                                                this_year_MB.PCT, this_year_CS.PCT) )


write.csv(ddply(all.sites , .(HY, Season, site), summarise, PCT = sum(nPCT==1)), "P:/Databases/tidy/Reports/ACOE/Fish all/PCT_for_fish.csv")

#PCT graphs#####
PCT_scatter_plot<- ggplot(PCT.all.sites, aes(x = HY , y = PCT, shape=site, color=site))+
  theme_bw()+
  geom_point()+
  xlab("") + ylab("Days Below PCT (13cm)")+
  geom_smooth(aes( x =as.numeric(factor(HY)), y=PCT), method=lm, se=FALSE, fullrange=TRUE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank())

PCT_scatter_plot.long.sites<- ggplot(PCT_df.long.sites, aes(x = HY , y = PCT, shape=What, color=What))+
  theme_bw()+
  geom_point()+
  xlab("") + ylab("Days Below PCT (13cm)")+
  geom_smooth(aes( x =as.numeric(factor(HY)), y=PCT), method=lm, se=FALSE, fullrange=TRUE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank()#,
        #axis.text=element_text(size=20),
        #axis.title=element_text(size=17,face="bold"),
        #legend.text = element_text(size=20)
        )


PCT_scatter_plot.short.sites<- ggplot(PCT_df.short.sites, aes(x = HY , y = PCT, shape=What, color=What))+
  theme_bw()+
  geom_point()+
  xlab("") + ylab("Days Below PCT (13cm)")+
  geom_smooth(aes( x =as.numeric(factor(HY)), y=PCT), method=lm, se=FALSE, fullrange=TRUE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank())


PCT.Exceedance.long.sites <- ggplot(PCT_df.long.sites, aes(x=Percent, y= PCT)) + 
    geom_line(aes(group=What, colour= What))+
    theme_bw() +                
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
          legend.title = element_blank())+ 
    geom_point(shape = 21,aes(color=What))+
    geom_point(data=this_year_PCT.long.sites, aes(group=What, colour= What))+
    geom_label_repel(data=this_year_PCT.long.sites, aes(label=HY, group=What, colour= What),  
                     show.legend = FALSE,
                     box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+  
    xlab("Percent Time Equaled or Exceeded") + ylab("Days Below PCT (13cm)") 
  
PCT.Exceedance.short.sites <- ggplot(PCT_df.short.sites, aes(x=Percent, y= PCT)) + 
    geom_line(aes(group=What, colour= What))+
    theme_bw() +                
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
          legend.title = element_blank())+ 
    geom_point(shape = 21,aes(color=What))+
    geom_point(data=this_year_PCT.short.sites, aes(group=What, colour= What))+
    geom_label_repel(data=this_year_PCT.short.sites, aes(label=HY, group=What, colour= What),  
                     show.legend = FALSE,
                     box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+  
    xlab("Percent Time Equaled or Exceeded") + ylab("Days Below PCT (13cm)") 

    
#PCT graph dump####    
PCT_scatter_plot
PCT_scatter_plot.long.sites
PCT_scatter_plot.short.sites    
PCT.Exceedance.long.sites
PCT.Exceedance.short.sites


#PCT correlation ####
TR.t.PCT$year<- c(1:nrow(TR.t.PCT))
PCT.model.TR<- summary(lm(TR.t.PCT$PCT~TR.t.PCT$year))
JB.t.PCT$year<- c(1:nrow(JB.t.PCT))
PCT.model.JB<- summary(lm(JB.t.PCT$PCT~JB.t.PCT$year))
HC.t.PCT$year<- c(1:nrow(HC.t.PCT))
PCT.model.HC<- summary(lm(HC.t.PCT$PCT~HC.t.PCT$year))
BS.t.PCT$year<- c(1:nrow(BS.t.PCT))
PCT.model.BS<- summary(lm(BS.t.PCT$PCT~BS.t.PCT$year))

EC.t.PCT$year<- c(1:nrow(EC.t.PCT))
PCT.model.EC<- summary(lm(EC.t.PCT$PCT~EC.t.PCT$year))
WJ.t.PCT$year<- c(1:nrow(WJ.t.PCT))
PCT.model.WJ<- summary(lm(WJ.t.PCT$PCT~WJ.t.PCT$year))
SB.t.PCT$year<- c(1:nrow(SB.t.PCT))
PCT.model.SB<- summary(lm(SB.t.PCT$PCT~SB.t.PCT$year))
MB.t.PCT$year<- c(1:nrow(MB.t.PCT))
PCT.model.MB<- summary(lm(MB.t.PCT$PCT~MB.t.PCT$year))
CS.t.PCT$year<- c(1:nrow(CS.t.PCT))
PCT.model.CS<- summary(lm(CS.t.PCT$PCT~CS.t.PCT$year))




# data structured after regions ####

#all data ####
LMB.data<- daily.TR.data[,1:6]
LMB.data$depth<- (daily.TR.data$depth+daily.EC.data$depth)/2
LMB.data$sal<- (daily.TR.data$sal+daily.EC.data$sal)/2
LMB.data$depthPOR<- (daily.TR.data$depthPOR+daily.EC.data$depthPOR)/2
LMB.data$SalPOR<- (daily.TR.data$SalPOR+daily.EC.data$SalPOR)/2

TC.data<- daily.WJ.data[,1:6]
TC.data$depth<- (daily.WJ.data$depth+daily.JB.data$depth)/2
TC.data$sal<- (daily.WJ.data$sal+daily.JB.data$sal)/2
TC.data$depthPOR<- (daily.WJ.data$depthPOR+daily.JB.data$depthPOR)/2
TC.data$SalPOR<- (daily.WJ.data$SalPOR+daily.JB.data$SalPOR)/2

LS.data<- daily.SB.data[,1:6]
LS.data$depth<- (daily.SB.data$depth+daily.HC.data$depth)/2
LS.data$sal<- (daily.SB.data$sal+daily.HC.data$sal)/2
LS.data$depthPOR<- (daily.SB.data$depthPOR+daily.HC.data$depthPOR)/2
LS.data$SalPOR<- (daily.SB.data$SalPOR+daily.HC.data$SalPOR)/2

SBB.data<- daily.MB.data[,1:6]
SBB.data$depth<- (daily.MB.data$depth+daily.BS.data$depth+daily.CS.data$depth)/3
SBB.data$sal<- (daily.MB.data$sal+daily.BS.data$sal+daily.CS.data$sal)/3
SBB.data$depthPOR<- (daily.MB.data$depthPOR+daily.BS.data$depthPOR+daily.CS.data$depthPOR)/3
SBB.data$SalPOR<- (daily.MB.data$SalPOR+daily.BS.data$SalPOR+daily.CS.data$SalPOR)/3

# structured data for graphs ####
daily.LMB.DF<- daily.TR.DF
daily.LMB.DF$depth<- (daily.TR.DF$depth+daily.EC.DF$depth)/2
daily.LMB.DF$sal<- (daily.TR.DF$sal+daily.EC.DF$sal)/2
daily.LMB.DF.fish1<- daily.TR.DF.fish1
daily.LMB.DF.fish1$depth<- (daily.TR.DF.fish1$depth+daily.EC.DF.fish1$depth)/2
daily.LMB.DF.fish1$sal<- (daily.TR.DF.fish1$sal+daily.EC.DF.fish1$sal)/2
daily.LMB.DF.SAV1<- daily.TR.DF.SAV1
daily.LMB.DF.SAV1$depth<- (daily.TR.DF.SAV1$depth+daily.EC.DF.SAV1$depth)/2
daily.LMB.DF.SAV1$sal<- (daily.TR.DF.SAV1$sal+daily.EC.DF.SAV1$sal)/2

daily.TC.DF<- daily.WJ.DF
daily.TC.DF$depth<- (daily.WJ.DF$depth+daily.JB.DF$depth)/2
daily.TC.DF$sal<- (daily.WJ.DF$sal+daily.JB.DF$sal)/2
daily.TC.DF.fish1<- daily.WJ.DF.fish1
daily.TC.DF.fish1$depth<- (daily.WJ.DF.fish1$depth+daily.JB.DF.fish1$depth)/2
daily.TC.DF.fish1$sal<- (daily.WJ.DF.fish1$sal+daily.JB.DF.fish1$sal)/2
daily.TC.DF.SAV1<- daily.WJ.DF.SAV1
daily.TC.DF.SAV1$depth<- (daily.WJ.DF.SAV1$depth+daily.JB.DF.SAV1$depth)/2
daily.TC.DF.SAV1$sal<- (daily.WJ.DF.SAV1$sal+daily.JB.DF.SAV1$sal)/2

daily.LS.DF<- daily.SB.DF
daily.LS.DF$depth<- (daily.SB.DF$depth+daily.HC.DF$depth)/2
daily.LS.DF$sal<- (daily.SB.DF$sal+daily.HC.DF$sal)/2
daily.LS.DF.fish1<- daily.SB.DF.fish1
daily.LS.DF.fish1$depth<- (daily.SB.DF.fish1$depth+daily.HC.DF.fish1$depth)/2
daily.LS.DF.fish1$sal<- (daily.SB.DF.fish1$sal+daily.HC.DF.fish1$sal)/2
daily.LS.DF.SAV1<- daily.SB.DF.SAV1
daily.LS.DF.SAV1$depth<- (daily.SB.DF.SAV1$depth+daily.HC.DF.SAV1$depth)/2
daily.LS.DF.SAV1$sal<- (daily.SB.DF.SAV1$sal+daily.HC.DF.SAV1$sal)/2

daily.SBB.DF<- daily.MB.DF
daily.SBB.DF$depth<- (daily.MB.DF$depth+daily.BS.DF$depth+daily.CS.DF$depth)/3
daily.SBB.DF$sal<- (daily.MB.DF$sal+daily.BS.DF$sal+daily.CS.DF$sal)/3
daily.SBB.DF.fish1<- daily.MB.DF.fish1
daily.SBB.DF.fish1$depth<- (daily.MB.DF.fish1$depth+daily.BS.DF.fish1$depth+daily.CS.DF.fish1$depth)/3
daily.SBB.DF.fish1$sal<- (daily.MB.DF.fish1$sal+daily.BS.DF.fish1$sal+daily.CS.DF.fish1$sal)/3
daily.SBB.DF.SAV1<- daily.MB.DF.SAV1
daily.SBB.DF.SAV1$depth<- (daily.MB.DF.SAV1$depth+daily.BS.DF.SAV1$depth+daily.CS.DF.SAV1$depth)/3
daily.SBB.DF.SAV1$sal<- (daily.MB.DF.SAV1$sal+daily.BS.DF.SAV1$sal+daily.CS.DF.SAV1$sal)/3

#region box plot month depth and sal ####   neeeds work ####


box.season.depth.region<- ggplot(hy.now %>% 
                                   filter(site != 'TP'), 
                                 aes(x = area, y = depth, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)")

box.season.sal.region<- ggplot(hy.now %>% 
                                 filter(site != 'TP'), 
                               aes(x = area, y = sal, fill= Season))+
  theme_bw()+
  geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)")


monthly.mean.depth.region<- ggplot(hy.now %>% 
                                     filter(site != 'TP'), 
                                   aes(x = factor(Month), y = depth, fill= area))+
  geom_boxplot(aes(fill=area, color =area)) +
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Water Level (cm)")


aggregate( depth ~ MONTH+ area , 
           hy.now %>% 
             filter(site != 'TP'), 
           mean )

hy.now %>% 
  filter(area == "SBB") %>% 
  aggregate( depth ~ LABEL+ area , 
             min ) %>% 
  filter(depth == min(depth)) 


monthly.mean.sal.region<- ggplot(hy.now %>% 
                                   filter(site != 'TP'), 
                                 aes(x = factor(Month), y = sal, fill= area))+
  theme_bw()+
  geom_boxplot(aes(fill=area, color =area))+
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme(legend.title = element_blank())+
  xlab("") + ylab("Salinity (psu)")


# region daily and POR with sample dates ####

#LMB
d.plot.LMB.DAILY<- ggplot(data = daily.LMB.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.LMB.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.LMB.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  xlab("LMB") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LMB.DF$depth)/5)*5, max(daily.LMB.DF$depth), 5))+
  #ylim(0, 65)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


s.plot.LMB.DAILY <- ggplot(data = daily.LMB.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.LMB.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.LMB.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("LMB") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LMB.DF$sal)/5)*5, max(daily.LMB.DF$sal), 5))+
  #ylim(0, 23)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#TC

d.plot.TC.DAILY<- ggplot(data = daily.TC.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.TC.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.TC.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  xlab("TC") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.TC.DF$depth)/5)*5, max(daily.TC.DF$depth), 5))+
  #ylim(0, 65)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


s.plot.TC.DAILY <- ggplot(data = daily.TC.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.TC.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.TC.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("TC") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.TC.DF$sal)/5)*5, max(daily.TC.DF$sal), 5))+
  #ylim(0, 25)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#LS

d.plot.LS.DAILY<- ggplot(data = daily.LS.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.LS.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.LS.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  xlab("LS") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LS.DF$depth)/5)*5, max(daily.LS.DF$depth), 5))+
  #ylim(0, 62)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


s.plot.LS.DAILY <- ggplot(data = daily.LS.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.LS.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.LS.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("LS") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LS.DF$sal)/5)*5, max(daily.LS.DF$sal), 5))+
  #ylim(0, 32)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#SBB

d.plot.SBB.DAILY<- ggplot(data = daily.SBB.DF, aes(x=as.Date(LABEL), y=depth, colour=what))+
  geom_line()+
  geom_point(data = daily.SBB.DF.fish1, aes(x=as.Date(fish_date), y=depth), colour= "red", shape=16, size=5)+
  geom_point(data = daily.SBB.DF.SAV1, aes(x=as.Date(sav_date), y=depth), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  geom_hline(yintercept=13, linetype="dashed", 
             color = "pink", size=1)+
  theme_bw() +
  xlab("SBB") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.SBB.DF$depth)/5)*5, max(daily.SBB.DF$depth), 5))+
  #ylim(0, 90)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


s.plot.SBB.DAILY <- ggplot(data = daily.SBB.DF, aes(x=as.Date(LABEL), y=sal, colour=what ))+
  geom_line()+
  geom_point(data = daily.SBB.DF.fish1, aes(x=as.Date(fish_date), y=sal), colour="red", shape=16, size=5)+
  geom_point(data = daily.SBB.DF.SAV1, aes(x=as.Date(sav_date), y=sal), colour= "green", shape=17, size=5)+
  scale_color_manual(values=c("Blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("SBB") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.SBB.DF$sal)/5)*5, max(daily.SBB.DF$sal), 5))+
  #ylim(0, 38)+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


#DAILY RANGE ####
TR_h$date<- format(as.POSIXct(TR_h$Date_Time),format='%Y/%m/%d')
TR.range.df<- ddply(TR_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                 depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                 sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
TR.range.df<-subset(TR.range.df, !(MONTH == "#VALUE!" ))
TR.range.df$site<- "TR"
TR.range.df$area<- "LMB"
EC_h$date<- format(as.POSIXct(EC_h$Date_Time),format='%Y/%m/%d')
EC.range.df<- ddply(EC_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
EC.range.df<-subset(EC.range.df, !(MONTH == "#VALUE!" ))
EC.range.df$site<- "EC"
EC.range.df$area<- "LMB"
WJ_h$date<- format(as.POSIXct(WJ_h$Date_Time),format='%Y/%m/%d')
WJ.range.df<- ddply(WJ_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
WJ.range.df<-subset(WJ.range.df, !(MONTH == "#VALUE!" ))
WJ.range.df$site<- "WJ"
WJ.range.df$area<- "TC"
JB_h$date<- format(as.POSIXct(JB_h$Date_Time),format='%Y/%m/%d')
JB.range.df<- ddply(JB_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
JB.range.df<-subset(JB.range.df, !(MONTH == "#VALUE!" ))
JB.range.df$site<- "JB"
JB.range.df$area<- "TC"
SB_h$date<- format(as.POSIXct(SB_h$Date_Time),format='%Y/%m/%d')
SB.range.df<- ddply(SB_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
SB.range.df<-subset(SB.range.df, !(MONTH == "#VALUE!" ))
SB.range.df$site<- "SB"
SB.range.df$area<- "LS"
HC_h$date<- format(as.POSIXct(HC_h$Date_Time),format='%Y/%m/%d')
HC.range.df<- ddply(HC_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
HC.range.df<-subset(HC.range.df, !(MONTH == "#VALUE!" ))
HC.range.df$site<- "HC"
HC.range.df$area<- "LS"
MB_h$date<- format(as.POSIXct(MB_h$Date_Time),format='%Y/%m/%d')
MB.range.df<- ddply(MB_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
MB.range.df<-subset(MB.range.df, !(MONTH == "#VALUE!" ))
MB.range.df$site<- "MB"
MB.range.df$area<- "SBB"
BS_h$date<- format(as.POSIXct(BS_h$Date_Time),format='%Y/%m/%d')
BS.range.df<- ddply(BS_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
BS.range.df<-subset(BS.range.df, !(MONTH == "#VALUE!" ))
BS.range.df$site<- "BS"
BS.range.df$area<- "SBB"
CS_h$date<- format(as.POSIXct(CS_h$Date_Time),format='%Y/%m/%d')
CS.range.df<- ddply(CS_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
CS.range.df<-subset(CS.range.df, !(MONTH == "#VALUE!" ))
CS.range.df$site<- "CS"
CS.range.df$area<- "SBB"
TP_h$date<- format(as.POSIXct(TP_h$Date_Time),format='%Y/%m/%d')
TP.range.df<- ddply(TP_h, .(YEAR, HY, MONTH, DAY, date, Season), summarise, 
                    depth.range = max(depth, na.rm=T) - min(depth, na.rm=T),
                    sal.range = max(sal, na.rm=T) - min(sal, na.rm=T))
TP.range.df<-subset(TP.range.df, !(MONTH == "#VALUE!" ))
TP.range.df$site<- "TP"
TP.range.df$area<- "SBB"

allsites.range.df<- rbind(TR.range.df,EC.range.df,WJ.range.df,JB.range.df,
                          SB.range.df,HC.range.df,MB.range.df,BS.range.df,CS.range.df, TP.range.df)
allsites.range.df$Season<- mapvalues(allsites.range.df$Season, from = c(1,2), to = c("Dry Season", "Wet Season"))
allsites.range.df$Season <- factor(allsites.range.df$Season, levels = c("Wet Season","Dry Season"))
allsites.range.df$site <- factor(allsites.range.df$site, levels = c("TR","EC","WJ","JB","SB","HC","MB","BS","CS", "TP"))
allsites.range.df$area <- factor(allsites.range.df$area, levels = c("LMB", "TC", "LS", "SBB"))
allsites.range.df$MONTH <- factor(allsites.range.df$MONTH, levels = c("6","7","8","9","10","11","12","1",
                                                                "2","3","4","5"))
allsites.range.df$MONTH <- month.abb[allsites.range.df$MONTH]

#box plots for daily sd

box.season.depth.site.range<- ggplot(allsites.range.df %>% 
                                       filter(HY == this_report), 
                                     aes(x = site, y = depth.range, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5, outlier.shape = NA)+
  scale_fill_manual(values=c("blue", "red"))+
  scale_y_continuous(limits = quantile(allsites.range.df$depth.range, c(0.1, 0.95), na.rm = TRUE))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Daily Range Water Level (cm)")



box.season.depth.site.range_POR<- ggplot(allsites.range.df %>% 
                                       filter(HY != this_report) %>% 
                                         filter(HY != '2000-01') %>% 
                                         filter(HY != '2001-02') %>% 
                                         filter(HY != '2002-03') %>%
                                         filter(HY != '2003-04') %>%
                                         filter(HY != '2004-05') %>%
                                         filter(HY != '2005-06') %>% 
                                         filter(HY != '2020-21') %>% 
                                         filter(HY != '2019-20') %>% 
                                         filter(HY != '2018-19') %>% 
                                         filter(HY != '2017-18') %>% 
                                         filter(HY != '2016-17')  , 
                                     aes(x = site, y = depth.range, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5, outlier.shape = NA)+
  scale_fill_manual(values=c("blue", "red"))+
  scale_y_continuous(limits = quantile(allsites.range.df$depth.range, c(0.1, 0.95), na.rm = TRUE))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Water Level (cm)")


box.season.depth.site.range_POR<- ggplot(allsites.range.df %>% 
                                           filter(HY == '2005-06')  , 
                                         aes(x = site, y = depth.range, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5, outlier.shape = NA)+
  scale_fill_manual(values=c("blue", "red"))+
  scale_y_continuous(limits = quantile(allsites.range.df$depth.range, c(0.1, 0.95), na.rm = TRUE))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Water Level (cm)")

summary(allsites.range.df )




box.season.sal.site.range<- ggplot(allsites.range.df %>% 
                                     filter(HY == this_report), 
                                   aes(x = site, y = sal.range, fill= Season))+
  scale_fill_manual(values=c("blue", "red"))+
  #scale_y_continuous(limits = quantile(allsites.range.df$depth.range, c(0.1, 0.9), na.rm = TRUE))+
  theme_bw()+
  geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+  #, outlier.shape = NA)+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Salinity (psu)")

box.season.depth.region.range<- ggplot(allsites.range.df %>% 
                                         filter(HY == this_report),
                                       aes(x = area, y = depth.range, fill= Season))+
  theme_bw()+
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Water Level (cm)")

box.season.sal.region.range<- ggplot(allsites.range.df %>% 
                                       filter(HY == this_report),
                                     aes(x = area, y = sal.range, fill= Season))+
  theme_bw()+
  geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+
  scale_fill_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Salinity (psu)")

allsites.range.df$MONTH<- factor(
  allsites.range.df$MONTH, 
  levels = c('Jun','Jul','Aug','Sep','Oct','Nov','Dec',
             'Jan','Feb','Mar','Apr','May'))

monthly.mean.depth.range.all<- ggplot(allsites.range.df %>% 
                                        filter(HY == this_report), aes(x = MONTH, y = depth.range, fill= area))+
  #geom_boxplot(aes(fill=area, color=area))+
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area),outlier.colour = NA)+
  theme_bw()+
  #scale_y_continuous(limits = quantile(allsites.range.df$sal.range, c(0.1, 0.95), na.rm = TRUE))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Water Level (cm)")

monthly.mean.sal.range.all<- ggplot(allsites.range.df %>% 
                                      filter(HY == this_report), aes(x = MONTH, y = sal.range, fill= area))+
  #geom_boxplot(aes(fill=area, color=area))+
  scale_fill_manual(values=c("red1","blue1","green1","yellow1"))+
  scale_color_manual(values=c("red1","blue1","green1","yellow1"))+
  geom_boxplot(aes(fill=area),outlier.colour = NA)+
  theme_bw()+
  #scale_y_continuous(limits = quantile(allsites.range.df$sal.range, c(0.1, 0.95), na.rm = TRUE))+
  theme(legend.title = element_blank())+
  xlab("") + ylab("Range Salinity (psu)")



allsites.range.df %>% 
  filter(HY == this_report)%>% 
  filter(area == 'LS') %>% 
  filter(sal.range == max(sal.range))
   
# ALL graphs INDIVIDUAL SITES ####

#line - daily depth with POR and sample dates
d.plot.TR.DAILY
d.plot.EC.DAILY
d.plot.WJ.DAILY
d.plot.JB.DAILY
d.plot.SB.DAILY
d.plot.HC.DAILY
d.plot.MB.DAILY
d.plot.BS.DAILY
d.plot.CS.DAILY
d.plot.TP.DAILY


#line - daily salinity with POR and sample dates
s.plot.TR.DAILY
s.plot.EC.DAILY
s.plot.WJ.DAILY
s.plot.JB.DAILY
s.plot.SB.DAILY
s.plot.HC.DAILY
s.plot.MB.DAILY
s.plot.BS.DAILY
s.plot.CS.DAILY
s.plot.TP.DAILY

#exceedance - depth
ex.TR.depth
ex.EC.depth
ex.WJ.depth
ex.JB.depth
ex.SB.depth
ex.HC.depth
ex.MB.depth
ex.BS.depth
ex.CS.depth
#exceedance - salinity
ex.TR.sal
ex.EC.sal
ex.WJ.sal
ex.JB.sal
ex.SB.sal
ex.HC.sal
ex.MB.sal
ex.BS.sal
ex.CS.sal

#boxplot seasonal depth and sal
box.season.depth.all
box.season.sal.all

#boxplot month
monthly.mean.depth.all
monthly.mean.sal.all

monthly.mean.depth.region
monthly.mean.sal.region

#PCT
PCT_scatter_plot
PCT_scatter_plot.long.sites
PCT_scatter_plot.short.sites    
PCT.Exceedance.long.sites
PCT.Exceedance.short.sites

#range
box.season.depth.site.range
box.season.sal.site.range
box.season.depth.region.range
box.season.sal.region.range
monthly.mean.depth.range.all


#line graph
annotate_figure(ggarrange(d.plot.TR.DAILY,d.plot.EC.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       TR","       EC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LMB, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(d.plot.WJ.DAILY,d.plot.JB.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       WJ","       JB"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("TC, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(d.plot.SB.DAILY,d.plot.HC.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       SB","       HC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LS, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(d.plot.MB.DAILY,d.plot.BS.DAILY,d.plot.CS.DAILY, nrow = 1, ncol = 3, 
                          labels=c("       MB","       BS","       CS"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("SBB, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))

annotate_figure(ggarrange(s.plot.TR.DAILY,s.plot.EC.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       TR","       EC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LMB, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(s.plot.WJ.DAILY,s.plot.JB.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       WJ","       JB"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("TC, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(s.plot.SB.DAILY,s.plot.HC.DAILY, nrow = 1, ncol = 2, 
                          labels=c("       SB","       HC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LS, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(s.plot.MB.DAILY,s.plot.BS.DAILY,s.plot.CS.DAILY, nrow = 1, ncol = 3, 
                          labels=c("       MB","       BS","       CS"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("SBB, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))

ggarrange(s.plot.sevenP.DAILY,d.plot.sevenP.DAILY, nrow = 1, ncol = 2,
                           common.legend = TRUE )

ggarrange(d.plot.TP.DAILY,s.plot.TP.DAILY, nrow = 1, ncol = 2,
          common.legend = TRUE )
#exceedance depth
annotate_figure(ggarrange(ex.TR.depth,ex.EC.depth, nrow = 1, ncol = 2, 
                          labels=c("       TR","       EC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LMB, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.WJ.depth,ex.JB.depth, nrow = 1, ncol = 2, 
                          labels=c("       WJ","       JB"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("TC, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.SB.depth,ex.HC.depth, nrow = 1, ncol = 2, 
                          labels=c("       SB","       HC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LS, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.MB.depth,ex.BS.depth,ex.CS.depth, nrow = 1, ncol = 3, 
                          labels=c("       MB","       BS","       CS"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("SBB, depth", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
#exceedance sal
annotate_figure(ggarrange(ex.TR.sal,ex.EC.sal, nrow = 1, ncol = 2, 
                          labels=c("       TR","       EC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LMB, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.WJ.sal,ex.JB.sal, nrow = 1, ncol = 2, 
                          labels=c("       WJ","       JB"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("TC, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.SB.sal,ex.HC.sal, nrow = 1, ncol = 2, 
                          labels=c("       SB","       HC"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("LS, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
annotate_figure(ggarrange(ex.MB.sal,ex.BS.sal,ex.CS.sal, nrow = 1, ncol = 3, 
                          labels=c("       MB","       BS","       CS"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("SBB, sal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))


#boxplot seasonal depth and sal
annotate_figure(ggarrange(box.season.depth.all,box.season.sal.all, nrow = 1, ncol = 2, 
                          labels=c("       A","       B"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("seasonal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))

#boxplot month
annotate_figure(ggarrange(monthly.mean.depth.all,monthly.mean.sal.all, nrow = 1, ncol = 2, 
                          labels=c("       A","       B"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("seasonal", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))

#PCT 3
annotate_figure(ggarrange(PCT_scatter_plot.long.sites,PCT.Exceedance.long.sites, nrow = 1, ncol = 2, 
                          labels=c("       A","       B"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("PCT long sites", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
#model results 
PCT.model.TR
PCT.model.JB
PCT.model.HC
PCT.model.BS


#PCT short
annotate_figure(ggarrange(PCT_scatter_plot.short.sites,PCT.Exceedance.short.sites, nrow = 1, ncol = 2, 
                          labels=c("       A","       B"), 
                          vjust= 2, hjust = -0.5,  common.legend = TRUE ), 
                bottom = text_grob("PCT short sites", color = "blue", hjust = 1, 
                                   x = 1, face = "italic", size = 10))
#model results
PCT.model.EC
PCT.model.WJ
PCT.model.SB
PCT.model.MB
PCT.model.CS


#region line graphs

d.plot.LMB.DAILY
d.plot.TC.DAILY
d.plot.LS.DAILY
d.plot.SBB.DAILY

s.plot.LMB.DAILY
s.plot.TC.DAILY
s.plot.LS.DAILY
s.plot.SBB.DAILY



#temp graphs 

TR.temp<- ggplot(data=TRday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(TRday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("TR")

EC.temp<- ggplot(data=ECday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(ECday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("EC")

WJ.temp<- ggplot(data=WJday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(WJday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("WJ")

JB.temp<- ggplot(data=JBday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(JBday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("JB")

SB.temp<- ggplot(data=SBday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(SBday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("SB")

HC.temp<- ggplot(data=HCday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(HCday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("HC")

sevenP.temp<- ggplot(data=sevenPday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(sevenPday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("7P")

TP.temp<- ggplot(data=TPday.now, aes(x=LABEL, y=temp))+
  geom_hline(aes(yintercept = ((90-32)*(5/9)), 
                 color = 'red'), 
             size=1)+
  annotate("text",x = as.POSIXct(TPday.now$LABEL[1]),  
           y=((90-32)*(5/9))+2.5, 
           label="32.2°C \n (90°F)", 
           color = 'red')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line()+
  ylab('Temperature °C')+
  theme_bw()+
  theme(legend.position="none")+
  xlab("TP")

ggarrange(TR.temp, EC.temp, nrow=2, ncol=1)
ggarrange(WJ.temp, JB.temp, nrow=2, ncol=1)
ggarrange(SB.temp, HC.temp, nrow=2, ncol=1)
ggarrange(sevenP.temp, TP.temp, nrow=2, ncol=1)


TRday.now$site<-"TR"
ECday.now$site<-"EC"
WJday.now$site<- "WJ"
JBday.now$site<- "JB"
SBday.now$site<- "SB"
HCday.now$site<- "HC"
JBday.now$site<- "JB"
TPday.now$site <- 'TP'
sevenPday.now$site <- '7P'
TRx<- TRday.now[TRday.now$temp == min(TRday.now$temp), ] %>% 
  dplyr::select(-rain)
ECx<- ECday.now[ECday.now$temp == min(ECday.now$temp), ]
WJx<- WJday.now[WJday.now$temp == min(WJday.now$temp), ] %>% 
  dplyr::select(-`Rain (in.)`)
JBx<- JBday.now[JBday.now$temp == min(JBday.now$temp), ] %>% 
  dplyr::select(-rain)
SBx<- SBday.now[SBday.now$temp == min(SBday.now$temp), ]
HCx<- HCday.now[HCday.now$temp == min(HCday.now$temp), ] %>% 
  dplyr::select(-rain)
TPx<- HCday.now[TPday.now$temp == min(TPday.now$temp), ] %>% 
  dplyr::select(-rain)
sevenPx<- sevenPday.now[sevenPday.now$temp == min(sevenPday.now$temp), ] 

min.temp<- rbind(TRx,ECx,WJx,JBx,SBx,HCx,TPx,sevenPx)

TRx<- TRday.now[TRday.now$temp == max(TRday.now$temp), ] %>% 
  dplyr::select(-rain)
ECx<- ECday.now[ECday.now$temp == max(ECday.now$temp), ]
WJx<- WJday.now[WJday.now$temp == max(WJday.now$temp), ] %>% 
  dplyr::select(-`Rain (in.)`)
JBx<- JBday.now[JBday.now$temp == max(JBday.now$temp), ] %>% 
  dplyr::select(-rain)
SBx<- SBday.now[SBday.now$temp == max(SBday.now$temp), ]
HCx<- HCday.now[HCday.now$temp == max(HCday.now$temp), ] %>% 
  dplyr::select(-rain)
TPx<- HCday.now[TPday.now$temp == max(TPday.now$temp), ] %>% 
  dplyr::select(-rain)
sevenPx<- sevenPday.now[sevenPday.now$temp == max(sevenPday.now$temp), ] 

max.temp<- rbind(TRx,ECx,WJx,JBx,SBx,HCx,TPx,sevenPx)







#####
#####
#####


#####analysis ####

ann.depth.ex
wet.depth.ex
dry.depth.ex

depth.all.sites<- rbind(wet.depth.ex, dry.depth.ex)
depth.all.sites$site<- as.factor(depth.all.sites$site)
depth.all.sites$Season<- as.factor(depth.all.sites$Season)

#checking to see the regional similarity ####
#https://rcompanion.org/rcompanion/d_07.html
model = lme(depth ~ site, random=~1|HY,
            data=depth.all.sites,
            method="REML")
anova.lme(model,
          type="sequential",
          adjustSigma = FALSE)
posthoc = glht(model,
               linfct = mcp(site="Tukey"))
mcs = summary(posthoc,
              test=adjusted("single-step"))
mcs
cld(mcs,
    level=0.05,
    decreasing=TRUE)


#checking season ####
#maybe include daily data?
depth.all.sites.thisyear <- which(depth.all.sites$HY == this_report)
depth.all.sites.thisyear<- depth.all.sites[c(depth.all.sites.thisyear),]
model1 = lme(depth ~ Season, random=~1|site,
            data=depth.all.sites.thisyear,
            method="REML")
anova.lme(model1,
          type="sequential",
          adjustSigma = FALSE)
posthoc = glht(model1,
               linfct = mcp(Season="Tukey"))
mcs = summary(posthoc,
              test=adjusted("single-step"))
mcs
cld(mcs,
    level=0.05,
    decreasing=TRUE)

#min max for all sites
hy.now[which.max(hy.now$depth),]
hy.now[which.min(hy.now$depth),]

 
hy.now %>% 
  group_by(area) %>% 
  slice(which.min(depth))

#monthly numbers
ddply(daily.LMB.DF, .(year(LABEL ), MONTH), summarise, mean.depth = mean(depth, na.rm=TRUE),
      min.depth = min(depth, na.rm=TRUE))
ddply(daily.TC.DF, .(year(LABEL ),MONTH), summarise, mean.depth = mean(depth, na.rm=TRUE),
      min.depth = min(depth, na.rm=TRUE))
ddply(daily.LS.DF, .(year(LABEL ),MONTH), summarise, mean.depth = mean(depth, na.rm=TRUE),
      min.depth = min(depth, na.rm=TRUE))
ddply(daily.SBB.DF, .(year(LABEL ),MONTH), summarise, mean.depth = mean(depth, na.rm=TRUE),
      min.depth = min(depth, na.rm=TRUE))

daily.LMB.DF$area<- "LMB"
daily.TC.DF$area<- "TC"
daily.LS.DF$area<- "LS"
daily.SBB.DF$area<- "SBB"

all_for_analysis<- rbind(daily.LMB.DF, daily.TC.DF, daily.LS.DF, daily.SBB.DF )
all.sites

TukeyHSD(aov(depth ~ site, data = all.sites[all.sites$HY == this_report,]))



depth.all.sites.thisyear <- which(depth.all.sites$HY == this_report)
depth.all.sites.thisyear<- depth.all.sites[c(depth.all.sites.thisyear),]
model1 = lme(depth ~ site, random=~1|area,
             data=all.sites[all.sites$HY == this_report,],
             method="REML")
anova.lme(model1,
          type="sequential",
          adjustSigma = FALSE)
#posthoc = glht(model1,
#               linfct = mcp(site="Tukey"))
mcs = summary(posthoc,
              test=adjusted("single-step"))
mcs
cld(mcs,
    level=0.05,
    decreasing=TRUE)

#Sea Level Rise ####
SLR<- read_excel("/Hydrology/SLR/KWMSL_1913-ALLdata.xlsm", 
         sheet = 'KWWL_1913-ALLdata')

SLR1<- ddply(SLR, .(hydroyear), summarise, MLLW = mean(MSL, na.rm=TRUE))
SLR2<- SLR1[88:nrow(SLR1),]

SLR_MLLW1 <- ggplot(SLR1, aes(x=hydroyear, y=MLLW)) + 
  geom_rect(xmin="2000-01", xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5,fill="grey90")+
  geom_vline(xintercept = "2000-01")+
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        panel.grid.major = element_line(colour = "grey80"),
        axis.text = element_text(size=12),
        text = element_text(size=15))+
  xlab("") + scale_y_continuous(name="MSL Relative to MLLW in Meters", limits=c(-0.2, 0.2),n.breaks = 10 )+
  geom_smooth(aes(x = as.numeric(factor(hydroyear)), y = MLLW), method = 'lm', se=F, colour="red")+ 
  scale_x_discrete(breaks = c("1913-14", "1918-19", "1923-24","1928-29", "1933-34", "1938-39", "1943-44",
                              "1948-49", "1953-54", "1958-59", "1963-64", "1968-69", "1973-74", "1978-79", "1983-84",
                              "1988-89", "1993-94", "1998-99", "2003-04", "2008-09", "2013-14", "2018-19",
                               '2022-23')) 


corr.line.SLR_MLLW1<-summary(lm(SLR1$MLLW~ as.numeric(factor(SLR1$hydroyear))))
#adding formula as text
#https://stackoverflow.com/questions/44647613/receive-the-equation-of-the-stat-smooth-in-ggplot2-r-mtcars-example


SLR_MLLW2 <-  ggplot(SLR2, aes(x=hydroyear, y=MLLW)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        panel.grid.major = element_line(colour = "grey80"),
        axis.text = element_text(size=12),
        text = element_text(size=15))+
  xlab("") + scale_y_continuous(name="MSL Relative to MLLW in Meters", limits=c(0, 0.2),n.breaks = 5 )+
  geom_smooth(aes(x = as.numeric(factor(hydroyear)), y = MLLW), method = "glm", method.args=list(family="binomial"),
              se =F, colour="red")+
  scale_x_discrete(breaks = c(SLR2$hydroyear))


corr.line.SLR_MLLW2<-summary(glm(SLR2$MLLW~ as.numeric(factor(SLR2$hydroyear))))
R2.SLR_MLLW2<- with(corr.line.SLR_MLLW2, 1 - deviance/null.deviance)


ggarrange(SLR_MLLW1, SLR_MLLW2)

#correlating flow with depth and sal? ####


flow<- read.csv("SDCS_flow_data_all.csv", header = T)


#this years flow
flow_now<- which(flow$HY == this_report)
flow.now<- flow[c(flow_now),]
#columns to remove
drops <- c("X","X.1","C.111..lower.._AF..pos.neg.","C.111..lower.._AF..pos.neg.","S18C.cf.s","S.197.cf.s",
           "S.199.cf.s","S200.cf.s","S.176.cfs","S.177.cfs","S.178.cfs","S.331.cfs","S.173.cfs","S.332D.cfs")
flow.now<-flow.now[ , !(names(flow.now) %in% drops)]

names(flow.now)[names(flow.now) == 'C.111.Levee.Flow..lower.'] <- 'C111_levee_lower'
names(flow.now)[names(flow.now) == 'C.111.Seepage..lower.'] <- 'C111_seepage_lower'
names(flow.now)[names(flow.now) == 'C.111.Overland.Flow..lower.'] <- 'C111_overland_lower'
names(flow.now)[names(flow.now) == 'C.111.Seepage..central.'] <- 'C111_seepage_central'
names(flow.now)[names(flow.now) == 'C.111.Flow..central.'] <- 'C111_flow_central'
names(flow.now)[names(flow.now) == 'TSB...............acre.ft.day.'] <- 'TSB_AF'
names(flow.now)[names(flow.now) == 'S18C._AF'] <- 'S18C_AF'
names(flow.now)[names(flow.now) == 'S.197._AF'] <- 'S197_AF'
names(flow.now)[names(flow.now) == 'S.199._AF'] <- 'S199_AF'
names(flow.now)[names(flow.now) == 'S200._AF'] <- 'S200_AF'
names(flow.now)[names(flow.now) == 'S.176._AF'] <- 'S176_AF'
names(flow.now)[names(flow.now) == 'S.177.AF'] <- 'S177_AF'
names(flow.now)[names(flow.now) == 'S.178._AF'] <- 'S178_AF'
names(flow.now)[names(flow.now) == 'S.331._AF'] <- 'S331_AF'
names(flow.now)[names(flow.now) == 'S.173._AF'] <- 'S173_AF'
names(flow.now)[names(flow.now) == 'S.332D._AF'] <- 'S332D_AF'
#change label to date
flow.now$LABEL<- as.Date(flow.now$LABEL, "%d-%b-%y") 
#change seasons
flow.now$Season<- mapvalues(flow.now$Season, from = c(1,2), to = c("Dry Season", "Wet Season"))


#combine flow data with hydro data
hydro_flow<-left_join(hy.now, flow.now)

ggplot(hydro_flow, aes(x=TSB_AF, y=depth, shape=site, color=area)) +
  geom_point()
ggplot(hydro_flow, aes(x=S176_AF, y=depth, shape=site, color=area)) +
  geom_point()

#need time lag...
#we know that the sites are lagging to the overland flow, we might want to analyze this different?
#https://online.stat.psu.edu/stat510/lesson/8/8.2 

library("afc")

ccf(hydro_flow$TSB_AF, hydro_flow$depth)
#need to do each site individually (?)... dont know if there is time for this report...
#acf (auto corrletaion function)	= An array with the same dimensions as lag containing the estimated acf.
#lag = A three dimensional array containing the lags at which the acf is estimated.



all.sites
mean.seasonal<-ddply(all.sites, .(site, HY, Season), summarise, depth = mean(depth, na.rm=T),
      sal = mean(sal, na.rm=T))
write.csv(mean.seasonal, "seasonal_data_for_Devon.csv")






# WBR ---------------------------------------------------------------------

birds<- read.csv("ROSP.csv", header = T)


birds$date<- as.Date(birds$date, "%m/%d/%Y")



all_sites$date<- as.character(all_sites$LABEL)


myfunc <- function(x,y){all_sites[all_sites$LABEL >= x & all_sites$LABEL <= y,]}

DATE1 <- as.Date(min(birds$date[birds$what == "Estimated mean lay date"], na.rm = T))
DATE2 <- as.Date(max(birds$date[birds$what == "approx. 21-day old age"], na.rm = T))

site_bird_range <- myfunc(DATE1, DATE2)


birds$Region[birds$Region == "Northeast"]  <- "NE"
birds$Region[birds$Region == "Northwest"]  <- "NW"
birds$Region[birds$Region == "Central"]  <- "C"
birds$Region[birds$Region == "Southeast"]  <- "SE"
birds$Region[birds$Region == "Southwest"]  <- "SW"
birds$Region[birds$Region == "BAY-WIDE"]  <- "FBW"


birds$Region <- factor(birds$Region, levels = c("NW", "SW", "C","NE", "SE", "FBW"))
birds$what<- factor(birds$what, levels = c("Estimated mean lay date", "Estimated mean hatch date", "approx. 21-day old age"))


















birdplot1 <- ggplot(birds, aes(y=what , x=date, colour = what)) + 
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom",
        axis.title.y=element_text(family = "sans", size = 9.7, margin=margin(2,1,0,0)),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank())+
  xlab("") + ylab("")+
  facet_grid(Region~., switch="both")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))

birdplot2 <- ggplot(birds, aes(y=what , x=date, colour = what,shape = what)) + 
  geom_point(size=3)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.title.y=element_text(family = "sans", size = 9.7, margin=margin(2,1,0,0)),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank())+
  xlab("") + ylab("")+
  facet_grid(Region~., switch="both")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))


Water1<- ggplot(data = all_sites, aes(x=LABEL, y=depth))+
  geom_line(aes(x=LABEL, y=depth,colour=long),size=1.5)+
  theme_bw()+
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme(legend.position="top")+
  xlab("") + ylab("Water Level (cm)")


Water2<- ggplot(data = site_bird_range, aes(x=LABEL, y=depth))+
  geom_line(aes(x=LABEL, y=depth,colour=long),size=2)+
  theme_bw()+
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme(legend.position="top")+
  xlab("") + ylab("Water Level (cm)")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))

Water3<- ggplot(data = site_bird_range, aes(x=LABEL, y=depth)) + 
  geom_line(mapping = aes(x=LABEL, y=depth, colour = region)) + 
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme_bw()+
  theme(legend.position="top")+
  xlab("") + ylab("Water Level (cm)")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))

Water4<- ggplot(data = site_bird_range, aes(x=LABEL, y=depth))+
  geom_line(aes(x=LABEL, y=depth,colour=long),size=2)+
  theme_bw()+
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme(legend.position= "none")+
  xlab("") + ylab("Water Level (cm)")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))

Water5<- ggplot(data = all_sites, aes(x=LABEL, y=depth))+
  geom_line(mapping = aes(x=LABEL, y=depth, colour = region)) + 
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme_bw()+
  theme(legend.position="top")+
  xlab("") + ylab("Water Level (cm)")

Water6<- ggplot(data = site_bird_range, aes(x=LABEL, y=depth)) + 
  geom_line(mapping = aes(x=LABEL, y=depth, colour = region)) + 
  stat_summary(aes(y = depth, group=1), fun=mean, colour="red", geom="line",group=1, size= 1)+
  theme_bw()+
  theme(legend.position="none")+
  xlab("") + ylab("Water Level (cm)")+
  #scale_x_date(limits=c(DATE1,DATE2),expand=c(0.1,0))

ggarrange(Water5, Water6, birdplot, nrow = 3, ncol = 1)

ggarrange(Water2, birdplot, nrow = 2, ncol = 1)




# figure 5 ----------------------------------------------------------------
#1990-1999, 2000-2009, 2010-2019
#2020-21

TR_day %>% 
  filter(YEAR >= 1990) %>% 
  mutate(year_range = cut(YEAR, 
                          breaks=c(1990, 2000, 2010, 2020), 
                          right = FALSE, 
                          labels = c('Mean 1990-1999',
                                     'Mean 2000-2009',
                                     'Mean 2010-2019'))) %>% 
  mutate(year_range = if_else(HY == this_report, 
                              HY, year_range)) %>% 
  mutate(MONTH_new = if_else(MONTH >= 6, 
                             ymd(format(LABEL, format="2000-%m-%d")),
                             ymd(format(LABEL, format="2001-%m-%d")))) %>% 
  ddply(., .(year_range, MONTH_new), summarise, 
        depth = mean(depth, na.rm=TRUE)) %>% 
  filter(MONTH_new != "2000-2-29") %>% 
  drop_na(year_range) %>% 
  ggplot(aes(x=MONTH_new, 
             y=depth,
             group=year_range,
             color=year_range))+
  geom_segment(aes(x=ymd('2000-11-1'),
                   xend=ymd('2001-4-1'),
                   y=13,yend=13), 
               color = 'pink',
               size = 1.5)+
  geom_label(aes(x=ymd('2000-11-1'), 
                 y=13,  
                 label = 'PCT'),
             color='pink',
             size = 5) +
  geom_line(size=1)+
  scale_x_date(breaks = date_breaks("months"), 
               labels = date_format("%b"),
               expand = c(0,0))+
  scale_colour_manual(values=c("#FCB626","#D2432B",'#00E428',"#0052A5"))+
  theme_bw()+
  xlab("") + ylab("Water Level (cm)")+
  theme(legend.title = element_blank(),
       legend.position = c(0.8, 0.80),
       axis.text = element_text(size=15),
        text = element_text(size=15),
       legend.key.size = unit(0.5, 'cm'),
      legend.text = element_text(size=15))+
  guides(linetype = guide_legend(override.aes = list(size = 10)))+
  scale_y_continuous(
    limits = c(0, 90),
    expand = c(0,0),
    breaks = seq(0,90,5)) 



# data to send ------------------------------------------------------------


#this is without cond and rain - needed? 
write.csv(
  rbind(TRday.now %>% dplyr::select(-rain) %>% mutate(site ='TR'),
        ECday.now %>% mutate(site ='EC'),
        WJday.now %>% mutate(site ='WJ'),
        JBday.now %>% dplyr::select(-rain) %>% mutate(site ='JB'),
        SBday.now %>% mutate(site ='SB'),
        HCday.now %>% dplyr::select(-rain) %>% mutate(site ='HC'),
        MBday.now %>% mutate(site ='MB'),
        BSday.now %>% dplyr::select(-rain) %>% mutate(site ='BS'),
        CSday.now %>% mutate(site ='CS')), 
  paste('/Databases/tidy/Reports/ACOE/Data to send/AUDUBON_HYDRO_data_', 
        this_report, 
        '.csv',
        sep = ''))



ANNUAL_JERRY <- TR.depth.exceedance_df %>% mutate(site = "TR") %>% 
  rbind(EC.depth.exceedance_df %>% mutate(site = "EC")) %>% 
  rbind(WJ.depth.exceedance_df %>% mutate(site = "WJ")) %>%
  rbind(JB.depth.exceedance_df %>% mutate(site = "JB")) %>%
  rbind(SB.depth.exceedance_df %>% mutate(site = "SB")) %>%
  rbind(HC.depth.exceedance_df %>% mutate(site = "HC")) %>%
  rbind(MB.depth.exceedance_df %>% mutate(site = "MB")) %>%
  rbind(BS.depth.exceedance_df %>% mutate(site = "BS")) %>%
  rbind(CS.depth.exceedance_df %>% mutate(site = "CS")) %>% 
  dplyr::filter(What == "Annual") %>% 
  dplyr::select(-What)

ANNUAL_JERRY %>% 
  arrange(HY)



