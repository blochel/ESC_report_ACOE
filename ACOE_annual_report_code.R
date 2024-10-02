
# ACOE annual report ------------------------------------------------------



# hydrology ---------------------------------------------------------------


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
library('tmaptools')
library('OpenStreetMap')


# #annual report setup -----------------------------------------------------




#HYDRO YEARS TO USE and remove
this_report<- "2022-23" 

#HYDRO YEAR TO REMOVE if some newer data that this HY is included, check tail() for NA values
remove_HY<- "2023-24"


#check columns, change names and order

#columns to remove; structured after regions, sites
drops <- c("X","X.1","X.2","X.3", "Sp..Cond...mS.cm.", "Sp..Cond.","Water.Level..ft.", "Stage..ft..NAVD88",
           "DO...sat.", "DO..mg.L.", "pH", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", "X.11", "X.12",
           "ORP", "X.13", "X.14", "X.15", "X.16", "X.17", "X.18", "Rain..in.." )



# hydro station location --------------------------------------------------

hydro_location <- read.csv('P:/GIS/Excel/Hydro/Hydro Stations.csv')

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

hydro_day <- rbind(sevenP_day %>% mutate(site = '7P') %>% mutate(area = '7P'), 
                   TR_day %>% dplyr::select(-c(rain)) %>% mutate(site = 'TR') %>% mutate(area = 'LMB'),
                   EC_day %>% mutate(site = 'EC') %>% mutate(area = 'LMB'),
                   WJ_day %>% dplyr::select(-c(`Rain (in.)`)) %>% mutate(site = 'WJ') %>% mutate(area = 'TC'),
                   JB_day %>% dplyr::select(-c(rain)) %>% mutate(site = 'JB') %>% mutate(area = 'TC'),
                   SB_day %>% mutate(site = 'SB') %>% mutate(area = 'LS'),
                   HC_day %>% dplyr::select(-c(rain)) %>% mutate(site = 'HC') %>% mutate(area = 'LS'),
                   MB_day %>% dplyr::select(-c(`Rain (in.)`)) %>% mutate(site = 'MB') %>% mutate(area = 'SBB'),
                   BS_day %>% dplyr::select(-c(rain)) %>% mutate(site = 'BS') %>% mutate(area = 'SBB'),
                   CS_day %>% mutate(site = 'CS') %>% mutate(area = 'SBB'),
                   TP_day %>% mutate(site = 'TP') %>% mutate(area = 'TP')) %>% 
  filter(HY != remove_HY) 


report_hy <- hydro_day %>% filter(HY == this_report)


# start dates table  ------------------------------------------------------

start_date_location <- hydro_day %>% 
  group_by(site) %>% 
  dplyr::summarize(start_date = as.Date(min(LABEL))) %>% 
  mutate(abs = factor(site, levels = c('7P','TR','EC','WJ','JB','SB','HC',
                                       'MB','BS','CS','TP')), 
         region = if_else(site == '7P', '7P', 
                          if_else(site == 'TR' | site == 'EC', 
                                  'LMB',
                                  if_else(site == 'WJ' | site == 'JB', 
                                          'TC', 
                                          if_else(site == 'SB' | site == 'HC', 
                                                  'LS',
                                                  if_else(site == 'MB' | site == 'BS' | site == 'CS', 
                                                          'SBB', 'TP')))))) %>% 
  arrange(by_group = abs) %>% 
  left_join(hydro_location %>% 
              dplyr::rename(full_name = Site, 
                     lat = Latitude, 
                     long = Longitude) %>% 
              dplyr::select(-c(state,age_fish,age_hydro,Sampling)), 
            by = join_by(abs))

start_date_table <- start_date_location %>% 
  dplyr::select(c(site, full_name, start_date, region, lat, long)) %>% 
  knitr::kable( format="html", escape=FALSE,
                caption = "Monitoring Start Dates & Location") %>% 
  kableExtra::kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12)




# site map ----------------------------------------------------------------

library('tmap')


hydro_location_all <- hydro_location %>% 
  dplyr::select(-c(Site, state, age_fish, age_hydro, Sampling)) %>% 
  dplyr::filter(abs != 'DJB' & abs != 'DTR' & abs != 'NR' & abs != 'RC' & abs != 'SC') %>% 
  mutate(region = if_else(abs == 'TR' | abs == 'EC', 'LMB', 
                          if_else(abs == '7P', '7P', 
                                  if_else(abs == 'BL' | abs == 'SD' | abs == 'LI', 'Cape',
                                          if_else(abs == 'JB' | abs == 'WJ', 'TC', 
                                                  if_else(abs == 'MB' | abs == 'BS' | abs == 'CS', 'SBB', 
                                                          if_else(abs == 'TP', 'TP', 'LS')
                                                  ) ) )))) %>% 
  rename(site = abs, 
         long = Longitude,
         lat = Latitude)


#start_date_location<- hydro_location_all




osm_sites <- read_osm(start_date_location %>% 
                       sf::st_as_sf(
                         coords = c('long', 'lat'),            
                         crs = 4326  ), 
                      ext=1.4)

tmap_mode('plot')
intro_map <- tm_basemap('Esri.WorldImagery')+
  tm_shape(osm_sites) +
  tm_rgb()+
  tm_shape(start_date_location %>% 
             sf::st_as_sf(
               coords = c('long', 'lat'),            
               crs = 4326  ),
           name = "region") +
  tm_symbols(size=1, 
             col = 'region',
             border.col = 'black') +
  tm_text("site", col = "black", size = 1, just = "top", ymod = 1.25)+
  tm_basemap(c("Esri.WorldImagery")) +
  tm_compass(size = 3, type = 'rose') +
  tm_scale_bar(text.size = 0.5) 



# SLR ---------------------------------------------------------------------

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

# hydro plots -------------------------------------------------------------


POR_hydro <- hydro_day %>% 
  mutate(date_number = paste(month(LABEL), day(LABEL), sep = '-')) %>% 
  group_by(area, date_number) %>% 
  dplyr::summarize(min_depth = min(depth, na.rm = T),
                   max_depth = max(depth, na.rm = T), 
                   min_sal = min(sal, na.rm = T),
                   max_sal = max(sal, na.rm = T),
                   min_temp = min(temp, na.rm = T),
                   max_temp = max(temp, na.rm = T),
                   D_Q25 = quantile(depth, probs = c(.25), na.rm = TRUE),
                   D_Q75 = quantile(depth, probs = c(.75), na.rm = TRUE),
                   S_Q25 = quantile(sal, probs = c(.25), na.rm = TRUE),
                   S_Q75 = quantile(sal, probs = c(.75), na.rm = TRUE),
                   T_Q25 = quantile(temp, probs = c(.25), na.rm = TRUE),
                   T_Q75 = quantile(temp, probs = c(.75), na.rm = TRUE),
                   .groups = 'drop') 



hydro_df <-  left_join(POR_hydro, 
          report_hy %>% 
            mutate(date_number = paste(month(LABEL), 
                                       day(LABEL), sep = '-')), 
          by = c('area','date_number') ) %>% 
  mutate(LABEL = as.Date(LABEL),
         area = factor(area, levels = c('7P','LMB','TC','LS','SBB','TP')))


# Depth  Plots -------------------------------------------------------------

depth_7P <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('7P')),
      aes(x = LABEL, 
          y= depth, 
          ymin = D_Q25, 
          ymax = D_Q75))

depth_LMB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LMB')),
  aes(x = LABEL, 
      y= depth, 
      ymin = D_Q25, 
      ymax = D_Q75))

depth_TC <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TC')),
  aes(x = LABEL, 
      y= depth, 
      ymin = D_Q25, 
      ymax = D_Q75))

depth_LS <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LS')),
  aes(x = LABEL, 
      y= depth, 
      ymin = D_Q25, 
      ymax = D_Q75))

depth_SBB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('SBB')),
  aes(x = LABEL, 
      y= depth, 
      ymin = D_Q25, 
      ymax = D_Q75))

depth_TP <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TP')),
  aes(x = LABEL, 
      y= depth, 
      ymin = D_Q25, 
      ymax = D_Q75))

depth_TP  +
  geom_ribbon(fill = "grey90")+ 
  geom_line(aes(y = min_depth), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  geom_line(aes(y = max_depth), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  geom_line(color = 'red',
            size = 1)+
  geom_line(aes(y = 13), 
            color = 'pink', 
            size = 1)+
  labs(y = 'Water Level (cm)', x = '') +
  theme_bw() +
  ylim(-15, 100)+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  #ggtitle("Audubon Florida Everglades Science Center - Depth")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")+
  facet_wrap(vars(area), ncol=2)


# Salinity Plots ----------------------------------------------------------

sal_7P <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('7P')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 

sal_LMB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LMB')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 

sal_TC <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TC')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 

sal_LS <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LS')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 

sal_SBB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('SBB')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 

sal_TP <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TP')),
  aes(x = LABEL, 
      y= sal, 
      ymin = S_Q25, 
      ymax = S_Q75)) 


sal_TP +
  geom_ribbon(fill = "grey90")+
  geom_line(color = 'red',
            linewidth = 1)+
  geom_line(aes(y = min_sal), 
            color = 'black', 
            linetype  = 2,
            linewidth = 0.5) +
  geom_line(aes(y = max_sal), 
            color = 'black', 
            linetype  = 2,
            linewidth = 0.5) +
  labs(y = 'Salinity (psu)', x = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  #ggtitle("Audubon Florida Everglades Science Center - Salinity")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(vars(area), ncol= 2)




ggarrange(
  ggplot(report_hy %>% 
           filter(area == 'LS') %>% 
           mutate(Season = if_else(Season == 1, 'Dry Season', 'Wet Season'))
         , aes(x = site, y = sal, fill= Season))+
    theme_bw()+
    geom_boxplot( notch = FALSE, outlier.colour="black", outlier.size=0.5)+
    scale_fill_manual(values=c("blue", "red"))+
    theme(legend.title = element_blank())+
    xlab("") + ylab("Salinity (psu)"),
  
  ggplot(report_hy%>% 
           filter(area == 'LS')%>% 
           mutate(Season = if_else(Season == 1, 'Dry Season', 'Wet Season'))
         , aes(x = site, y = depth, fill= Season))+
    theme_bw()+
    geom_boxplot(notch = FALSE, outlier.colour="black", outlier.size=0.5)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    scale_fill_manual(values=c("blue", "red"))+
    theme(legend.title = element_blank())+
    xlab("") + ylab("Water Level (cm)"),
  common.legend = TRUE)



# Temperature plots ----------------------------------------------------------


temp_7P <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('7P')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_LMB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LMB')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_TC <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TC')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_LS <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('LS')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_SBB <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('SBB')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_TP <- ggplot(#hydro_df, 
  subset(hydro_df, area %in% c('TP')),
  aes(x = LABEL, 
      y= temp, 
      ymin = T_Q25, 
      ymax = T_Q75)) 

temp_TP +
  geom_ribbon(fill = "grey90")+
  geom_line(color = 'red',
            linewidth = 1)+
  geom_line(aes(y = min_temp), 
            color = 'black', 
            linetype  = 2,
            linewidth = 0.5) +
  geom_line(aes(y = max_temp), 
            color = 'black', 
            linetype  = 2,
            linewidth = 0.5) +
  labs(y = expression('Temperature (°C)'), x = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  #ggtitle("Audubon Florida Everglades Science Center - Temperature")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(vars(area))


# SAV ---------------------------------------------------------------------

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

all_SAV<- bind_rows(SevenP_SAV, TR_SAV,EC_SAV,WJ_SAV,
                    JB_SAV,SB_SAV,HC_SAV,
                    MB_SAV,BS_SAV %>% 
                      filter(`Hydro. Year` != '96-97'),
                    CS_SAV,TP_SAV) %>% 
  #dplyr::select(-c(notes, `...28`)) %>% 
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
         area = factor(area, levels = c('7P',"TR","EC","WJ","JB","SB",
                                        "HC","MB","BS","CS", "TP")), 
         site = factor(site, 
                       levels = c('7P',
                         "TR1","TR2","TR3","TR4A","TR5","TR6", 
                                  "EC1","EC2","EC3",
                                  "WJ1","WJ2","JB1","JB2","JB3","JB4","JB5","JB6",
                                  "SB1","SB2","SB3",
                                  "HC1","HC1A","HC2","HC3","HC4A","HC5","HC6",
                                  "MB1","MB2","MB3","BS1","BS2","BS3","BS4","CS1",
                                  'TP1')),
         Month = factor(toupper(month.abb[month(Date)]),
                        levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                                   'DEC',"JAN","FEB","MAR",'APR',"MAY")),
         region = factor(region, levels = c('7P',"LMB","TC","LS","SBB", "TP")),
         Season = 
           if_else(month(Date) >= 6 & month(Date) <= 11, 
                   'Wet Season',
                   'Dry Season'),
         Season = factor(Season, levels = c('Wet Season', 'Dry Season'))) %>% 
  as.data.frame() %>% 
  filter(HY != '1995-96') %>% 
  mutate(Month = if_else(Month == 'DEC', 'NOV', Month))



#will need this if next years hydro year data has started to come in
#remove_HY <- which(all_SAV$HY == remove_report)
all_SAV <- all_SAV %>% 
  filter(HY != remove_HY) %>% 
  mutate(
    Month = factor(toupper(month.abb[month(Date)]),
                   levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                              'DEC',"JAN","FEB","MAR",'APR',"MAY"))
  )

SAV.now<- all_SAV %>% 
  filter(HY == this_report)%>% 
  mutate(
    Month = factor(toupper(month.abb[month(Date)]),
                   levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                              'DEC',"JAN","FEB","MAR",'APR',"MAY"))
  )
SAV.POR<- all_SAV %>% 
  filter(HY != this_report)%>% 
  mutate(
    Month = factor(toupper(month.abb[month(Date)]),
                   levels = c('JUN',"JUL",'AUG',"SEP",'OCT',"NOV",
                              'DEC',"JAN","FEB","MAR",'APR',"MAY"))
  )



# SAV Plots ---------------------------------------------------------------

region_plot <- 'TP'

sites_sav <- ggplot(SAV.now %>% 
                      filter(region == region_plot), 
                    aes(x=site, y=TOTAL)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV %\n Coverage") 

months_sav <- ggplot(SAV.now %>% 
                       filter(region == region_plot), 
                     aes(x=Month, y=TOTAL, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV %\n Coverage")   

seasons_sav <- ggplot(SAV.now %>% 
                        filter(region == region_plot), 
                      aes(x=Season, y=TOTAL, fill=area)) + 
  geom_boxplot(aes(fill=area, color =area)) +
  geom_boxplot(aes(fill=area ), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "right",
        legend.title = element_blank())+
  xlab("") + ylab("Total SAV %\n Coverage")   


ggarrange(sites_sav, ggarrange(months_sav, seasons_sav, 
                                            ncol=2, labels=c("       B", "       C"),  legend = "none"),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(months_sav, seasons_sav, ncol=2, labels=c("       A", "       B"),  legend = "none")


# sav exceedance ----------------------------------------------------------

# data work ---------------------------------------------------------------

SAV.now.wet<- SAV.now[SAV.now$Season == "Wet Season", ]
SAV.now.dry<- SAV.now[SAV.now$Season == "Dry Season", ]

# this year mean annual and seasonal TOTAL NUMBERS
#7p
total_this_year_ann_7P<- mean(SAV.now[SAV.now$site == "7P", ]$TOTAL)
total_this_year_wet_7P<- mean(SAV.now.wet[SAV.now.wet$site == "7P", ]$TOTAL)
total_this_year_dry_7P<- mean(SAV.now.dry[SAV.now.dry$site == "7P", ]$TOTAL)
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
#7P 
total.7P.ann <- ddply(all_SAV[all_SAV$site == "7P", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "7P", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.7P.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.7P.wet <- total.season.ex[c(wet.ex.xyz),]
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
#TP
total.TP.ann <- ddply(all_SAV[all_SAV$site == "TP1", ], .(HY, site), summarise, total = mean(TOTAL))
total.season.ex<- ddply(all_SAV[all_SAV$site == "TP1", ], .(HY, Season, site), summarise, total = mean(TOTAL))
dry.ex.xyz <- which(total.season.ex$Season == "Dry Season")
total.TP.dry <- total.season.ex[c(dry.ex.xyz),]
wet.ex.xyz <- which(total.season.ex$Season == "Wet Season")
total.TP.wet <- total.season.ex[c(wet.ex.xyz),]

ann.total.ex<- rbind(total.7P.ann, total.TR.ann, total.EC.ann,total.JB.ann, total.WJ.ann, 
                     total.SB.ann, total.HC.ann,total.MB.ann, total.BS.ann, total.CS.ann, 
                     total.TP.ann)
wet.total.ex<- rbind(total.7P.wet, total.TR.wet, total.EC.wet,total.JB.wet, total.WJ.wet, 
                     total.SB.wet, total.HC.wet,total.MB.wet, total.BS.wet, total.CS.wet,
                     total.TP.wet)
dry.total.ex<- rbind(total.7P.dry,total.TR.dry, total.TR.dry, total.EC.dry,total.JB.dry, total.WJ.dry, 
                     total.SB.dry, total.HC.dry,total.MB.dry, total.BS.dry, total.CS.dry,
                     total.TP.dry)



#PERCENT ON ex.Y-AXIS ####
y.ex.7P<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "7P", ]$HY)-1))
y.ex.7P.wet<-seq(from = 0, to = 1, by = 1/(length(wet.total.ex[wet.total.ex$site == "7P", ]$HY)-1))
y.ex.7P.dry<-seq(from = 0, to = 1, by = 1/(length(dry.total.ex[dry.total.ex$site == "7P", ]$HY)-1))
y.ex.TR<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "TR1", ]$HY)-1))
y.ex.EC<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "EC1", ]$HY)-1))
y.ex.WJ<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "WJ1", ]$HY)-1))
y.ex.JB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "JB1", ]$HY)-1))
y.ex.SB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "SB1", ]$HY)-1))
y.ex.HC<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "HC1A", ]$HY)-1))
y.ex.MB<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "MB1", ]$HY)-1))
y.ex.BS<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "BS1", ]$HY)-1))
y.ex.CS<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "CS1", ]$HY)-1))
y.ex.TP<-seq(from = 0, to = 1, by = 1/(length(ann.total.ex[ann.total.ex$site == "TP1", ]$HY)-1))


#create an ind by sorting total; largest to smallest for wet, dry and annual variable ####
#LMB total####
#7P
total.wet.ind.7P <-order(-total.7P.wet$total)
total.dry.ind.7P <-order(-total.7P.dry$total)
total.ann.ind.7P <-order(-total.7P.ann$total)
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

#TP
TP.total.wet.ind<-order(-total.TP.wet$total)
TP.total.dry.ind<-order(-total.TP.dry$total)
TP.total.ann.ind<-order(-total.TP.ann$total)


#7P
total.wet.df.7P<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.7P[-1]),What=rep("Wet Season", 
                                                                               (length(total.7P.ann$HY)-1)),
                           HY=as.factor(total.7P.wet$HY[total.wet.ind.7P]), 
                           total=total.7P.wet$total[total.wet.ind.7P] )
total.dry.df.7P<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.7P[-1]),What=rep("Dry Season", 
                                                                               (length(total.7P.ann$HY)-1)),
                           HY=as.factor(total.7P.dry$HY[total.dry.ind.7P]), 
                           total=total.7P.dry$total[total.dry.ind.7P] )
total.ann.df.7P<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.7P),What=rep("Annual", 
                                                                               length(total.7P.ann$HY)),
                           HY=as.factor(total.7P.ann$HY[total.ann.ind.7P]), 
                           total=total.7P.ann$total[total.ann.ind.7P] )
total.exceedance_df.7P<- data.frame(bind_rows(total.ann.df.7P, total.wet.df.7P, total.dry.df.7P) )
total.exceedance_df.7P$Percent<- factor(total.exceedance_df.7P$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.7P))
total.exceedance_df.7P$What<- as.factor(total.exceedance_df.7P$What)
this_year_7P.total_df <- as.data.frame(total.exceedance_df.7P[total.exceedance_df.7P$HY == this_report, ])

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
#TP
TP.total.wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TP),What=rep("Wet Season", 
                                                                               length(total.TP.wet$HY)),
                           HY=as.factor(total.TP.wet$HY[TP.total.wet.ind]), 
                           total=total.TP.wet$total[TP.total.wet.ind] )
TP.total.dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TP),What=rep("Dry Season", 
                                                                               length(total.TP.dry$HY)),
                           HY=as.factor(total.TP.dry$HY[TP.total.dry.ind]), 
                           total=total.TP.dry$total[TP.total.dry.ind] )
TP.total.ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex.TP),What=rep("Annual", 
                                                                               length(total.TP.ann$HY)),
                           HY=as.factor(total.TP.ann$HY[TP.total.ann.ind]), 
                           total=total.TP.ann$total[TP.total.ann.ind] )
TP.total.exceedance_df<- data.frame(bind_rows(TP.total.ann.df, TP.total.wet.df, TP.total.dry.df) )
TP.total.exceedance_df$Percent<- factor(TP.total.exceedance_df$Percent, 
                                        levels=label_percent(accuracy=1)(y.ex.TP))
TP.total.exceedance_df$What<- as.factor(TP.total.exceedance_df$What)
this_year_TP.total_df <- as.data.frame(TP.total.exceedance_df[TP.total.exceedance_df$HY == this_report, ])

#7P total####
#7P
ex.7P.total <- ggplot(total.exceedance_df.7P, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_7P.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_7P.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Total SAV %")

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

#TP
ex.TP.total <- ggplot(TP.total.exceedance_df, aes(x=Percent, y= total)) + geom_line(aes(group=What, colour= What))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_TP.total_df, aes(group=What, colour= What))+
  geom_label_repel(data=this_year_TP.total_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") +ylab("Total SAV %")

ex.7P.total

ggarrange(ex.TR.total, ex.EC.total,
          labels=c("TR", "EC"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(ex.WJ.total, ex.JB.total,
          labels=c("WJ", "JB"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(ex.SB.total, ex.HC.total,
          labels=c("SB", "HC"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)

ggarrange(ex.MB.total, ex.BS.total, ex.CS.total,
          labels=c("MB", "BS", "CS"), nrow= 3, vjust= 2, hjust = -0.5, common.legend = TRUE)

ex.TP.total


# EHV ---------------------------------------------------------------------

#not doing this right now

# fish --------------------------------------------------------------------

#download data from access (explainations in code)
fish_family<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/species list w order fmily.csv", header=T)
fish_salinity<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/species list w salinity classes.csv", header=T)


#PERCENT 
# ACCESS: export/analyze data > %catch  > all hydro_yr > all months > select sites > select nets > 
# all fish, combine size categories > year x month x site, numbers, raw, per net > save excel in folder 
fish_catch_percent<-read.csv("/Databases/tidy/Reports/ACOE/Fish all/percent_fish.csv", header=T, check.names=FALSE) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season')) %>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))


#AVAIBALE 
# ACCESS: export/analyze data > summarized/multivariate > all hydro_yr > all months > select sites > select nets > 
# all fish, use size constraint: SL <= 6.5cm > year x month x site x net type, numbers & biomass, area: per M2 > 
# save excel in folder > Summary Biomass = biomass_available, Summary Numbers = density_avaiable
fish_biomass_available<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/biomass_fish_available.csv", header = T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

fish_density_available<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/density_fish_available.csv", header = T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

#fish_biomass_available_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/biomass_fish_available_TP.csv", header = T)
#fish_density_available_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/density_fish_available_TP.csv", header = T)
# WEIGHTED  
# ACCESS: export/analyze data > summarized/multivariate > all hydro_yr > all months > select sites > select nets > 
# all fish, combine size categories, use size constraint SL <=13.5cm > year x month x site, stratification = replicates, 
# numbers and biomass > save excel in folder > Summary Biomass = biomass_fish, Summary Numbers = density_fish
fish_biomass_weighted<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/biomass_fish.csv", header=T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(month  >= 6 & month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(year), 
         Month = as.numeric(month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

fish_density_weighted<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/density_fish.csv", header=T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(month  >= 6 & month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(year), 
         Month = as.numeric(month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

#fish_biomass_weighted_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/biomass_fish_TP.csv", header=T)
#fish_density_weighted_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/density_fish_TP.csv", header=T)
#CICHLID
# SAME AS ABOVE but without size constraint SL. AVAIABLE = per net (9M^2). WEIGHTED = per meter. 

fish_biomass_fcich_weighted<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/weighted_biomass_for_cichlid.csv", header=T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(month  >= 6 & month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(year), 
         Month = as.numeric(month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

fish_density_fcich_weighted<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/weighted_density_for_cichlid.csv", header=T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(month  >= 6 & month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(year), 
         Month = as.numeric(month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))


#fish_biomass_fcich_weighted_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/weighted_biomass_for_cichlid_TP.csv", header=T)
#fish_density_fcich_weighted_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/weighted_density_for_cichlid_TP.csv", header=T) 

fish_biomass_fcich_available<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/available_biomass_for_cichlid.csv", header=T) %>% 
  filter(TSC_site_code != '7P' )%>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

fish_density_fcich_available<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/available_density_for_cichlid.csv", header=T) %>% 
  filter(TSC_site_code != '7P' ) %>% 
  mutate(sampledate = as.Date(sampledate, tryFormats = c("%m/%d/%Y"))) %>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

#fish_biomass_fcich_available_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/available_biomass_for_cichlid_TP.csv", header=T)
#fish_density_fcich_available_TP<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/available_density_for_cichlid_TP.csv", header=T) %>% 
#  mutate(sampledate = as.Date(sampledate, tryFormats = c("%m/%d/%Y"))) 


# done 
total_fish_species<- read.csv("/Databases/tidy/Reports/ACOE/Fish all/count_fish.csv", header= T)%>% 
  mutate(Season = if_else(Month  >= 6 & Month  <= 11, 
                          'Wet Season', 'Dry Season'))%>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         HY = if_else(Month >= 6, paste(Year, sub('..', '', Year+1), sep = '-'), 
                      paste(Year-1, sub('..', '', Year), sep = '-')))

fish_df<- left_join(fish_family, fish_salinity, "SPP...")



#this_year = "2022"


total_fish_species$TSC_site_code<- factor(total_fish_species$TSC_site_code, 
                                          levels = c("TR","EC","WJ","JB","SB","HC","MB","BS","CS",'TP','7P')) 
total_fish_species <- total_fish_species %>% 
  filter(TSC_site_code != '7P')

sample_total_hydro_yr<- ddply(total_fish_species, .(HY,hydro_yr, TSC_site_code), summarise, total = round(sum(TOTAL, na.rm = T)))


total_fish_hydro_yr<- pivot_wider(sample_total_hydro_yr, names_from = TSC_site_code, values_from = total) 
#names(total_fish_hydro_yr)[names(total_fish_hydro_yr) == "hydro_yr"] <- "Hydro Year"



asian_eel <- ddply(total_fish_species, .(HY, TSC_site_code), summarise, total = round(sum(Monopterus.albus, na.rm = T)))


total_ex <- total_fish_hydro_yr %>% 
  filter(hydro_yr >= 2005 ) %>% 
  #dplyr::select(-Total) %>% 
  #dplyr::rename(HY = `Hydro Year`) %>% 
  #mutate_at(c(2:10), as.numeric) %>% 
  mutate(LMB = as.numeric(TR)+as.numeric(EC), 
         TC = as.numeric(WJ)+as.numeric(JB), 
         LS = as.numeric(SB)+as.numeric(HC), 
         SBB = as.numeric(MB)+as.numeric(BS)+as.numeric(CS))


total_ex_region <- total_ex %>% 
  dplyr::select(HY, LMB) %>% 
  arrange(desc(LMB)) %>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( .)-1))), 
         region = 'LMB') %>% 
  dplyr::rename(total = LMB) %>% 
  rbind(
    total_ex %>% 
      dplyr::select(HY, TC) %>% 
      arrange(desc(TC)) %>% 
      mutate(Percent = 
               c(seq(0,1, 1/(nrow( .)-1))), 
             region = 'TC') %>% 
      dplyr::rename(total = TC)
  ) %>% 
  rbind(
    total_ex %>% 
      dplyr::select(HY, LS) %>%
      arrange(desc(LS)) %>% 
      mutate(Percent = 
               c(seq(0,1, 1/(nrow( .)-1))), 
             region = 'LS') %>% 
      dplyr::rename(total = LS)
  ) %>% 
  rbind(
    total_ex %>% 
      dplyr::select(HY, SBB) %>%
      arrange(desc(SBB)) %>% 
      mutate(Percent = 
               c(seq(0,1, 1/(nrow( .)-1))), 
             region = 'SBB') %>% 
      dplyr::rename(total = SBB)
  ) %>% 
  mutate(region = factor(region, c('LMB', 'TC', 'LS', 'SBB')))


# total fish exceedance  --------------------------------------------------


total_ex_region %>% 
  #filter(region == 'LMB') %>% 
  ggplot( aes(x=Percent, y= total)) + 
  geom_line(aes(group=region, colour= region))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ 
  geom_point(shape = 21,aes(color=region))+
  geom_point(size = 2.5, 
             data = total_ex_region %>% 
               #filter(region == 'LMB') %>% 
               filter(HY == this_report),
             aes(group=region, colour= region))+
  geom_label_repel(data = total_ex_region %>% 
                     #filter(region == 'LMB') %>% 
                     filter(HY == this_report),
                   aes(label=HY, group=region, colour= region),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(total_ex_region %>% 
        filter(region == 'LMB') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = 'Prey Fish Totals'  )+
  labs(title = "Prey Fish Totals",
       subtitle = "HY: 2005-06 - 2022-23")


# fish sal  ---------------------------------------------------------------


fish <- total_fish_species %>% 
  pivot_longer(Cyprinodon.variegatus:Lepisosteus.platyrhincus...6.5.cm.) %>% 
  dplyr::select(-c(watershed, Inst.Temp, Inst.Sal, Inst.Depth, Inst.Max.Depth, Inst.Min.Depth,
                   Inst.8AM.Depth, temp.C, sal.ppt, dep.site., X..of.nets)) %>% 
  rename(sci_name = name,
         site = TSC_site_code) %>% 
  mutate(sci_name = sub("\\.", " ", sci_name),
         sci_name = sub('..6.5.cm.', '', sci_name))

salinty <- read.csv('P:/Databases/tidy/Reports/ACOE/SSR/new_sal_fish_list2.csv') %>% 
  mutate(sal_class = if_else(is.na(sal_class_new), sal_class_old, sal_class_new)) %>% 
  dplyr::select(-c(sal_class_new,sal_class_old, X, SPP...)) %>% 
  filter(!grepl('BIG', code))


fish.df <- left_join(fish , salinty,
                     by = join_by(sci_name)) %>% 
 # filter(sci_name != 'unknown') %>% 
  mutate(sal_class = factor(sal_class, 
                            levels = c('freshwater','oligohaline','mesohaline','polyhaline','euhaline')),
         site = factor(site, 
                       levels = c('TR','EC','WJ','JB','SB','HC','MB','BS','CS','TP'))) %>% 
  drop_na(sal_class) %>% 
  mutate(region = if_else(site == 'TR' | site == 'EC', 'LMB', 
                          if_else(site == 'WJ' | site == 'JB', 'TC', 
                                  if_else(site == 'SB' | site == 'HC', 'LS', 
                                          if_else(site == 'MB'| site == 'BS' | site == 'CS', 'SBB', 'TP')))))



left_join(
  fish.df %>% 
    #filter(Year > 2012) %>% 
  group_by(region, HY,  sal_class) %>% 
  summarise(n_class_value = sum(value, na.rm = TRUE)),
  fish.df %>% 
  group_by(region, HY) %>% 
  summarise(total_fish_class_value = sum(value, na.rm = TRUE))) %>% 
  mutate(Pct = n_class_value / total_fish_class_value,
         hydro.yr = factor(HY, 
                           levels = rev(c("1990-91", "1991-92", "1992-93", "1993-94", "1994-95", "1995-96", "1996-97",
                                          "1997-98", "1998-99", "1999-00", "2000-01", "2001-02",
                                          "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
                                          "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
                                          "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", 
                                          "2020-21", "2021-22", "2022-23", "2023-24"))), 
         sal_class= factor(sal_class, 
                           levels = rev(c('freshwater','oligohaline','mesohaline','polyhaline','euhaline')))) %>% 
filter(region == 'TP') %>% 
ggplot(aes(forcats::fct_rev(HY), (Pct), fill = sal_class)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = percent, expand = c(0, 0))+
  #facet_wrap(~site) + 
  xlab('') + ylab('')+
  geom_hline(aes(yintercept=0.05),col = 'red',  size = 1)+ # 5% intercept 
  coord_flip()+
  guides(fill=guide_legend(title=""))  + 
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_wrap(~region)+
  scale_fill_manual("legend", 
                    values = c("freshwater" = "green", 
                               "oligohaline" = "darkgreen", 
                               "mesohaline" = "lightblue", 
                               "polyhaline" = "orange", 
                               "euhaline" = "red"),
                    breaks=c('freshwater','oligohaline','mesohaline','polyhaline','euhaline'))



# available density / biomass ---------------------------------------------

avaiable_biomass_density_df <- 
  left_join(
    left_join(
      #biomass database work
      fish_biomass_available %>% 
        mutate(total_biomass_fish = TOTAL,
               cichlid_biomass = Cichlasoma.urophthalmus + Cichlosoma.urophthalmus..6.5.cm.) %>% 
        dplyr::select(c(sample_month, TSC_site_code, sampledate, Season, net.type, HY, total_biomass_fish,cichlid_biomass)),
      #density database work
      fish_density_available%>% 
        mutate(total_density_fish = TOTAL,
               cichlid_density = Cichlasoma.urophthalmus + Cichlosoma.urophthalmus..6.5.cm.) %>% 
        dplyr::select(c(sample_month, TSC_site_code, sampledate, net.type, HY, total_density_fish, cichlid_density))
    ) %>% 
      #database deep nets
      filter(net.type == 'deep') %>% 
      dplyr::rename(total_biomass_fish_creek = total_biomass_fish, 
                    total_density_fish_creek = total_density_fish,
                    total_biomass_cich_creek = cichlid_biomass, 
                    total_density_cich_creek = cichlid_density),
    left_join(
      #biomass database work
      fish_biomass_available %>% 
        mutate(total_biomass_fish = TOTAL,
               cichlid_biomass = Cichlasoma.urophthalmus + Cichlosoma.urophthalmus..6.5.cm.) %>% 
        dplyr::select(c(sample_month, TSC_site_code, sampledate, net.type, HY, total_biomass_fish,cichlid_biomass)),
      #density database work
      fish_density_available%>% 
        mutate(total_density_fish = TOTAL,
               cichlid_density = Cichlasoma.urophthalmus + Cichlosoma.urophthalmus..6.5.cm.) %>% 
        dplyr::select(c(sample_month, TSC_site_code, sampledate, net.type, HY, total_density_fish,cichlid_density))
    ) %>% 
      #database flats nets
      filter(net.type == 'flats') %>% 
      dplyr::rename(total_biomass_fish_flats = total_biomass_fish, 
                    total_density_fish_flats = total_density_fish,
                    total_biomass_cich_flats = cichlid_biomass, 
                    total_density_cich_flats = cichlid_density),
    by = c("sample_month", "TSC_site_code", "sampledate", "HY")) %>% 
  dplyr::select(-c('net.type.x','net.type.y')) %>% 
  #filter between flats and deep nets to find avaiablility 
  mutate(
    available_density = case_when(
      total_density_fish_creek > total_density_fish_flats ~ total_density_fish_creek,
      total_density_fish_creek < total_density_fish_flats ~ total_density_fish_flats, 
      is.na(total_density_fish_creek) ~ total_density_fish_flats,
      is.na(total_density_fish_flats) ~ total_density_fish_creek),
    available_biomass = case_when(
      total_biomass_fish_creek > total_biomass_fish_flats ~ total_biomass_fish_creek,
      total_biomass_fish_creek < total_biomass_fish_flats ~ total_biomass_fish_flats, 
      is.na(total_biomass_fish_creek) ~ total_biomass_fish_flats,
      is.na(total_biomass_fish_flats) ~ total_biomass_fish_creek),
    available_cich_density = case_when(
      total_density_cich_creek > total_density_cich_flats ~ total_density_cich_creek,
      total_density_cich_creek < total_density_cich_flats ~ total_density_cich_flats, 
      is.na(total_density_cich_creek) ~ total_density_cich_flats,
      is.na(total_density_cich_flats) ~ total_density_cich_creek),
    available_cich_biomass = case_when(
      total_biomass_cich_creek > total_biomass_cich_flats ~ total_biomass_cich_creek,
      total_biomass_cich_creek < total_biomass_cich_flats ~ total_biomass_cich_flats, 
      is.na(total_biomass_cich_creek) ~ total_biomass_cich_flats,
      is.na(total_biomass_cich_flats) ~ total_biomass_cich_creek)) %>% 
  #if no cichlids found change NA to 0
  mutate(available_cich_density = if_else(is.na(available_cich_density), 
                                          0, available_cich_density),
         available_cich_biomass = if_else(is.na(available_cich_biomass), 
                                          0, available_cich_biomass)) %>% 
  mutate(region = case_when((TSC_site_code == 'TR') ~ 'LMB',
                            (TSC_site_code == 'EC') ~ 'LMB',
                            (TSC_site_code == 'JB') ~ 'TC' ,
                            (TSC_site_code == 'WJ') ~ 'TC' ,
                            (TSC_site_code == 'HC') ~ 'LS' ,
                            (TSC_site_code == 'SB') ~ 'LS' , 
                            (TSC_site_code == 'MB') ~ 'SBB' , 
                            (TSC_site_code == 'BS') ~ 'SBB' , 
                            (TSC_site_code == 'CS') ~ 'SBB',
                            (TSC_site_code == 'TP') ~ 'TP')) %>% 
  mutate(region = factor(region, 
                         levels = c('LMB', 'TC', 'LS', 'SBB', 'TP')),
         TSC_site_code =factor(TSC_site_code, 
                               levels = c('TR','EC','WJ','JB','SB',
                                          'HC','MB','BS','CS', 'TP')) )



avaiable_biomass <-
  avaiable_biomass_density_df %>% 
  dplyr::select(-c(total_density_fish_creek,
                   total_density_fish_flats,
                   available_density,
                   total_density_cich_creek,
                   total_density_cich_flats,
                   available_cich_density))

avaiable_density <-
  avaiable_biomass_density_df %>% 
  dplyr::select(-c(total_biomass_fish_creek,
                   total_biomass_fish_flats,
                   available_biomass,
                   total_biomass_cich_creek,
                   total_biomass_cich_flats,
                   available_cich_biomass))

#LMB
bio_dens_LMB_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "LMB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "LMB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "LMB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "LMB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_LMB_available<-
  bio_dens_LMB_available %>% 
  filter(HY == this_report)

LMB_available_ex<-
  ggplot(bio_dens_LMB_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_LMB_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_LMB_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_LMB_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Fish~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Fish~Biomass~(g/m^2)  )))+
  ggtitle("LMB")

#LMB cich
bio_dens_cich_LMB_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "LMB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "LMB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "LMB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "LMB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_cich_LMB_available<-
  bio_dens_cich_LMB_available %>% 
  filter(HY == this_report)

LMB_cich_available_ex<-
  ggplot(bio_dens_cich_LMB_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_cich_LMB_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_cich_LMB_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_cich_LMB_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Cichlid~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Cichlid~Biomass~(g/m^2)  )))+
  ggtitle("LMB")


#TC
bio_dens_TC_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "TC") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "TC") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "TC") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "TC") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_TC_available<-
  bio_dens_TC_available %>% 
  filter(HY == this_report)

TC_available_ex<-
  ggplot(bio_dens_TC_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_TC_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_TC_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_TC_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Fish~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Fish~Biomass~(g/m^2)  )))+
  ggtitle("TC")

#TC cich
bio_dens_cich_TC_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "TC") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "TC") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "TC") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "TC") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_cich_TC_available<-
  bio_dens_cich_TC_available %>% 
  filter(HY == this_report)

TC_cich_available_ex<-
  ggplot(bio_dens_cich_TC_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_cich_TC_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_cich_TC_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_cich_TC_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Cichlid~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Cichlid~Biomass~(g/m^2)  )))+
  ggtitle("TC")

#LS
bio_dens_LS_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "LS") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "LS") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "LS") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "LS") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_LS_available<-
  bio_dens_LS_available %>% 
  filter(HY == this_report)

LS_available_ex<-
  ggplot(bio_dens_LS_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_LS_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_LS_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_LS_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Fish~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Fish~Biomass~(g/m^2)  )))+
  ggtitle("LS")

#LS cich
bio_dens_cich_LS_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "LS") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "LS") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "LS") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "LS") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_cich_LS_available<-
  bio_dens_cich_LS_available %>% 
  filter(HY == this_report)

LS_cich_available_ex<-
  ggplot(bio_dens_cich_LS_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_cich_LS_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_cich_LS_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_cich_LS_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Cichlid~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Cichlid~Biomass~(g/m^2)  )))+
  ggtitle("LS")
#SBB
bio_dens_SBB_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "SBB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "SBB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "SBB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "SBB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_SBB_available<-
  bio_dens_SBB_available %>% 
  filter(HY == this_report)

SBB_available_ex<-
  ggplot(bio_dens_SBB_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_SBB_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_SBB_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_SBB_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Fish~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Fish~Biomass~(g/m^2)  )))+
  ggtitle("SBB")

#SBB cich
bio_dens_cich_SBB_available <-
  rbind(
    avaiable_density %>% 
      filter(region == "SBB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
      mutate(Season = "Annual Density", .before = region),
    avaiable_biomass %>% 
      filter(region == "SBB") %>% 
      ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE))%>% 
      mutate(Season = "Annual Biomass", .before = region)) %>% 
  arrange(Season, desc(avaiable))%>% 
  mutate(Percent = 
           c(seq(0,1, 1/(nrow( avaiable_density %>% 
                                 filter(region == "SBB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_density, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Density", .before = region))-1)),
             seq(0,1, 1/(nrow( avaiable_biomass %>% 
                                 filter(region == "SBB") %>% 
                                 ddply(.(HY,region), summarise, avaiable = mean(available_cich_biomass, na.rm=TRUE)) %>% 
                                 mutate(Season = "Annual Biomass", .before = region))-1))))

this_report_bio_dens_cich_SBB_available<-
  bio_dens_cich_SBB_available %>% 
  filter(HY == this_report)

SBB_cich_available_ex<-
  ggplot(bio_dens_cich_SBB_available, aes(x=Percent, y= avaiable)) + geom_line(aes(group=Season, colour= Season))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.87,0.855),
        legend.title = element_blank())+ geom_point(shape = 21,aes(color=Season))+
  geom_point(data=this_report_bio_dens_cich_SBB_available, aes(group=Season, colour= Season))+
  geom_label_repel(data=this_report_bio_dens_cich_SBB_available, aes(label=HY, group=Season, colour= Season),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+ 
  scale_x_continuous(breaks = round(
    c(bio_dens_cich_SBB_available %>% 
        filter(Season == 'Annual Biomass') %>% 
        pull(Percent)),1),
    labels = scales::label_percent())+
  #change box.padding and point padding if boxes are in the wrong place
  labs(x = "Percent Time Equaled or Exceeded", y = expression(atop(Available~Cichlid~density~(Nr~of~fish/m^2), 
                                                                   Avaiable~Cichlid~Biomass~(g/m^2)  )))+
  ggtitle("SBB")



ggarrange(LMB_available_ex, TC_available_ex,
          LS_available_ex,SBB_available_ex,
          ncol = 2, 
          nrow = 2,
          common.legend =T )




# flow --------------------------------------------------------------------



S197.key  <- "91435"


library("SFNRC")


s197.flow <- getHydro(dbkey = S197.key, startDate = "20190101")%>% 
  dplyr::select(-site)


beginDate <- "2022-06-01"
finalDate <- "2023-05-31"


s197_now<- s197.flow[s197.flow$date >= beginDate & s197.flow$date <= finalDate,]


S197_plot<-   ggplot(s197_now %>% 
                       mutate(date = as.Date(date)) %>% 
                       drop_na(date)  %>% 
                       group_by(date = lubridate::floor_date(date, "day")) , 
                     aes(x=date, y=value)) +
  geom_bar( stat="identity")+
  scale_y_continuous(name = "Flow (cf/s)")+
  ggtitle("S197 Flow")+
  scale_x_date(date_breaks = "1 month")+
  theme_bw()+  
  theme(axis.title.y = element_text(color = "black", size=13),
        axis.text.x = element_text(angle = 45, hjust = 1))


# all plots? ---------------------------------------------------------------


