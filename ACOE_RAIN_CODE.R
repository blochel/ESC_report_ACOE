#RAIN
#Annual, wet/dry season - Exceedance curves 				
#Monthly with POR - Bargraph						
#Cumulative with POR rain - bargrapth + line 				

library('tidyverse')
library("dplyr")
library("plyr")
library("lubridate")
library("zoo")
library("ggplot2")
library("scales")
library("reshape2")
library("gridExtra")
library("ggrepel")
library('readxl')
library('ggpubr')


rain<- read_excel('/Hydrology/Rainfall and SDCS flow/Rainfall all data_NOAA & ENP.xlsx') %>% 
  mutate(Season = if_else(Season == 1, 
                          'Dry Season', 
                          'Wet Season'),
         MonthDay = paste(month.abb[MONTH], 
                          DAY, sep="-" ))



#create date column in "rain"
#rain$date<- as.Date(rain$LABEL, "%d-%b-%y")   

#choose what hydro year it is - change green text to the HY of interest. 
this_report<- "2022-23" 
remove_report <- "2023-24"



# remove HY ---------------------------------------------------------------

rain <- rain %>% filter(HY != remove_report)
        



# por and this year -------------------------------------------------------

                                                        
hy.now<- which(rain$HY == this_report)
hy.POR<- which(rain$HY != this_report)

#different was of working with data frame
rain.POR<- rain[-c(hy.now),]
rain.now<- rain[c(hy.now),]




### Annual, wet/dry season - Exceedance curves  ####

# this years seasonal rainfall

rain.now.dry <- which(rain.now$Season == "Dry Season")
rain.now.dry <- rain.now[c(rain.now.dry),]
rain.now.wet <- which(rain.now$Season == "Wet Season")
rain.now.wet <- rain.now[c(rain.now.wet),]

#mean this year and season rainfall
this_year_ann<- sum(rain.now$Mean)
this_year_wet<- sum(rain.now.wet$Mean)
this_year_dry<- sum(rain.now.dry$Mean)


#mean all other years and seasons rainfall
ann.ex<- ddply(rain, .(HY), summarise, total_rain = sum(Mean))
season.ex<- ddply(rain, .(HY, Season), summarise, total_rain = sum(Mean))
dry.ex <- which(season.ex$Season == "Dry Season")
dry.ex <- season.ex[c(dry.ex),]
wet.ex <- which(season.ex$Season == "Wet Season")
wet.ex <- season.ex[c(wet.ex),]

# % for x-axis bargraph
y.ex<-seq(from = 0, to = 1, by = 1/(length(ann.ex$HY)-1))

#create an ind by sorting rain; largest to smallest for wet, dry and annual rainfall 
wet.ind<-order(-wet.ex$total_rain)
dry.ind<-order(-dry.ex$total_rain)
ann.ind<-order(-ann.ex$total_rain)

#create df for ggplot
Wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),What=rep("Wet Season", length(ann.ex$HY)),
                  HY=as.factor(wet.ex$HY[wet.ind]), 
                  Rain=wet.ex$total_rain[wet.ind] )
Dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),What=rep("Dry Season", length(ann.ex$HY)),
                  HY=as.factor(dry.ex$HY[dry.ind]), 
                  Rain=dry.ex$total_rain[dry.ind] )
Ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),What=rep("Annual", length(ann.ex$HY)),
                  HY=as.factor(ann.ex$HY[ann.ind]), 
                  Rain=ann.ex$total_rain[ann.ind] )
#bind all the individual df together, and change to factors
exceedance_df<- data.frame(bind_rows(Ann.df, Wet.df, Dry.df) )
exceedance_df$Percent<- factor(exceedance_df$Percent, levels=label_percent(accuracy=1)(y.ex))
exceedance_df$What<- as.factor(exceedance_df$What)

 

#highlight this years rain as points

this_year_df <- as.data.frame(exceedance_df[exceedance_df$HY == this_report, ])



#creating the exceedance graph
p1 <- ggplot(exceedance_df, aes(x=Percent, y= Rain)) + geom_line(aes(group=What, colour= What))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
            legend.title = element_blank(),
            axis.text = element_text(size=12),
            text = element_text(size=13))+ 
            geom_point(shape = 21,aes(color=What))+
            geom_point(data=this_year_df, aes(group=What, colour= What))+
            geom_label_repel(data=this_year_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
            box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+                  #change box.padding and point padding if boxes are in the wrong place
            xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")

p1



### Monthly with POR - Bargraph                 ####
# the bargraphs for POR can be changed into a 25-75 quantile graph (later)

# rain.now
zdf<- ddply(rain.now, .(MONTH), summarise, total_rain = sum(Mean))
vdf<- unique(rain.now$MONTH)
rain.now.month<- zdf[vdf,]
hy_now<- c("NOW", "NOW","NOW","NOW","NOW","NOW","NOW","NOW","NOW","NOW","NOW","NOW")
rain.now.month$what<-hy_now
colnames(rain.now.month)<-c("Month","Total_Rain", "Period")

# rain.POR
z1df<- ddply(rain.POR, .(HY, MONTH), summarise, total_rain = sum(Mean))
z2df<- ddply(z1df, .(MONTH), summarise, total_rain = mean(total_rain))
v1df<- unique(rain.now$MONTH)
rain.POR.month<- z2df[v1df,]
hy_POR<- c("POR","POR","POR","POR","POR","POR","POR","POR","POR","POR","POR","POR")
rain.POR.month$what<-hy_POR
colnames(rain.POR.month)<-c("Month", "Total_Rain", "Period")

#create one dataframe
month.data<- bind_rows(rain.now.month, rain.POR.month)
month.data<- month.data[,-4]
month.data$Month<-month.abb[month.data$Month]
month.data$Month<-factor(month.data$Month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May"))



#the graph 

#look up how to make bargraphs in ggplot2
p2 <- ggplot(month.data, aes(x=Month, y=Total_Rain, fill=Period))                                  #what data to use
p2 <- p2 + geom_bar(stat="identity", color="black", width=0.6, position=position_dodge())          #set bargraphs next to eachother
p2 <- p2 + ylim(0,max(month.data$Total_Rain)) + xlab("") + ylab("Rainfall (in.)")                                          #change y-axis max(month.data$Total_Rain)
p2 <- p2 + theme_bw()
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1),                                #change x-axis text angle
                 legend.position = "top",                                                          #legend position
                 axis.text = element_text(size=12),
                 text = element_text(size=13))                                                  
p2 <- p2 + scale_fill_manual(values=c("steelblue", "red1"),                                             #change colour
                  name="Rainfall (in)",                                                            #change legend title
                  breaks=c("NOW", "POR"),                                                          #whata data
                  labels=c(this_report, "POR mean"))                                               #change legend lables
p2









### Cumulative with POR rain - bargrapth + line ####

#calculate the average daily rain fall 
first.POR.df<- ddply(rain.POR, .(MonthDay), summarise, daily_mean_POR = mean(Mean))

#create a data frame so dates care sorted after HY structure
xdf<-data.frame(matrix( data = 1:nrow(first.POR.df), ncol = 2, nrow = nrow(first.POR.df)))
ydf<- unique(rain.POR$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name

#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
daily.rain.POR<- merge(xydf,first.POR.df, by = "MonthDay")
daily.rain.POR<- daily.rain.POR[order(daily.rain.POR$ind),]    

#Cumulative rain for this HY
cum.rain.now<- cumsum(rain$Mean[hy.now])
#Cumulative rain for POR
cum.rain.POR<- cumsum(daily.rain.POR$daily_mean_POR)
#total annual rainfall
total_annual_rain<- ddply(rain, .(HY), summarise, total_rain = sum(Mean))


#barplot for cum rain
cum.rain.POR<- as.data.frame(cum.rain.POR)
cum.rain.POR<-cum.rain.POR[1:length(cum.rain.now),]
cum.rain.now<-as.data.frame(cum.rain.now)
days_year<- nrow(cum.rain.now)

#was in wrinting date
date_x <-as.data.frame(daily.rain.POR$MonthDay)
date_grp<-as.character(date_x[1:days_year,])
x_grp<-format(date_grp, format="%d-%B")
x_grp<-as.Date(x_grp, "%B")

#data frames for the grapth
df.now <- data.frame(grp=1:days_year,val=cum.rain.now)          #column name now data =cum.rain.now
df.POR <- data.frame(grp=1:days_year,val=cum.rain.POR)          #column name POR data = val

#maybe test something like this, need to get date_gpr to remain a set order!
df.now <- cbind(data.frame(grp=1:days_year,val=cum.rain.now), date_grp) 
df.now$date_grp <- factor(df.now$date_grp, levels = df.now[,3])

df.now$date <- as.Date(df.now$date_grp, format= "%b-%d")
df.now$date <- format(df.now$date, format="%b-%d")

#add years to df, doesnt matter what years, just to keep the order when making the graph
year <- rep(c(2001,2002), times= (c(214,151)))
df.now <- cbind(df.now,year)
df.now$final_date <- as.Date(with(df.now, paste(year, date ,sep="-")), "%Y-%b-%d")
df.POR$final_date<-df.now$final_date
cum_rain_for_graph<- cbind(df.POR,df.now)

#the grapth-                      
p3 <- ggplot(data = cum_rain_for_graph[,1:6], aes(x=final_date , y=cum.rain.now)) 
p3 <- p3 + geom_bar( aes(x=final_date , y=cum.rain.now), stat="identity", width =1, colour="steelblue", fill="steelblue") 
p3 <- p3 + geom_line( aes(x=final_date , y=val), size =1, colour="red")
p3 <- p3 + xlab("") + ylab("Rainfall (in.)") 
p3 <- p3 + scale_x_date(date_breaks = "1 month",labels = date_format("%b"), limits = c(min(cum_rain_for_graph$final_date), max(cum_rain_for_graph$final_date)))  
p3 <- p3 + guides(col = guide_legend(ncol = 2))
p3 <- p3 + scale_y_continuous(breaks = c(seq(0, max(cum_rain_for_graph$cum.rain.now+5), 10)), 
                              limits = c(0,max(cum_rain_for_graph$cum.rain.now)+5))
p3 <- p3 + theme_bw()
p3 <- p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.text = element_text(size=12),
                 text = element_text(size=13))
p3


#need to add legend... maybe there is a way to create a seperate legend box and then fill it in?
#p3 <- p3 + theme(legend.position = c(0, 1),legend.justification = c(0, 1))+  scale_color_manual(values = c("steelblue","red")) 



max(cum_rain_for_graph$val)

###if needed add all graphs on one page####






#some additional information
rain.max.this.year<- max(rain.now$Mean)
rain.max.ind<- which(rain.now$Mean==rain.max.this.year)
date.rain.max.this.year<- rain.now$LABEL[rain.max.ind]
rain.max.POR<- max(rain.POR$Mean)
rain.max.POR.ind<- which(rain.POR$Mean==rain.max.POR)
date.rain.max.POR<- rain.POR$LABEL[rain.max.POR.ind]



#how this year compares to other years on POR (for the exceedance graph)
this_year_df
#what and when was the maximum rainfall for this year
rain.max.this.year
date.rain.max.this.year
#what and when was the maximum rainfall for POR
rain.max.POR
date.rain.max.POR
#all tables on one page
grid.arrange(p1,grid.arrange(p2,p3, ncol=2), nrow = 2)

ggarrange(p1, ggarrange(p2, p3, ncol=2, labels=c("         B", "       C"),  common.legend = TRUE),
          labels=c("       A"), nrow= 2, vjust= 2, hjust = -0.5)








# exceedance expatiation  ------------------------------------------------
D_ex <- exceedance_df %>% 
  filter(What == 'Annual') %>% 
ggplot( aes(x=Percent, y= Rain, group = 1)) + 
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        text = element_text(size=13))+ 
  geom_point(shape = 21,aes(colour= HY, fill = HY),show.legend = FALSE)+
  geom_point(data=this_year_df%>% 
               filter(What == 'Annual'), 
             aes(group=What, colour= What),
             size = 3, 
             show.legend = FALSE)+
  geom_point(data=exceedance_df%>% 
               filter(What == 'Annual')%>% 
               filter(HY == '1996-97'), 
             aes(colour= 'green'),
             size = 3, 
             show.legend = FALSE)+
  geom_label_repel(data=this_year_df%>% 
                     filter(What == 'Annual'), 
                   aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+
  geom_label_repel(data=exceedance_df%>% 
                     filter(What == 'Annual')%>% 
                     filter(HY == '1996-97'), 
                   aes(label='median'),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+                  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")+
  #arrow down
  geom_segment(x = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               y = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain), 
               xend =  exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               yend = 42,
               arrow = arrow(length = unit(0.03, "npc"), ends = "last"))+
  #arrow horizontal 
  geom_segment(x = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               y = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain), 
               xend =  '0%', 
               yend = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain),
               arrow = arrow(length = unit(0.03, "npc"), ends = "last"), 
               color = 'green') +
  geom_segment(x = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               y = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain), 
               xend =  '100%', 
               yend = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain),
               arrow = arrow(length = unit(0.03, "npc"), ends = "last"), 
               color = 'red')


C_ex <- exceedance_df %>% 
  filter(What == 'Annual') %>% 
  ggplot( aes(x=Percent, y= Rain, group = 1)) + 
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        text = element_text(size=13))+ 
  geom_point(shape = 21,aes(colour= HY, fill = HY),show.legend = FALSE)+
  geom_point(data=this_year_df%>% 
               filter(What == 'Annual'), 
             aes(group=What, colour= What),
             size = 3, 
             show.legend = FALSE)+
  geom_point(data=exceedance_df%>% 
               filter(What == 'Annual')%>% 
               filter(HY == '1996-97'), 
             aes(colour= 'green'),
             size = 3, 
             show.legend = FALSE)+
  geom_label_repel(data=this_year_df%>% 
                     filter(What == 'Annual'), 
                   aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+
  geom_label_repel(data=exceedance_df%>% 
                     filter(What == 'Annual')%>% 
                     filter(HY == '1996-97'), 
                   aes(label='median'),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+                  #change box.padding and point padding if boxes are in the wrong place
  xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")+
  #arrow down
  geom_segment(x = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               y = exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Rain), 
               xend =  exceedance_df%>% 
                 filter(What == 'Annual')%>% 
                 filter(HY == '1996-97') %>% pull(Percent), 
               yend = 42,
               arrow = arrow(length = unit(0.03, "npc"), ends = "last"))

#filter after percent == 50 instead of HY


E_ex <- exceedance_df %>% 
  filter(What == 'Annual') %>% 
  ggplot( aes(x=Percent, y= Rain, group = 1)) + 
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        text = element_text(size=13))+ 
  geom_point(shape = 21,aes(colour= HY, fill = HY),show.legend = FALSE)+
  geom_point(data=this_year_df%>% 
               filter(What == 'Annual'), 
             aes(group=What, colour= What),
             size = 3, 
             show.legend = FALSE)+
   geom_label_repel(data=this_year_df%>% 
                     filter(What == 'Annual'), 
                   aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+
  xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")


F_ex <- exceedance_df %>% 
  filter(What == 'Annual') %>% 
  ggplot( aes(x=Percent, y= Rain, group = 1)) + 
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        text = element_text(size=13))+ 
  geom_point(shape = 21,aes(colour= HY, fill = HY),show.legend = FALSE)+
  geom_point(data=this_year_df%>% 
               filter(What == 'Annual'), 
             aes(group=What, colour= What),
             size = 3, 
             show.legend = FALSE)+
  geom_label_repel(data=this_year_df%>% 
                     filter(What == 'Annual'), 
                   aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+
  xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")+
  #arrow down
  geom_segment(x = this_year_df%>% 
                 filter(What == 'Annual')%>% pull(Percent), 
               y = this_year_df%>% 
                 filter(What == 'Annual') %>% pull(Rain), 
               xend =  this_year_df%>% 
                 filter(What == 'Annual') %>% pull(Percent), 
               yend = 42,
               arrow = arrow(length = unit(0.03, "npc"), ends = "last")) +
  #arrow horizontal 
  geom_segment(x = this_year_df%>% 
                 filter(What == 'Annual')%>% pull(Percent), 
               y = this_year_df%>% 
                 filter(What == 'Annual') %>% pull(Rain), 
               xend =  '0%', 
               yend = this_year_df%>% 
                 filter(What == 'Annual') %>% pull(Rain),
               arrow = arrow(length = unit(0.03, "npc"), ends = "last"))

ggarrange(C_ex, D_ex,  nrow = 2,
          labels=c("       C","       D" ))

ggarrange(E_ex, F_ex,  nrow = 2,
          labels=c("       E","       F" ))

