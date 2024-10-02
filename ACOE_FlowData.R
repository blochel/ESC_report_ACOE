
# Flow Annual Report ------------------------------------------------------


flow <- read_xls('/Hydrology/Rainfall and SDCS flow/SDCS flow data_all.xls')



TSB_FLOW <- flow %>% 
  filter(HY == '2021-22') %>% 
  ggplot(aes(y = dbhydro_TSB_AF, x = as.Date(LABEL))) +
  geom_line() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y")) +
  xlab("") + ylab("Flow (ac.ft)") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())
  







### Jerrys Flow Table ####

# 4/15/2020 this needs quite a bit of work, will take time. outline is currently after HY18-19s calculations, there will
# be years when water goes a differnt way, this is what needs to be figured out...

#Input to Frog Pond	=S332D+S176
input_frog_pond= flow$S.332D._AF + flow$S.176._AF
flow<-cbind(flow,input_frog_pond)
#Frogpond Output	=S200+S199+S177
frog_pond_output= flow$S200._AF + flow$S.199._AF + flow$S.177.AF 
flow<-cbind(flow,frog_pond_output)
#Upper Reach Seepage (Frogpond out-Input)	=(S200+S199+S177)-(S332D+S176)
upper_reach_seepage_frog_pond_output= (flow$S200._AF + flow$S.199._AF + flow$S.177.AF) - (flow$S.332D._AF + flow$S.176._AF)
flow<-cbind(flow,upper_reach_seepage_frog_pond_output)
#S200 Contribution to TSB Flow "CHECK THIS CODE with Jerry"
s200_contribution_to_tsb_flow= ifelse(flow$TS...............acre.ft.day.>upper_reach_seepage_frog_pond_output, upper_reach_seepage_frog_pond_output, flow$S200._AF-upper_reach_seepage_frog_pond_output)
flow<-cbind(flow,s200_contribution_to_tsb_flow)
# 'if TSB>(S200-Seepage; upper reach seepage) then equals (S200-seepage; upper reach seepage), if TSB<(S200-Seepage; upper reach seepage) then equals TSB with (S200-Seepage; upper reach seepage)-TSB as   recharge to headwaters, i.e., TSHB Contribution (next line in table) is negative - 

#T.S. Headwaters Basin Contribution To TSB	=TSB-(S200 contibution to TSB)
t_s_headwaters_basin_contribution_to_tsb= flow$TS...............acre.ft.day.-s200_contribution_to_tsb_flow
flow<-cbind(flow,t_s_headwaters_basin_contribution_to_tsb)
#Seepage (S-177 to S-18C)	=S18C-S177-S178
seepage_s177_to_s18c= flow$S18C._AF - flow$S.177.AF - flow$S.178._AF
flow<-cbind(flow,seepage_s177_to_s18c)
#S199 Contribution to Lower TS flow	=S199- Seepage (S-177 to S18C)
s199_contribution_to_lower_ts_flow= flow$S.199._AF - seepage_s177_to_s18c
flow<-cbind(flow,s199_contribution_to_lower_ts_flow)

#C-111 levee Discharge	=S18C-S197 (when S-18C exceeds S197)
c111_levee_discarge=  ifelse (flow$S18C._AF>flow$S.197._AF, flow$S18C._AF-flow$S.197._AF, 0)
flow<-cbind(flow,c111_levee_discarge)
#Seepage S-18C to S197	=S-197-S18C (When S-197 exceeds S18C)
seepage_s18c_to_s197= ifelse (flow$S18C._AF<flow$S.197._AF, flow$S.197._AF-flow$S18C._AF, 0)
flow<-cbind(flow,seepage_s18c_to_s197)
#C-111 Sheet Flow to Florida Bay	=C-111 Overland-Seepage (S18C to S-197)
c111_sheet_flow_to_florida_bay= c111_levee_discarge - seepage_s18c_to_s197
flow<-cbind(flow,c111_sheet_flow_to_florida_bay)
#Total Flow via TS	=TSB+S199 Contribution
total_flow_via_ts= flow$TS...............acre.ft.day.+s199_contribution_to_lower_ts_flow
flow<-cbind(flow,total_flow_via_ts)
#Total Flow to FB	=TS Contribution+Overland C-111
total_flow_to_fb= c111_sheet_flow_to_florida_bay+total_flow_via_ts
flow<-cbind(flow,total_flow_to_fb)
#TL OutFlow to Estuaries (Should equal input)	=TS Contribution+Overland C-111+S197
tl_outflow_to_estuaries= c111_sheet_flow_to_florida_bay+total_flow_via_ts+flow$S.197._AF
flow<-cbind(flow,tl_outflow_to_estuaries)
#Total Input	=Input to Frog Pond +EVER+S178
total_input= input_frog_pond + flow$S.178._AF + t_s_headwaters_basin_contribution_to_tsb
flow<-cbind(flow,total_input)
#Difference	=Output-Input
difference= tl_outflow_to_estuaries - total_input
flow<-cbind(flow,difference)
#% Difference	=Difference/Total Outflow
pro_difference= difference/total_input
flow<-cbind(flow,pro_difference)


#SCWP Conribution to TS flow into FB	=S200 contribution +S199 Contribution	
scwp_contribution_to_ts_flow_into_fb= s200_contribution_to_tsb_flow + t_s_headwaters_basin_contribution_to_tsb + s199_contribution_to_lower_ts_flow
flow<-cbind(flow,scwp_contribution_to_ts_flow_into_fb)
#Percentage Flow from SC reaching FB Via TS	=SCWP Contribution/(S199+S200)	
percentage_flow_from_sc_reaching_fb_via_ts= scwp_contribution_to_ts_flow_into_fb / (flow$S200._AF + flow$S.199._AF)
flow<-cbind(flow,percentage_flow_from_sc_reaching_fb_via_ts)


#group by season and hy 
by_season <- group_by(flow, Season)
by_hy <- group_by(flow, HY)
by_hy_season <- group_by(flow, HY, Season)


test<-summarize(by_hy_season, total_flow_to_fb= mean(total_flow_to_fb, na.rm = T))
write.csv(test, "test123.csv")

### C-111 SEEPAGE ####
### C111 overland flow, TSB bar graph####

### S-18C TSB ####
### S200 S199, annual and month (bar graph) ####
S200<- 
  S199<- 
  
  
  ### Table (TCD1.)JERRY TABLE. Flows in thousands Acre-Feet (kAcFt)  ####

### exceedance curves, TSB S197 S18C C111 overland flow ####
