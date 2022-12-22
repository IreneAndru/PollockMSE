
MSE.MPs<-c('MP1','MP2','MP3','MP4','MP5','MP6','MP7')
mps2<-length(MSE.MPs)
LRP<-14350
USR<-22960
PercentChange<-0.15
Obj4Limit<-3000
MSE <-runMSE(OM=OM, MPs=MSE.MPs)
#MSE1<-MSE
#MSE2<-MSE
OMNAME<-'OM2'

#Objective 1
summary_obj1<-data.frame(MP=NA, OM=NA, Year=NA, PropBelowLRP=NA, SSB_mean=NA, SSB_min=NA, SSB_max=NA, TAC_sum=NA)
for(i in seq(1:mps2)){
  MPName<-MSE.MPs[i]
  ssb_out<-MSE@SSB[,i,]
  ssb_out_mean<-apply(ssb_out,2,mean)
  ssb_out_min<-apply(ssb_out,2,min)
  ssb_out_max<-apply(ssb_out,2,max)
  ssb_out_test<-matrix(ssb_out-LRP, ncol=proyears2 )
  ssb_out_test_sum<-apply(apply(ssb_out_test, 2, function(x) (x<0)),2,function(x) sum(x))/nsim2 #FALSE is 0, TRUE is 1
  TAC_out<-MSE@TAC[,i,]
  TAC_out_SumPerYr<-apply(TAC_out,2,sum)/1000
  #TAC_out_mean<-sum(TAC_out_MnPerYr)
    summary_obj1_temp<-data.frame(MP=rep(MPName,proyears2), OM=rep(paste(OMNAME),proyears2), Year=seq(1,proyears2,1), PropBelowLRP=ssb_out_test_sum, SSB_mean=ssb_out_mean, SSB_min=ssb_out_min, SSB_max=ssb_out_max, TAC_sum=TAC_out_SumPerYr)
summary_obj1<-rbind(summary_obj1, summary_obj1_temp)
summary_obj1<-subset(summary_obj1, !is.na(Year))
}
assign(paste("summary_obj1_", OMNAME, sep=""), summary_obj1)

#Objective 2
summary_obj2<-data.frame(MP=NA, OM=NA, Year=NA, PropAboveUSR=NA)
for(i in seq(1:mps2)){
  MPName<-MSE.MPs[i]
  ssb_out<-MSE@SSB[,i,]
  ssb_out_mean<-apply(ssb_out,2,mean)
  ssb_out_test<-matrix(USR-ssb_out, ncol=proyears2 )
  ssb_out_test_sum<-apply(apply(ssb_out_test, 2, function(x) (x<0)),2,function(x) sum(x))/nsim2 #FALSE is 0, TRUE is 1
  summary_obj2_temp<-data.frame(MP=rep(MPName,proyears2), OM=rep(paste(OMNAME),proyears2), Year=seq(1,proyears2,1), PropAboveUSR=ssb_out_test_sum)
  summary_obj2<-rbind(summary_obj2, summary_obj2_temp)
  summary_obj2<-subset(summary_obj2, !is.na(Year))
}
assign(paste("summary_obj2_", OMNAME, sep=""), summary_obj2)



#Objective 3
OM_name<-OMNAME
TAC_change<-data.frame(MP=NA, OM=NA, Year=NA, Sim=NA, TAC=NA, TACChange=NA, PercExceeded=NA)
TAC_change_all<-data.frame(MP=NA, OM=NA, Year=NA, Sim=NA, TAC=NA, TACChange=NA, PercExceeded=NA)
TAC_change_all_out<-data.frame(MP=NA, OM=NA, Year=NA, Sim=NA, TAC=NA, TACChange=NA, PercExceeded=NA)
TAC_change_perSim<-data.frame(MP=NA, OM=NA, Sim=NA, YearsPercExceed=NA)

for(i in seq(1:mps2)){ #Each MP
MPName<-MSE.MPs[i]
TAC_out<-MSE@TAC[,i,]
for (j in seq(1,nsim2,1)){ #Each simulation
  for (k in seq(1,proyears2-1,1)){ #Each projected year
    TAC_change_temp<-data.frame(MP=MPName, OM=OM_name, Year=k, Sim=j, TAC=TAC_out[j,k], TACChange=(TAC_out[j,k+1]-TAC_out[j,k])/TAC_out[j,k])
    TAC_change_temp$PercExceeded<-with(TAC_change_temp, abs(TACChange)>PercentChange)
    TAC_change<-rbind(TAC_change, TAC_change_temp)
    TAC_change<-subset(TAC_change, !is.na(MP))
  }
  TAC_change_all<-rbind(TAC_change_all, TAC_change)
  TAC_change<-data.frame(MP=NA, OM=NA, Year=NA, Sim=NA, TAC=NA, TACChange=NA, PercExceeded=NA)
}
TAC_change_all_out<-rbind(TAC_change_all_out,TAC_change_all)
TAC_change_all<-data.frame(MP=NA, OM=NA, Year=NA, Sim=NA, TAC=NA, TACChange=NA, PercExceeded=NA)

}
TAC_change_all_out<-subset(TAC_change_all_out, !is.na(MP))
TAC_Outputs_plot<-aggregate(TAC~MP+OM+Year, TAC_change_all_out, FUN=function(x){quantile(x,c(0.025, 0.05, 0.25,0.5, 0.75, 0.95, 0.975)) })


assign(paste("summary_obj4_TACPlots_", OMNAME, sep=""), TAC_Outputs_plot)


assign(paste("summary_obj3_EvalPlot_", OMNAME, sep=""), TAC_change_all_out)
#
#MSE1 is OM1, MSE2 is OM2


#apply(mat,2,function(x){quantile(x,c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975)) })
#TAC_change_perOM_mean<-aggregate(YearsPercExceed.PercExceeded~MP+OM+YearsPercExceed.MP, TAC_change_perSim, FUN=mean)
#TAC_change_perOM_sd<-aggregate(YearsPercExceed.PercExceeded~MP+OM+YearsPercExceed.MP, TAC_change_perSim, FUN=sd)
#TAC_change_perOM<-data.frame(OM=OM_name, MP=MPName, MeanYearsExceeded=TAC_change_perOM_mean$YearsPercExceed.PercExceeded, SD=TAC_change_perOM_sd$YearsPercExceed.PercExceeded)
#assign(paste("TAC_change", OM_name, sep=""), TAC_change_perOM)
#assign(paste("TAC_change_all", OM_name, sep=""), TAC_change_all)


setwd("C:/Users/Andrushchenkoi/Documents/Git/PollockMSE/Scorecard")
#SUmmaryPlots for #Objective 1, Once all OMs are run
summary_obj1_all<-rbind(summary_obj1_OM1, summary_obj1_OM2)
#summary_obj1_all<-rbind(summary_obj1_all, summary_obj1_OM3)
summary_obj1_all$Year2<-summary_obj1_all$Year+2019

ggplot(data=summary_obj1_all, aes(Year2, PropBelowLRP, col=OM))+geom_line()+facet_wrap(~MP)+theme_bw()+ylim(0,0.5)+geom_hline(yintercept=c(0.05, 0.25), linetype=2, col="red")+xlim(2021,2035)+ylab("Proportion Of Simulations Below LRP")+xlab("Year")+geom_vline(xintercept=c(2022))+geom_vline(xintercept=c(2034))
ggsave("Objective1_Plot1.png", width=11.5, height=7, units="in")

#TerminalYearTable 
Obj1_Limit1<-0.05
Obj1_Limit2<-0.25
summary_obj1_all$PropBelowLRPLessThanLowerLimit<-with(summary_obj1_all, ifelse(PropBelowLRP>Obj1_Limit1, 'Fail','Pass'))
summary_obj1_all$PropBelowLRPLessThanUpperLimit<-with(summary_obj1_all, ifelse(PropBelowLRP>Obj1_Limit2, 'Fail','Pass'))
TerminalYearTable<-subset(summary_obj1_all, Year2==2034)
TerminalYearTable<-subset(TerminalYearTable, select=c('MP','OM','PropBelowLRP'))
TerminalYearTable<-TerminalYearTable[order(TerminalYearTable$MP),]
write.csv(TerminalYearTable, "Objective1_TerminalYearTable.csv")

#AcrossProjectedYearsTable 
Obj1_Limit1<-0.05
Obj1_Limit2<-0.25
AcrossYearsTable<-aggregate(PropBelowLRP~MP+OM, subset(summary_obj1_all, Year2>2020&Year2<2035), FUN="mean")
AcrossYearsTable$PropBelowLRPLessThanLowerLimit<-with(AcrossYearsTable, ifelse(PropBelowLRP>Obj1_Limit1, 'Fail','Pass'))
AcrossYearsTable$PropBelowLRPLessThanUpperLimit<-with(AcrossYearsTable, ifelse(PropBelowLRP>Obj1_Limit2, 'Fail','Pass'))
AcrossYearsTable<-subset(AcrossYearsTable, select=c('MP','OM','PropBelowLRP'))
AcrossYearsTable<-AcrossYearsTable[order(AcrossYearsTable$MP),]
write.csv(AcrossYearsTable, "Objective1_AcrossYearsTable.csv")


ggplot(data=summary_obj1_all, aes(Year2, SSB_mean, col=OM))+geom_line()+geom_line(aes(Year2, SSB_min, col=OM), linetype=2)+geom_line(aes(Year2, SSB_max, col=OM), linetype=2)+facet_wrap(~MP)+theme_bw()+geom_hline(yintercept=c(LRP, USR))+ylab("SSB")+xlab("Year")+geom_vline(xintercept=c(2022,2034))+xlim(2021,2035)
ggsave("Objective1_Plot1_SSB.png", width=11.5, height=7, units="in")


#Objective 2, Once all OMs are run
summary_obj2_all<-rbind(summary_obj2_OM1, summary_obj2_OM2)
#summary_obj2_all<-rbind(summary_obj2_all, summary_obj2_OM3)

summary_obj2_all$Year2<-summary_obj2_all$Year+2019

ggplot(data=summary_obj2_all, aes(Year2, PropAboveUSR, col=OM))+geom_line()+facet_wrap(~MP)+theme_bw()+ylim(0,1)+geom_hline(yintercept=c(0.5, 0.75), linetype=2, col="red")+ylab("Proportion Of Simulations Above USR")+xlab("Year")+geom_vline(xintercept=c(2022, 2034))+xlim(2021,2035)
ggsave("Objective2_Plot1.png", width=11.5, height=7, units="in")

#TerminalYearTable 
Obj2_Limit1<-0.5
Obj2_Limit2<-0.75
summary_obj2_all$PropAboveUSRMoreThanLowerLimit<-with(summary_obj2_all, ifelse(PropAboveUSR<Obj2_Limit1, 'Fail','Pass'))
summary_obj2_all$PropAboveUSRMoreThanUpperLimit<-with(summary_obj2_all, ifelse(PropAboveUSR<Obj2_Limit2, 'Fail','Pass'))
TerminalYearTable2<-subset(summary_obj2_all, Year2==2034)
TerminalYearTable2<-subset(TerminalYearTable2, select=c('MP','OM','PropAboveUSR'))
TerminalYearTable2<-TerminalYearTable2[order(TerminalYearTable2$MP),]
write.csv(TerminalYearTable2, "Objective2_TerminalYearTable.csv")

#AcrossProjectedYearsTable 
Obj2_Limit2<-0.75
AcrossYearsTable2<-aggregate(PropAboveUSR~MP+OM, subset(summary_obj2_all, Year2>2020&Year2<2035), FUN="mean")
AcrossYearsTable2$PropAboveUSRMoreThanLowerLimit<-with(AcrossYearsTable2, ifelse(PropAboveUSR<Obj2_Limit1, 'Fail','Pass'))
AcrossYearsTable2$PropAboveUSRMoreThanUpperLimit<-with(AcrossYearsTable2, ifelse(PropAboveUSR<Obj2_Limit2, 'Fail','Pass'))
AcrossYearsTable2<-subset(AcrossYearsTable2, select=c('MP','OM','PropAboveUSR'))
AcrossYearsTable2<-AcrossYearsTable2[order(AcrossYearsTable2$MP),]
write.csv(AcrossYearsTable2, "Objective2_AcrossYearsTable.csv")


#####################
#Objective 3
summary_obj3_all<-rbind(summary_obj3_EvalPlot_OM1, summary_obj3_EvalPlot_OM2)
#summary_obj3_all<-rbind(summary_obj3_all, summary_obj3_EvalPlot_OM3)
summary_obj3_all$LimitTest<-with(summary_obj3_all, ifelse(TAC<Obj4Limit, 0,1))

summary_obj3_short<-subset(summary_obj3_all, Year>2&Year<11)
short_years<-length(table(summary_obj3_short$Year))
summary_obj3_med<-subset(summary_obj3_all, Year>2&Year<17)
med_years<-length(table(summary_obj3_med$Year))
summary_obj3_long<-subset(summary_obj3_all, Year>2&Year<27)
long_years<-length(table(summary_obj3_long$Year))

#
summary_obj3_short<-subset(summary_obj3_all, Year==10)
short_years<-length(table(summary_obj3_short$Year))
summary_obj3_med<-subset(summary_obj3_all, Year==16)
med_years<-length(table(summary_obj3_med$Year))
summary_obj3_long<-subset(summary_obj3_all, Year==25)
long_years<-length(table(summary_obj3_long$Year))

TAC_Outputs_Eval_short<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_short, FUN=sum)
TAC_Outputs_Eval_short$PropYearExceeded<-TAC_Outputs_Eval_short$PercExceeded/short_years
TAC_Outputs_Eval_short$Term<-"Short (2023-2030)"
TAC_Outputs_Eval_med<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_med, FUN=sum)
TAC_Outputs_Eval_med$PropYearExceeded<-TAC_Outputs_Eval_med$PercExceeded/med_years
TAC_Outputs_Eval_med$Term<-"Med (2023-2036)"
TAC_Outputs_Eval_long<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_long, FUN=sum)
TAC_Outputs_Eval_long$PropYearExceeded<-TAC_Outputs_Eval_long$PercExceeded/long_years
TAC_Outputs_Eval_long$Term<-"Long(2023-2045)"

TAC_Outputs_Eval_all<-rbind(TAC_Outputs_Eval_short, TAC_Outputs_Eval_med)
TAC_Outputs_Eval_all<-rbind(TAC_Outputs_Eval_all, TAC_Outputs_Eval_long)

ggplot(TAC_Outputs_Eval_all, aes(PercExceeded))+geom_bar(aes(fill=OM), position="dodge")+facet_grid(Term~MP)+theme_bw()+xlab("NumYears Change in TAC Exceeded Limit")+ylab("Number of Records")
ggsave("Objective3_Plot1.png", width=11.5, height=7, units="in")

#Table for Objective Three
#summary_obj4_sum<-aggregate(PercExceeded~MP+OM+Sim, summary_obj4_all, sum)
#summary_obj4_sum$PercYearsExceeded<-summary_obj4_sum$PercExceeded/proyears
#summary_obj4_sum2<-aggregate(PercYearsExceeded~MP+OM, summary_obj4_sum, mean)
#write.csv(summary_obj4_sum2, "Objective3_TableProps.csv")

TAC_Change_short<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_short, FUN=sum)
#TAC_Change_short$PercYearsExceeded<-TAC_Change_short$PercExceeded/proyears
TAC_Change_short$PercYearsExceeded<-TAC_Change_short$PercExceeded
TAC_Change_short2<-aggregate(PercYearsExceeded~MP+OM, TAC_Change_short, mean)
TAC_Change_short2$Term<-"Short (2023-2030)"
TAC_Change_med<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_med, FUN=sum)
#TAC_Change_med$PercYearsExceeded<-TAC_Change_med$PercExceeded/proyears
TAC_Change_med$PercYearsExceeded<-TAC_Change_med$PercExceeded
TAC_Change_med2<-aggregate(PercYearsExceeded~MP+OM, TAC_Change_med, mean)
TAC_Change_med2$Term<-"Med (2023-2036)"
TAC_Change_long<-aggregate(PercExceeded~MP+OM+Sim, summary_obj3_long, FUN=sum)
#TAC_Change_long$PercYearsExceeded<-TAC_Change_long$PercExceeded/proyears
TAC_Change_long$PercYearsExceeded<-TAC_Change_long$PercExceeded
TAC_Change_long2<-aggregate(PercYearsExceeded~MP+OM, TAC_Change_long, mean)
TAC_Change_long2$Term<-"Long(2023-2045)"

TAC_Change_all<-rbind(TAC_Change_short2, TAC_Change_med2)
TAC_Change_all<-rbind(TAC_Change_all, TAC_Change_long2)

Obj3_Table<-reshape2::recast(TAC_Change_all, MP+OM~Term)
write.csv(Obj3_Table, "Objective3_Table.csv")

#####################
#Objective 4
summary_obj4_all<-rbind(summary_obj4_TACPlots_OM1, summary_obj4_TACPlots_OM2) #FIx this
#summary_obj4_all<-rbind(summary_obj4_all, summary_obj4_TACPlots_OM3) #Fix this

summary_obj4_all$Year2<-summary_obj4_all$Year+2019
names(summary_obj4_all)

ggplot(data=summary_obj4_all, aes(Year2, TAC[,4], col=OM))+geom_line()+geom_line(aes(Year2, TAC[,1], col=OM), linetype=2)+geom_line(aes(Year2, TAC[,7], col=OM), linetype=3)+facet_wrap(~MP)+theme_bw()+geom_hline(yintercept=c(Obj4Limit))+ylab("TAC")+xlab("Year")+geom_vline(xintercept=c(2022, 2030, 2036, 2044))+xlim(2021,2044)
ggsave("Objective4_Plot1_TAC.png", width=11.5, height=7, units="in")

#
TAC_short<-aggregate(TAC~MP+OM+Year, summary_obj3_short, FUN=median)
TAC_short$Term<-"Short (2023-2030)"
TAC_short$Test<-with(TAC_short, ifelse(TAC>Obj4Limit, 1, 0))
TAC_short2<-aggregate(Test~MP+OM+Term, TAC_short, FUN="sum")
TAC_short2$PropYears<-with(TAC_short2, Test/short_years)

TAC_med<-aggregate(TAC~MP+OM+Year, summary_obj3_med, FUN=median)
TAC_med$Term<-"Med (2023-2036)"
TAC_med$Test<-with(TAC_med, ifelse(TAC>Obj4Limit, 1, 0))
TAC_med2<-aggregate(Test~MP+OM+Term, TAC_med, FUN="sum")
TAC_med2$PropYears<-with(TAC_med2, Test/med_years)

TAC_long<-aggregate(TAC~MP+OM+Year, summary_obj3_long, FUN=median)
TAC_long$Term<-"Long(2023-2045)"
TAC_long$Test<-with(TAC_long, ifelse(TAC>Obj4Limit, 1, 0))
TAC_long2<-aggregate(Test~MP+OM+Term, TAC_long, FUN="sum")
TAC_long2$PropYears<-with(TAC_long2, Test/long_years)

TAC_all<-rbind(TAC_short2, TAC_med2)
TAC_all<-rbind(TAC_all, TAC_long2)

TAC_all2<-data.frame(TAC_all[-5])
Obj4_TableYears<-reshape2::recast(TAC_all2, MP+OM~Term)
write.csv(Obj4_TableYears, "Objective4_TableYears.csv")

TAC_all2<-data.frame(TAC_all[-4])
Obj4_TableProps<-reshape2::recast(TAC_all2, MP+OM~Term)
write.csv(Obj4_TableProps, "Objective4_TableProps.csv")


#Yield
Yield<-subset(summary_obj1_all, select=c('MP','OM','Year','TAC_sum'))


summary_yield_short<-subset(Yield, Year>2&Year<11)
short_years<-length(table(summary_yield_short$Year))
summary_yield_med<-subset(Yield, Year>2&Year<17)
med_years<-length(table(summary_yield_med$Year))
summary_yield_long<-subset(Yield, Year>2&Year<27)
long_years<-length(table(summary_yield_long$Year))


Yield_Outputs_Eval_short<-aggregate(TAC_sum~MP+OM, summary_yield_short, FUN=sum)
Yield_Outputs_Eval_short$Term<-"Short (2023-2030)"
Yield_Outputs_Eval_med<-aggregate(TAC_sum~MP+OM, summary_yield_med, FUN=sum)
Yield_Outputs_Eval_med$Term<-"Med (2023-2036)"
Yield_Outputs_Eval_long<-aggregate(TAC_sum~MP+OM, summary_yield_long, FUN=sum)
Yield_Outputs_Eval_long$Term<-"Long(2023-2045)"

Yield_Outputs_Eval_all<-rbind(Yield_Outputs_Eval_short, Yield_Outputs_Eval_med)
Yield_Outputs_Eval_all<-rbind(Yield_Outputs_Eval_all, Yield_Outputs_Eval_long)

Yield_Table<-reshape2::recast(Yield_Outputs_Eval_all, MP+OM~Term)
write.csv(Yield_Table, "Yield_Table.csv")

Yield_Table_Prop<-Yield_Table
Yield_Table_Prop[,3]<-Yield_Table_Prop[,3]/min(Yield_Table_Prop[,3])-1
Yield_Table_Prop[,4]<-Yield_Table_Prop[,4]/min(Yield_Table_Prop[,4])-1
Yield_Table_Prop[,5]<-Yield_Table_Prop[,5]/min(Yield_Table_Prop[,5])-1
write.csv(Yield_Table_Prop, "Yield_Table_Prop.csv")

#Yield$Year<-as.character(Yield$Year) #You might ask why am I doing that? I couldn't tell you why. I just know that without it, it doesn't work. Alternative using dplyr below.
#Yield_Annual<-reshape2::recast(Yield, Year~OM+MP)
