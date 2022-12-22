#Candidate MPs for the Pollock MSE

#Packages
library(zoo)

# Geometric Mean and Rolling GM Mean functions
GMean<-function(x){
  exp(mean(log(x)))
}
RunningGMean<-function(x,n){
  rollapply(x,width=n, GMean)
}

#MP1 - Calculate the change in the most recent 3 Year Geometric Mean of Survey and the preceding one, with 15% or 250mt limit on change, whichever is greatest, and apply it to the TAC in the previous year (starting with 2022 TAC). 

MP1<-function(x, Data, reps=10)
{
  yrs_ave<-3 #How many years in GMmean
  PercLimit<-0.15#Percent limit on change
  MTLimit<-250#Mt limit on change
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    recentI<- RunningGMeanSeries[length(RunningGMeanSeries)]#Most recent year
    precedingI<-RunningGMeanSeries[length(RunningGMeanSeries)-1] #Geometric mean of the preceding three years
    slope<-(-1*(1-recentI/precedingI)) #Slope (change) between the most recent 3-year GM and the one preceding that.
    y <- max(Data@Year) - Data@LHYear + 1 #What is the most recent year in Simulation
    TAC_start<-Data@MPrec[x]
    TAC_new<-TAC_start+(TAC_start*slope) #New TAC
    TAC_change_MT<-TAC_new-TAC_start #change in MT
    TwoFiddyChangePerc<-(MTLimit/TAC_start)*(TAC_change_MT/abs(TAC_change_MT)) #What 250 represents in Percent Change, including direction
    PercLimit_dir<-PercLimit*(TAC_change_MT/abs(TAC_change_MT))
    ChangeLimit<-ifelse(abs(PercLimit)>abs(TwoFiddyChangePerc), PercLimit_dir, TwoFiddyChangePerc)
    if(abs(slope)>abs(ChangeLimit)){slope<-ChangeLimit} else {slope<-slope}
    TAC_new<-TAC_start+(TAC_start*slope) 
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}


class(MP1)<-"MP"

#MP2 - Calculate the change in the most recent 3 Year Geometric Mean of Survey and the preceding one, with 15% limit, applied to the TAC in the previous year (starting with 2022 TAC). 
MP2<-function(x, Data, reps=10)
{
  yrs_ave<-3 #How many years in GMmean
  PercLimit<-0.15 #Perc limit on change
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    recentI<- RunningGMeanSeries[length(RunningGMeanSeries)]#Most recent year
    precedingI<-RunningGMeanSeries[length(RunningGMeanSeries)-1] #Geometric mean of the preceding three years
    slope<-(-1*(1-recentI/precedingI)) #Slope (change) between the most recent 3-year GM and the one preceding that.
    y <- max(Data@Year) - Data@LHYear + 1 #What is the most recent year in Simulation
    TAC_start<-Data@MPrec[x]
    TAC_new<-TAC_start+(TAC_start*slope) #New TAC
    TAC_change_MT<-TAC_new-TAC_start
    PercLimit_dir<-PercLimit*(TAC_change_MT/abs(TAC_change_MT))
    if(abs(slope)>abs(PercLimit_dir)){slope<-PercLimit_dir} else {slope<-slope}
    TAC_new<-TAC_start+(TAC_start*slope) 
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}

class(MP2)<-"MP"

#MP3 - Biomass based - ramp. HCR is set with an F of 0.06 in the Critical Zone and 0.187 in the Healthy zone. Cautious zone is a ramp between the two. Recent three year geometric mean of q-adjusted survey biomass. Where that biomass falls along the x axis of the HCR determines the f rate applied to provide TAC advice. 

MP3<-function(x, Data, reps=10)
{
  Fcrit<-0.06
  Flim<-0.187
  lrp<-14350
  LowerControlPoint<-14350 #(if you want to use a lower control point above the LRP, change this)
  usr<-22960
  UpperControlPoint<-22960
  #ChangeInCritical<-1
  #ChangeInCautious<-0.15
  #ChangeInHealthy<-0.2
  yrs_ave<-3 #How many years in average
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    currentI<- RunningGMeanSeries[length(RunningGMeanSeries)] #Running 3yr geomean of survey points
    precedingI<-RunningGMeanSeries[length(RunningGMeanSeries)-1] #Geometric mean of the preceding three years
    #q parameter. geometric mean of the historic years of survey/ssb. #HARDWIRED INTO THE MPs (needs the historic part of the MSE run to be calculated). DOESN"T CHANGE BETWEEN OMs or MPs. Applied as a single q to biomass; in reality, it will be applied as the at-age model q from the survey to survey catch at age, with the WAA applied afterwards to produce an estimate of SSB. 
    q<-0.2416536 
    current_SSB<-currentI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    preceding_SSB<-precedingI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    if(current_SSB <= lrp){mult <- 0}else {if(current_SSB < usr){mult <- (current_SSB-lrp)/(usr-lrp)}else{mult <- 1}}#Multiplier for F based on which zone the current-SSB falls in.
    f_raw<-mult*(Flim-Fcrit)+Fcrit #The f associated with the current_SSB based on the harvest control rule (with no limitations on change)
    TAC_new<-current_SSB*(1-exp(-f_raw))
    #TAC_start<-Data@MPrec[x]#The current TAC (the one that's being changed)
    #F_start<-(-log((1-TAC_start/preceding_SSB)))#which f the current TAC represents
    #if (TAC_start>preceding_SSB){F_start<-0.1} else {F_start<-F_start}
    #if(F_start<=Fcrit){Zone_CurrentTAC<-"Critical"} else {if(F_start<Flim){Zone_CurrentTAC<-"Cautious"} else {Zone_CurrentTAC<-"Healthy"}}#Which zone that f falls on the HCR.
    #if(Zone_CurrentTAC=="Critical"){ChangeLimit<-ChangeInCritical} else {if(Zone_CurrentTAC=="Cautious"){ChangeLimit<-ChangeInCautious} else {ChangeLimit<-ChangeInHealthy}}#Defines the change limit based on the zone the previous years's TAC falls in
    #TAC_change_MT<-TAC_new-TAC_start #change in MT without imposing limit
    #TAC_change_perc<-(TAC_change_MT)/TAC_start#What that change in raw TAC represents in percent
    #ChangeLimit_dir<-ChangeLimit*(TAC_change_MT/abs(TAC_change_MT)) #Adding the same directionality to the change limit.
    #if(Zone_CurrentTAC=="Critical"){change<-ChangeInCritical} else {if(abs(ChangeLimit_dir)>abs(ChangeLimit_dir)){change<-PercLimit_dir} else {change<-ChangeLimit_dir}}#Defines the change limit based on the zone the previous years's TAC falls in
    #TAC_new<-TAC_start+(TAC_start*change) #Calculate new TAC
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}

class(MP3)<-"MP"

#MP4 - Biomass based - ramp. HCR is set with an F of 0.04 in the Critical Zone and 0.187 in the Healthy zone. Cautious zone is a ramp between the two. Recent three year geometric mean of q-adjusted survey biomass. Where that biomass falls along the x axis of the HCR determines the f rate applied to provide TAC advice. The change from the previous year's TAC to this year's advice is limited to 15% change when this year's 3-year GM biomass falls in the cautious zone and 20% when this year's 3-year GM biomass falls in the healthy zone. 

MP4<-function(x, Data, reps=10)
{
  Fcrit<-0.04
  Flim<-0.187
  lrp<-14350
  LowerControlPoint<-14350 #(if you want to use a lower control point above the LRP, change this)
  usr<-22960
  UpperControlPoint<-22960
  ChangeInCritical<-1
  ChangeInCautious<-0.15
  ChangeInHealthy<-0.2
  yrs_ave<-3 #How many years in average
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    currentI<- RunningGMeanSeries[length(RunningGMeanSeries)] #Running 3yr geomean of survey points
    precedingI<-RunningGMeanSeries[length(RunningGMeanSeries)-1] #Geometric mean of the preceding three years
    #q parameter. geometric mean of the historic years of survey/ssb. #HARDWIRED INTO THE MPs (needs the historic part of the MSE run to be calculated). DOESN"T CHANGE BETWEEN OMs or MPs. Applied as a single q to biomass; in reality, it will be applied as the at-age model q from the survey to survey catch at age, with the WAA applied afterwards to produce an estimate of SSB. 
    q<-0.2416536 
    current_SSB<-currentI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    preceding_SSB<-precedingI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    if(current_SSB <= lrp){mult <- 0}else {if(current_SSB < usr){mult <- (current_SSB-lrp)/(usr-lrp)}else{mult <- 1}}#Multiplier for F based on which zone the current-SSB falls in.
    f_raw<-mult*(Flim-Fcrit)+Fcrit #The f associated with the current_SSB based on the harvest control rule (with no limitations on change)
    TAC_new<-current_SSB*(1-exp(-f_raw))
    TAC_start<-Data@MPrec[x]#The current TAC (the one that's being changed)
    F_start<-(-log((1-TAC_start/preceding_SSB)))#which f the current TAC represents
    if (TAC_start>preceding_SSB){F_start<-0.1} else {F_start<-F_start}
    if(F_start<=Fcrit){Zone_CurrentTAC<-"Critical"} else {if(F_start<Flim){Zone_CurrentTAC<-"Cautious"} else {Zone_CurrentTAC<-"Healthy"}}#Which zone that f falls on the HCR.
    if(Zone_CurrentTAC=="Critical"){ChangeLimit<-ChangeInCritical} else {if(Zone_CurrentTAC=="Cautious"){ChangeLimit<-ChangeInCautious} else {ChangeLimit<-ChangeInHealthy}}#Defines the change limit based on the zone the previous years's TAC falls in
    TAC_change_MT<-TAC_new-TAC_start #change in MT without imposing limit
    TAC_change_perc<-(TAC_change_MT)/TAC_start#What that change in raw TAC represents in percent
    ChangeLimit_dir<-ChangeLimit*(TAC_change_MT/abs(TAC_change_MT)) #Adding the same directionality to the change limit.
    if(Zone_CurrentTAC=="Critical"){change<-ChangeInCritical} else {if(abs(ChangeLimit_dir)>abs(TAC_change_perc)){change<-TAC_change_perc} else {change<-ChangeLimit_dir}}#Defines the change limit based on the zone the previous years's TAC falls in
    TAC_new<-TAC_start+(TAC_start*change) #Calculate new TAC
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}

class(MP4)<-"MP"

#MP5 - Biomass based - ramp. HCR is set with an F of 0.06 in the Critical Zone and 0.187 in the Healthy zone. Cautious zone is a ramp between the two. Recent three year geometric mean of q-adjusted survey biomass. Where that biomass falls along the x axis of the HCR determines the f rate applied to provide TAC advice. The change from the previous year's TAC to this year's advice is limited to 15% change when this year's 3-year GM biomass falls in the cautious zone and 20% when this year's 3-year GM biomass falls in the healthy zone. 

MP5<-function(x, Data, reps=10)
{
  Fcrit<-0.06
  Flim<-0.187
  lrp<-14350
  LowerControlPoint<-14350 #(if you want to use a lower control point above the LRP, change this)
  usr<-22960
  UpperControlPoint<-22960
  ChangeInCritical<-1
  ChangeInCautious<-0.15
  ChangeInHealthy<-0.2
  yrs_ave<-3 #How many years in average
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    currentI<- RunningGMeanSeries[length(RunningGMeanSeries)] #Running 3yr geomean of survey points
    precedingI<-RunningGMeanSeries[length(RunningGMeanSeries)-1] #Geometric mean of the preceding three years
    #q parameter. geometric mean of the historic years of survey/ssb. #HARDWIRED INTO THE MPs (needs the historic part of the MSE run to be calculated). DOESN"T CHANGE BETWEEN OMs or MPs. Applied as a single q to biomass; in reality, it will be applied as the at-age model q from the survey to survey catch at age, with the WAA applied afterwards to produce an estimate of SSB. 
    q<-0.2416536 
    current_SSB<-currentI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    preceding_SSB<-precedingI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    if(current_SSB <= lrp){mult <- 0}else {if(current_SSB < usr){mult <- (current_SSB-lrp)/(usr-lrp)}else{mult <- 1}}#Multiplier for F based on which zone the current-SSB falls in.
    f_raw<-mult*(Flim-Fcrit)+Fcrit #The f associated with the current_SSB based on the harvest control rule (with no limitations on change)
    TAC_new<-current_SSB*(1-exp(-f_raw))
    TAC_start<-Data@MPrec[x]#The current TAC (the one that's being changed)
    F_start<-(-log((1-TAC_start/preceding_SSB)))#which f the current TAC represents
    if (TAC_start>preceding_SSB){F_start<-0.1} else {F_start<-F_start}
    if(F_start<=Fcrit){Zone_CurrentTAC<-"Critical"} else {if(F_start<Flim){Zone_CurrentTAC<-"Cautious"} else {Zone_CurrentTAC<-"Healthy"}}#Which zone that f falls on the HCR.
    if(Zone_CurrentTAC=="Critical"){ChangeLimit<-ChangeInCritical} else {if(Zone_CurrentTAC=="Cautious"){ChangeLimit<-ChangeInCautious} else {ChangeLimit<-ChangeInHealthy}}#Defines the change limit based on the zone the previous years's TAC falls in
    TAC_change_MT<-TAC_new-TAC_start #change in MT without imposing limit
    TAC_change_perc<-(TAC_change_MT)/TAC_start#What that change in raw TAC represents in percent
    ChangeLimit_dir<-ChangeLimit*(TAC_change_MT/abs(TAC_change_MT)) #Adding the same directionality to the change limit.
    if(Zone_CurrentTAC=="Critical"){change<-ChangeInCritical} else {if(abs(ChangeLimit_dir)>abs(TAC_change_perc)){change<-TAC_change_perc} else {change<-ChangeLimit_dir}}#Defines the change limit based on the zone the previous years's TAC falls in
    TAC_new<-TAC_start+(TAC_start*change) #Calculate new TAC
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}

class(MP5)<-"MP"

#MP6 - Biomass based - 3 Bands. HCR is set with an F of 0.06 in the Critical Zone and 0.187 in the Healthy zone. Cautious zone consists of three bands between the LARP and USR, a constant TAC of 836mt in the lower 1/3 of the cautious zone, a constant TAC of 1675 mt in the middle 1/3 of the cautious zone and a constant TAC of 2706 mt in the upper 1/3 of the Cautious zone. Recent three year geometric mean of q-adjusted survey biomass. Where that biomass falls along the x axis of the HCR determines the f (in Healthy) or the TAC (if in cautious) for advice. 

MP6<-function(x, Data, reps=10){
  yrs_ave<-3 #How many years in average
  Fcrit<-0.06
  Flim<-0.187
  lrp<-14350
  usr<-22960
  nbands<-3
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    currentI<- RunningGMeanSeries[length(RunningGMeanSeries)] #Running 3yr geomean of survey points
    #q parameter. geometric mean of the historic years of survey/ssb. #HARDWIRED INTO THE MPs (needs the historic part of the MSE run to be calculated). DOESN"T CHANGE BETWEEN OMs or MPs. Applied as a single q to biomass; in reality, it will be applied as the at-age model q from the survey to survey catch at age, with the WAA applied afterwards to produce an estimate of SSB. 
    q<-0.2443367 
    current_SSB<-currentI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    Biomass_breaks<-(usr-lrp)/nbands #Distance between breaks.
    HCRdf<-data.frame(Band=NA, BiomassValue=NA, Fvalue=NA, TACvalue=NA) #Just setting the format of the table showing the HCR.
    for (i in seq(0, nbands,1)){ #This loop generates each 'break point' for number of nbands. Fun fact: if you have four bands, you only have 3 break points. This is why nbands-1.
      Biomass1<-lrp+i*Biomass_breaks #Value of the break in Biomass
      Fmult<-(Biomass1-lrp)/(usr-lrp) #Multiplier needed to get F at that Biomass. Code starting to look familliar?
      F1<-Fmult*(Flim-Fcrit)+Fcrit #This is the F value associated with the biomass defined as 'Biomass1'
      TAC1<-Biomass1*(1-exp(-F1))
      temp<-data.frame(Band=i, BiomassValue=Biomass1, Fvalue=F1, TACvalue=TAC1) #Creates a data table with the values JUST FOR THIS LOOP.
      HCRdf<-rbind(HCRdf, temp)
    }
    HCRdf<-subset(HCRdf, !is.na(Band))
    if(current_SSB<=lrp){Fapplied<-Fcrit} else (Fapplied<-Flim)
    TACapplied<-ifelse(current_SSB>lrp&current_SSB<HCRdf$BiomassValue[2], HCRdf$TACvalue[1], HCRdf$TACvalue[2])
    TACapplied<-ifelse(current_SSB>HCRdf$BiomassValue[3]&current_SSB<=usr, HCRdf$TACvalue[3], TACapplied)
    if(current_SSB<=lrp|current_SSB>usr){TAC_new<-current_SSB*(1-exp(-Fapplied))} else {TAC_new<-TACapplied}
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}


class(MP6)<-"MP"

#MP7 - Biomass based - 4 Bands. HCR is set with an F of 0.06 in the Critical Zone and 0.187 in the Healthy zone. Cautious zone consists of four bands between the LRP and USR, a constant TAC of 836mt in the lower 1/4 of the cautious zone, a constant TAC of 1446 mt in the next 1/4 of the cautious zone, a constant TAC of 2167 mt in the third 1/4 of the cautious zone, and a constant TAC of 2992 mt in the upper 1/4 of the Cautious zone. Recent three year geometric mean of q-adjusted survey biomass. Where that biomass falls along the x axis of the HCR determines the f (in Healthy) or the TAC (if in cautious) for advice. 

MP7<-function(x, Data, reps=10){
  yrs_ave<-3 #How many years in average
  Fcrit<-0.06
  Flim<-0.187
  lrp<-14350
  usr<-22960
  nbands<-4
  curYr <- length(Data@Cat[x,]) # most current year of data
  if (curYr==39){TAC_new<-4259} else {if (curYr==40|curYr==41){TAC_new<-3407} else{
    vals <- Data@AddInd[x,1,] # values of the RV survey index... RV=[,1,], Acoustic=[,2,]
    RunningGMeanSeries<-RunningGMean(vals, yrs_ave) #smoothed index, ave specified above
    currentI<- RunningGMeanSeries[length(RunningGMeanSeries)] #Running 3yr geomean of survey points
    #q parameter. geometric mean of the historic years of survey/ssb. #HARDWIRED INTO THE MPs (needs the historic part of the MSE run to be calculated). DOESN"T CHANGE BETWEEN OMs or MPs. Applied as a single q to biomass; in reality, it will be applied as the at-age model q from the survey to survey catch at age, with the WAA applied afterwards to produce an estimate of SSB. 
    q<-0.2443367 
    current_SSB<-currentI/q #q-adjusted 3-year geometric mean of survey (aka. estimate of population biomass)
    Biomass_breaks<-(usr-lrp)/nbands #Distance between breaks.
    HCRdf<-data.frame(Band=NA, BiomassValue=NA, Fvalue=NA, TACvalue=NA) #Just setting the format of the table showing the HCR.
    for (i in seq(0, nbands,1)){ #This loop generates each 'break point' for number of nbands. Fun fact: if you have four bands, you only have 3 break points. This is why nbands-1.
      Biomass1<-lrp+i*Biomass_breaks #Value of the break in Biomass
      Fmult<-(Biomass1-lrp)/(usr-lrp) #Multiplier needed to get F at that Biomass. Code starting to look familliar?
      F1<-Fmult*(Flim-Fcrit)+Fcrit #This is the F value associated with the biomass defined as 'Biomass1'
      TAC1<-Biomass1*(1-exp(-F1))
      temp<-data.frame(Band=i, BiomassValue=Biomass1, Fvalue=F1, TACvalue=TAC1) #Creates a data table with the values JUST FOR THIS LOOP.
      HCRdf<-rbind(HCRdf, temp)
    }
    HCRdf<-subset(HCRdf, !is.na(Band))
    if(current_SSB<=lrp){Fapplied<-Fcrit} else (Fapplied<-Flim)
    TACapplied<-ifelse(current_SSB>lrp&current_SSB<=HCRdf$BiomassValue[2], HCRdf$TACvalue[1], HCRdf$TACvalue[2])
    TACapplied<-ifelse(current_SSB>HCRdf$BiomassValue[3]&current_SSB<=HCRdf$BiomassValue[4], HCRdf$TACvalue[2], TACapplied)
    TACapplied<-ifelse(current_SSB>HCRdf$BiomassValue[4]&current_SSB<=usr, HCRdf$TACvalue[3], TACapplied)
    if(current_SSB<=lrp|current_SSB>usr){TAC_new<-current_SSB*(1-exp(-Fapplied))} else {TAC_new<-TACapplied}
  }}
  Rec <- new("Rec") #Create Rec object
  Rec@TAC <- TAC_new # assign the TAC to the TAC slot
  Rec # return the Rec object
}

class(MP7)<-"MP"
