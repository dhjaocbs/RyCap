##Population Statistics##

MeanScoreHigh <- mean(MasterData_Adult$TotalScoreHigh)
VarScoreHigh <- var(MasterData_Adult$TotalScoreHigh)
sdScoreHigh <- sd(MasterData_Adult$TotalScoreHigh)

##PROVINCE vs CANADA MEAN - TTEST TABLE##

ProvinceMeanTable <- NULL
ProvinceMeanTable <- as.data.frame(ProvinceMeanTable)
for (i in 1:length(levels(MasterData_Adult$Province))) {
  
  T <-  t.test(MasterData_Adult$TotalScoreHigh[MasterData_Adult$Province == levels(MasterData_Adult$Province)[i]], mu=MeanScoreHigh, sigma = sdScoreHigh)
  
  SigCheck <- as.numeric(T$p.value) < 0.05
  
  if (T$statistic >0) {
    ProvinceCanCompare <- "Healthier than population"
  }  else {ProvinceCanCompare <- "Less Healthy than population"
  }
  
  ProvinceMeanTable[i,1] <- as.character(levels(MasterData_Adult$Province)[i])
  ProvinceMeanTable[i,2] <- percent(as.numeric(T$estimate))
  ProvinceMeanTable[i,3] <- as.numeric(T$p.value)
  ProvinceMeanTable[i,4] <- SigCheck
  ProvinceMeanTable[i,5] <- ProvinceCanCompare
  ProvinceMeanTable[i,6] <- percent(T$conf.int[1])
  ProvinceMeanTable[i,7] <- percent(T$conf.int[2])
  
}

names(ProvinceMeanTable) <- c("Province", "%Healthy", "P", "Significant", "CanadaCompare", "Lower 95", "Upper 95")




##AGE vs CANADA MEAN - TTEST TABLE##

AgeMeanTable <- NULL
AgeMeanTable <- as.data.frame(AgeMeanTable)
for (i in 1:length(levels(MasterData_Adult$AgeGroup2))) {
  
  T <-  t.test(MasterData_Adult$TotalScoreHigh[MasterData_Adult$AgeGroup2 == levels(MasterData_Adult$AgeGroup2)[i]], mu=MeanScoreHigh, sigma = sdScoreHigh)
  
  SigCheck <- as.numeric(T$p.value) < 0.05
  
  if (T$statistic >0) {
    AgeCanCompare <- "Healthier than population"
  }  else {AgeCanCompare <- "Less Healthy than population"
  }
  
  AgeMeanTable[i,1] <- as.character(levels(MasterData_Adult$AgeGroup2)[i])
  AgeMeanTable[i,2] <- percent(as.numeric(T$estimate))
  AgeMeanTable[i,3] <- as.numeric(T$p.value)
  AgeMeanTable[i,4] <- SigCheck
  AgeMeanTable[i,5] <- AgeCanCompare
  AgeMeanTable[i,6] <- percent(T$conf.int[1])
  AgeMeanTable[i,7] <- percent(T$conf.int[2])
  
}

names(AgeMeanTable) <- c("AgeGroup", "%Healthy", "P", "Significant", "CanadaCompare", "Lower 95", "Upper 95")


## ANSWER 1 vs ANSWER 2 MEAN for 2 level variables - TTEST TABLE##

#Isolate the indicators that can be analyzed using T Test#
FacManyOrdLevel <- NULL
FacManyCatLevel <- NULL
Fac2Level <- NULL

for (i in 12:44){
  
  if (nlevels(MasterData_Adult[,i]) == 2){
    x <- names(MasterData_Adult[i])
    Fac2Level <- cbind(Fac2Level, x)
    
  }else if (nlevels(MasterData_Adult[,i]) > 2 & is.ordered(MasterData_Adult[,i])){
    x <- names(MasterData_Adult[i])
    FacManyOrdLevel <- cbind(FacManyOrdLevel, x)
    
  }else if(nlevels(MasterData_Adult[,i]) > 2 & !is.ordered(MasterData_Adult[,i])){
    x <- names(MasterData_Adult[i])
    FacManyCatLevel <- cbind(FacManyCatLevel, x)
    
  }
}
FacManyOrdLevel <- as.vector(FacManyOrdLevel)
FacManyOrdLevel
FacManyCatLevel <- as.vector(FacManyCatLevel)
FacManyCatLevel
Fac2Level <- as.vector(Fac2Level)
Fac2Level


#Run TTests on each Indicator, and save results#
for(i in Fac2Level){
  
  assign(paste0(i,"_ttest"), 
         t.test(subset(MasterData_Adult, MasterData_Adult[,i] == levels(MasterData_Adult[,i])[1], select = TotalScoreHigh),
                subset(MasterData_Adult, MasterData_Adult[,i] == levels(MasterData_Adult[,i])[2], select = TotalScoreHigh)
         ))
}    

#Print a quick list of the names to quickly access all the results#
for(i in Fac2Level){
  zz <- print(as.name(paste0(i,"_ttest")))}

SexMF_ttest
Doc_ttest
Asthma_ttest
Fibromyalgia_ttest
Arthritis_ttest
HighBP_ttest
Diabetes_ttest
HeartDisease_ttest
MoodDisorder_ttest
Anxiety_ttest
CommunityBelong_ttest
IsoInjury_ttest
RepInjury_ttest
Injury_Comb_ttest


#Summarize the results in a table#
Indic2Level_ttestTable <-NULL
Indic2Level_ttestTable <- as.data.frame(Indic2Level_ttestTable)
tr_using <-NULL
tr_using <- as.data.frame(tr_using)

for(i in Fac2Level){
  
  tr <- t.test(subset(MasterData_Adult, MasterData_Adult[,i] == levels(MasterData_Adult[,i])[1], select = TotalScoreHigh),
               subset(MasterData_Adult, MasterData_Adult[,i] == levels(MasterData_Adult[,i])[2], select = TotalScoreHigh)
  )
  
  SigCheck <- as.numeric(tr$p.value) < 0.05
  
  if (tr$statistic <0 & SigCheck == TRUE) {
    VariableCompare <- "V1 has a significantly smaller proportion of individuals with healthy habits than V2"
    ResultSummary <- "V1 < V2"
  }  else if (tr$statistic >0 & SigCheck == TRUE){
    VariableCompare <- "V1 has a significantly larger proportion of individuals with healthy habits than V2"
    ResultSummary <- "V1 > V2"
  } else {
    VariableCompare <- "V1 & V2 do not significantly differ in the proportion of individuals with healthy habits"
    ResultSummary <- "V1 = V2"
  }
  
  tr_using[1,1] <- i
  tr_using[1,2] <- levels(MasterData_Adult[,i])[1]
  tr_using[1,3] <- percent(as.numeric(tr$estimate[1]))
  tr_using[1,4] <- levels(MasterData_Adult[,i])[2]
  tr_using[1,5] <- percent(as.numeric(tr$estimate[2]))
  tr_using[1,6] <- as.numeric(tr$p.value)
  tr_using[1,7] <- ResultSummary
  tr_using[1,8] <- VariableCompare
  
  
  Indic2Level_ttestTable <- rbind(Indic2Level_ttestTable, tr_using)
  tr_using <-NULL
  tr_using <- as.data.frame(tr_using)
}

names(Indic2Level_ttestTable) <- c("Indicator", "Var1", "%Healthy1", "Var2", "%Healthy2","p-Value", "ResultSummary", "ResultLong")


#Look at the summary table of results

View(Indic2Level_ttestTable)