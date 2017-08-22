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


save(ProvinceMeanTable, file = "ProvinceMeanTable.Rdata")

##AGE vs CANADA MEAN - TTEST TABLE##

#1
#Var = AgeGroup (More Groups)

AgeMeanTable_more <- NULL
AgeMeanTable_more <- as.data.frame(AgeMeanTable_more)
for (i in 1:length(levels(MasterData_Adult$AgeGroup))) {
  
  T <-  t.test(MasterData_Adult$TotalScoreHigh[MasterData_Adult$AgeGroup == levels(MasterData_Adult$AgeGroup)[i]], mu=MeanScoreHigh, sigma = sdScoreHigh)
  
  SigCheck <- as.numeric(T$p.value) < 0.05
  
  if (T$statistic >0) {
    AgeCanCompare <- "Healthier than population"
  }  else {AgeCanCompare <- "Less Healthy than population"
  }
  
  AgeMeanTable_more[i,1] <- as.character(levels(MasterData_Adult$AgeGroup)[i])
  AgeMeanTable_more[i,2] <- percent(as.numeric(T$estimate))
  AgeMeanTable_more[i,3] <- as.numeric(T$p.value)
  AgeMeanTable_more[i,4] <- SigCheck
  AgeMeanTable_more[i,5] <- AgeCanCompare
  AgeMeanTable_more[i,6] <- percent(T$conf.int[1])
  AgeMeanTable_more[i,7] <- percent(T$conf.int[2])
  
}

names(AgeMeanTable_more) <- c("AgeGroup", "%Healthy", "P", "Significant", "CanadaCompare", "Lower 95", "Upper 95")


save(AgeMeanTable_more, file = "AgeMeanTable_more.Rdata")

#2
#Var = AgeGroup2 (Less Groups)

AgeMeanTable_less <- NULL
AgeMeanTable_less <- as.data.frame(AgeMeanTable_less)
for (i in 1:length(levels(MasterData_Adult$AgeGroup2))) {
  
  T <-  t.test(MasterData_Adult$TotalScoreHigh[MasterData_Adult$AgeGroup2 == levels(MasterData_Adult$AgeGroup2)[i]], mu=MeanScoreHigh, sigma = sdScoreHigh)
  
  SigCheck <- as.numeric(T$p.value) < 0.05
  
  if (T$statistic >0) {
    AgeCanCompare <- "Healthier than population"
  }  else {AgeCanCompare <- "Less Healthy than population"
  }
  
  AgeMeanTable_less[i,1] <- as.character(levels(MasterData_Adult$AgeGroup2)[i])
  AgeMeanTable_less[i,2] <- percent(as.numeric(T$estimate))
  AgeMeanTable_less[i,3] <- as.numeric(T$p.value)
  AgeMeanTable_less[i,4] <- SigCheck
  AgeMeanTable_less[i,5] <- AgeCanCompare
  AgeMeanTable_less[i,6] <- percent(T$conf.int[1])
  AgeMeanTable_less[i,7] <- percent(T$conf.int[2])
  
}

names(AgeMeanTable_less) <- c("AgeGroup", "%Healthy", "P", "Significant", "CanadaCompare", "Lower 95", "Upper 95")


save(AgeMeanTable_less, file = "AgeMeanTable_less.Rdata")


## Compare group means for 2 level variables - TTEST TABLE##

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

names(Indic2Level_ttestTable) <- c("Indicator", "Group1", "%Healthy1", "Group2", "%Healthy2","p-Value", "ResultSummary", "ResultLong")


#Look at the summary table of results

View(Indic2Level_ttestTable)


#Save results

save(Indic2Level_ttestTable, file = "Indic2Level_ttestTable.Rdata")