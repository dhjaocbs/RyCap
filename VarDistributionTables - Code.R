MasterData_Aggreg <- MasterData_Adult[c(12:34, 38, 40:44, 10:11, 45:46)]


VarDistTable_4Scores <- NULL
VarDistTable_4Scores <- as.data.frame(VarDistTable_4Scores)

for (k in 1:(length(MasterData_Aggreg)-4)){
  bb <- aggregate(x = list(Freq = MasterData_Aggreg$OverallScore),
                  by = list(Score = MasterData_Aggreg$OverallScore, 
                            Response = MasterData_Aggreg[[k]]), 
                  FUN = length)
  
  bb$Variable <- names(MasterData_Aggreg[k])
  
  
  for(i in 1:nlevels(bb[["Response"]])){
    for(j in 1:nrow(bb)){
      
      if (as.character(bb[j,"Response"]) == levels(bb[["Response"]])[i]){
        bb$Rel[j] <- bb[j,"Freq"]/sum(bb$Freq[bb$Response == levels(bb[["Response"]])[i]])
      }
    }
  }
  bb$Percent <- percent(bb$Rel)
  VarDistTable_4Scores <- rbind(VarDistTable_4Scores, bb)
}


VarDistTable_Binom <- NULL
VarDistTable_Binom <- as.data.frame(VarDistTable_Binom)

for (k in 1:(length(MasterData_Aggreg)-4)){
  bb <- aggregate(x = list(Freq = MasterData_Aggreg$HealthHabitResult),
                  by = list(Score = MasterData_Aggreg$HealthHabitResult, 
                            Variable = MasterData_Aggreg[[k]]), 
                  FUN = length)
  
  bb$Attr <- names(MasterData_Aggreg[k])
  
  
  for(i in 1:nlevels(bb[["Variable"]])){
    for(j in 1:nrow(bb)){
      
      if (as.character(bb[j,"Variable"]) == levels(bb[["Variable"]])[i]){
        bb$Rel[j] <- bb[j,"Freq"]/sum(bb$Freq[bb$Variable == levels(bb[["Variable"]])[i]])
      }
    }
  }
  bb$Percent <- percent(bb$Rel)
  VarDistTable_Binom <- rbind(VarDistTable_Binom, bb)
}

VarDistTable_Healthy <- VarDistTable_Binom[VarDistTable_Binom$Score == "Healthy",]