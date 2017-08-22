MasterData_VarSel <- MasterData_Adult[c(12:32, 34,38: 44,10:11,45:46)]

sum(complete.cases(MasterData_VarSel))

MasterData_VarSel_Clean <- MasterData_VarSel[complete.cases(MasterData_VarSel),]

MasterData_VarSel_Clean_1 <- MasterData_VarSel_Clean[-c(30:32)]
MasterData_VarSel_Clean_2 <- MasterData_VarSel_Clean[-c(31:33)]


boruta1 <- Boruta(HealthHabitResult~., data = MasterData_VarSel_Clean_1, doTrace = 2)

boruta2 <- Boruta(TotalScore~., data = MasterData_VarSel_Clean_2, doTrace = 2)


save(boruta1, file = "boruta2.Rdata")

print(boruta1)

plot(boruta1, xlab = "", xaxt = "n")
k <-lapply(1:ncol(boruta2$ImpHistory),function(i)
  boruta2$ImpHistory[is.finite(boruta2$ImpHistory[,i]),i])
names(k) <- colnames(boruta2$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta2$ImpHistory), cex.axis = 0.7)


save(boruta2, file = "boruta2.Rdata")

print(boruta2)

plot(boruta2, xlab = "", xaxt = "n")
k <-lapply(1:ncol(boruta2$ImpHistory),function(i)
  boruta2$ImpHistory[is.finite(boruta2$ImpHistory[,i]),i])
names(k) <- colnames(boruta2$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta2$ImpHistory), cex.axis = 0.7)


##



data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type = "raw")

pred <- predict(glm_1, MasterData_h2o[c(myX, "HealthHabitResult")])



model <- naiveBayes(x = MasterData_h2o[myX], y = MasterData_h2o$HealthHabitResult,)



laplace = 0, ...


(y = "TotalScore", x = myX, training_frame = MasterData_h2odf,
  
  MasterData_h2o)

y<-MasterData_Adult$TotalScore/4

divide             
summary(glm(MasterData_Adult$TotalScoreHigh ~ OH, family = binomial()))
MasterData_Adult$OralHealth, MasterData_Adult$TotalScoreHigh, family = binomial())




summary(MasterData_Adult$OralHealth)