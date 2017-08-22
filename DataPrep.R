RawData_FULL <- read_csv("C:/Users/User/Dropbox/Capstone/cchs-82M0013-E-2014-Annual-component/cchs-82M0013-E-2014-Annual-component_F1.csv")

CCHS_2014_Just_Variable_Names <- read_csv("C:/Users/User/Dropbox/Capstone/CCHS_2014_Just_Variable_Names.csv")

CCHS_2014_Index_NoDup <- read_csv("C:/Users/User/Dropbox/Capstone/CCHS_2014_Index_NoDup.csv")

Indicator_List <- merge(x = CCHS_2014_Just_Variable_Names, y = CCHS_2014_Index_NoDup, by.x = "VarName", by.y = "VName", all.x = TRUE)

View(Indicator_List)


Indicator_List[163,] <- Indicator_List[1,]
Indicator_List[1:162,] <- Indicator_List[2:163,]
Indicator_List <- Indicator_List[1:162,]

View(Indicator_List)

#(Variables_Using vectored in other page - 1 method)

Variables_Using <- sapply(CCHS_2014_Just_Variable_Names[,1], as.character)

CCHS_Only_IndList <- RawData_FULL[,as.vector(Variables_Using)]
View(CCHS_Only_IndList)

write.csv(CCHS_Only_IndList, file = "RawData_CCHS_Filtered_162.csv", row.names = FALSE)
write.csv(Indicator_List, file = "Variables_CCHS_Filtered_162.csv", row.names = FALSE)


VarList <- Indicator_List
RawData <- CCHS_Only_IndList
head(VarList)

RawData_Labeled <- RawData
VarList[162,"Label"] = "ID"
names(RawData_Labeled)<-VarList$Label[match(names(RawData_Labeled),VarList$VarName)] 

#sections <- as.character(unique(VarList$Section))

Alc_RawData <- RawData_Labeled[,c(162,1:15)]
Chronic_RawData <- RawData_Labeled[,c(162,16:24)]
Gen_RawData <- RawData_Labeled[,c(162,25:26, 56)]
House_RawData <- RawData_Labeled[,c(162,27:33)]
Edu_RawData <- RawData_Labeled[,c(162,34:36)]
Smoke_RawData <- RawData_Labeled[,c(162,37:39, 159:161)]
Flu_RawData <- RawData_Labeled[,c(162,40:43)]
Veg_RawData <- RawData_Labeled[,c(162,44:45)]
Self_RawData <- RawData_Labeled[,c(162,46:53, 68:69)]
Loc_RawData <- RawData_Labeled[,c(162,54:55)]
BMI_RawData <- RawData_Labeled[,c(162,58:64)]
Inj_RawData <- RawData_Labeled[,c(162,57,65:67, 158)]
Active_RawData <- RawData_Labeled[,c(162,70:157)]

##Alcohol##
Alc_RawData <- RawData_Labeled[,c(162,1:15)]

head(Alc_RawData)
str(Alc_RawData)
Alc_RawData <- as.data.frame(lapply(Alc_RawData,as.factor))

Alc_RawData$ID <- as.character(Alc_RawData$ID)
#Alc_RawData[1] <- lapply(Alc_RawData[1],as.character)
str(Alc_RawData)

#levels(Alc_RawData$`Drank alcohol - past 12 months`) <- c(1:3, 3:4)
#levels(Alc_RawData$`Frequency of drinking alcohol`) <- c('ONCE A MONTH', 'ONCE A MONTH', '2 TO 3 TIMES/MO', 'ONCE/WEEK', '2 TO 3 TIMES/WK', '4 TO 6 TIMES/WK', 'EVERY DAY', 'NOT APPLICABLE', 'DONT KNOW', 'REFUSAL', 'NOT STATED')

Alc_RawData$HeavyDrinker <- Alc_RawData$Frequency.of.drinking.4.female...5.male.or.more.drinks
#levels(Alc_RawData$HeavyDrinker) <-c( rep('No',2), rep('Yes', 4), 'No', rep(NA, 3))
levels(Alc_RawData$HeavyDrinker) <- list("No" = c("1", "2","96"),
                                         "Yes" = 3:6)  
str(Alc_RawData)



##DIET##
Veg_RawData <- RawData_Labeled[,c(162,44:45)]
Veg_RawData$ID <- as.character(Veg_RawData$ID)

str(Veg_RawData)
summary(Veg_RawData)

Veg_RawData <- cbind(Veg_RawData, RawData_FULL[,c("FVCDCAR", "FVCDFRU", "FVCDJUI", "FVCDPOT", "FVCDSAL", "FVCDVEG")])

Veg_RawData <- Veg_RawData[,c(1,4:9, 2:3)]

Veg_RawData <- as.data.frame(Veg_RawData)



Veg_RawData[Veg_RawData > 999 & Veg_RawData< 1000] <- NA
Veg_RawData$VFDaily_withNA <- rowSums(Veg_RawData[,2:7], na.rm = TRUE)
Veg_RawData$NACount <- rowSums(as.data.frame(lapply(lapply(Veg_RawData, is.na), as.numeric)))

Veg_RawData$VFDaily_withNA[Veg_RawData$NACount > 4 & Veg_RawData$VFDaily_withNA < 5] <- NA


str(Veg_RawData)

Veg_RawData$VegFruitDaily <- factor(as.factor(Veg_RawData$VFDaily_withNA < 5),
                                   levels = c(TRUE, FALSE),
                                   labels = c('< 5','5 OR MORE'),
                                   ordered = TRUE)

str(Veg_RawData)
summary(Veg_RawData)


##SMOKING##

str(Smoke_RawData)
summary(Smoke_RawData)
Smoke_RawData <- as.data.frame(lapply(Smoke_RawData, as.factor))
Smoke_RawData$ID <- as.character(Smoke_RawData$ID)
Smoke_RawData$Smoker <- droplevels(factor(Smoke_RawData$Type.of.smoker,
                                          levels = c(1, 2, 3),
                                          labels = c('Smoker', 'smoker','Non Smoker'),
                                          ordered = FALSE))

##PHYSICAL ACTIVITY##

str(Active_RawData)
summary(Active_RawData)

Active_RawData <- as.data.frame(Active_RawData)
Active_RawData$ID <- as.character(Active_RawData$ID)

#3.0 kcal/kg/day or more = physically active
#1.5 to 2.9 kcal/kg/day = moderately active
#less than 1.5 kcal/kg/day = inactive.

Active_RawData$PhysicallyActive[Active_RawData$`Daily energy expenditure - (D)`< 1.5] <- "Inactive"
Active_RawData$PhysicallyActive[Active_RawData$`Daily energy expenditure - (D)`>= 1.5] <- "Active"
Active_RawData$PhysicallyActive[Active_RawData$`Daily energy expenditure - (D)`> 99] <- NA

Active_RawData$PhysicallyActive <- as.factor(Active_RawData$PhysicallyActive)


##sCORING HEALTH LifeStyle Choices##


#HealthHabits <- merge.data.frame(Active_RawData, Smoke_RawData, Alc_RawData, Gen_RawData, Veg_RawData,
#                                     by = "ID")

HealthHabits <- Reduce(function(...) merge(..., by= "ID"),
                            list(Active_RawData, Smoke_RawData, Alc_RawData, Veg_RawData))

str(HealthHabits)


HealthHabits <- HealthHabits[c("ID", "HeavyDrinker", "VegFruitDaily", "Smoker", "PhysicallyActive")]

HealthHabits$AlcScore <-as.numeric(HealthHabits$HeavyDrinker == "No")
HealthHabits$DietScore <-as.numeric(HealthHabits$VegFruitDaily == "5 OR MORE")
HealthHabits$SmokeScore <-as.numeric(HealthHabits$Smoker == "Non Smoker")
HealthHabits$ExerciseScore <-as.numeric(HealthHabits$PhysicallyActive == "Active")


HealthHabits$TotalScore <- rowSums(HealthHabits[c("AlcScore","DietScore","SmokeScore","ExerciseScore")])

HH_noNA <- HealthHabits
#NACheck <- is.na(HH_noNA$TotalScore)
HH_noNA <- HH_noNA[!is.na(HH_noNA$TotalScore),]

anyNA(HH_noNA)

HH_noNA$OverallScore <- factor(as.factor(HH_noNA$TotalScore), ordered = TRUE)

summary(HH_noNA)


##General Info##


View(Gen_RawData)
summary(Gen_RawData)
Gen_RawData <- as.data.frame(lapply(Gen_RawData, as.factor))
Gen_RawData$ID <- as.character(Gen_RawData$ID)

str(Gen_RawData)

Gen_RawData$AgeGroup <-factor(Gen_RawData$Age....G.,
                              labels = c('12 TO 14 YEARS', '15 TO 17 YEARS', '18 TO 19 YEARS', '20 TO 24 YEARS', '25 TO 29 YEARS', '30 TO 34 YEARS', '35 TO 39 YEARS', '40 TO 44 YEARS', '45 TO 49 YEARS', '50 TO 54 YEARS', '55 TO 59 YEARS', '60 TO 64 YEARS', '65 TO 69 YEARS', '70 TO 74 YEARS', '75 TO 79 YEARS', '80 YEARS OR MORE'),
                              ordered  = TRUE)


Gen_RawData$AgeGroup2 <- Gen_RawData$AgeGroup
levels(Gen_RawData$AgeGroup2) <- list('12 TO 17 YEARS' = c('12 TO 14 YEARS', '15 TO 17 YEARS'),
                                      '18 TO 34 YEARS' = c('18 TO 19 YEARS', '20 TO 24 YEARS', '25 TO 29 YEARS', '30 TO 34 YEARS'),
                                      '35 TO 49 YEARS' = c('35 TO 39 YEARS', '40 TO 44 YEARS', '45 TO 49 YEARS'),
                                      '50 TO 64 YEARS' = c('50 TO 54 YEARS', '55 TO 59 YEARS', '60 TO 64 YEARS'),
                                      '65 YEARS OR MORE' = c('65 TO 69 YEARS', '70 TO 74 YEARS', '75 TO 79 YEARS', '80 YEARS OR MORE'))





Gen_RawData$SexMF <- Gen_RawData$Sex
#levels(Gen_RawData$SexMF) <- c('M','F')
levels(Gen_RawData$SexMF) <- list('M' = 1, 'F' = 2)
str(Gen_RawData)

#Gen_RawData$Doc <- Gen_RawData$Has.regular.medical.doctor
#levels(Gen_RawData$Doc) <- c(1:2, rep(NA, 2))
#str(Gen_RawData)

Gen_RawData$Doc <- Gen_RawData$Has.regular.medical.doctor
#levels(Gen_RawData$Doc) <- c('Yes', 'No', rep(NA, 2))
levels(Gen_RawData$Doc) <- list('No' = 2,
                                'Yes' = 1)
str(Gen_RawData)


##Chronic Disease##

View(Chronic_RawData)
Chronic_RawData <- as.data.frame(lapply(Chronic_RawData, as.factor))
Chronic_RawData$ID <- as.character(Chronic_RawData$ID)

summary(Chronic_RawData)

Chronic_RawData$Asthma <- factor(Chronic_RawData$Has.asthma,
                                 levels = c(2, 1),
                                 labels = c("No", "Yes"))

Chronic_RawData$Fibromyalgia <- factor(Chronic_RawData$Has.fibromyalgia,
                                 levels = c(2, 1),
                                 labels = c("No", "Yes"))

Chronic_RawData$Arthritis <- factor(Chronic_RawData$Has.arthritis,
                                 levels = c(2, 1),
                                 labels = c("No", "Yes"))

Chronic_RawData$HighBP <- factor(Chronic_RawData$Has.high.blood.pressure,
                                 levels = c(2, 1),
                                 labels = c("No", "Yes"))

Chronic_RawData$Diabetes <- factor(Chronic_RawData$Has.diabetes,
                                 levels = c(2, 1),
                                 labels = c("No", "Yes"))


Chronic_RawData$HeartDisease <- factor(Chronic_RawData$Has.heart.disease,
                                   levels = c(2, 1),
                                   labels = c("No", "Yes"))

Chronic_RawData$MoodDisorder <- factor(Chronic_RawData$Has.a.mood.disorder,
                                   levels = c(2, 1),
                                   labels = c("No", "Yes"))

Chronic_RawData$Anxiety <- factor(Chronic_RawData$Has.an.anxiety.disorder,
                                       levels = c(2, 1),
                                       labels = c("No", "Yes"))



##House##
View(House_RawData)
House_RawData <- as.data.frame(House_RawData)

summary(House_RawData)
str(House_RawData)

House_RawData$RelStatus <- factor(House_RawData$`Marital status - (G)`,
                                  levels = 1:4,
                                  labels = c("MARRIED", "COMMON-LAW", "WIDOW/SEP/DIV", "NEVER MARRIED"))


House_RawData$HousePeople <- factor(House_RawData$`Household size - (D, G)`,
                                  levels = 1:5,
                                  labels = c("1", "2", "3", "4", "5 or More"),
                                  ordered = TRUE)

House_RawData$TopHouseEdu <- factor(House_RawData$`Highest level of education - household 4 levels - (D)`,
                                    levels = 1:4,
                                    labels = c("< THAN SECONDARY GRADUATE", "SECONDARY GRADUATE", "SOME POST-SECONDARY", "POST-SECONDARY GRADUATE"),
                                    ordered = TRUE)


##Education##

Edu_RawData <- RawData_Labeled[,c(162,34:36)]


View(Edu_RawData)
Edu_RawData <- as.data.frame(lapply(Edu_RawData, as.factor))
Edu_RawData$ID <- as.character(Edu_RawData$ID)

summary(Edu_RawData)
str(Edu_RawData)

Edu_RawData$EduLevel <- factor(Edu_RawData$Highest.level.of.education...respondent.4.levels...D.,
                                    levels = 1:4,
                                    labels = c("< THAN SECONDARY GRADUATE", "SECONDARY GRADUATE", "SOME POST-SECONDARY", "POST-SECONDARY CERTIFICATION"),
                                    ordered = TRUE)

##sELF##

Self_RawData <- RawData_Labeled[,c(162,46:53, 68:69)]


View(Self_RawData)
Self_RawData <- as.data.frame(lapply(Self_RawData, as.factor))
Self_RawData$ID <- as.character(Self_RawData$ID)

summary(Self_RawData)
str(Self_RawData)

Self_RawData$PerceivedHealth <- droplevels(factor(Self_RawData$Self.perceived.health,
                                                  levels = 5:1,
                                                  labels = c("Low", "Low", "Medium", "High", "High"),
                                                  ordered = TRUE))

Self_RawData$MentalHealth <- droplevels(factor(Self_RawData$Self.perceived.mental.health,
                                                  levels = 5:1,
                                                  labels = c("Low", "Low", "Medium", "High", "High"),
                                                  ordered = TRUE))

Self_RawData$LifeSatisfaction <- factor(Self_RawData$Satisfaction.with.life.in.general,
                                        ordered = TRUE)

levels(Self_RawData$LifeSatisfaction) <- list("Low" = 0:3,
                                              "Medium" = 4:6,
                                              "High" = 7:10)


Self_RawData$Stress <- factor(Self_RawData$Perceived.life.stress,
                                        ordered = TRUE)

levels(Self_RawData$Stress) <- list("Low" = 1:2,
                                    "Medium" = 3,
                                    "High" = 4:5)

Self_RawData$CommunityBelong <- factor(Self_RawData$Sense.of.belonging.to.local.community,
                              ordered = TRUE)

levels(Self_RawData$CommunityBelong) <- list("Weak" = 3:4,
                                             "Strong" = 1:2)
Self_RawData$OralHealth <- factor(Self_RawData$Self.perceived.health.of.teeth.and.mouth,
                                  levels = 5:1,
                                  labels = c("POOR", "FAIR", "GOOD", "VERY GOOD", "EXCELLENT"),
                                  ordered = TRUE)
##BMI##

BMI_RawData <- RawData_Labeled[,c(162,58:64)]

summary(BMI_RawData)
str(BMI_RawData)


BMI_RawData$BMI_Adult <- factor(BMI_RawData$`BMI class. (18 +) / self- report - Intern. standard (D,G)`,
                                levels = 1:4,
                                labels = c("UNDERWEIGHT", "NORMAL WEIGHT", "OVERWEIGHT", "OBESE"),
                                ordered = TRUE)

BMI_RawData$BMI_Youth <- factor(BMI_RawData$`Youth BMI / self-report - (D)`,
                                levels = 1:4,
                                labels = c("Neither Nor", "Overweight", "Obese", "Severely Obese"),
                                ordered = TRUE)


BMI_RawData$BMI_Adult_Comb <- droplevels(factor(BMI_RawData$`BMI class. (18 +) / self- report - Intern. standard (D,G)`,
                                                levels = c(1:4,6, 9),
                                                labels = c("Not Overweight", "Not Overweight", "Overweight", "Obese", "Youth", "UNK")))

BMI_RawData$BMI_Youth_Comb <- droplevels(factor(BMI_RawData$`Youth BMI / self-report - (D)`,
                                                levels = c(1:4,9),
                                                labels = c("Not Overweight", "Overweight", "Obese", "Obese", "UNK")))


BMI_RawData$BMI_Comb <- BMI_RawData$BMI_Adult_Comb

BMI_RawData$BMI_Comb[BMI_RawData$BMI_Adult_Comb == "Youth"] <- BMI_RawData$BMI_Youth_Comb[BMI_RawData$BMI_Adult_Comb == "Youth"]

BMI_RawData$BMI_Comb <- factor(BMI_RawData$BMI_Comb,
                               levels = c("Not Overweight", "Overweight", "Obese"),
                               ordered = TRUE)

BMI_RawData$BMI_Raw <- BMI_RawData$`BMI / self-report - (D,G)`
BMI_RawData$BMI_Raw[BMI_RawData$BMI_Raw > 999 & BMI_RawData$BMI_Raw < 1000] <- NA


##LOCATION##

Loc_RawData <- RawData_Labeled[,c(162,54:55)]

summary(Loc_RawData)

Loc_RawData <- as.data.frame(lapply(Loc_RawData, as.factor))

HealthRegions <- read_csv("C:/Users/User/Dropbox/Capstone/HealthRegions.csv")
ProvinceIndex <- read_csv("C:/Users/User/Dropbox/Capstone/Province_Index.csv")

Loc_RawData$HealthReg<-HealthRegions$Label[match(Loc_RawData$Health.Region....G.,HealthRegions$Value)]

Loc_RawData$Province<-ProvinceIndex$Label[match(Loc_RawData$Province.of.residence.of.respondent..G.,ProvinceIndex$Value)]

Loc_RawData <- as.data.frame(lapply(Loc_RawData, as.factor))
Loc_RawData$ID <- as.character(Loc_RawData$ID)

summary(Loc_RawData)


##INJURY##

Inj_RawData <- RawData_Labeled[,c(162,57,65:67, 158)]
str(Inj_RawData)

Inj_RawData <- as.data.frame(lapply(Inj_RawData, as.factor))
Inj_RawData$ID <- as.character(Inj_RawData$ID)

Inj_RawData$IsoInjury <- factor(Inj_RawData$Injured.in.past.12.months,
                                  levels = 2:1,
                                  labels = c("No", "Yes"),
                                  ordered = FALSE)

Inj_RawData$RepInjury <- factor(Inj_RawData$Repetitive.strain.injury,
                               levels = 2:1,
                               labels = c("No", "Yes"),
                               ordered = FALSE)

Inj_RawData$Injury_Comb <- Inj_RawData$IsoInjury

Inj_RawData$Injury_Comb[Inj_RawData$Repetitive.strain.injury == 1] <- "Yes"


summary(Inj_RawData)


##Combine##

CombinedData <- Reduce(function(...) merge(..., by= "ID", all = FALSE),
                           list(HH_noNA, Gen_RawData, Chronic_RawData, House_RawData, Edu_RawData, Self_RawData, BMI_RawData, Loc_RawData, Inj_RawData))

summary(CombinedData)
View(CombinedData)

n <- names(CombinedData)
n

MasterData <- CombinedData[,c(1:11, 15:18, 28:35, 43:45, 49, 60:65, 73:78, 81:82, 88:90)]

#TESTING#

MasterData_tst <- MasterData

str(MasterData_tst)
summary(MasterData_tst)

MasterData_tst$TotalScoreHigh <- MasterData_tst$TotalScore
MasterData_tst$TotalScoreHigh[MasterData_tst$TotalScore < 3] <- 0
MasterData_tst$TotalScoreHigh[MasterData_tst$TotalScore >= 3] <- 1
MasterData_tst$TotalScoreHigh <- as.numeric(MasterData_tst$TotalScoreHigh)
MasterData_tst$HealthHabitResult <- factor(MasterData_tst[,length(MasterData_tst)],
                                             labels = c("Unhealthy", "Healthy"))

str(MasterData_tst)

MasterData_Adult <- droplevels(MasterData_tst[MasterData_tst$AgeGroup2 > "12 TO 17 YEARS",])
str(MasterData_Adult)
