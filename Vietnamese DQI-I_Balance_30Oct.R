setwd("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Balance")
Diet <- readRDS("Kyst5_FCT.rds")
names(Diet)
#Macronutrient ratio
#Compute total carb/protein/fat and total kcal for each fcode that individuals consume
#Total Energy
Diet$Quant.En <- round((Diet$Quantity * Diet$Energy)/100, digit= 2)
Energy <- aggregate(Diet$Quant.En ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Energy)
colnames(Energy)[colnames(Energy)=="Diet$INDI"] <- "INDI"
colnames(Energy)[colnames(Energy)=="Diet$District"] <- "District"
colnames(Energy)[colnames(Energy)=="Diet$Member"] <- "Member"
colnames(Energy)[colnames(Energy)=="Diet$Quant.En"] <- "Quant.En"

#Total Fat
Diet$Quant.Fat <- round((Diet$Quantity * Diet$Fat)/100, digit= 2)
Fat <- aggregate(Diet$Quant.Fat ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Fat)
colnames(Fat)[colnames(Fat)=="Diet$INDI"] <- "INDI"
colnames(Fat)[colnames(Fat)=="Diet$District"] <- "District"
colnames(Fat)[colnames(Fat)=="Diet$Member"] <- "Member"
colnames(Fat)[colnames(Fat)=="Diet$Quant.Fat"] <- "Quant.Fat"

#Convert gram to kcal
Fat$Quant.Fat.kcal <- Fat$Quant.Fat * 9

#Total Carb
Diet$Quant.Carb <- round((Diet$Quantity * Diet$Carb)/100, digit= 2)
Carb <- aggregate(Diet$Quant.Carb ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Carb)
colnames(Carb)[colnames(Carb)=="Diet$INDI"] <- "INDI"
colnames(Carb)[colnames(Carb)=="Diet$District"] <- "District"
colnames(Carb)[colnames(Carb)=="Diet$Member"] <- "Member"
colnames(Carb)[colnames(Carb)=="Diet$Quant.Carb"] <- "Quant.Carb"

#Convert gram to kcal
Carb$Quant.Carb.kcal <- Carb$Quant.Carb * 4

#Total Protein
Diet$Quant.Protein <- round((Diet$Quantity * Diet$Prot)/100, digit= 2)
Protein <- aggregate(Diet$Quant.Protein ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Protein)
colnames(Protein)[colnames(Protein)=="Diet$INDI"] <- "INDI"
colnames(Protein)[colnames(Protein)=="Diet$District"] <- "District"
colnames(Protein)[colnames(Protein)=="Diet$Member"] <- "Member"
colnames(Protein)[colnames(Protein)=="Diet$Quant.Protein"] <- "Quant.Protein"

#Convert gram to kcal
Protein$Quant.Protein.kcal <- Protein$Quant.Protein * 4

library(plyr)

Balance <- join_all(list(Energy, Carb, Protein, Fat), by=c("INDI", "District", "Member"), type='left')

detach("package:plyr", unload=TRUE)

names(Balance)
Balance <- Balance %>% select(INDI:Quant.En, Quant.Carb.kcal, Quant.Protein.kcal, Quant.Fat.kcal)

Balance$Sum.kcal <- Balance$Quant.Carb.kcal + Balance$Quant.Protein.kcal + Balance$Quant.Fat.kcal
#Calculate the % of each macronutrient/total kcal
Balance$rate.Carb <- round((Balance$Quant.Carb.kcal/Balance$Sum.kcal)*100, digits = 2)
Balance$rate.Pro <- round((Balance$Quant.Protein.kcal/Balance$Sum.kcal)*100, digits = 2)
Balance$rate.Fat <- round((Balance$Quant.Fat.kcal/Balance$Sum.kcal)*100, digits = 2)

#Assign score 
Balance$Score_original <- ifelse(Balance$rate.Carb >=55 & Balance$rate.Carb <=65 & Balance$rate.Pro >=10 & Balance$rate.Pro <=15 & Balance$rate.Fat >=15 & Balance$rate.Pro <=25, 6, ifelse(Balance$rate.Carb >=52 & Balance$rate.Carb <=68 & Balance$rate.Pro >=9 & Balance$rate.Pro <=16 & Balance$rate.Fat >=13 & Balance$rate.Pro <=27, 4,ifelse(Balance$rate.Carb >=50 & Balance$rate.Carb <=70 & Balance$rate.Pro >=8 & Balance$rate.Pro <=17 & Balance$rate.Fat >=12 & Balance$rate.Pro <=30, 2, 0)))
Balance$Score_modified <- ifelse(Balance$rate.Carb >=55 & Balance$rate.Carb <=65 & Balance$rate.Pro >=13 & Balance$rate.Pro <=20 & Balance$rate.Fat >=20 & Balance$rate.Pro <=25, 6, ifelse(Balance$rate.Carb >=50 & Balance$rate.Carb <=68 & Balance$rate.Pro >=12 & Balance$rate.Pro <=30 & Balance$rate.Fat >=13 & Balance$rate.Pro <=27, 4,ifelse(Balance$rate.Carb >=45 & Balance$rate.Carb <=70 & Balance$rate.Pro >=10 & Balance$rate.Pro <=35 & Balance$rate.Fat >=12 & Balance$rate.Pro <=30, 2, 0)))

hist(Balance$Score_modified)

colnames(Balance)[colnames(Balance)=="Score_modified"] <- "Score_Macro"

Balance.full <- Balance

Balance <- Balance.full %>% filter(Member == 1| Member == 2)

Balance <- Balance %>% select(INDI:Member, Score_Macro)

#Fatty acid ratio
#SFA
Diet$Quant.SFA <- round((Diet$Quantity * Diet$SFA)/100, digit= 2)
SFA <- aggregate(Diet$Quant.SFA ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(SFA)
colnames(SFA)[colnames(SFA)=="Diet$INDI"] <- "INDI"
colnames(SFA)[colnames(SFA)=="Diet$District"] <- "District"
colnames(SFA)[colnames(SFA)=="Diet$Member"] <- "Member"
colnames(SFA)[colnames(SFA)=="Diet$Quant.SFA"] <- "Quant.SFA"

#PUFA
Diet$Quant.PUFA <- round((Diet$Quantity * Diet$PUFA_FA)/100, digit= 2)
PUFA <- aggregate(Diet$Quant.PUFA ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(PUFA)
colnames(PUFA)[colnames(PUFA)=="Diet$INDI"] <- "INDI"
colnames(PUFA)[colnames(PUFA)=="Diet$District"] <- "District"
colnames(PUFA)[colnames(PUFA)=="Diet$Member"] <- "Member"
colnames(PUFA)[colnames(PUFA)=="Diet$Quant.PUFA"] <- "Quant.PUFA"

#MUFA
Diet$Quant.MUFA <- round((Diet$Quantity * Diet$MUFA_FA)/100, digit= 2)
MUFA <- aggregate(Diet$Quant.MUFA ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(MUFA)
colnames(MUFA)[colnames(MUFA)=="Diet$INDI"] <- "INDI"
colnames(MUFA)[colnames(MUFA)=="Diet$District"] <- "District"
colnames(MUFA)[colnames(MUFA)=="Diet$Member"] <- "Member"
colnames(MUFA)[colnames(MUFA)=="Diet$Quant.MUFA"] <- "Quant.MUFA"

#Combine all fatty acids:
library(plyr)

Fatty.Acid <- join_all(list(SFA, PUFA, MUFA), by=c("INDI", "District", "Member"), type='left')

detach("package:plyr", unload=TRUE)

#Compute P/S and M/S

summary(Fatty.Acid)

Fatty.Acid$P_S <- round(Fatty.Acid$Quant.PUFA/Fatty.Acid$Quant.SFA, digits = 1)
Fatty.Acid$M_S <- round(Fatty.Acid$Quant.MUFA/Fatty.Acid$Quant.SFA, digits = 1)

#Assign score 
Fatty.Acid$Score.FA <- ifelse(Fatty.Acid$P_S >= 1 & Fatty.Acid$P_S <= 1.5 & Fatty.Acid$M_S >= 1 & Fatty.Acid$M_S <= 1.5, 4, ifelse(Fatty.Acid$P_S >= 0.8 & Fatty.Acid$P_S <= 1.7 & Fatty.Acid$M_S >= 0.8 & Fatty.Acid$M_S <= 1.7, 2, 0))

names(Fatty.Acid)

Fatty.Acid.adult <- Fatty.Acid %>% filter(Member == 1| Member ==2)

Fatty.Acid.adult <- Fatty.Acid.adult %>% select(INDI:Member, Score.FA)

#Combine macronutrient ratio and fatty acid ratio scores together
Balance <- left_join(Balance, Fatty.Acid.adult)

summary(Balance)

Balance$tot.Balance <- Balance$Score_Macro + Balance$Score.FA

write_rds(Balance, "Balance.rds")
write_csv(Balance, "Balance.csv")






