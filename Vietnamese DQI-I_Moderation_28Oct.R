setwd("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Moderation")
Diet <- readRDS("Kyst5_FCT.rds")

#Total Energy

Diet$Quant.En <- round((Diet$Quantity * Diet$Energy)/100, digit= 2)
Energy <- aggregate(Diet$Quant.En ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Energy)
colnames(Energy)[colnames(Energy)=="Diet$INDI"] <- "INDI"
colnames(Energy)[colnames(Energy)=="Diet$District"] <- "District"
colnames(Energy)[colnames(Energy)=="Diet$Member"] <- "Member"
colnames(Energy)[colnames(Energy)=="Diet$GLCA"] <- "GLCA"
colnames(Energy)[colnames(Energy)=="Diet$Quant.En"] <- "Quant.En"

#Total Fat

Diet$Quant.Fat <- round((Diet$Quantity * Diet$Fat)/100, digit= 2)
Fat <- aggregate(Diet$Quant.Fat ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Fat)
colnames(Fat)[colnames(Fat)=="Diet$INDI"] <- "INDI"
colnames(Fat)[colnames(Fat)=="Diet$District"] <- "District"
colnames(Fat)[colnames(Fat)=="Diet$Member"] <- "Member"
colnames(Fat)[colnames(Fat)=="Diet$GLCA"] <- "GLCA"
colnames(Fat)[colnames(Fat)=="Diet$Quant.Fat"] <- "Quant.Fat"

Fat <- left_join(Fat, Energy)
#Convert gram to kcal
Fat$Quant.Fat.kcal <- Fat$Quant.Fat * 9
Fat$Percent <- round((Fat$Quant.Fat.kcal/Fat$Quant.En)*100, digit = 2)
summary(Fat$Percent)

#Assign score for Fat: <=20% energy: 6, > 20 - 30%: 3, >30%: 0
Fat$score <- ifelse(Fat$Percent <= 20, 6, ifelse(Fat$Percent > 20 & Fat$Percent <= 30, 3, 0))
Fat.adult <- Fat %>% filter(Fat$Member == "1" | Fat$Member == "2")

names(Fat.adult)
Fat.adult <- Fat.adult %>% select(INDI:Member, score)
colnames(Fat.adult)[colnames(Fat.adult)=="score"] <- "Total Fat"

#Saturated Fat
names(Diet)

Diet$Quant.SFA <- round((Diet$Quantity * Diet$SFA)/100, digit= 2)
SFA <- aggregate(Diet$Quant.SFA ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(SFA)
colnames(SFA)[colnames(SFA)=="Diet$INDI"] <- "INDI"
colnames(SFA)[colnames(SFA)=="Diet$District"] <- "District"
colnames(SFA)[colnames(SFA)=="Diet$Member"] <- "Member"
colnames(SFA)[colnames(SFA)=="Diet$Quant.SFA"] <- "Quant.SFA"

SFA <- left_join(SFA, Energy)
#Convert gram to kcal
SFA$Quant.SFA.kcal <- SFA$Quant.SFA * 9
SFA$Percent <- round((SFA$Quant.SFA.kcal/SFA$Quant.En)*100, digit = 2)
summary(SFA$Percent)

#Assign score for SFA: <=7% energy: 6, > 7 - 10%: 3, >10%: 0
SFA$score <- ifelse(SFA$Percent <= 7, 6, ifelse(SFA$Percent > 7 & SFA$Percent <= 10, 3, 0))
SFA.adult <- SFA %>% filter(SFA$Member == "1" | SFA$Member == "2")
names(SFA.adult)
SFA.adult <- SFA.adult %>% select(INDI:Member, score)
colnames(SFA.adult)[colnames(SFA.adult)=="score"] <- "SFA"

#Cholesterol
names(Diet)

Diet$Quant.Chol <- round((Diet$Quantity * Diet$Cholesterol)/100, digit= 2)
Cholesterol <- aggregate(Diet$Quant.Chol ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Cholesterol)
colnames(Cholesterol)[colnames(Cholesterol)=="Diet$INDI"] <- "INDI"
colnames(Cholesterol)[colnames(Cholesterol)=="Diet$District"] <- "District"
colnames(Cholesterol)[colnames(Cholesterol)=="Diet$Member"] <- "Member"
colnames(Cholesterol)[colnames(Cholesterol)=="Diet$Quant.Chol"] <- "Quant.Chol"

#Assign score for Cholesterol: <= 300mg.d: 6, > 300 - 400mg: 3, > 400mg: 0
Cholesterol$score <- ifelse(Cholesterol$Quant.Chol <= 300, 6, ifelse(Cholesterol$Quant.Chol > 300 & Cholesterol$Quant.Chol <= 400, 3, 0))

Cholesterol.adult <- Cholesterol %>% filter(Cholesterol$Member == "1" | Cholesterol$Member == "2")
names(Cholesterol.adult)
Cholesterol.adult <- Cholesterol.adult %>% select(INDI:Member, score)
colnames(Cholesterol.adult)[colnames(Cholesterol.adult)=="score"] <- "Cholesterol"

#Sodium
names(Diet)

Diet$Quant.Sodium <- round((Diet$Quantity * Diet$Natri)/100, digit= 2)
Sodium <- aggregate(Diet$Quant.Sodium ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)

names(Sodium)
colnames(Sodium)[colnames(Sodium)=="Diet$INDI"] <- "INDI"
colnames(Sodium)[colnames(Sodium)=="Diet$District"] <- "District"
colnames(Sodium)[colnames(Sodium)=="Diet$Member"] <- "Member"
colnames(Sodium)[colnames(Sodium)=="Diet$Quant.Sodium"] <- "Quant.Sodium"

Sodium <- Sodium %>% filter(Member == 1 | Member == 2)
#Compute sodium score based on quartile or decile or percentile: https://www.r-bloggers.com/quartiles-deciles-and-percentiles/
#Based on quartile

Quartile.sodium <- data.frame(quantile(Sodium$Quant.Sodium))

#The same as using summary function
summary(Sodium$Quant.Sodium)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#3.6    613.3   1060.5   1582.0   1659.8 261220.4 

#Based on decile

Decile.sodium <- data.frame(quantile(Sodium$Quant.Sodium, prob = seq(0, 1, length = 11), type = 5))

#Other percentile
quantile(Sodium$Quant.Sodium, c(.15, .50, .85)) 
#15%      50%      85% 
#469.994 1107.430 2090.346 

#Assign score based on 15th, 50th and 85th percentile: <= 470mg: 6, >=2090: 0 - the between values are linear approximate
Sodium$score <- ifelse(Sodium$Quant.Sodium <= 470, 6, ifelse(Sodium$Quant.Sodium >= 2090, 0, round(2090/Sodium$Quant.Sodium, digit = 2)))
#Set up the regression model: http://www.ddiez.com/teac/r/linear_models.php
Score <- c(0, 6)
Quant <- c(2090, 470)
plot(Score, Quant)
fit <- lm(Score ~ Quant)
abline(fit)
fit
summary(fit)
#Fit the equation to calculate the score
Sodium$score_raw <- as.numeric((fit$coefficients["Quant"] * Sodium$Quant.Sodium) + fit$coefficients[1])

#Explain the model
#fit$coefficients - this command will list out the 2 coefficients for the regression, including slope ["Quant"] and intercept [1]
#fit$coefficients["Quant"]
#fit$coefficients[1]: this is the intercept, stand at the first column --> so "1"
Sodium$score_raw <- round(as.numeric((fit$coefficients["Quant"] * Sodium$Quant.Sodium) + fit$coefficients[1]), digits =2)
#Reference: https://stats.stackexchange.com/questions/188042/how-to-predict-a-value-using-r

#Compute clean score by setting cap for those values smaller than 0 = 0, and larger than 6 = 6
Sodium$score_clean <- ifelse(Sodium$score_raw < 0, 0, ifelse(Sodium$score_raw >6, 6,Sodium$score_raw))

Sodium.adult <- Sodium %>% filter(Sodium$Member == "1" | Sodium$Member == "2")
names(Sodium.adult)
Sodium.adult <- Sodium.adult %>% select(INDI:Member, score_clean)
colnames(Sodium.adult)[colnames(Sodium.adult)=="score_clean"] <- "Sodium"

#Check the "outliers" : those are values is greater than 90th percentile: > 2322mg/d
#choose the nintieth percentile for lowest score to comply with most of recommendation: 2322

Sodium.out <- Sodium %>% filter(Sodium$Quant.Sodium > 2322) #2322 is the 90th percentile value
summary(Sodium.out$Quant.Sodium)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2325    2660    3128    6633    3908  261220

boxplot(Sodium.out$Quant.Sodium)

#Empty calories food
#Identify which food items is empty calories: if the sum of nutrient densities across nutrients examined in a food is <1, the food is considered to be an empty calorie food.
#Priori empty calories food: https://en.m.wikipedia.org/wiki/Empty_calories
#Sugar: cake, cookies, sweets, candy, soft drinks, fruit-flavored sweet beverages and other foods containing mostly added sugars (including High-fructose corn syrup).
#Fat: margarine, shortening and other fats and oils.
#Alcohol: beer, wine, hard spirits and other alcoholic beverages. 

#Create new column for diet to list out the quantity of empty calories food (in kcal): if the Empty_Cal = 1 (yes, that is the empty calories food) then keep the energy of that food item, if Empty_Cal = 1 then assign 0
Diet$Quant.Empty <- ifelse(Diet$Empty_Cal == "1", Diet$Quant.En, 0)

#Calculate sum of energy from empty calories food
Empty_Calories <- aggregate(Diet$Quant.Empty ~ Diet$INDI + Diet$District + Diet$Member, data = Diet, FUN = sum, na.rm=TRUE)
names(Empty_Calories)
colnames(Empty_Calories)[colnames(Empty_Calories)=="Diet$INDI"] <- "INDI"
colnames(Empty_Calories)[colnames(Empty_Calories)=="Diet$District"] <- "District"
colnames(Empty_Calories)[colnames(Empty_Calories)=="Diet$Member"] <- "Member"
colnames(Empty_Calories)[colnames(Empty_Calories)=="Diet$Quant.Empty"] <- "Quant.Empty"

#Join with the energy value to compute percentage
Empty_Calories <- left_join(Empty_Calories, Energy)
Empty_Calories$percent.Empty <- round((Empty_Calories$Quant.Empty/Empty_Calories$Quant.En)*100, digits = 2)

#Assign score with conditions:
# <=3% of total energy/d - 6
# > 3-10% of total energy/d - 3
# >10% of total energy/d - 0

Empty_Calories$score <- ifelse(Empty_Calories$percent.Empty <= 3, 6, ifelse(Empty_Calories$percent.Empty >3 & Empty_Calories$percent.Empty <=10, 3, 0))
Empty_Calories.adult <- Empty_Calories %>% filter(Empty_Calories$Member == "1" | Empty_Calories$Member == "2")

names(Empty_Calories.adult)
Empty_Calories.adult <- Empty_Calories.adult %>% select(INDI:Member, score)
colnames(Empty_Calories.adult)[colnames(Empty_Calories.adult)=="score"] <- "Empty_Calorie_Food"


#Combine all components: Total fat, SFA, Cholesterol, Sodium, Empty calorie food
library(plyr)

Moderation <- join_all(list(Fat.adult, SFA.adult, Cholesterol.adult, Sodium.adult, Empty_Calories.adult), by=c("INDI", "District", "Member"), type='left')

detach("package:plyr", unload=TRUE)

#Compute total Moderation score
Moderation$tot.moderation <- rowSums((Moderation[,4:8]), na.rm = T)

summary(Moderation$tot.moderation)
write_rds(Moderation, "Moderation.rds")
write_csv(Moderation, "Moderation.csv")


#Old code - dont use
#Or longer approach for quintile sodium
library(schoRsch)
names(Sodium.1)

test<- Sodium.1 %>% select(INDI, Quant.Sodium) %>%
  distinct() 
test$quantile.sod =ntiles(test, dv="Quant.Sodium", bins=10)

Sodium.1 <- merge(Sodium.1, test, by = "INDI")
names(Sodium.1)

Sodium.1 <- Sodium.1 %>% select(-c(Quant.Sodium.y)) 

colnames(Sodium.1)[colnames(Sodium.1)=="Quant.Sodium.x"] <- "Quant.Sodium"

Sodium.2 <- Sodium.1 %>% 
  spread(quantile.sod, Quant.Sodium) 

names(Sodium.1)
Sodium.2 <- Sodium.1 %>% group_by(quantile.sod) %>% summarize(mean.quan = mean(Quant.Sodium)) 




