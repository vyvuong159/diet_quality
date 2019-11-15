setwd("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Adequacy/Relative serving size")
library(tidyverse)

#Compute score for fruits, vegetables and grain groups
Diet <- readRDS("DQI with total serving.rds")


#GeneraL scoring criteria: score assigned on the basis of the percentage attainment of the recommended intakes on a continuous scale, which ranges from 0 points for 0% to 5 points for 100%, with a cap at 5 points.

#Vegetables (DQI == 5): Vietnam RDA: >= 4 servings/day: may compute based on absolute intake and density method: per 2000kcal of total energy intake
#Absolute intake
#Transpose rows to columns so that tot_svg groups become columns (unit: serving)
veg <- Diet %>% select(INDI:tot_svg)
veg <- veg %>% 
  spread(DQI, tot_svg) 

length(unique(veg$INDI)) #1070

veg <- veg %>% select(INDI:Member, "5")

colnames(veg)[colnames(veg)=="5"] <- "tot_svg"

#Assign "0" for those individuals who didn't eat vegetables at all (those with NA value)
veg$tot_svg[is.na(veg$tot_svg)] <- 0

#Compute % of attainment of Vietnam RDA (unit: %, 4 serving is 100%)
veg$percent <- (100 * veg$tot_svg)/4

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
veg$score_raw <- round(((5 * veg$percent)/100), digit = 2)

view(veg %>% filter(veg$score_raw >=5))

#Set cap of 5 point for all scores that are larger than 5
veg$score_clean <- ifelse(veg$score_raw >= 5, 5, veg$score_raw)

view(veg %>% filter(veg$score_clean >5)) #0 observation!

write_rds(veg, "veg.rds")

#Fruits (DQI == 4): Vietnam RDA: >= 3 servings/day: may compute based on absolute intake and density method: per 2000kcal of total energy intake
#Absolute intake
#Transpose rows to columns so that tot_svg groups become columns (unit: serving)
fruit <- Diet %>% select(INDI:tot_svg)
fruit <- fruit %>% 
  spread(DQI, tot_svg) 

length(unique(fruit$INDI)) #1070

fruit <- fruit %>% select(INDI:Member, "4")

colnames(fruit)[colnames(fruit)=="4"] <- "tot_svg"

#Assign "0" for those individuals who didn't eat vegetables at all (those with NA value)
fruit$tot_svg[is.na(fruit$tot_svg)] <- 0

#Compute % of attainment of Vietnam RDA (unit: %, 4 serving is 100%)
fruit$percent <- (100 * fruit$tot_svg)/3

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
fruit$score_raw <- round(((5 * fruit$percent)/100), digit = 2)

view(fruit %>% filter(fruit$score_raw >=5))

#Set cap of 5 point for all scores that are larger than 5
fruit$score_clean <- ifelse(fruit$score_raw >= 5, 5, fruit$score_raw)

view(fruit %>% filter(fruit$score_clean >5)) #0 observation!

write_rds(fruit, "fruit.rds")

names(Diet)

#Grains/starchy staple (DQI == 3): Vietnam RDA: >= 400g/day. There are 2 recommendations: absolute intake of GRAIN (as food group): 400g/day, relative intake of CARB (as nutrient): percentage or absolute recommended amount based on gender and age
#Absolute intake
#Transpose rows to columns so that tot_svg groups become columns (unit: serving)
starch <- Diet %>% select(INDI:DQI, Gram)
  
starch <- starch %>% 
  spread(DQI, Gram) 

length(unique(starch$INDI)) #1070

starch <- starch %>% select(INDI:Member, "3")

colnames(starch)[colnames(starch)=="3"] <- "Gram"

#Assign "0" for those individuals who didn't eat vegetables at all (those with NA value)
starch$Gram[is.na(starch$Gram)] <- 0

#Compute % of attainment of Vietnam RDA (unit: %, 4 serving is 100%)
starch$percent <- (100 * starch$Gram)/400

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
starch$score_raw <- round(((5 * starch$percent)/100), digit = 2)

view(starch %>% filter(starch$score_raw >=5))

#Set cap of 5 point for all scores that are larger than 5
starch$score_clean <- ifelse(starch$score_raw >= 5, 5, starch$score_raw)

view(starch %>% filter(starch$score_clean >5)) #0 observation!

write_rds(starch, "starch.rds")


#Remove outliers: http://rpubs.com/Mentors_Ubiqum/removing_outliers - store Rcodes in another script file

#Fiber: different requirements for men and women at different ages (15-19, 20-29, 30-49, 50-69)
#Men: >= 38g/d, >=38, >38, >=30 respectively for each age range
#Women: >= 26g/d, >=25, >25, >=21 respectively for each age range

#Load files (The Diet/Kyst file in this has quantity of FCODE)
FCT <- readRDS("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Adequacy/Relative serving size/FCT_DQI_master_full.rds")
Kyst <- read_csv("Kyst1bV5.csv")

Kyst$FCODE <- as.character(Kyst$FCODE)
#Check duplicated rows for several individuals: 16957091, 16957911, 16957092, 16957912 
Kyst <- Kyst[-c(5733, 5735, 5737, 5739, 5741, 5743, 5745, 5747, 5749, 5751, 5753, 5755,5757, 5759, 5761, 5763, 5765, 5767, 5769, 5771, 5773, 5775, 5777, 5779, 5781, 5783, 5785, 5787, 5789, 5791, 5793, 5795, 5797, 5799, 5801, 5803, 5805, 5807, 5809, 5811, 5813,5815, 5817, 5819, 5821),] 
Kyst <- Kyst[-c(6337, 6339, 6341, 6343, 6345, 6347, 6349, 6351, 6353, 6355, 6357, 6359, 6361, 6363, 6365, 6367, 6369, 6371, 6373, 6375, 6377, 6379, 6381, 6383, 6385, 6387, 6389, 6391, 6393, 6395, 6397, 6399, 6401, 6403, 6405, 6407, 6409, 6411, 6413, 6415, 6417, 6419, 6421, 6423, 6425, 6427, 6429, 6431),] 

#After removing these individuals, there are 23,163 observations left

names(FCT)

Kyst <- left_join(Kyst, FCT)

names(Kyst)

Kyst$ENERC_KCAL <- NULL
Kyst$Energy <- NULL
colnames(Kyst)[colnames(Kyst)=="Energy.new"] <- "Energy"  

write_rds(Kyst, "Kyst5_FCT.rds")

names(Kyst)

Kyst.Fiber <- Kyst %>% select(INDI:HHID, FAO_MDD_Grp:DQI_Proteinsource, Fiber)

#Compute total fiber each indivudal consumed
Kyst.Fiber$Quant.Fiber <- (Kyst.Fiber$Quantity*Kyst.Fiber$Fiber)/100

Kyst.Fiber_grouped <- aggregate(Kyst.Fiber$Quant.Fiber ~ Kyst.Fiber$INDI + Kyst.Fiber$District + Kyst.Fiber$Member, data = Kyst.Fiber, FUN = sum, na.rm=TRUE)

Kyst.Fiber_grouped_GLCA <- aggregate(Kyst.Fiber$Quant.Fiber ~ Kyst.Fiber$INDI + Kyst.Fiber$District + Kyst.Fiber$Member + Kyst.Fiber$GLCA, data = Kyst.Fiber, FUN = sum, na.rm=TRUE)

names(Kyst.Fiber_grouped)
colnames(Kyst.Fiber_grouped)[colnames(Kyst.Fiber_grouped)=="Kyst.Fiber$INDI"] <- "INDI"
colnames(Kyst.Fiber_grouped)[colnames(Kyst.Fiber_grouped)=="Kyst.Fiber$District"] <- "District"
colnames(Kyst.Fiber_grouped)[colnames(Kyst.Fiber_grouped)=="Kyst.Fiber$Member"] <- "Member"
colnames(Kyst.Fiber_grouped)[colnames(Kyst.Fiber_grouped)=="Kyst.Fiber$Quant.Fiber"] <- "Quant.Fiber"

write_csv(Kyst.Fiber, "Kyst.Fiber.csv")

names(Kyst.Fiber_grouped_GLCA)
colnames(Kyst.Fiber_grouped_GLCA)[colnames(Kyst.Fiber_grouped_GLCA)=="Kyst.Fiber$INDI"] <- "INDI"
colnames(Kyst.Fiber_grouped_GLCA)[colnames(Kyst.Fiber_grouped_GLCA)=="Kyst.Fiber$District"] <- "District"
colnames(Kyst.Fiber_grouped_GLCA)[colnames(Kyst.Fiber_grouped_GLCA)=="Kyst.Fiber$Member"] <- "Member"
colnames(Kyst.Fiber_grouped_GLCA)[colnames(Kyst.Fiber_grouped_GLCA)=="Kyst.Fiber$Quant.Fiber"] <- "Quant.Fiber"
colnames(Kyst.Fiber_grouped_GLCA)[colnames(Kyst.Fiber_grouped_GLCA)=="Kyst.Fiber$GLCA"] <- "GLCA"

write_csv(Kyst.Fiber_grouped_GLCA, "Kyst.Fiber.GLCA.csv")

#Get the 24h recall data that has age and merge with the 24h data that has quantity of fiber
d <- readRDS("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Adequacy/Relative serving size/DATA24h_9Oct.rds")

d <- d %>% select(madbgddt, Age)
colnames(d)[colnames(d)=="madbgddt"] <- "INDI"

Kyst.Fiber_grouped$INDI <- as.character(Kyst.Fiber_grouped$INDI)
dF.Fib <- left_join(Kyst.Fiber_grouped, d)

dF.Fib %>% filter(Member == c(1,2)) %>% summary(dF.Fib$Age)
which(is.na(dF.Fib$Age))
#Compute % of attainment of Vietnam RDA 
#Subset data for different gender and age range
men.15_49.Fib <- dF.Fib %>% filter(Member ==1 & Age >=15 & Age <= 49)
summary(men.15_49.Fib)

men.50_69.Fib <- dF.Fib %>% filter(Member ==1 & Age >=50 & Age <= 69)
summary(men.50_69.Fib)

women.15_19.Fib <- dF.Fib %>% filter(Member ==2 & Age >=15 & Age <= 19)
summary(women.15_19.Fib)

women.20_49.Fib <- dF.Fib %>% filter(Member ==2 & Age >=20 & Age <= 49)
summary(women.20_49.Fib)

women.50_69.Fib <- dF.Fib %>% filter(Member ==2 & Age >=50 & Age <= 69)
summary(women.50_69.Fib)

#Compute % of attainment
men.15_49.Fib$percent <- (100 * men.15_49.Fib$Quant.Fiber)/38
men.50_69.Fib$percent <- (100 * men.50_69.Fib$Quant.Fiber)/30
women.15_19.Fib$percent <- (100 * women.15_19.Fib$Quant.Fiber)/26
women.20_49.Fib$percent <- (100 * women.20_49.Fib$Quant.Fiber)/25
women.50_69.Fib$percent <- (100 * women.50_69.Fib$Quant.Fiber)/21

#Combine these sections together
dF1.Fib <- rbind(men.15_49.Fib, men.50_69.Fib, women.15_19.Fib, women.20_49.Fib, women.50_69.Fib)

#Check the number of individuals
dF.Fib %>% filter(Member == 1 | Member ==2) %>% View() #713: the right number of dF1

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
dF1.Fib$score_raw <- round(((5 * dF1.Fib$percent)/100), digit = 2)

view(dF1.Fib %>% filter(dF1.Fib$score_raw >=5))

#Set cap of 5 point for all scores that are larger than 5
dF1.Fib$score_clean <- ifelse(dF1.Fib$score_raw >= 5, 5, dF1.Fib$score_raw)

dF1.Fib %>% filter(dF1.Fib$score_clean >5) %>%View() #0 observation!

hist(dF1.Fib$score_clean)
Fiber <- dF1.Fib

write_rds(Fiber, "Fiber.rds")

#Calcium: different requirements for men and women at different ages:
#Men: 
#15-19: >= 1000mg/d, <3000 
#20-29: >= 800, <2500
#30-49: >=800, <2500
#50-69: >=800, <2000
#Women:
#15-19: >= 1000mg/d, <3000 
#20-29: >= 800, <2500
#30-49: >=800, <2500
#50-69: >=900, <2000

Kyst.CA <- Kyst %>% select(INDI:HHID, FAO_MDD_Grp:DQI_Proteinsource, Calci)

names(Kyst.CA)
#Compute total calcium each indivudal consumed
Kyst.CA$Quant.CA <- (Kyst.CA$Quantity*Kyst.CA$Calci)/100

Kyst.CA_grouped <- aggregate(Kyst.CA$Quant.CA ~ Kyst.CA$INDI + Kyst.CA$District + Kyst.CA$Member, data = Kyst.CA, FUN = sum, na.rm=TRUE)

Kyst.CA_grouped_GLCA <- aggregate(Kyst.CA$Quant.CA ~ Kyst.CA$INDI + Kyst.CA$District + Kyst.CA$Member + Kyst.CA$GLCA, data = Kyst.CA, FUN = sum, na.rm=TRUE)

names(Kyst.CA_grouped)
colnames(Kyst.CA_grouped)[colnames(Kyst.CA_grouped)=="Kyst.CA$INDI"] <- "INDI"
colnames(Kyst.CA_grouped)[colnames(Kyst.CA_grouped)=="Kyst.CA$District"] <- "District"
colnames(Kyst.CA_grouped)[colnames(Kyst.CA_grouped)=="Kyst.CA$Member"] <- "Member"
colnames(Kyst.CA_grouped)[colnames(Kyst.CA_grouped)=="Kyst.CA$Quant.CA"] <- "Quant.CA"

names(Kyst.CA_grouped)

write_csv(Kyst.CA_grouped, "Kyst.CA.csv")

names(Kyst.CA_grouped_GLCA)
colnames(Kyst.CA_grouped_GLCA)[colnames(Kyst.CA_grouped_GLCA)=="Kyst.CA$INDI"] <- "INDI"
colnames(Kyst.CA_grouped_GLCA)[colnames(Kyst.CA_grouped_GLCA)=="Kyst.CA$District"] <- "District"
colnames(Kyst.CA_grouped_GLCA)[colnames(Kyst.CA_grouped_GLCA)=="Kyst.CA$Member"] <- "Member"
colnames(Kyst.CA_grouped_GLCA)[colnames(Kyst.CA_grouped_GLCA)=="Kyst.CA$Quant.CA"] <- "Quant.CA"
colnames(Kyst.CA_grouped_GLCA)[colnames(Kyst.CA_grouped_GLCA)=="Kyst.CA$GLCA"] <- "GLCA"

write_csv(Kyst.CA_grouped_GLCA, "Kyst.CA.GLCA.csv")


#Get the 24h recall data that has age and merge with the 24h data that has quantity of fiber

Kyst.CA_grouped$INDI <- as.character(Kyst.CA_grouped$INDI)
dF.CA <- left_join(Kyst.CA_grouped, d)

dF.CA %>% filter(Member == c(1,2)) %>% summary(dF.CA$Age)

#Compute % of attainment of Vietnam RDA 
#Subset data for different gender and age range
men.15_19.CA <- dF.CA %>% filter(Member ==1 & Age >=15 & Age <= 19)
summary(men.15_19.CA) #0 men members in this age range

men.20_49.CA <- dF.CA %>% filter(Member ==1 & Age >=20 & Age <= 49)
summary(men.20_49.CA)

men.50_69.CA <- dF.CA %>% filter(Member ==1 & Age >=50 & Age <= 69)
summary(men.50_69.CA)

women.15_19.CA <- dF.CA %>% filter(Member ==2 & Age >=15 & Age <= 19)
summary(women.15_19.CA)

women.20_49.CA <- dF.CA %>% filter(Member ==2 & Age >=20 & Age <= 49)
summary(women.20_49.CA)

women.50_69.CA <- dF.CA %>% filter(Member ==2 & Age >=50 & Age <= 69)
summary(women.50_69.CA)

#Need to Deal with overconsumption of CA: 
#men.15_19 <3000mg/d
#men.20_49 <2500
#men.50_69 <2000
#women.15_19 <3000
#women.20_49 <2500
#women.50_6 <2000

#Compute % of attainment and corresponding raw and clean score: 
men.15_19.CA$percent <- (100 * men.15_19.CA$Quant.CA)/1000
men.15_19.CA$overUL <- men.15_19.CA$Quant.CA - 3000 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
men.15_19.CA$score_raw.1 <- round(((5 * men.15_19.CA$percent)/100), digit = 2)
men.15_19.CA$score_raw.2 <- (men.15_19.CA$overUL * (-0.5)) /500 #Each 500mg/d larger than UL value will be taken 0.5 point
#Set all values <= 0 to 0, other values keep value of score_raw.2
men.15_19.CA$score_raw.3 <- ifelse(men.15_19.CA$score_raw.2 > 0, 0, men.15_19.CA$score_raw.2) #Set maximum 5 point 
men.15_19.CA$score_raw.4 <- ifelse(men.15_19.CA$score_raw.1 >= 5, 5, men.15_19.CA$score_raw.1) #See if the maximum point will be minus some points due to exceeding UL


men.20_49.CA$percent <- (100 * men.20_49.CA$Quant.CA)/800
men.20_49.CA$overUL <- men.20_49.CA$Quant.CA - 2500 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
men.20_49.CA$score_raw.1 <- round(((5 * men.20_49.CA$percent)/100), digit = 2)
men.20_49.CA$score_raw.2 <- (men.20_49.CA$overUL * (-0.5)) /500
#Set all values <= 0 to 0, other values keep value of score_raw.2
men.20_49.CA$score_raw.3 <- ifelse(men.20_49.CA$score_raw.2 > 0, 0, men.20_49.CA$score_raw.2)
men.20_49.CA$score_raw.4 <- ifelse(men.20_49.CA$score_raw.1 >= 5, 5, men.20_49.CA$score_raw.1)


men.50_69.CA$percent <- (100 * men.50_69.CA$Quant.CA)/800
men.50_69.CA$overUL <- men.50_69.CA$Quant.CA - 2000 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
men.50_69.CA$score_raw.1 <- round(((5 * men.50_69.CA$percent)/100), digit = 2)
men.50_69.CA$score_raw.2 <- (men.50_69.CA$overUL * (-0.5)) /500
#Set all values <= 0 to 0, other values keep value of score_raw.2
men.50_69.CA$score_raw.3 <- ifelse(men.50_69.CA$score_raw.2 > 0, 0, men.50_69.CA$score_raw.2)
men.50_69.CA$score_raw.4 <- ifelse(men.50_69.CA$score_raw.1 >= 5, 5, men.50_69.CA$score_raw.1)

women.15_19.CA$percent <- (100 * women.15_19.CA$Quant.CA)/1000
women.15_19.CA$overUL <- women.15_19.CA$Quant.CA - 3000 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
women.15_19.CA$score_raw.1 <- round(((5 * women.15_19.CA$percent)/100), digit = 2)
women.15_19.CA$score_raw.2 <- (women.15_19.CA$overUL * (-0.5)) /500
#Set all values <= 0 to 0, other values keep value of score_raw.2
women.15_19.CA$score_raw.3 <- ifelse(women.15_19.CA$score_raw.2 > 0, 0, women.15_19.CA$score_raw.2)
women.15_19.CA$score_raw.4 <- ifelse(women.15_19.CA$score_raw.1 >= 5, 5, women.15_19.CA$score_raw.1)

women.20_49.CA$percent <- (100 * women.20_49.CA$Quant.CA)/800
women.20_49.CA$overUL <- women.20_49.CA$Quant.CA - 2500 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
women.20_49.CA$score_raw.1 <- round(((5 * women.20_49.CA$percent)/100), digit = 2)
women.20_49.CA$score_raw.2 <- (women.20_49.CA$overUL * (-0.5)) /500
#Set all values <= 0 to 0, other values keep value of score_raw.2
women.20_49.CA$score_raw.3 <- ifelse(women.20_49.CA$score_raw.2 > 0, 0, women.20_49.CA$score_raw.2)
women.20_49.CA$score_raw.4 <- ifelse(women.20_49.CA$score_raw.1 >= 5, 5, women.20_49.CA$score_raw.1)

women.50_69.CA$percent <- (100 * women.50_69.CA$Quant.CA)/900
women.50_69.CA$overUL <- women.50_69.CA$Quant.CA - 2000 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
women.50_69.CA$score_raw.1 <- round(((5 * women.50_69.CA$percent)/100), digit = 2)
women.50_69.CA$score_raw.2 <- (women.50_69.CA$overUL * (-0.5)) /500
#Set all values <= 0 to 0, other values keep value of score_raw.2
women.50_69.CA$score_raw.3 <- ifelse(women.50_69.CA$score_raw.2 > 0, 0, women.50_69.CA$score_raw.2)
women.50_69.CA$score_raw.4 <- ifelse(women.50_69.CA$score_raw.1 >= 5, 5, women.50_69.CA$score_raw.1)


#Combine these sections together
dF1.CA <- rbind(men.15_19.CA, men.20_49.CA, men.50_69.CA, women.15_19.CA, women.20_49.CA, women.50_69.CA)

#Compute clean score
dF1.CA$score_clean <- dF1.CA$score_raw.3 + dF1.CA$score_raw.4

dF1.CA %>% filter(dF1.CA$score_clean >5) %>%View() #0 observation!

hist(dF1.CA$score_clean)
Calci <- dF1.CA

write_rds(Calci, "Calci.rds")

#Vitamin C: >=100mg/d

Kyst.VitC <- Kyst %>% select(INDI:HHID, FAO_MDD_Grp:DQI_Proteinsource, Vit.C)

#Compute total fiber each indivudal consumed
Kyst.VitC$Quant.VitC <- (Kyst.VitC$Quantity*Kyst.VitC$Vit.C)/100

Kyst.VitC_grouped <- aggregate(Kyst.VitC$Quant.VitC ~ Kyst.VitC$INDI + Kyst.VitC$District + Kyst.VitC$Member, data = Kyst.VitC, FUN = sum, na.rm=TRUE)

Kyst.VitC_grouped_GLCA <- aggregate(Kyst.VitC$Quant.VitC ~ Kyst.VitC$INDI + Kyst.VitC$District + Kyst.VitC$Member + Kyst.VitC$GLCA, data = Kyst.VitC, FUN = sum, na.rm=TRUE)

names(Kyst.VitC_grouped)
colnames(Kyst.VitC_grouped)[colnames(Kyst.VitC_grouped)=="Kyst.VitC$INDI"] <- "INDI"
colnames(Kyst.VitC_grouped)[colnames(Kyst.VitC_grouped)=="Kyst.VitC$District"] <- "District"
colnames(Kyst.VitC_grouped)[colnames(Kyst.VitC_grouped)=="Kyst.VitC$Member"] <- "Member"
colnames(Kyst.VitC_grouped)[colnames(Kyst.VitC_grouped)=="Kyst.VitC$Quant.VitC"] <- "Quant.VitC"

write_csv(Kyst.VitC, "Kyst.VitC.csv")

names(Kyst.VitC_grouped_GLCA)
colnames(Kyst.VitC_grouped_GLCA)[colnames(Kyst.VitC_grouped_GLCA)=="Kyst.VitC$INDI"] <- "INDI"
colnames(Kyst.VitC_grouped_GLCA)[colnames(Kyst.VitC_grouped_GLCA)=="Kyst.VitC$District"] <- "District"
colnames(Kyst.VitC_grouped_GLCA)[colnames(Kyst.VitC_grouped_GLCA)=="Kyst.VitC$Member"] <- "Member"
colnames(Kyst.VitC_grouped_GLCA)[colnames(Kyst.VitC_grouped_GLCA)=="Kyst.VitC$Quant.VitC"] <- "Quant.VitC"
colnames(Kyst.VitC_grouped_GLCA)[colnames(Kyst.VitC_grouped_GLCA)=="Kyst.VitC$GLCA"] <- "GLCA"

write_csv(Kyst.VitC_grouped_GLCA, "Kyst.VitC.GLCA.csv")


#Get the 24h recall data that has age and merge with the 24h data that has quantity of fiber

Kyst.VitC_grouped$INDI <- as.character(Kyst.VitC_grouped$INDI)
dF.VitC <- left_join(Kyst.VitC_grouped, d)

dF.VitC %>% filter(Member == c(1,2)) %>% summary(dF$Age)
which(is.na(dF.VitC$Age))
#Compute % of attainment of Vietnam RDA 

#Compute % of attainment: 
dF.VitC$percent <- (100 * dF.VitC$Quant.VitC)/100

#Filter only adult men and women
dF1.VitC <- dF.VitC %>% filter(Member == 1 |  Member ==2)

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
dF1.VitC$score_raw <- round(((5 * dF1.VitC$percent)/100), digit = 2)

view(dF1.VitC %>% filter(dF1.VitC$score_raw >=5))

summary(dF1.VitC$Quant.VitC)
which(dF1.VitC$Quant.VitC == 0) #Individuals at row num. 114, 123, 283, 480

#Set cap of 5 point for all scores that are larger than 5
dF1.VitC$score_clean <- ifelse(dF1.VitC$score_raw >= 5, 5, dF1.VitC$score_raw)

dF1.VitC %>% filter(dF1.VitC$score_clean >5) %>%View() #0 observation!

hist(dF1.VitC$score_clean)
which(dF1.VitC$score_clean == 0) #Same individuals at row num. 114, 123, 283, 480

VitC <- dF1.VitC

write_rds(VitC, "VitC.rds")

#Protein: different requirements for men and women at different ages (15-19, 20-29, 30-49, 50-69)
#Men: >= 38g/d, >=38, >38, >=30 respectively for each age range
#Women: >= 26g/d, >=25, >25, >=21 respectively for each age range

names(Kyst)

Kyst.Protein <- Kyst %>% select(INDI:HHID, FAO_MDD_Grp:DQI_Proteinsource, Energy, Prot)

#Compute total protein and total energy each indivudal consumed
Kyst.Protein$Quant.Prot <- round((Kyst.Protein$Quantity*Kyst.Protein$Prot)/100, digit =2)
Kyst.Protein$Quant.Energy <- round((Kyst.Protein$Quantity*Kyst.Protein$Energy)/100, digit=2)

Kyst.Prot.protein_grouped <- aggregate(Kyst.Protein$Quant.Prot ~ Kyst.Protein$INDI + Kyst.Protein$District + Kyst.Protein$Member, data = Kyst.Protein, FUN = sum, na.rm=TRUE)
Kyst.Prot.protein_grouped_GLCA <- aggregate(Kyst.Protein$Quant.Prot ~ Kyst.Protein$INDI + Kyst.Protein$District + Kyst.Protein$Member + Kyst.Protein$GLCA, data = Kyst.Protein, FUN = sum, na.rm=TRUE)

Kyst.Prot.energy_grouped <- aggregate(Kyst.Protein$Quant.Energy ~ Kyst.Protein$INDI + Kyst.Protein$District + Kyst.Protein$Member, data = Kyst.Protein, FUN = sum, na.rm=TRUE)
Kyst.Prot.energy_grouped_GLCA <- aggregate(Kyst.Protein$Quant.Energy ~ Kyst.Protein$INDI + Kyst.Protein$District + Kyst.Protein$Member + Kyst.Protein$GLCA, data = Kyst.Protein, FUN = sum, na.rm=TRUE)


names(Kyst.Prot.protein_grouped)
colnames(Kyst.Prot.protein_grouped)[colnames(Kyst.Prot.protein_grouped)=="Kyst.Protein$INDI"] <- "INDI"
colnames(Kyst.Prot.protein_grouped)[colnames(Kyst.Prot.protein_grouped)=="Kyst.Protein$District"] <- "District"
colnames(Kyst.Prot.protein_grouped)[colnames(Kyst.Prot.protein_grouped)=="Kyst.Protein$Member"] <- "Member"
colnames(Kyst.Prot.protein_grouped)[colnames(Kyst.Prot.protein_grouped)=="Kyst.Protein$Quant.Prot"] <- "Quant.Prot"

names(Kyst.Prot.energy_grouped)
colnames(Kyst.Prot.energy_grouped)[colnames(Kyst.Prot.energy_grouped)=="Kyst.Protein$INDI"] <- "INDI"
colnames(Kyst.Prot.energy_grouped)[colnames(Kyst.Prot.energy_grouped)=="Kyst.Protein$District"] <- "District"
colnames(Kyst.Prot.energy_grouped)[colnames(Kyst.Prot.energy_grouped)=="Kyst.Protein$Member"] <- "Member"
colnames(Kyst.Prot.energy_grouped)[colnames(Kyst.Prot.energy_grouped)=="Kyst.Protein$Quant.Energy"] <- "Quant.Energy"


names(Kyst.Prot.protein_grouped_GLCA)
colnames(Kyst.Prot.protein_grouped_GLCA)[colnames(Kyst.Prot.protein_grouped_GLCA)=="Kyst.Protein$INDI"] <- "INDI"
colnames(Kyst.Prot.protein_grouped_GLCA)[colnames(Kyst.Prot.protein_grouped_GLCA)=="Kyst.Protein$District"] <- "District"
colnames(Kyst.Prot.protein_grouped_GLCA)[colnames(Kyst.Prot.protein_grouped_GLCA)=="Kyst.Protein$Member"] <- "Member"
colnames(Kyst.Prot.protein_grouped_GLCA)[colnames(Kyst.Prot.protein_grouped_GLCA)=="Kyst.Protein$Quant.Prot"] <- "Prot"
colnames(Kyst.Prot.protein_grouped_GLCA)[colnames(Kyst.Prot.protein_grouped_GLCA)=="Kyst.Protein$GLCA"] <- "GLCA"

names(Kyst.Prot.energy_grouped_GLCA)
colnames(Kyst.Prot.energy_grouped_GLCA)[colnames(Kyst.Prot.energy_grouped_GLCA)=="Kyst.Protein$INDI"] <- "INDI"
colnames(Kyst.Prot.energy_grouped_GLCA)[colnames(Kyst.Prot.energy_grouped_GLCA)=="Kyst.Protein$District"] <- "District"
colnames(Kyst.Prot.energy_grouped_GLCA)[colnames(Kyst.Prot.energy_grouped_GLCA)=="Kyst.Protein$Member"] <- "Member"
colnames(Kyst.Prot.energy_grouped_GLCA)[colnames(Kyst.Prot.energy_grouped_GLCA)=="Kyst.Protein$Quant.Energy"] <- "Quant.Energy"
colnames(Kyst.Prot.energy_grouped_GLCA)[colnames(Kyst.Prot.energy_grouped_GLCA)=="Kyst.Protein$GLCA"] <- "GLCA"


Kyst.Protein.Energy <- left_join(Kyst.Prot.protein_grouped, Kyst.Prot.energy_grouped)
write_csv(Kyst.Protein.Energy, "Kyst.Protein.csv")

#Compute % of protein out of the total energy intake
Kyst.Protein.Energy$Percen.Prot <- round(((Kyst.Protein.Energy$Quant.Prot*4)/Kyst.Protein.Energy$Quant.Energy)*100, digit =2)

summary(Kyst.Protein.Energy$Percen.Prot)
which(Kyst.Protein.Energy$Percen.Prot > 20)

#Compute % of attainment: 
Kyst.Protein.Energy$percent <- (100 * Kyst.Protein.Energy$Percen.Prot)/13

#Compute score based on % of attainment (unit: point/score, 100% is 5 point - maximum)
Kyst.Protein.Energy$score_raw <- round(((5 * Kyst.Protein.Energy$percent)/100), digit = 2)

view(Kyst.Protein.Energy %>% filter(Kyst.Protein.Energy$score_raw >=5))

#Set cap of 5 point for all scores that are larger than 5
Kyst.Protein.Energy$score_clean <- ifelse(Kyst.Protein.Energy$score_raw >= 5, 5, Kyst.Protein.Energy$score_raw)

view(Kyst.Protein.Energy %>% filter(Kyst.Protein.Energy$score_clean >5)) #0 observation!

write_rds(Kyst.Protein.Energy, "Protein.rds")

#Iron:  VDG suggest based on bioavailability, we can  chose 15% since iron bioavailability has been estimated to be in the range of 14–18% for mixed diets 
#Reference: https://academic.oup.com/ajcn/article/91/5/1461S/4597424
#But still assign bioavailability for accuracy

names(Kyst)
Kyst.Iron <- Kyst %>% select(INDI:HHID, FAO_MDD_Grp:DQI_Proteinsource, Vit.C, Iron)

names(Kyst.Iron)
#Compute total iron each indivudal consumed
Kyst.Iron$Quant.Iron <- (Kyst.Iron$Quantity*Kyst.Iron$Iron)/100

Kyst.Iron_grouped <- aggregate(Kyst.Iron$Quant.Iron ~ Kyst.Iron$INDI + Kyst.Iron$District + Kyst.Iron$Member, data = Kyst.Iron, FUN = sum, na.rm=TRUE)

Kyst.Iron_grouped_GLCA <- aggregate(Kyst.Iron$Quant.Iron ~ Kyst.Iron$INDI + Kyst.Iron$District + Kyst.Iron$Member + Kyst.Iron$GLCA, data = Kyst.Iron, FUN = sum, na.rm=TRUE)

names(Kyst.Iron_grouped)
colnames(Kyst.Iron_grouped)[colnames(Kyst.Iron_grouped)=="Kyst.Iron$INDI"] <- "INDI"
colnames(Kyst.Iron_grouped)[colnames(Kyst.Iron_grouped)=="Kyst.Iron$District"] <- "District"
colnames(Kyst.Iron_grouped)[colnames(Kyst.Iron_grouped)=="Kyst.Iron$Member"] <- "Member"
colnames(Kyst.Iron_grouped)[colnames(Kyst.Iron_grouped)=="Kyst.Iron$Quant.Iron"] <- "Quant.Iron"

names(Kyst.Iron_grouped_GLCA)
colnames(Kyst.Iron_grouped_GLCA)[colnames(Kyst.Iron_grouped_GLCA)=="Kyst.Iron$INDI"] <- "INDI"
colnames(Kyst.Iron_grouped_GLCA)[colnames(Kyst.Iron_grouped_GLCA)=="Kyst.Iron$District"] <- "District"
colnames(Kyst.Iron_grouped_GLCA)[colnames(Kyst.Iron_grouped_GLCA)=="Kyst.Iron$Member"] <- "Member"
colnames(Kyst.Iron_grouped_GLCA)[colnames(Kyst.Iron_grouped_GLCA)=="Kyst.Iron$Quant.Iron"] <- "Quant.Iron"
colnames(Kyst.Iron_grouped_GLCA)[colnames(Kyst.Iron_grouped_GLCA)=="Kyst.Iron$GLCA"] <- "GLCA"

summary(Kyst.Iron_grouped)

which.max(Kyst.Iron_grouped$Quant.Iron)
boxplot(Kyst.Iron_grouped$Quant.Iron)
which(Kyst.Iron_grouped$Quant.Iron >= 50)
#[1]   77   85  433  441  454  780 1030: corresponding individual id: 16957091, 16957911, 16957092, 16957912, 17259332
#These individuals has duplicated food intake: 16957091, 16957911, 16957092, 16957912 


#Just to check  bioavailability 10% and 15% based on amount of meat or Vitamin C: 
#Bioavailability 10%: when the diet has 30g - 90g of meat or fish per day or 25mg - 75mg of vitamin C per day 
#Bioavailability 15%: when the diet has > 90g of meat or fish per day or > 75mg of vitamin C per day 
#Compute gram for meat
Kyst.meat <- readRDS("Sum.DQI.gram_wide.rds")

length(unique(Kyst.meat$INDI)) #1070
names(Kyst.meat)

Kyst.meat <- Kyst.meat %>% select(INDI: Gram1)

length(unique(Kyst.meat$INDI)) #1070

colnames(Kyst.meat)[colnames(Kyst.meat)=="Gram1"] <- "Meat.g"

Kyst.meat$INDI <- as.character(Kyst.meat$INDI)
Kyst.meat$District <- as.character(Kyst.meat$District)
Kyst.meat$Member <- as.character(Kyst.meat$Member)

Kyst.VitC_grouped$INDI <- as.character(Kyst.VitC_grouped$INDI)
Kyst.VitC_grouped$District <- as.character(Kyst.VitC_grouped$District)
Kyst.VitC_grouped$Member <- as.character(Kyst.VitC_grouped$Member)

Kyst.meat.VitC <- left_join(Kyst.meat, Kyst.VitC_grouped)

#Assign Bioavailability level 10 or 15 to new column
Kyst.meat.VitC$Bio <- ifelse(Kyst.meat.VitC$Meat.g >= 30 & Kyst.meat.VitC$Meat.g <=90 & Kyst.meat.VitC$Quant.VitC >= 25 & Kyst.meat.VitC$Quant.VitC <= 75, 10,15)
Kyst.meat.VitC.adult <- Kyst.meat.VitC %>% filter(Member == 1 | Member == 2)
which(Kyst.meat.VitC.adult$Bio <15)

#Get the 24h recall data that has age and merge with the 24h data that has quantity of fiber

Kyst.Iron_grouped$INDI <- as.character(Kyst.Iron_grouped$INDI)
dF.Iron <- left_join(Kyst.Iron_grouped, d)


#Compute % of attainment of Vietnam RDA 
#Subset data for different gender and age range
men.15_19.Iron <- dF.Iron %>% filter(Member ==1 & Age >=15 & Age <= 19)
summary(men.15_19.Iron) #0 men members in this age range

men.20_69.Iron <- dF.Iron %>% filter(Member ==1 & Age >=20 & Age <= 69)
summary(men.20_69.Iron)

women.15_19.Iron <- dF.Iron %>% filter(Member ==2 & Age >=15 & Age <= 19)
summary(women.15_19.Iron)

women.20_69.Iron <- dF.Iron %>% filter(Member ==2 & Age >=20 & Age <= 69)
summary(women.20_69.Iron)


#Need to Deal with overconsumption of CA: all members <45mg/d


#Compute % of attainment and corresponding raw and clean score: 
men.15_19.Iron$percent <- (100 * men.15_19.Iron$Quant.Iron)/11.6
men.15_19.Iron$overUL <- men.15_19.Iron$Quant.Iron - 45 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
men.15_19.Iron$score_raw.1 <- round(((5 * men.15_19.Iron$percent)/100), digit = 2)
men.15_19.Iron$score_raw.2 <- (men.15_19.Iron$overUL * (-0.5)) /5 #Each 5mg/d larger than UL value will be taken 0.5 point
#Set all values <= 0 to 0, other values keep value of score_raw.2
men.15_19.Iron$score_raw.3 <- ifelse(men.15_19.Iron$score_raw.2 > 0, 0, men.15_19.Iron$score_raw.2) 
men.15_19.Iron$score_raw.4 <- ifelse(men.15_19.Iron$score_raw.1 >= 5, 5, men.15_19.Iron$score_raw.1) #Set cap of 5 points

men.20_69.Iron$percent <- (100 * men.20_69.Iron$Quant.Iron)/7.9
men.20_69.Iron$overUL <- men.20_69.Iron$Quant.Iron - 45 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
men.20_69.Iron$score_raw.1 <- round(((5 * men.20_69.Iron$percent)/100), digit = 2)
men.20_69.Iron$score_raw.2 <- (men.20_69.Iron$overUL * (-0.5)) /5 #Each 5mg/d larger than UL value will be taken 0.5 point
#Set all values <= 0 to 0, other values keep value of score_raw.2
men.20_69.Iron$score_raw.3 <- ifelse(men.20_69.Iron$score_raw.2 > 0, 0, men.20_69.Iron$score_raw.2) 
men.20_69.Iron$score_raw.4 <- ifelse(men.20_69.Iron$score_raw.1 >= 5, 5, men.20_69.Iron$score_raw.1)

women.15_19.Iron$percent <- (100 * women.15_19.Iron$Quant.Iron)/29.7
women.15_19.Iron$overUL <- women.15_19.Iron$Quant.Iron - 45 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
women.15_19.Iron$score_raw.1 <- round(((5 * women.15_19.Iron$percent)/100), digit = 2)
women.15_19.Iron$score_raw.2 <- (women.15_19.Iron$overUL * (-0.5)) /5 #Each 5mg/d larger than UL value will be taken 0.5 point
#Set all values <= 0 to 0, other values keep value of score_raw.2
women.15_19.Iron$score_raw.3 <- ifelse(women.15_19.Iron$score_raw.2 > 0, 0, women.15_19.Iron$score_raw.2) 
women.15_19.Iron$score_raw.4 <- ifelse(women.15_19.Iron$score_raw.1 >= 5, 5, women.15_19.Iron$score_raw.1) #Set cap of 5 points

women.20_69.Iron$percent <- (100 * women.20_69.Iron$Quant.Iron)/26.1
women.20_69.Iron$overUL <- women.20_69.Iron$Quant.Iron - 45 #Those value is >= 0 --> exceed UL. Disregard those value < 0 --> assign 0 point to minus in score_raw.3
women.20_69.Iron$score_raw.1 <- round(((5 * women.20_69.Iron$percent)/100), digit = 2)
women.20_69.Iron$score_raw.2 <- (women.20_69.Iron$overUL * (-0.5)) /5 #Each 5mg/d larger than UL value will be taken 0.5 point
#Set all values <= 0 to 0, other values keep value of score_raw.2
women.20_69.Iron$score_raw.3 <- ifelse(women.20_69.Iron$score_raw.2 > 0, 0, women.20_69.Iron$score_raw.2) 
women.20_69.Iron$score_raw.4 <- ifelse(women.20_69.Iron$score_raw.1 >= 5, 5, women.20_69.Iron$score_raw.1)

#Combine these sections together
dF1.Iron <- rbind(men.15_19.Iron, men.20_69.Iron, women.15_19.Iron, women.20_69.Iron)

#Compute clean score
dF1.Iron$score_clean <- dF1.Iron$score_raw.3 + dF1.Iron$score_raw.4

dF1.Iron %>% filter(dF1.Iron$score_clean >5) %>%View() #0 observation!

hist(dF1.Iron$score_clean)
Iron <- dF1.Iron

write_rds(Iron, "Iron")

#Combine Adequacy section: Veg, Fruit, grain, fiber, protein, iron, calci, vitamin C
names(veg)
Adequacy_Veg <- veg %>% select(INDI:Member, score_clean)
Adequacy_Veg <- Adequacy_Veg %>% filter(Adequacy_Veg$Member == 1 | Adequacy_Veg$Member ==2)
names(Adequacy_Veg)
colnames(Adequacy_Veg)[colnames(Adequacy_Veg)=="score_clean"] <- "Veg"

names(fruit)
Adequacy_Fruit <- fruit %>% select(INDI:Member, score_clean)
Adequacy_Fruit <- Adequacy_Fruit %>% filter(Adequacy_Fruit$Member == 1 | Adequacy_Fruit$Member ==2)
names(Adequacy_Fruit)
colnames(Adequacy_Fruit)[colnames(Adequacy_Fruit)=="score_clean"] <- "Fruit"

names(starch)
Adequacy_Starch <- starch %>% select(INDI:Member, score_clean)
Adequacy_Starch <- Adequacy_Starch %>% filter(Adequacy_Starch$Member == 1 | Adequacy_Starch$Member ==2)
names(Adequacy_Starch)
colnames(Adequacy_Starch)[colnames(Adequacy_Starch)=="score_clean"] <- "Starch"

names(Fiber)
Adequacy_Fiber <- Fiber %>% select(INDI:Member, score_clean)
Adequacy_Fiber <- Adequacy_Fiber %>% filter(Adequacy_Fiber$Member == 1 | Adequacy_Fiber$Member ==2)
names(Adequacy_Fiber)
colnames(Adequacy_Fiber)[colnames(Adequacy_Fiber)=="score_clean"] <- "Fiber"

names(Kyst.Protein.Energy)
Adequacy_Protein <- Kyst.Protein.Energy %>% select(INDI:Member, score_clean)
Adequacy_Protein <- Adequacy_Protein %>% filter(Adequacy_Protein$Member == 1 | Adequacy_Protein$Member ==2)
names(Adequacy_Protein)
colnames(Adequacy_Protein)[colnames(Adequacy_Protein)=="score_clean"] <- "Protein"

names(Iron)
Adequacy_Iron <- Iron %>% select(INDI:Member, score_clean)
Adequacy_Iron <- Adequacy_Iron %>% filter(Adequacy_Iron$Member == 1 | Adequacy_Iron$Member ==2)
names(Adequacy_Iron)
colnames(Adequacy_Iron)[colnames(Adequacy_Iron)=="score_clean"] <- "Iron"

names(Calci)
Adequacy_Calci <- Calci %>% select(INDI:Member, score_clean)
Adequacy_Calci <- Adequacy_Calci %>% filter(Adequacy_Calci$Member == 1 | Adequacy_Calci$Member ==2)
names(Adequacy_Calci)
colnames(Adequacy_Calci)[colnames(Adequacy_Calci)=="score_clean"] <- "Calci"

names(VitC)
Adequacy_VitC <- VitC %>% select(INDI:Member, score_clean)
Adequacy_VitC <- Adequacy_VitC %>% filter(Adequacy_VitC$Member == 1 | Adequacy_VitC$Member ==2)
names(Adequacy_VitC)
colnames(Adequacy_VitC)[colnames(Adequacy_VitC)=="score_clean"] <- "VitC"

#Combine all components: Veg, Fruit, grain, fiber, protein, iron, calci, vitamin C
library(plyr)

Adequacy <- join_all(list(Adequacy_Veg, Adequacy_Fruit, Adequacy_Starch, Adequacy_Fiber, Adequacy_Protein, Adequacy_Iron, Adequacy_Calci, Adequacy_VitC), by=c("INDI", "District", "Member"), type='left')

detach("package:plyr", unload=TRUE)

#Compute total Adequacy score
Adequacy$tot.adequacy <- rowSums((Adequacy[,4:11]), na.rm = T)
  
summary(Adequacy$tot.adequacy)
write_rds(Adequacy, "Adequacy.rds")
write_csv(Adequacy, "Adequacy.csv")

