setwd("/Volumes/GoogleDrive/My Drive/Capstone Project/Data/Matching 3 datasets/Analyze association/5 Nov")

library(tidyr)
library(tidyverse)
#Import datasets: File that matched CB & 24h and File needs to get code (OutPutKyv4)
d <- readRDS("An_CB_24h.rds")
Kyst1bV4 <- readRDS("VDQI.rds")

#Change madbgddt to INDI
names(d)[14] <- "INDI"
#Check the different individual IDs between 2 files before merging
setdiff(d$INDI, Kyst1bV4$INDI) #306 Individuals are children and people in the village 6666649 were deleted, probably NIN deleted them before sending to CIAT.

A<- setdiff(Kyst1bV4$INDI, d$INDI)
##These 120 individuals disappeared because they don't match with CB list

#Filter only adults
d.adult <- d %>% filter(BoMeTre == "1" | BoMeTre == "2") #581 adults including 1 commune in 6666649

setdiff(d.adult$INDI, Kyst1bV4$INDI)
#[1] "6666649021" "6666649022" "6666649061" "6666649062" "6666649081" "6666649082" "6666649091"
#[8] "6666649092" "6666649121" "6666649122" "6666649141" "6666649142" "6666649201" "6666649202"
#[15] "6666649231" "6666649232"

setdiff(Kyst1bV4$INDI, d.adult$INDI)

#Merge 2 datasets
dF <- merge (d, Kyst1bV4, by = "INDI", all = FALSE)

length(unique(dF$magd)) #288 unique households
length(unique(dF$magddt)) #565 unique individuals

names(dF)

#Reorder columns
dF <- dF %>%  select(INDI, magd, magddt, Full_ID:name, A2, tdt, bmt, BoMeTre, gender:birthyear, age.2018, Weight.1:Total_Score)

#Remove column of name of individuals after making sure everything is good to go
dF$A2 <- NULL
dF$tdt <- NULL
dF$name <- NULL

#Calculate BMI
#BMI formula: weight (kg) / [height (m)]2

dF$Weight <- rowMeans(dF[c("Weight.1", "Weight.2")], na.rm = T)
dF$Height <- (rowMeans(dF[c("Height.1", "Height.2")], na.rm = T))/100
dF$Waist <- rowMeans(dF[c("Waist.1", "Waist.2")], na.rm = T)
dF$BMI <- round(dF$Weight/(dF$Height^2), digits = 2)

summary(dF$Weight)
summary(dF$Height)
summary(dF$Waist)
summary(dF$BMI)


which.max(dF$Weight)
which.min(dF$Weight)

which.max(dF$Height)
which.min(dF$Height)

which.max(dF$Waist)
which.min(dF$Waist)

which.max(dF$BMI)
which.min(dF$BMI)

#Assign nutrition status according to BMI
#Underweight: <18.5
#Normal: 18.5 - 24.9
#Overweight: 25 - 29.9
#Obesity: 30 - 34.9: Obesity class I, 35 - 39.9: Obesity class II
#Extreme obesity: >40: Obesity class III

#Assign BMI status: underweight, normal, overweight, obesity class I, obesity class II, obesity class III or extreme obesity
dF$BMI.stat <- ifelse(dF$BMI < 18.5, "underweight", ifelse(dF$BMI >= 18.5 & dF$BMI < 25, "normal", ifelse(dF$BMI >=25 & dF$BMI < 30, "overweight", ifelse(dF$BMI >=30 & dF$BMI < 35, "obesity.I", ifelse(dF$BMI >= 35 & dF$BMI < 40, "obesity.II", "obesity.III")))))

#Assign overweight&obesity status: BMI >= 25 --> Yes or "1", otherwise No or "0"
dF$overweight <- ifelse(dF$BMI >= 25, "1", "0")

#Assign overweight level: Overweight - 1, obesity I - 2, obesity II - 3, obesity III - 4, otherwise 0
dF$overweight.lev <- ifelse(dF$BMI.stat == "overweight", "1", ifelse(dF$BMI.stat == "obesity.I", "2", ifelse(dF$BMI.stat == "obesity.II", "3", ifelse(dF$BMI.stat == "obesity.III", "4", "0"))))

#Assign WC status: Male >=90: abnormal - 1, Female >= 80: abnormal - 1, otherwise - 0
dF$Member <- as.character(dF$Member)
dF$waist.stat <- ifelse(dF$Member == "1" & dF$Waist >= 90, "1", ifelse(dF$Member == "2" & dF$Waist >= 80, "1", "0")) #1: abnormal, 0: normal

#Assign disease risk status: either waist status = 1 or overweight status = 1, if either value has NA value then NA
dF$disease.risk <- ifelse(dF$waist.stat == "1" | dF$overweight == "1", "1", "0")

#Assign disease risk level based on the combination of BMI and WC of Shankuan Zhu
#Reference: https://onlinelibrary.wiley.com/doi/full/10.1038/oby.2004.73
#Limitation: the range to divide into different level is to broad
#Strength: considerate both BMI and WC as predictive risk factor
dF$disease.risk.lev <- ifelse(dF$Waist < 100 & dF$BMI <30 & dF$BMI >= 25 | dF$Waist >=90 & dF$Member == "1", "1", ifelse(dF$Waist < 90 & dF$BMI <30 & dF$BMI >= 25 | dF$Waist >=80 & dF$Member == "2", "1", ifelse(dF$BMI >= 30 | dF$Waist >= 100 & dF$Member == "1", "2", ifelse(dF$BMI >=30 | dF$Waist >=90 & dF$Member =="2", "2", "0"))))

#Assign disease risk level based on the combination of WHO 
#Reference: https://www.nhlbi.nih.gov/files/docs/guidelines/prctgd_c.pdf
#Limitation: ignore individuals who have abnormal WC as long as they have normal BMI
#Strength: divide into detailed level: increased - high - very high - extremely high
dF$disease.risk.lev.who <- ifelse(dF$Waist < 90 & dF$BMI <30 & dF$BMI >= 25 & dF$Member == "1", "Increased", ifelse(dF$Waist < 80 & dF$BMI <30 & dF$BMI >= 25 & dF$Member == "2", "Increased", ifelse(dF$BMI >= 30 & dF$BMI < 35 & dF$Waist < 90 & dF$Member == "1", "High", ifelse(dF$BMI >=25 & dF$BMI < 30 & dF$Waist >=90 & dF$Member =="1", "High", ifelse(dF$BMI >= 30 & dF$BMI < 35 & dF$Waist < 80 & dF$Member == "2", "High", ifelse(dF$BMI >= 25 & dF$BMI < 30 & dF$Waist >=80 & dF$Member =="2", "High", ifelse(dF$BMI >= 35 & dF$BMI < 40 & dF$Waist < 90 & dF$Member == "1", "Very High",ifelse(dF$BMI >=30 & dF$BMI < 35 & dF$Waist >=90 & dF$Member =="1", "Very High", ifelse(dF$BMI >=35 & dF$BMI < 40 & dF$Waist >=90 & dF$Member =="1", "Very High", ifelse(dF$BMI >= 35 & dF$BMI < 40 & dF$Waist <80 & dF$Member == 2, "Very High", ifelse(dF$BMI >30 & dF$BMI <35 & dF$Waist>80 & dF$Member == "2", "Very High", ifelse(dF$BMI >=35 & dF$BMI <40 & dF$Waist>80 & dF$Member == "2", "Very High", ifelse(dF$BMI >=40, "Extremely High", "Normal")))))))))))))

#Combine Zhu and WHO: Either BMI or WC is abnormal & WHO disease risk level = normal --> 1, WHO disease risk level = increased -->2, WHO disease risk level = high --> 3, WHO disease risk level = very high -->4, WHO disease risk level = extremely high --> 5, otherwise --> 0
dF$disease.risk.com <- ifelse(dF$disease.risk == "1" & dF$disease.risk.lev.who == "Normal", "1", ifelse(dF$disease.risk == "1" & dF$disease.risk.lev.who == "Increased", "2", ifelse(dF$disease.risk == "1" & dF$disease.risk.lev.who == "High", "3", ifelse(dF$disease.risk == "1" & dF$disease.risk.lev.who == "Very High", "4", ifelse(dF$disease.risk == "1" & dF$disease.risk.lev.who == "Extremely High", "5", "0")))))

#Some preliminary graphs
#DQI 
DQI.avg <- dF %>% group_by(District,Member) %>% summarise(Variety = round(mean(tot_variety, na.rm=TRUE),2), 
                                                          Adequacy = round(mean(tot.adequacy, na.rm=T), 2), 
                                                          Moderation = round(mean(tot.moderation, na.rm = T), 2), 
                                                          Balance = round(mean(tot.Balance, na.rm=T), 2)) 
#Transform to long format
DQI.avg.1 <- gather(DQI.avg, "DQI_Component", "Score", c(3:6))      

#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
DQI.avg.1 <- DQI.avg.1 %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                              District == "2" ~ "Peri-urban",
                                                              District == "3" ~ "Rural"))

DQI.avg.1 <- DQI.avg.1 %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                            Member == "2" ~ "Women"))
                                                            
#setting the levels of the DQI_Components

DQI.avg.1$DQI_Component = factor(DQI.avg.1$DQI_Component, levels = c("Variety", "Adequacy","Moderation","Balance" ))

sort(unique(DQI.avg.1$DQI_Component))

##setting factor levels of the district & member

DQI.avg.1$district_f = factor(DQI.avg.1$District.Names, levels=c('Urban','Peri-urban','Rural'))
DQI.avg.1$member_f = factor(DQI.avg.1$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(DQI.avg.1, aes(member_f, Score, fill=DQI_Component)) + 
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~district_f) +    
  labs(title="Mean DQI component score in 3 districts", x="Member", y="DQI score")

#BMI
BMI.avg <- dF %>% group_by(District,Member) %>% summarise(BMI = round(mean(BMI, na.rm=TRUE),2))
                                                          

#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
BMI.avg <- BMI.avg %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                            District == "2" ~ "Peri-urban",
                                                            District == "3" ~ "Rural"))

BMI.avg <- BMI.avg %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                          Member == "2" ~ "Women"))


##setting factor levels of the district & member

BMI.avg$district_f = factor(BMI.avg$District.Names, levels=c('Urban','Peri-urban','Rural'))
BMI.avg$member_f = factor(BMI.avg$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(BMI.avg, aes(member_f, BMI, fill=member_f)) + 
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~district_f) +    
  labs(title="Mean BMI 3 districts", x="Member", y="weight (kg)/ (height(m))^2")

#by %
BMI.risk <- dF %>% group_by(District, Member) %>% summarise("Normal_0" = length(which(overweight== "0")), 
                                                           "Risk_1" = length(which(overweight== "1")))


BMI.risk$total_col = apply(BMI.risk[3:4], 1, sum)

BMI.risk = mutate(BMI.risk, "Normal" = (Normal_0 / total_col)*100,
                 "At risk" = (Risk_1 / total_col)*100)

#Transform to long format
BMI.risk.1 <- gather(BMI.risk, "BMI", "Percent", c(6:7))      

BMI.risk.1 <- BMI.risk.1 %>% select(District, Member, BMI, Percent)
#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
BMI.risk.1 <- BMI.risk.1 %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                            District == "2" ~ "Peri-urban",
                                                            District == "3" ~ "Rural"))

BMI.risk.1 <- BMI.risk.1 %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                          Member == "2" ~ "Women"))

#setting the levels of the Disease risk level

BMI.risk.1$BMI = factor(BMI.risk.1$BMI, levels = c("At risk", "Normal"))

sort(unique(BMI.risk.1$BMI))

##setting factor levels of the district & member

BMI.risk.1$district_f = factor(BMI.risk.1$District.Names, levels=c('Urban','Peri-urban','Rural'))
BMI.risk.1$member_f = factor(BMI.risk.1$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(BMI.risk.1, aes(member_f, Percent, fill= BMI)) + 
  geom_bar(stat = "identity", position="stack")+
  facet_grid(~district_f) +    
  labs(title="BMI categorization in 3 districts", x="Member", y="Percent")


#WC
#By mean
WC.avg <- dF %>% group_by(District,Member) %>% summarise(Waist.Circumference = round(mean(Waist, na.rm=TRUE),2))


#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
WC.avg <- WC.avg %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                        District == "2" ~ "Peri-urban",
                                                        District == "3" ~ "Rural"))

WC.avg <- WC.avg %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                      Member == "2" ~ "Women"))


##setting factor levels of the district & member

WC.avg$district_f = factor(WC.avg$District.Names, levels=c('Urban','Peri-urban','Rural'))
WC.avg$member_f = factor(WC.avg$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(WC.avg, aes(member_f, Waist.Circumference, fill=member_f)) + 
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~district_f) +    
  labs(title="Mean Waist Circumference in 3 districts", x="Member", y="Waist Circumference in cm")

#by %
WC.risk <- dF %>% group_by(District, Member) %>% summarise("Normal_0" = length(which(waist.stat== "0")), 
                                                           "Risk_1" = length(which(waist.stat== "1")))
                                                                

WC.risk$total_col = apply(WC.risk[3:4], 1, sum)

WC.risk = mutate(WC.risk, "Normal" = (Normal_0 / total_col)*100,
                          "At risk" = (Risk_1 / total_col)*100)
                     
#Transform to long format
WC.risk.1 <- gather(WC.risk, "Waist_Circumference", "Percent", c(6:7))      

WC.risk.1 <- WC.risk.1 %>% select(District, Member, Waist_Circumference, Percent)
#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
WC.risk.1 <- WC.risk.1 %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                                      District == "2" ~ "Peri-urban",
                                                                      District == "3" ~ "Rural"))

WC.risk.1 <- WC.risk.1 %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                                    Member == "2" ~ "Women"))

#setting the levels of the Disease risk level

WC.risk.1$Waist_Circumference = factor(WC.risk.1$Waist_Circumference, levels = c("At risk", "Normal"))

sort(unique(WC.risk.1$Waist_Circumference))

##setting factor levels of the district & member

WC.risk.1$district_f = factor(WC.risk.1$District.Names, levels=c('Urban','Peri-urban','Rural'))
WC.risk.1$member_f = factor(WC.risk.1$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(WC.risk.1, aes(member_f, Percent, fill=Waist_Circumference)) + 
  geom_bar(stat = "identity", position="stack")+
  facet_grid(~district_f) +    
  labs(title="Waist Circumference categorization in 3 districts", x="Member", y="Percent")




#Disease risk 
Disease.risk <- dF %>% group_by(District, Member) %>% summarise("lev_0" = length(which(disease.risk.com== "0")), 
                                                                "lev_1" = length(which(disease.risk.com== "1")), 
                                                                "lev_2" = length(which(disease.risk.com== "2")), 
                                                                "lev_3" = length(which(disease.risk.com== "3")),
                                                                "lev_4" = length(which(disease.risk.com== "4")))
                                                             
Disease.risk$total_col = apply(Disease.risk[3:7], 1, sum)

Disease.risk = mutate(Disease.risk, 
                "0" = (lev_0 / total_col)*100,
                "1" = (lev_1 / total_col)*100,
                "2" = (lev_2 / total_col)*100,
                "3" = (lev_3 / total_col)*100,
                "4" = (lev_4 / total_col)*100)


#Transform to long format
Disease.risk.1 <- gather(Disease.risk, "Disease_Risk_Level", "Percent", c(9:13))      

Disease.risk.1 <- Disease.risk.1 %>% select(District, Member, Disease_Risk_Level, Percent)
#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
Disease.risk.1 <- Disease.risk.1 %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                            District == "2" ~ "Peri-urban",
                                                            District == "3" ~ "Rural"))

Disease.risk.1 <- Disease.risk.1 %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                          Member == "2" ~ "Women"))

#setting the levels of the Disease risk level

Disease.risk.1$Disease_Risk_Level = factor(Disease.risk.1$Disease_Risk_Level, levels = c("4", "3","2","1", "0"))

sort(unique(Disease.risk.1$Disease_Risk_Level))

##setting factor levels of the district & member

Disease.risk.1$district_f = factor(Disease.risk.1$District.Names, levels=c('Urban','Peri-urban','Rural'))
Disease.risk.1$member_f = factor(Disease.risk.1$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(Disease.risk.1, aes(member_f, Percent, fill=Disease_Risk_Level)) + 
  geom_bar(stat = "identity", position="stack")+
  facet_grid(~district_f) +    
  labs(title="Disease Risk Level in 3 districts", x="Member", y="Percent")

#MDD-W
#Merge MDD-W with the merged datasets
MDD <- readRDS("MDD-W.wide.rds")
MDD$INDI <- as.character(MDD$INDI)
MDD$Member <- as.character(MDD$Member)
MDD$District <- as.character(MDD$District)

dF <- left_join(dF, MDD, by = "INDI")

names(dF)
colnames(dF)[colnames(dF)=="District.x"] <- "District"
colnames(dF)[colnames(dF)=="Member.x"] <- "Member"
colnames(dF)[colnames(dF)=="1"] <- "Starchy"
colnames(dF)[colnames(dF)=="2"] <- "Pulses"
colnames(dF)[colnames(dF)=="3"] <- "Nuts_seeds"
colnames(dF)[colnames(dF)=="4"] <- "Dairy"
colnames(dF)[colnames(dF)=="5"] <- "Meat_poultry_fish"
colnames(dF)[colnames(dF)=="6"] <- "Egg"
colnames(dF)[colnames(dF)=="7"] <- "Darkgreen_veg"
colnames(dF)[colnames(dF)=="8"] <- "VitA_fruitveg"
colnames(dF)[colnames(dF)=="9"] <- "Other_vegs"
colnames(dF)[colnames(dF)=="10"] <- "Other_fruits"
colnames(dF)[colnames(dF)=="Total_Score"] <- "VDQI"


dF <- dF %>% select(INDI:age.2018, District, Member, BMI:disease.risk.com, WDDS, MDD_W, VDQI, knowledge_score, tot_variety:tot.moderation, Starchy:Other_fruits, Weight:Waist, D2A:D2C_1, AA17:A21)

write_rds(dF, "dF.multiple.variables.rds")

#Plot MDD-W
#Calculate % achieveing MDD-W
#Calculate % of individuals eat each food group
#Calculate number of member (Father - Mom - Child) in each district
#District 1 - Member 1
nrow(subset(dF, dF$District== "1" & dF$Member == "1")) #106
#District 1 - Member 2
nrow(subset(dF, dF$District== "1" & dF$Member == "2")) #109


#District 2 - Member 1
nrow(subset(dF, dF$District== "2" & dF$Member == "1")) #69
#District 2 - Member 2
nrow(subset(dF, dF$District== "2" & dF$Member == "2")) #63


#District 3 - Member 1
nrow(subset(dF, dF$District== "3" & dF$Member == "1")) #108
#District 3 - Member 2
nrow(subset(dF, dF$District== "3" & dF$Member == "2")) #110


No.of.people <- c(106, 109, 69, 63, 108, 110)

WDDS.dF <- dF %>% select(INDI, District, Member, WDDS, MDD_W)

WDDS.FGr <- WDDS.dF %>% group_by(District, Member) %>% summarise("Achieve" = length(which(MDD_W== "1")),
                                                                   "Not_achieve" = length(which(MDD_W== "0")))

#Double check
nrow(subset(WDDS.dF, WDDS.dF$District== "1" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 1)) #92
nrow(subset(WDDS.dF, WDDS.dF$District== "1" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 1)) #96

nrow(subset(WDDS.dF, WDDS.dF$District== "1" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 0)) #14
nrow(subset(WDDS.dF, WDDS.dF$District== "1" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 0)) #13

nrow(subset(WDDS.dF, WDDS.dF$District== "2" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 1)) #59
nrow(subset(WDDS.dF, WDDS.dF$District== "2" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 1)) #58

nrow(subset(WDDS.dF, WDDS.dF$District== "2" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 0)) #10
nrow(subset(WDDS.dF, WDDS.dF$District== "2" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 0)) #5

nrow(subset(WDDS.dF, WDDS.dF$District== "3" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 1)) #79
nrow(subset(WDDS.dF, WDDS.dF$District== "3" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 1)) #86

nrow(subset(WDDS.dF, WDDS.dF$District== "3" & WDDS.dF$Member == "1" & WDDS.dF$MDD_W == 0)) #29
nrow(subset(WDDS.dF, WDDS.dF$District== "3" & WDDS.dF$Member == "2" & WDDS.dF$MDD_W == 0)) #24
#Check done##
WDDS.FGr$Total.people <- No.of.people

WDDS.FGr <- mutate(WDDS.FGr, "Achieve.pct" = round((Achieve/Total.people)*100, digits = 2),
                   "Not_achieve.pct" = round((Not_achieve/Total.people)*100, digits = 2))


#Plot

#Transform to long format
WDDS.FGr.1 <- gather(WDDS.FGr, "MDD_W","Percent", c(6:7))      

WDDS.FGr.1 <- WDDS.FGr.1 %>% select(District, Member, MDD_W, Percent)

#### expanding color palette to include 13 colors; one for each food group in graph
library(RColorBrewer)
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

###adding district and member names
WDDS.FGr.1 <- WDDS.FGr.1 %>%mutate(District.Names = case_when(District == "1" ~ "Urban",
                                                              District == "2" ~ "Peri-urban",
                                                              District == "3" ~ "Rural"))

WDDS.FGr.1 <- WDDS.FGr.1 %>%mutate(Member.Names = case_when(Member == "1" ~ "Men",
                                                            Member == "2" ~ "Women"))
                                                            

#setting the levels of the Disease risk level

WDDS.FGr.1$MDD_W  = factor(WDDS.FGr.1$MDD_W, levels = c("Not_achieve.pct", "Achieve.pct"))

sort(unique(WDDS.FGr.1$MDD_W))

##setting factor levels of the district & member

WDDS.FGr.1$district_f = factor(WDDS.FGr.1$District.Names, levels=c('Urban','Peri-urban','Rural'))
WDDS.FGr.1$member_f = factor(WDDS.FGr.1$Member.Names, levels=c('Men','Women'))


#### running the plot 

ggplot(WDDS.FGr.1, aes(member_f, Percent, fill= MDD_W)) + 
  geom_bar(stat = "identity", position="stack")+
  facet_grid(~district_f) +    
  labs(title="MDD-W categorization in 3 districts", x="Member", y="Percent")


