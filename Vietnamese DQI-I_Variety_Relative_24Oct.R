setwd("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Variety/Relative serving size")
library(readr)
library(tidyverse)

Diet <- read_csv("Kyst1bV5.csv") #23,256 observations
FCT <- read_rds("FCT_DQI_master_full.rds")

#Check duplicated rows for several individuals: 16957091, 16957911, 16957092, 16957912 
Diet <- Diet[-c(5733, 5735, 5737, 5739, 5741, 5743, 5745, 5747, 5749, 5751, 5753, 5755,5757, 5759, 5761, 5763, 5765, 5767, 5769, 5771, 5773, 5775, 5777, 5779, 5781, 5783, 5785, 5787, 5789, 5791, 5793, 5795, 5797, 5799, 5801, 5803, 5805, 5807, 5809, 5811, 5813,5815, 5817, 5819, 5821),] 
Diet <- Diet[-c(6337, 6339, 6341, 6343, 6345, 6347, 6349, 6351, 6353, 6355, 6357, 6359, 6361, 6363, 6365, 6367, 6369, 6371, 6373, 6375, 6377, 6379, 6381, 6383, 6385, 6387, 6389, 6391, 6393, 6395, 6397, 6399, 6401, 6403, 6405, 6407, 6409, 6411, 6413, 6415, 6417, 6419, 6421, 6423, 6425, 6427, 6429, 6431),] 

#After removing these individuals, there are 23,163 observations left

#Calcuate Component 1: Variety
#Modify the FCT: regroup food items, categorize protein source, add serving size: done


FCT$Serving_size <- NULL

colnames(FCT)[colnames(FCT)=="New_Serving_size"] <- "Serving_size"  
  
names(Diet)


#Get DQI Foodgroup for Diet
names(FCT)
FCT <- FCT %>% select(FCODE:Energy)
Diet$FCODE <- as.character(Diet$FCODE)
FCT$FCODE <- as.character(FCT$FCODE)
setdiff(Diet$FCODE, FCT$FCODE) #there are 9 OR 5 FCODEs (dependending on OutPutKy (9) or Kystvb5 (5)) which are not matched --> delete them
Diet <- left_join(Diet, FCT)


#The following are in the variety group (Column DQI): meat/poultry/fish/egg, dairy/beans, grains, fruits, and vegetables
#Remove food items that are not in the variety group 
#Check the number of rows that have NA value in DQI column
Diet_NA <- Diet[which(is.na(Diet$FAO_MDD_Grp)), ] #39 Observations
unique(Diet_NA$FAO_MDD_Grp)
#[1] NA  

Diet_NA.1 <- Diet[which(is.na(Diet$DQI)), ] #5562 Observations
unique(Diet_NA.1$DQI)
#[1] NA  

unique(Diet_NA.1$FCODE)

Diet[which(is.na(Diet$GLCA)), ]
#Remove rows that has NA in DQI column
Diet <- Diet[!is.na(Diet$DQI),] #17,601 observations (Correct number: Clean Diet data has 23,163 observations, then minus 5562 NA observations = 17,601)

names(Diet)
Diet$ENERC_KCAL <- NULL
Diet$Energy <- NULL
colnames(Diet)[colnames(Diet)=="Energy.new"] <- "Energy"  


#Check if there is any NA value in Serving_size column
Diet_NA.2 <- Diet[which(is.na(Diet$Serving_size)), ] #0! yay

#Compute total number of serving for each FCODE
Diet$tot_svg <- round(Diet$Quantity/Diet$Serving_size, digits = 2)

####Compute general variety score (5 serving: 15 point, 4 svg:12, 3 svg: 9, 2 svg: 6, 1 svg: 3, 0 svg: 0)

#Aggregate total serving by DQI Food group for each individual
#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Diet$INDI)) #1070

#Aggregate
Sum.DQI.Member <- aggregate(Diet$tot_svg ~ Diet$INDI + Diet$District + Diet$Member + Diet$DQI, data = Diet, FUN = sum, na.rm=TRUE)

#Change column name
colnames(Sum.DQI.Member)[colnames(Sum.DQI.Member)=="Diet$INDI"] <- "INDI"
colnames(Sum.DQI.Member)[colnames(Sum.DQI.Member)=="Diet$District"] <- "District"
colnames(Sum.DQI.Member)[colnames(Sum.DQI.Member)=="Diet$Member"] <- "Member"
colnames(Sum.DQI.Member)[colnames(Sum.DQI.Member)=="Diet$DQI"] <- "DQI"
colnames(Sum.DQI.Member)[colnames(Sum.DQI.Member)=="Diet$tot_svg"] <- "tot_svg"


#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Sum.DQI.Member$INDI)) #1070

#Transpose rows to columns so that tot_svg groups become columns (unit: serving)
Sum.DQI.Member_wide <- Sum.DQI.Member %>% 
  spread(DQI, tot_svg) 

Total_serving_DQI <- Sum.DQI.Member_wide

#Assign NA value to cells that has value less than 1 serving (the condition to count each food group is that food group is consumed equal or larger than 1 serving)
Total_serving_DQI$`1` <- ifelse(Total_serving_DQI$`1` < 1, NA, Total_serving_DQI$`1`)
Total_serving_DQI$`2` <- ifelse(Total_serving_DQI$`2` < 1, NA, Total_serving_DQI$`2`)
Total_serving_DQI$`3` <- ifelse(Total_serving_DQI$`3` < 1, NA, Total_serving_DQI$`3`)
Total_serving_DQI$`4` <- ifelse(Total_serving_DQI$`4` < 1, NA, Total_serving_DQI$`4`)
Total_serving_DQI$`5` <- ifelse(Total_serving_DQI$`5` < 1, NA, Total_serving_DQI$`5`)

#Aggregate total gram by DQI Food group for each individual
#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Diet$INDI)) #1070

#Aggregate
Sum.DQI.Member.gram <- aggregate(Diet$Quantity ~ Diet$INDI + Diet$District + Diet$Member + Diet$DQI, data = Diet, FUN = sum, na.rm=TRUE)

names(Sum.DQI.Member.gram)

#Change column name
colnames(Sum.DQI.Member.gram)[colnames(Sum.DQI.Member.gram)=="Diet$INDI"] <- "INDI"
colnames(Sum.DQI.Member.gram)[colnames(Sum.DQI.Member.gram)=="Diet$District"] <- "District"
colnames(Sum.DQI.Member.gram)[colnames(Sum.DQI.Member.gram)=="Diet$Member"] <- "Member"
colnames(Sum.DQI.Member.gram)[colnames(Sum.DQI.Member.gram)=="Diet$DQI"] <- "DQI"
colnames(Sum.DQI.Member.gram)[colnames(Sum.DQI.Member.gram)=="Diet$Quantity"] <- "Gram"

write_rds(Sum.DQI.Member.gram, "Sum.DQI.gram.rds")
#Transform to wide format
Sum.DQI.Member.gram_wide <- Sum.DQI.Member.gram %>% 
  spread(DQI, Gram) 

Sum.DQI.Member.gram_wide$tot.gram <- rowSums(Sum.DQI.Member.gram_wide[,c(4:8)], na.rm = T)

colnames(Sum.DQI.Member.gram_wide)[colnames(Sum.DQI.Member.gram_wide)=="1"] <- "Gram1"
colnames(Sum.DQI.Member.gram_wide)[colnames(Sum.DQI.Member.gram_wide)=="2"] <- "Gram2"
colnames(Sum.DQI.Member.gram_wide)[colnames(Sum.DQI.Member.gram_wide)=="3"] <- "Gram3"
colnames(Sum.DQI.Member.gram_wide)[colnames(Sum.DQI.Member.gram_wide)=="4"] <- "Gram4"
colnames(Sum.DQI.Member.gram_wide)[colnames(Sum.DQI.Member.gram_wide)=="5"] <- "Gram5"

write_rds(Sum.DQI.Member.gram_wide, "Sum.DQI.gram_wide.rds")

#Compute kcal from food intake
Diet$Kcal <- round((Diet$Energy * Diet$Quantity)/100, digit = 2)

#Compute total energy in kcal for each GLCA food group code for each individual
Sum.DQI.Member.kcal <- aggregate(Diet$Kcal ~ Diet$INDI + Diet$District + Diet$Member + Diet$DQI, data = Diet, FUN = sum, na.rm=TRUE)

names(Sum.DQI.Member.kcal)

#Change column name
colnames(Sum.DQI.Member.kcal)[colnames(Sum.DQI.Member.kcal)=="Diet$INDI"] <- "INDI"
colnames(Sum.DQI.Member.kcal)[colnames(Sum.DQI.Member.kcal)=="Diet$District"] <- "District"
colnames(Sum.DQI.Member.kcal)[colnames(Sum.DQI.Member.kcal)=="Diet$Member"] <- "Member"
colnames(Sum.DQI.Member.kcal)[colnames(Sum.DQI.Member.kcal)=="Diet$DQI"] <- "DQI"
colnames(Sum.DQI.Member.kcal)[colnames(Sum.DQI.Member.kcal)=="Diet$Kcal"] <- "Kcal"

#Transform to wide format
Sum.DQI.Member.kcal_wide <- Sum.DQI.Member.kcal %>% 
  spread(DQI, Kcal) 

Sum.DQI.Member.kcal_wide$tot.kcal <- rowSums(Sum.DQI.Member.kcal_wide[,c(4:8)], na.rm = T)

colnames(Sum.DQI.Member.kcal_wide)[colnames(Sum.DQI.Member.kcal_wide)=="1"] <- "Kcal1"
colnames(Sum.DQI.Member.kcal_wide)[colnames(Sum.DQI.Member.kcal_wide)=="2"] <- "Kcal2"
colnames(Sum.DQI.Member.kcal_wide)[colnames(Sum.DQI.Member.kcal_wide)=="3"] <- "Kcal3"
colnames(Sum.DQI.Member.kcal_wide)[colnames(Sum.DQI.Member.kcal_wide)=="4"] <- "Kcal4"
colnames(Sum.DQI.Member.kcal_wide)[colnames(Sum.DQI.Member.kcal_wide)=="5"] <- "Kcal5"

#Combine total serving and gram
Sum.DQI <- left_join(Sum.DQI.Member, Sum.DQI.Member.gram)
Sum.DQI.Full <- left_join(Sum.DQI, Sum.DQI.Member.kcal)
Sum.DQI.Full_wide <- left_join(Sum.DQI.Member.gram_wide, Sum.DQI.Member.kcal_wide)

write_csv(Sum.DQI.Full, "DQI with total serving.csv")
write_rds(Sum.DQI.Full, "DQI with total serving.rds")
write_rds(Sum.DQI.Full_wide, "DQI with total serving_wide.rds")

#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Sum.DQI.Member$INDI)) #1070
#Compute "variety" score: 
#Count number of values in columns 1:5 of a row that are not NA
#Reference: https://stackoverflow.com/questions/39648111/count-how-many-values-in-some-cells-of-a-row-are-not-na-in-r
Total_serving_DQI$svg_count <- rowSums(!is.na(Total_serving_DQI[,4:8]))

#Compute score: assign score to each counting number: 5 - 15, 4 - 12, 3 - 9, 2- 6, 1- 3, 0-0
Total_serving_DQI$variety_general <- ifelse(Total_serving_DQI$svg_count == 5, 15, ifelse(Total_serving_DQI$svg_count ==4, 12, ifelse(Total_serving_DQI$svg_count ==3, 9, ifelse(Total_serving_DQI$svg_count==2, 6, ifelse(Total_serving_DQI$svg_count == 1, 3, 0)))))
####Done general variety score####

###Compute Protein_source variety score: >=3 different sources/day: 5, 2 sources: 3, 1 source: 1, None: 0 - condition: Intake of more than half the serving size per day is considered to be meaningful consumption.)
#Replace NA value in Proteinsource col with 0
Diet$DQI_Proteinsource[is.na(Diet$DQI_Proteinsource)] <- 0

#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Diet$INDI)) #1070

#Aggregate
Sum.Protein.Member <- aggregate(Diet$tot_svg ~ Diet$INDI + Diet$District + Diet$Member + Diet$DQI_Proteinsource, data = Diet, FUN = sum, na.rm=FALSE)

#Change column name
colnames(Sum.Protein.Member)[colnames(Sum.Protein.Member)=="Diet$INDI"] <- "INDI"
colnames(Sum.Protein.Member)[colnames(Sum.Protein.Member)=="Diet$District"] <- "District"
colnames(Sum.Protein.Member)[colnames(Sum.Protein.Member)=="Diet$Member"] <- "Member"
colnames(Sum.Protein.Member)[colnames(Sum.Protein.Member)=="Diet$DQI_Proteinsource"] <- "Proteinsource"
colnames(Sum.Protein.Member)[colnames(Sum.Protein.Member)=="Diet$tot_svg"] <- "tot_svg"

#Check the number of unique individuals in Diet file to make sure after aggregating, we dont lose any individuals
length(unique(Sum.Protein.Member$INDI)) #1070

#Transpose rows to columns so that tot_svg groups become columns (unit: serving)- Remove column "0": this is non-protein source
Sum.Protein.Member_wide <- Sum.Protein.Member %>% 
  spread(Proteinsource, tot_svg) 

Total_serving_protein <- Sum.Protein.Member_wide

Total_serving_protein$`0` <- NULL

#Assign NA value to cells that has value less than or equal to 0.5 serving (the condition to count each food group is that food group is consumed larger than 0.5 serving)
Total_serving_protein$`1` <- ifelse(Total_serving_protein$`1` <= 0.5, NA, Total_serving_protein$`1`)
Total_serving_protein$`2` <- ifelse(Total_serving_protein$`2` <= 0.5, NA, Total_serving_protein$`2`)
Total_serving_protein$`3` <- ifelse(Total_serving_protein$`3` <= 0.5, NA, Total_serving_protein$`3`)
Total_serving_protein$`4` <- ifelse(Total_serving_protein$`4` <= 0.5, NA, Total_serving_protein$`4`)
Total_serving_protein$`5` <- ifelse(Total_serving_protein$`5` <= 0.5, NA, Total_serving_protein$`5`)
Total_serving_protein$`6` <- ifelse(Total_serving_protein$`6` <= 0.5, NA, Total_serving_protein$`6`)

#Compute "variety_protein" score
#Reference: https://stackoverflow.com/questions/39648111/count-how-many-values-in-some-cells-of-a-row-are-not-na-in-r
Total_serving_protein$svg_count <- rowSums(!is.na(Total_serving_protein[,4:9]))

#Compute score
Total_serving_protein$variety_protein <- ifelse(Total_serving_protein$svg_count >= 3, 5, ifelse(Total_serving_protein$svg_count ==2, 3, ifelse(Total_serving_protein$svg_count ==1, 1, 0)))
####Done general variety_Protein score####

#Combine variety_general and variety_protein
names(Total_serving_protein)
names(Total_serving_DQI)
protein_score <- Total_serving_protein %>% select(INDI:Member, variety_protein)
general_score <- Total_serving_DQI %>% select(INDI:Member, variety_general)

Variety <- left_join(general_score, protein_score)

#Compute total variety score
Variety$tot_variety <- Variety$variety_general + Variety$variety_protein

Variety <- Variety %>% filter(Member == 1 | Member == 2)

write_csv(Variety, "Variety.csv")
write_rds(Variety, "Variety.rds")

##Done##




