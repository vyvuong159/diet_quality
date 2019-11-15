setwd("/Volumes/GoogleDrive/My Drive/Capstone Project/Data/Matching 3 datasets/Analyze association/11 Nov - Food Environment")
library(readxl)
library(tidyverse)

#Cau Giay (Urban site)
#Load datasets

Distance_CG <- read_excel("Distance from hh to all outlets_ CG.xlsx", 
                          +     sheet == "Distance _ CG")
Outlet_CG <- read_excel("File food outlets_Clean _ Cau Giay.xlsx", 
                        +     sheet == "Cau Giay - Urban area - Total")

length(unique(Distance_CG$ID_POINT)) #159 unique households

names(Outlet_CG)
#Change name of columns to merge 2 datasets
colnames(Outlet_CG)[colnames(Outlet_CG)=="Observation nr."] <- "ID_NEAR"

#Merge 2 datasets = ID_NEAR
dF <- merge(Distance_CG, Outlet_CG, by = "ID_NEAR", all = F)
length(unique(dF$ID_POINT)) #Still 159, but lose some observations because there are no outlets in the Outlet_CG dataset

setdiff(Distance_CG$ID_NEAR, dF$ID_NEAR)
#"16" "48"

#List unique type of outlets
unique(dF$`type of food outlets (again)`)
#[1] "Casual dining restaurant"                                     
#[2] "Small-scale street vendors/ Pop up Semi-permanent stand"      
#[3] "New style convenience stores"                                 
#[4] "A traditional groceries store: Independent Small grocers"     
#[5] "mobile vendor"                                                
#[6] "Restaurant and diners"                                        
#[7] "Bakery"                                                       
#[8] "Cafe"                                                         
#[9] "A traditional groceries store: Food/Drink/Tobacco specialists"
#[10] "Mobile vendor"                                                
#[11] "Restaurants and diners"                                       
#[12] "Bia Hoi"                                                      
#[13] "A traditional groceries store: independent Small grocers"     
#[14] "Bia hoi"                                                      
#[15] "Fruit and vegetable shop"  

colnames(dF)[colnames(dF)=="type of food outlets (again)"] <- "Outlet_Type"

#Create outlet code
#1 - supermarket, 2- convenient store, 3-specialized shop, 4-formal open market, 5-Traditional groceries store, 6-Informal street market/vendor, 7-Casual restaurant, 8-Formal restaurant/cafe
dF$Outlet_Code <- ifelse(dF$Outlet_Type == "Casual dining restaurant" | dF$Outlet_Type == "Bia Hoi" | dF$Outlet_Type == "Bia hoi", "7", 
                  ifelse(dF$Outlet_Type == "Small-scale street vendors/ Pop up Semi-permanent stand" |dF$Outlet_Type == "mobile vendor" |dF$Outlet_Type == "Mobile vendor", "6", 
                  ifelse(dF$Outlet_Type == "New style convenience stores" | dF$Outlet_Type== "Fruit and vegetable shop" | dF$Outlet_Type == "Bakery", "3", 
                  ifelse(dF$Outlet_Type == "A traditional groceries store: Independent Small grocers" | dF$Outlet_Type == "A traditional groceries store: Food/Drink/Tobacco specialists" | dF$Outlet_Type== "A traditional groceries store: independent Small grocers", "5",
                  ifelse(dF$Outlet_Type== "Restaurant and diners" | dF$Outlet_Type == "Restaurants and diners" | dF$Outlet_Type == "Cafe", "8", NA)))))

dF %>% filter(is.na(Outlet_Code)) %>% view() #0 observation

colnames(dF)[colnames(dF)=="ID_POINT"] <- "Full_ID"

dF$District <- "1"

#Dong Anh (Peri-urban site)

Distance_DA <- read_excel("Distance from hh to all outlets_ DA.xlsx", 
                          +     sheet = "Distance _ DA")
Outlet_DA <- read_excel("File food outlets_Clean _ Dong Anh.xlsx", 
                        +     sheet = "Dong Anh - Peri _ ")

length(unique(Distance_DA$ID_POINT)) #159 unique households

names(Outlet_DA)
#Change name of columns to merge 2 datasets
colnames(Outlet_DA)[colnames(Outlet_DA)=="Observation nr."] <- "ID_NEAR"

#Merge 2 datasets = ID_NEAR
dF.DA <- merge(Distance_DA, Outlet_DA, by = "ID_NEAR", all = F)
length(unique(dF.DA$ID_POINT)) #Still 159, but lose some observations because there are no outlets in the Outlet_CG dataset

setdiff(Distance_DA$ID_NEAR, dF.DA$ID_NEAR)
#0: no difference

#List unique type of outlets
unique(dF.DA$`type of food outlets (again)`)
#[1] "Small-scale street vendors/ Pop up Semi-permanent stand"      
#[2] "A traditional groceries store: Independent Small grocers"     
#[3] NA                                                             
#[4] "small-scale street vendors/ Pop up Semi-permanent stand"      
#[5] "Casual dining restaurant"                                     
#[6] "Mobile vendor"                                                
#[7] "A traditional groceries store: Food/Drink/Tobacco specialists"
#[8] "Bakery"                                                       
#[9] "na"                                                           
#[10] "Bia hoi"                                                      
#[11] "New style convenience stores"                                 
#[12] "Wet market"                                                   
#[13] "Cafe"                                                         
#[14] "Fruit and vegetable shop"                                     
#[15] "Bia Hoi"  

colnames(dF.DA)[colnames(dF.DA)=="type of food outlets (again)"] <- "Outlet_Type"

#Create outlet code
#1 - supermarket, 2- convenient store, 3-specialized shop, 4-formal open market, 5-Traditional groceries store, 6-Informal street market/vendor, 7-Casual restaurant, 8-Formal restaurant/cafe
dF.DA$Outlet_Code <- ifelse(dF.DA$Outlet_Type == "Casual dining restaurant" | dF.DA$Outlet_Type == "Bia Hoi" | dF.DA$Outlet_Type == "Bia hoi", "7", 
                     ifelse(dF.DA$Outlet_Type == "Small-scale street vendors/ Pop up Semi-permanent stand" |dF.DA$Outlet_Type == "small-scale street vendors/ Pop up Semi-permanent stand"|dF.DA$Outlet_Type == "Mobile vendor", "6", 
                     ifelse(dF.DA$Outlet_Type == "Wet market", "4",
                     ifelse(dF.DA$Outlet_Type == "New style convenience stores" | dF.DA$Outlet_Type== "Fruit and vegetable shop" | dF.DA$Outlet_Type == "Bakery", "3", 
                     ifelse(dF.DA$Outlet_Type == "A traditional groceries store: Independent Small grocers" | dF.DA$Outlet_Type == "A traditional groceries store: Food/Drink/Tobacco specialists", "5",
                     ifelse(dF.DA$Outlet_Type == "Cafe", "8", NA))))))

#Check NA value
dF.DA %>% filter(is.na(Outlet_Code)) %>% view()
#Unique ID_NEAR (establishments): 100, 121, 124, 36, 71, 90, 99 --> need to check manually what type of outlet these establishments are by checking photo or/and google maps
#100: A traditional groceries store
#121: A traditional groceries store
#124: A traditional groceries store
#36: can't identify
#71: A traditional groceries store
#90: A traditional groceries store
#99: A traditional groceries store

#Fill in outlet_code for these missing values
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "100"] <- "5"
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "121"] <- "5"
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "124"] <- "5"
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "71"] <- "5"
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "90"] <- "5"
dF.DA$Outlet_Code[dF.DA$ID_NEAR == "99"] <- "5"

dF.DA <- dF.DA[!is.na(dF.DA$Outlet_Code),]

#Change name of some variables, and fill in district code
colnames(dF.DA)[colnames(dF.DA)=="ID_POINT"] <- "Full_ID"

dF.DA$District <- "2"

#Moc Chau
Distance_MC <- read_excel("Distance from hh to all outlets_ MC.xlsx", 
                          +     sheet = "Distance _ MC")
Outlet_MC <- read_excel("File food outlets_Clean _ Moc Chau.xlsx", 
                        +     sheet = "Moc Chau - Rural area _ Total")

length(unique(Distance_MC$ID_POINT)) #112 unique households

names(Outlet_MC)
#Change name of columns to merge 2 datasets
colnames(Outlet_MC)[colnames(Outlet_MC)=="Observation nr."] <- "ID_NEAR"

#Merge 2 datasets = ID_NEAR
dF.MC <- merge(Distance_MC, Outlet_MC, by = "ID_NEAR", all = F)
length(unique(dF.MC$ID_POINT)) #Still 112

setdiff(Distance_MC$ID_NEAR, dF.MC$ID_NEAR)
#0 observations

#List unique type of outlets
unique(dF.MC$`Type of outlet`)
#[1] "A traditional groceries store: Independent Small grocers"
#[2] "Casual dining restaurant"                                
#[3] "na"                                                      
#[4] "Small-scale street vendors/ Pop up Semi-permanent stand" 
#[5] "Wet Market"                                              
#[6] "Bai Hoi" 

colnames(dF.MC)[colnames(dF.MC)=="Type of outlet"] <- "Outlet_Type"

#Create outlet code
#1 - supermarket, 2- convenient store, 3-specialized shop, 4-formal open market, 5-Traditional groceries store, 6-Informal street market/vendor, 7-Casual restaurant, 8-Formal restaurant/cafe
dF.MC$Outlet_Code <- ifelse(dF.MC$Outlet_Type == "Casual dining restaurant" | dF.MC$Outlet_Type == "Bai Hoi", "7", 
                  ifelse(dF.MC$Outlet_Type == "Small-scale street vendors/ Pop up Semi-permanent stand", "6", 
                  ifelse(dF.MC$Outlet_Type == "A traditional groceries store: Independent Small grocers", "5",
                  ifelse(dF.MC$Outlet_Type== "Wet Market", "4", NA))))

dF.MC %>% filter(is.na(Outlet_Code)) %>% view() 

#Unique ID_NEAR that have NA values: 21: small street market
#Fill in outlet_code for these missing values
dF.MC$Outlet_Code[dF.MC$ID_NEAR == "21"] <- "6"

colnames(dF.MC)[colnames(dF.MC)=="ID_POINT"] <- "Full_ID"

dF.MC$District <- "3"


#Count the number of unique establishment in each outlet type within x METERS from participants' residence
#1000meters (or 1km)
#Subset dataset that has the distance <= 1000 meters
CG.1000 <- dF %>% filter(DISTANCE <= 1000)
DA.1000 <- dF.DA %>% filter(DISTANCE <= 1000)
MC.1000 <- dF.MC %>% filter(DISTANCE <= 1000)

#Extract unique ID_NEAR (establishment)
CG.1000.unique <- CG.1000[!duplicated(CG.1000$ID_NEAR), ]
DA.1000.unique <- DA.1000[!duplicated(DA.1000$ID_NEAR), ]
MC.1000.unique <- MC.1000[!duplicated(MC.1000$ID_NEAR), ]

#Aggregate by outlet code
unique(CG.1000$Outlet_Code)
#"7" "6" "3" "5" "8"
#So there are no 1, 2, 4 outlet type within 1km in CG

unique(DA.1000$Outlet_Code)
#"6" "5" "7" "3" "4" "8" 
#So there are no 1, 2 outlet type within 1km in DA

unique(MC.1000$Outlet_Code)
#"5" "7" "6" "4"
#So there are no 1, 2, 3, 8 outlet type within 1km in MC

#RBIND 3 datasets
names(CG.1000.unique)
names(DA.1000.unique)
names(MC.1000.unique)

CG.1000.unique <- CG.1000.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #262 obs, 15 var
DA.1000.unique <- DA.1000.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #228 obs, 15 var
MC.1000.unique <- MC.1000.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #49obs, 15 var

dF.combined <- rbind(CG.1000.unique, DA.1000.unique, MC.1000.unique) #539 Observations

Outlet.type.1000 <- dF.combined %>% group_by(District) %>% summarise("1" = length(which(Outlet_Code== "1")), 
                                                                         "2" = length(which(Outlet_Code== "2")),
                                                                         "3" = length(which(Outlet_Code== "3")), 
                                                                         "4" = length(which(Outlet_Code== "4")), 
                                                                         "5" = length(which(Outlet_Code== "5")), 
                                                                         "6" = length(which(Outlet_Code== "6")), 
                                                                         "7" = length(which(Outlet_Code== "7")), 
                                                                         "8" = length(which(Outlet_Code== "8")))

#Double check the number - can ignore these codes
CG.1000.3 <- CG.1000 %>% filter(Outlet_Code== "3")
length(unique(CG.1000.3$ID_NEAR)) #14

CG.1000.5 <- CG.1000 %>% filter(Outlet_Code== "5")
length(unique(CG.1000.5$ID_NEAR)) #55

CG.1000.6 <- CG.1000 %>% filter(Outlet_Code== "6")
length(unique(CG.1000.6$ID_NEAR)) #63

CG.1000.7 <- CG.1000 %>% filter(Outlet_Code== "7")
length(unique(CG.1000.7$ID_NEAR)) #101

CG.1000.8 <- CG.1000 %>% filter(Outlet_Code== "8")
length(unique(CG.1000.8$ID_NEAR)) #29



#500meters 
#Get mode value/mean/median for distance to determine which distance we want to examine

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(dF$DISTANCE) #4382.553
getmode(dF.DA$DISTANCE) #9966.452
getmode(dF.MC$DISTANCE) #12460

summary(dF$DISTANCE)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#1.439 1101.589 1853.510 2143.470 3049.268 5739.615   

summary(dF.DA$DISTANCE)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#5.388  3322.653  6136.477  6680.103  9668.917 16880.938 

summary(dF.MC$DISTANCE)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#5.95 10654.20 12494.18 13878.75 19131.66 37608.48 

#Subset dataset that has the distance <= 1000 meters
CG.500 <- dF %>% filter(DISTANCE <= 500)
DA.500 <- dF.DA %>% filter(DISTANCE <= 500)
MC.500 <- dF.MC %>% filter(DISTANCE <= 500)

#Extract unique ID_NEAR (establishment)
CG.500.unique <- CG.500[!duplicated(CG.500$ID_NEAR), ]
DA.500.unique <- DA.500[!duplicated(DA.500$ID_NEAR), ]
MC.500.unique <- MC.500[!duplicated(MC.500$ID_NEAR), ]

#Aggregate by outlet code
unique(CG.500$Outlet_Code)
#"7" "6" "3" "5" "8"
#So there are no 1, 2, 4 outlet type within 500m in CG

unique(DA.500$Outlet_Code)
#"6" "5" "7" "3" "4" "8" 
#So there are no 1, 2 outlet type within 500m in DA

unique(MC.500$Outlet_Code)
#"5" "7" "6" "4"
#So there are no 1, 2, 3, 8 outlet type within 500m in MC

#RBIND 3 datasets
names(CG.500.unique)
names(DA.500.unique)
names(MC.500.unique)

CG.500.unique <- CG.500.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #262 obs, 15 var
DA.500.unique <- DA.500.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #228 obs, 15 var
MC.500.unique <- MC.500.unique %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #49obs, 15 var

dF.combined.500 <- rbind(CG.500.unique, DA.500.unique, MC.500.unique) #537 Observations

Outlet.type.500 <- dF.combined.500 %>% group_by(District) %>% summarise("1" = length(which(Outlet_Code== "1")), 
                                                                     "2" = length(which(Outlet_Code== "2")),
                                                                     "3" = length(which(Outlet_Code== "3")), 
                                                                     "4" = length(which(Outlet_Code== "4")), 
                                                                     "5" = length(which(Outlet_Code== "5")), 
                                                                     "6" = length(which(Outlet_Code== "6")), 
                                                                     "7" = length(which(Outlet_Code== "7")), 
                                                                     "8" = length(which(Outlet_Code== "8")))
#Number of outlets within 500 - 1000 - 2000m are not different at all because it depends on the location of the residence

write_rds(Outlet.type.1000, "Outlet.type.1000.byCB.rds")
write_rds(Outlet.type.500, "Outlet.type.500.byCB.rds")
write_rds(dF.Full, "Food.environment.full.byCB.rds")

#Calculate the average distance from each household's residence to each type of outlet
Average.dis <- dF.Full %>% group_by(Full_ID, Outlet_Code, District) %>% summarise("Average_Distance" = mean(DISTANCE)) 

#Mean average for each district
Average.dis.mean <- aggregate(Average.dis$Average_Distance ~ Average.dis$District + Average.dis$Outlet_Code, data = Average.dis, FUN = mean, na.rm=TRUE)

names(Average.dis.mean)
colnames(Average.dis.mean)[colnames(Average.dis.mean)=="Average.dis$District"] <- "District"
colnames(Average.dis.mean)[colnames(Average.dis.mean)=="Average.dis$Outlet_Code"] <- "Outlet_Code"
colnames(Average.dis.mean)[colnames(Average.dis.mean)=="Average.dis$Average_Distance"] <- "Mean.Average_Distance"

write_rds(Average.dis, "Average.distance.byCB.rds")
write_rds(Average.dis.mean, "Mean.Average.distance.byCB.rds")


#Calculate the shortest distance from study participant's residence to each type of outlet
dF.CG <- dF
dF.CG <- dF.CG %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #41658 obs, 15 var
dF.DA <- dF.DA %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #38532 obs, 15 var
dF.MC <- dF.MC %>% select(ID_NEAR:DISTANCE, District, Outlet_Type, 'Grains, roots, and rubers':Outlet_Code) #5650 obs, 15 var

dF.Full <- rbind(dF.CG, dF.DA, dF.MC)
length(unique(dF.Full$Full_ID)) #430

Shortest.dis <- dF.Full %>% group_by(Full_ID, Outlet_Code, District) %>% summarise("Shortest_Distance" = min(DISTANCE)) 
length(unique(Shortest.dis$Full_ID)) #430

#Calculate mean shortest distance
Shortest.dis.mean <- aggregate(Shortest.dis$Shortest_Distance ~ Shortest.dis$District + Shortest.dis$Outlet_Code, data = Shortest.dis, FUN = mean, na.rm=TRUE)

names(Shortest.dis.mean)
colnames(Shortest.dis.mean)[colnames(Shortest.dis.mean)=="Shortest.dis$District"] <- "District"
colnames(Shortest.dis.mean)[colnames(Shortest.dis.mean)=="Shortest.dis$Outlet_Code"] <- "Outlet_Code"
colnames(Shortest.dis.mean)[colnames(Shortest.dis.mean)=="Shortest.dis$Shortest_Distance"] <- "Mean.Shortest_Distance"

write_rds(Shortest.dis, "Shortest.distance.byCB.rds")
write_rds(Shortest.dis.mean, "Mean.Shortest.distance.byCB.rds")

#Calculate average distance to the 3 closest different outlets

#OutPutHuong data
setwd("/Volumes/GoogleDrive/My Drive/Capstone Project/Data/Matching 3 datasets/Analyze association/11 Nov - Food Environment/OutPutHuong")
Point_Purchase <- read_excel("OutPutHuong.PointofPurchase.xlsx")
Distance_Purchase <- read_excel("OutPutHuong.DistanceofPurchase.xlsx")

#Code of outlet: 
#1 - Traditional market, 2 - supermarket, 3 - convenience store, 4 - specialized store, 
#5 - street vendor, 6 - direct purchase from farm, 7 - other, 8 - don't know

#Create INDI for 2 datasets
Point_Purchase$INDI <- paste0(Point_Purchase$MaDBGD, Point_Purchase$BoMeTre, collaspe = NULL)
Distance_Purchase$INDI <- paste0(Distance_Purchase$MaDBGD, Distance_Purchase$BoMeTre, collaspe = NULL)

names(Point_Purchase)
colnames(Point_Purchase)[colnames(Point_Purchase)=="MA_TP_Code"] <- "FCODE"
colnames(Distance_Purchase)[colnames(Distance_Purchase)=="MA_TP_Code"] <- "FCODE"

Point_Purchase <- Point_Purchase %>% select(INDI, Time, MaDBGD, BoMeTre, huyen, FCODE, k7a, FAO_MDD_Grp)
data = left_join(Point_Purchase, Distance_Purchase)

names(data)
data <- data %>% select(INDI, MaDBGD, huyen, BoMeTre, Time:FCODE, FAO_MDD_Grp, NAME_LOC, Value_V, Value_E, AvgOfk8, k7a)

#Subset data for CG - DA - MC
Urban <- data %>% filter(huyen  == "1")
Peri_urban <- data %>% filter(huyen  == "2")
Rural <- data %>% filter(huyen  == "3")

table(Urban$k7a)
table(Peri_urban$k7a)
table(Rural$k7a)

#Get Food Group code
FCT <- readRDS("FCT_DQI_master_full.rds")
FCT <- FCT %>% select(FCODE, Food_Grp, FAO_MDD_Grp)

data$FAO_MDD_Grp <- NULL #Delete the old FAO Group to replace by the modified FAO Group
data$FCODE <- as.character(data$FCODE)
data <- left_join(data, FCT)

names(data)
data <- data %>% select(INDI:NAME_LOC, Food_Grp, FAO_MDD_Grp, AvgOfk8, k7a, Value_V, Value_E)

#Calculate the average distance from each member's residence to each type of outlet

Average.dis.24h <- data %>% group_by(INDI, k7a, huyen, BoMeTre, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 
Average.dis.24h.household <- data %>% group_by(MaDBGD, k7a, huyen, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 

#Average from time 1 and time 2
Average.dis.24h.1 <- Average.dis.24h %>% group_by(INDI, k7a, huyen, BoMeTre) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 
Average.dis.24h.1.household <- Average.dis.24h.household %>% group_by(MaDBGD, k7a, huyen) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 


#Transpose to wide format
Average.dis.24h.1$k7a <- as.character(Average.dis.24h.1$k7a)
Average.dis.24h.1_wide <- Average.dis.24h.1 %>% spread("k7a", "Average_Distance") 
Average.dis.24h.1.household_wide <- Average.dis.24h.1.household %>% spread("k7a", "Average_Distance") 

#Rename for individuals dataset
names(Average.dis.24h.1)
colnames(Average.dis.24h.1)[colnames(Average.dis.24h.1)=="huyen"] <- "District"
colnames(Average.dis.24h.1)[colnames(Average.dis.24h.1)=="k7a"] <- "Outlet_Code"
colnames(Average.dis.24h.1)[colnames(Average.dis.24h.1)=="BoMeTre"] <- "Member"

names(Average.dis.24h.1_wide)
colnames(Average.dis.24h.1_wide)[colnames(Average.dis.24h.1_wide)=="huyen"] <- "District"
colnames(Average.dis.24h.1_wide)[colnames(Average.dis.24h.1_wide)=="BoMeTre"] <- "Member"

#Rename for household dataset
names(Average.dis.24h.1.household)
colnames(Average.dis.24h.1.household)[colnames(Average.dis.24h.1.household)=="k7a"] <- "Outlet_Code"
colnames(Average.dis.24h.1.household)[colnames(Average.dis.24h.1.household)=="huyen"] <- "District"


names(Average.dis.24h.1.household_wide)
colnames(Average.dis.24h.1_wide)[colnames(Average.dis.24h.1_wide)=="huyen"] <- "District"


#Mean average for each district
Average.dis.24h.mean <- aggregate(Average.dis.24h.1$Average_Distance ~ Average.dis.24h.1$District + Average.dis.24h.1$Outlet_Code, data = Average.dis.24h.1, FUN = mean, na.rm=TRUE)
Average.dis.24h.mean.m <- aggregate(Average.dis.24h.1$Average_Distance ~ Average.dis.24h.1$District + Average.dis.24h.1$Member + Average.dis.24h.1$Outlet_Code, data = Average.dis.24h.1, FUN = mean, na.rm=TRUE)

names(Average.dis.24h.mean)
colnames(Average.dis.24h.mean)[colnames(Average.dis.24h.mean)=="Average.dis.24h.1$District"] <- "District"
colnames(Average.dis.24h.mean)[colnames(Average.dis.24h.mean)=="Average.dis.24h.1$Outlet_Code"] <- "Outlet_Code"
colnames(Average.dis.24h.mean)[colnames(Average.dis.24h.mean)=="Average.dis.24h.1$Average_Distance"] <- "Mean.Average_Distance"

names(Average.dis.24h.mean.m)
colnames(Average.dis.24h.mean.m)[colnames(Average.dis.24h.mean.m)=="Average.dis.24h.1$District"] <- "District"
colnames(Average.dis.24h.mean.m)[colnames(Average.dis.24h.mean.m)=="Average.dis.24h.1$Outlet_Code"] <- "Outlet_Code"
colnames(Average.dis.24h.mean.m)[colnames(Average.dis.24h.mean.m)=="Average.dis.24h.1$Average_Distance"] <- "Mean.Average_Distance"
colnames(Average.dis.24h.mean.m)[colnames(Average.dis.24h.mean.m)=="Average.dis.24h.1$Member"] <- "Member"


write_rds(Average.dis.24h.1, "Average distance to outlets.by24h.rds")
write_rds(Average.dis.24h.1_wide, "Average distance to outlets.by24h_wide.rds")
write_rds(Average.dis.24h.1.household_wide, "Average distance to outlets.household.by24h_wide.rds")


write_rds(Average.dis.24h.mean, "Mean Average distance to outlets.district.by24h.rds")
write_rds(Average.dis.24h.mean.m, "Mean Average distance to outlets.district-member.by24h.rds")

#Calculate the shortest distance from member's house to each type of outlet
Shortest.dis.24h <- data %>% group_by(INDI, k7a, Time, huyen, BoMeTre) %>% summarise("Shortest_Distance" = min(AvgOfk8, na.rm = T)) 

length(unique(Shortest.dis.24h$INDI)) #1104

Shortest.dis.24h.1 <- Shortest.dis.24h %>% group_by(INDI, k7a, huyen, BoMeTre) %>% summarise("Shortest_Distance" = min(Shortest_Distance, na.rm = T)) 

length(unique(Shortest.dis.24h.1$INDI)) #1104

#By household
Shortest.dis.24h.household <- data %>% group_by(MaDBGD, k7a, huyen) %>% summarise("Shortest_Distance" = min(AvgOfk8, na.rm = T)) 
length(unique(Shortest.dis.24h.household$MaDBGD)) #369


#Transpose to wide format
names(Shortest.dis.24h.household)
Shortest.dis.24h.household_wide <- Shortest.dis.24h.household %>% spread("k7a", "Shortest_Distance") 

names(Shortest.dis.24h.household_wide)
colnames(Shortest.dis.24h.household_wide)[colnames(Shortest.dis.24h.household_wide)=="huyen"] <- "District"

#Calculate mean shortest distance by district
#Replace inf value by na
is.na(Shortest.dis.24h.1) <- sapply(Shortest.dis.24h.1, is.infinite)
is.na(Shortest.dis.24h.1_wide) <- sapply(Shortest.dis.24h.1_wide, is.infinite)
is.na(Shortest.dis.24h.household) <- sapply(Shortest.dis.24h.household, is.infinite)
is.na(Shortest.dis.24h.household_wide) <- sapply(Shortest.dis.24h.household_wide, is.infinite)


Shortest.dis.mean.24h <- aggregate(Shortest.dis.24h.1$Shortest_Distance ~ Shortest.dis.24h.1$huyen + Shortest.dis.24h.1$BoMeTre + Shortest.dis.24h.1$k7a, data = Shortest.dis.24h.1, FUN = mean, na.rm=TRUE)
Shortest.dis.mean.24h.household <- aggregate(Shortest.dis.24h.household$Shortest_Distance ~ Shortest.dis.24h.household$huyen + Shortest.dis.24h.household$k7a, data = Shortest.dis.24h.household, FUN = mean, na.rm=TRUE)

names(Shortest.dis.mean.24h)
colnames(Shortest.dis.mean.24h)[colnames(Shortest.dis.mean.24h)=="Shortest.dis.24h.1$huyen"] <- "District"
colnames(Shortest.dis.mean.24h)[colnames(Shortest.dis.mean.24h)=="Shortest.dis.24h.1$BoMeTre"] <- "Member"
colnames(Shortest.dis.mean.24h)[colnames(Shortest.dis.mean.24h)=="Shortest.dis.24h.1$k7a"] <- "Outlet_Code"
colnames(Shortest.dis.mean.24h)[colnames(Shortest.dis.mean.24h)=="Shortest.dis.24h.1$Shortest_Distance"] <- "Mean.Shortest_Distance"

names(Shortest.dis.24h.1)
colnames(Shortest.dis.24h.1)[colnames(Shortest.dis.24h.1)=="k7a$huyen"] <- "Outlet_Code"
colnames(Shortest.dis.24h.1)[colnames(Shortest.dis.24h.1)=="huyen"] <- "District"
colnames(Shortest.dis.24h.1)[colnames(Shortest.dis.24h.1)=="BoMeTre"] <- "Member"

names(Shortest.dis.24h.1_wide)
colnames(Shortest.dis.24h.1_wide)[colnames(Shortest.dis.24h.1_wide)=="huyen"] <- "District"
colnames(Shortest.dis.24h.1_wide)[colnames(Shortest.dis.24h.1_wide)=="BoMeTre"] <- "Member"

names(Shortest.dis.mean.24h.household)
colnames(Shortest.dis.mean.24h.household)[colnames(Shortest.dis.mean.24h.household)=="Shortest.dis.24h.household$huyen"] <- "District"
colnames(Shortest.dis.mean.24h.household)[colnames(Shortest.dis.mean.24h.household)=="Shortest.dis.24h.household$k7a"] <- "Outlet_Code"
colnames(Shortest.dis.mean.24h.household)[colnames(Shortest.dis.mean.24h.household)=="Shortest.dis.24h.household$Shortest_Distance"] <- "Mean.Shortest_Distance"

write_rds(Shortest.dis.24h.1, "Shortest.distance.by24h.rds")
write_rds(Shortest.dis.24h.1_wide, "Shortest.distance_wide.by24h.rds")

write_rds(Shortest.dis.24h.1, "Shortest.distance.household.by24h.rds")
write_rds(Shortest.dis.24h.household_wide, "Shortest.distance_wide.household.by24h.rds")

write_rds(Shortest.dis.mean.24h, "Mean.Shortest.distance.by24h.rds")
write_rds(Shortest.dis.mean.24h.household, "Mean.Shortest.distance.household.by24h.rds")

#Calculate the mean distance to buy each food group
#Include grouping by type of outlet
Average.dis.foodgroup.24h <- data %>% group_by(INDI, k7a, huyen, Food_Grp, BoMeTre, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 

Average.dis.foodgroup.24h.household <- data %>% group_by(MaDBGD, k7a, huyen, Food_Grp, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 

#Average from time 1 and time 2
Average.dis.foodgroup.24h.1 <- Average.dis.foodgroup.24h %>% group_by(INDI, k7a, huyen, Food_Grp, BoMeTre) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 
Average.dis.foodgroup.24h.household.1 <- Average.dis.foodgroup.24h.household %>% group_by(MaDBGD, k7a, huyen, Food_Grp) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 

#Exclude grouping by type of outlet
Average.dis.foodgroup.24h.nooutlet <- data %>% group_by(INDI, huyen, Food_Grp, BoMeTre, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 

Average.dis.foodgroup.24h.household.nooutlet <- data %>% group_by(MaDBGD, huyen, Food_Grp, Time) %>% summarise("Average_Distance" = mean(AvgOfk8, na.rm = T)) 

#Average from time 1 and time 2
Average.dis.foodgroup.24h.nooutlet.1 <- Average.dis.foodgroup.24h.nooutlet %>% group_by(INDI, huyen, Food_Grp, BoMeTre) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 
Average.dis.foodgroup.24h.household.nooutlet.1 <- Average.dis.foodgroup.24h.household.nooutlet %>% group_by(MaDBGD, huyen, Food_Grp) %>% summarise("Average_Distance" = mean(Average_Distance, na.rm = T)) 

#Transpose to wide format
Average.dis.foodgroup.24h.nooutlet.1_wide <- Average.dis.foodgroup.24h.nooutlet.1 %>% spread("Food_Grp", "Average_Distance") 
Average.dis.foodgroup.24h.household.nooutlet.1_wide <- Average.dis.foodgroup.24h.household.nooutlet.1 %>% spread("Food_Grp", "Average_Distance") 

Average.dis.foodgroup.24h.nooutlet.1_wide <- Average.dis.foodgroup.24h.nooutlet.1_wide %>% select(INDI:BoMeTre, '1','2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22')
Average.dis.foodgroup.24h.household.nooutlet.1_wide <- Average.dis.foodgroup.24h.household.nooutlet.1_wide %>% select(MaDBGD:huyen, '1','2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22')

#Calculate mean average distance based on food group
Mean.Average.dis.foodgroup.24h.nooutlet <- aggregate(Average.dis.foodgroup.24h.nooutlet.1$Average_Distance ~ Average.dis.foodgroup.24h.nooutlet.1$huyen + Average.dis.foodgroup.24h.nooutlet.1$BoMeTre + Average.dis.foodgroup.24h.nooutlet.1$Food_Grp, data = Average.dis.foodgroup.24h.nooutlet.1, FUN = mean, na.rm=TRUE)
names(Mean.Average.dis.foodgroup.24h.nooutlet)

colnames(Mean.Average.dis.foodgroup.24h.nooutlet)[colnames(Mean.Average.dis.foodgroup.24h.nooutlet)=="Average.dis.foodgroup.24h.nooutlet.1$huyen"] <- "District"
colnames(Mean.Average.dis.foodgroup.24h.nooutlet)[colnames(Mean.Average.dis.foodgroup.24h.nooutlet)=="Average.dis.foodgroup.24h.nooutlet.1$BoMeTre"] <- "Member"
colnames(Mean.Average.dis.foodgroup.24h.nooutlet)[colnames(Mean.Average.dis.foodgroup.24h.nooutlet)=="Average.dis.foodgroup.24h.nooutlet.1$Food_Grp"] <- "Food_Grp"
colnames(Mean.Average.dis.foodgroup.24h.nooutlet)[colnames(Mean.Average.dis.foodgroup.24h.nooutlet)=="Average.dis.foodgroup.24h.nooutlet.1$Average_Distance"] <- "Average_Distance"

 
 
#Find out the FCODEs that do not have value in FCT:
#20482: nộm/ salat trộn các loại rau
#20448: bột ngũ cốc
#20206: Nước ép táo
#20186: Nước ép dứa
#20476: xôi ruốc
#20413: chả cá (các loại)
#20472: dồi chó
#Consider if we should delete these FCODEs

