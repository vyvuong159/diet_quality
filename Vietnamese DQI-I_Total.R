setwd("~/Study/U.S./Cornell/Capstone Project/Data/24h/Diet Quality Indicators/DQI-I/Total score")

Variety <- readRDS("Variety.rds")
Adequacy <- readRDS("Adequacy.rds")
Moderation <- readRDS("Moderation.rds")
Balance <- readRDS("Balance.rds")

#Combine all DQI-I sections: Variety, Adequacy, Moderation, Balance

library(plyr)

VDQI <- join_all(list(Variety, Adequacy, Moderation, Balance), by=c("INDI", "District", "Member"), type='left')

detach("package:plyr", unload=TRUE)

names(VDQI)

VDQI <- VDQI %>% select(INDI:Member, tot_variety, tot.adequacy, tot.moderation, tot.Balance)

VDQI$Total_Score <- rowSums(VDQI[,c(4, 5, 6, 7)])

hist(VDQI$Total_Score)

lm(VDQI$Total_Score ~ VDQI$District)

summary(lm(VDQI$Total_Score ~ VDQI$District))

plot(District ~ Total_Score, data=VDQI)
abline(lm(District ~ Total_Score, data=VDQI))

cor.test(VDQI$Total_Score, VDQI$District, method=c("pearson", "kendall", "spearman"))

#Calculate mean DQI-I
AverageDQI <- VDQI %>% group_by(District, Member) %>% summarise(AverageDQI = mean(Total_Score))

#Tukey test
#District
VDQI$District <- as.factor(VDQI$District)
G1 <- lm(Total_Score ~ District, data = VDQI)
G1.aov<- aov(G1)
tukey.test <- TukeyHSD(G1.aov)
tukey.test
#Gender
VDQI$Member <- as.factor(VDQI$Member)
G2 <- lm(Total_Score ~ Member, data = VDQI)
G2.aov<- aov(G2)
tukey.test <- TukeyHSD(G2.aov)
tukey.test




