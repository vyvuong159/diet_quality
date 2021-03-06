## Introduction
This project is my master thesis which examines the association between food environment and diet quality and nutritional status between urban - rural areas in Vietnam.

## Research questions
- Is there any association between personal determinants of food choices (gender, income, education, nutrition literacy), neighborhood built food environments (food outlets density and distance to the nearest food outlet) and diet quality?
- Is there any association between personal determinants of food choices (gender, income, education, nutrition literacy), neighborhood built food environments (food outlets density and distance to the nearest food outlet) and nutritional outcomes?
- Is there any association between diet quality and nutritional outcomes? 

## Tasks
- Prepare data:
  - Reshape dataframe
  - Join 4 datasets 
  - Detect and impute missing values
  - Detect anomalies
  - Validate data quality
- Develop indexes for food environment, diet quality and nutritional status of the observed participants.
  - Food environment: I calculated (1) Proximity: distance to the nearest foot outlet, (2) Density: number of food outlets per 1000 residents.
  - Diet quality: I adapted the Diet Quality Index - International ([DQI-I](https://inddex.nutrition.tufts.edu/data4diets/indicator/diet-quality-index-international-dqi-i) to Vietnam context. This index is composed of 
  (1) Variety: the number of food groups and number of protein sources that a person consumed in one day
  (2) Adequacy: the percent that a person achieved the Recommended Daily Allowance or national food guidelines for necessary macro and micronutrients (healthy food)
  (3) Moderation: the quantity of unhealthy food that a person consumed in a day
  (4) the ratio among macronutrients and the ratio among different fatty acids.
  - Nutritional status: I calculated the BMI and non-communicable disease risk level for each person.
- Conduct exploratory data analysis
- Run regression models to examine the association among these parameters:
  - The relationship between diet quality and nutritional outcomes (underweight, BMI-based overweight/obesity, central obesity) were first assessed by the mixed effect logistic regression to account for clustered data because people from the same neighborhood might have more similar measurements than measurements of people in different neighborhoods. However, the results showed that the residual variance between neighborhood is estimated to be zero or very close to zero, indicating the clustering effect did not contribute to the variability of the response, i.e. nutritional status. Therefore, we used normal binary logistic regression models using the “normal weight” as the reference category instead. 
   - The association between food environment (outlet density and distance to the nearest food outlet) and standardized diet quality score were examined by mixed effect models (package lme4 in R [33] ) to control for clustering effect, i.e. people from the same neighborhood share the same value for outlet density and proximity and might also share other undetectable neighborhood characteristics. The linear mixed models were used for continuous scores (DQI-V, Adequacy, Moderation) and the Poisson mixed models were applied for discrete scores (Variety, Balance). As fixed effects, we entered outlet density or distance, urban – rural setting, gender, age, education level, income level, household size and knowledge score (without interaction term) into the model. As random effects, we had random intercept for neighborhood variable.
   - Covariates were determined a priori based on previous literatures. The first model analyzed the crude data, i.e. with diet quality score as the only explanatory variable. The second model was adjusted for sex, urban - rural settings, age, education level, income level, household size and total energy intake.

## Data source
The data were collected by the International Center for Tropical Agriculture (CIAT), including 4 datasets: 
- Consumer behavior: socio-demographic characteristics, food purchasing and preparing habits, knowledge and attitude toward nutrition, healthy eating and food safety
- 24h recall dietary assessment: information about all foods and beverages and dietary supplements consumed by the respondents in the past 24 hours, most commonly, from midnight to midnight the previous day
- Anthropometry: height, weight, waist circumference
- Built food environment: urban - periurban - rural setting, geocode the locations of food outlets and study participants' residence

## Conclusions
- This study found that personal food environment factors associated with diet quality were education, income, nutrition literacy score and gender but in different directions and magnitudes. Rural people in general and women in particular
had lower odds of central obesity than urban people.
  - Women had lower Moderation scores than men. 
  - Education was positively associated with the Variety and the Adequacy components both in women and men while being negatively associated with the Moderation component among women. 
  - Income was negatively associated with the Moderation component in both women and men. 
  - External food environment, especially the urban – rural setting, played a significant role in determining diet quality. Namely, people in the rural site, both women and men, had higher Moderation scores compared with urban people. Our study did not find a significant relationship between built food environment (proximity, density) and diet quality.
- Women were more likely to bear the double burden of malnutrition, with a higher risk for both underweight and central obesity. Men were more likely to have general overweight than women in this population. Age was positively associated with female abdominal obesity while income was negatively associated with female abdominal obesity. We observed increased prevalence and odds of overweight among higher education level among men. There was no association between built food environment and nutritional status.
- This study did not find a significant association between diet quality and and nutritional status.
