# diet_quality
Examine the association between food environment and diet quality and nutritional status between urban - rural areas in Vietnam.
Datasets: credit to the International Center for Tropical Agriculture (CIAT): they collected 4 datasets: Consumer behavior, 24h recall dietary assessment, anthropometry (height, weight, waist circumference) and built food environment (geocode the locations of food outlets and study participants' residences)
First I built indexes for food environment, diet quality and nutritional status of the observed participants.
Food environment: I calculated (1) Proximity: distance to the nearest foot outlet, (2) Diversity: number of food outlets within a certain walking distance (such as 500m, 1000m) and (3) Variety: average distance to the 3 closest different chain name food outlets.
Diet quality: I adapted the Diet Quality Index - International to Vietnam context. This index is composed of (1) Variety: the number of food groups and number of protein sources that a person consumed in one day, (2) Adequacy: the percent that a person achieved the Recommended Daily Allowance or national food guidelines for necessary macro and micronutrients (healthy food), (3) Moderation: the quantity of unhealthy food that a person consumed in a day, and (4) the ratio among macronutrients and the ratio among different fatty acids.
Nutritional status: I calculated the BMI and non-communicable disease risk level for each person.
Then run regression model to examine the association among these parameters
