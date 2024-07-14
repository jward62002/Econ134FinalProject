---
  title: "Econ134 Final Project"
author: "John Ward"
date: "2024-03-16"
output: html_document
---
  
# Clear the working space
rm(list = ls())
library(tidyverse)
library(stargazer)
library(faux)
library(beepr)
library(sandwich)
library(modelsummary)
# turn off scientific notation except for big numbers
options(scipen = 9) 
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
set.seed(42)



MHT <- list() # this will be our random treatment assignment indicator 
# create 1000 treatment cases
for(i in 1:130) {
  MHT[i] = 1
}
# create 1000 controls
for(i in 131:260) {
  MHT[i] = 0
}       

# Create three variables
D <- list() # treatment received indicator - note the combination of treatment received (D) and 
# treatment assigned Z
# determines our 'compliers' and 'never takers'
status <- list() # label for our compliers and never takers
y <- list() # outcome values

# the first 200 of the treatment cases will be 'never takers' with an average outcome (y) value = 5
for(i in 1:26) {
  status[i] = 'never taker'
  y[i] = 5
  D[i] = 0 # never takers have a treatment received indicator = 0 regardless of treatment assignment (Z)
}

# the remaining treatment cases will be 'compliers' with an average outcome (y) value = 25
for(i in 27:130) {
  status[i] = 'complier'
  y[i] = 45
  D[i] = 1 # compliers have a treatment received indicator = 1 when treatment assigned Z = 1
}

# the first 200 of our controls will be 'never takers' with an average outcome (y) value = 5
for(i in 131:156) {
  status[i] = 'never taker'
  y[i] = 5
  D[i] = 0 # never takers have a treatment received indicator = 0 regardless of treatment assignment (Z)
}

# the remaining controls will be 'compliers' with average outcome (y) value = 18
for(i in 157:260) {
  status[i] = 'complier'
  y[i] = 18
  D[i] = 0  # compliers have a treatment received indicator = 0 when treatment assigned Z = 0
}

# turn lists generated above into a data frame
y <- as.numeric(as.character(unlist(y)))
MHT <- as.numeric(as.character(unlist(MHT)))
D <- as.numeric(as.character(unlist(D)))
status <- as.character(unlist(status))

df <- data.frame(y, MHT, D, status)

# Create df1
df1 <- df %>% mutate(processing_efficiency = runif(n(), min = 0, max = 100), 
                     fruits_per_tree = round(pmin(abs(rnorm(n(), mean = 20, sd = 10)), max=675)),
                     popularity_of_bird_hunting = sample(1:10, n(), replace = TRUE),
                     baobab_trees = sample(100:500, n(), replace = TRUE),
                     baobab_pulps = 100 *
                       .25 * processing_efficiency +
                       15 * fruits_per_tree *
                       -.2 * popularity_of_bird_hunting +
                       25 * processing_efficiency * fruits_per_tree,
                     MHT = 0)

# Create df2
df2 <- df %>% mutate(processing_efficiency = runif(n(), min = 0, max = 100), 
                     fruits_per_tree = round(pmin(abs(rnorm(n(), mean = 20, sd = 10)), max=675)),
                     popularity_of_bird_hunting = sample(1:10, n(), replace = TRUE),
                     baobab_trees = sample(100:500, n(), replace = TRUE),
                     baobab_pulps = baobab_trees *
                       1 * processing_efficiency +
                       30 * fruits_per_tree *
                       -.2 * popularity_of_bird_hunting +
                       25 * processing_efficiency * fruits_per_tree,
                     MHT = 1)
df_all <- rbind(df1,df2)


### Introduction

#Interest in the baobab tree has been growing in popularity following the commercialization of baobab products (seed oil, fruit pulp) with the FDA and EU's recognition of the fruit pulp as a food supplement.  It is marketed in the commodity trade as baobab pulp, and the volumes of trade available are highly subject to fluctuation.  Despite the fluctuating markets, there is still a demand of baobab due to several key products.  Most notably, and what we will be addressing, baobab fruit pulp.  This product is "used in preparing cool and hot drinks in rural areas and has recently, become the most popular drink during fasting months all over the country of Sudan" (Eltahir 2020).  Our objective is to optimize the number of baobab fruit pulp which in theory boosts the exports of the good.  Increasing exports of this good will hopefully lead to increased GDP of the Ghanese economy.  The particular link in this chain that we hope to improve with an intervention is the implementation of a model harvesting technique (MHT) as inspired by the (Eltahir 2020) paper.  We are concerned solely with the outcome variable (baobab_pulps) because sales is a function of the volatile market for this product and is thus beyond the scope of our organization.  This paper will refer to the treatment as MHT, representing those communities who had received a training course on this subject.  This training course includes modules that ensure communities harvest selective fruits (not ones prior to ripe), correctly manage termite outbreaks, perfect the timing of a harvest, and overall education on the value of baobab trees for their local villages.  Other notable factors considered are the bird hunters, as mentioned in the Eltahir paper, "The bird hunters hit small fruits, flowers, buds and immature fruit while shooting the birds" (Eltahir 2020).  These hunters can have an overall negative impact on the number of baobab fruit, which is factored into our model.  We depict the flow of materials from raw source to marketable product in Fig 1.  First, the number of baobab trees in Ghana, the expansion of which we have deemed beyond the scope of our capacities. Next, the popularity of bird hunters on a scale of 1-10 ranked by a field surveyer.  This ranking was conducted over the period of several weeks and we have conviction in our methods and data.  Next in line was the amount of fruits per tree, which is a function of tree fertility which we believe we can affect with the implementation of our MHT.  This MHT, we believe, will effect the processing efficiency.  By processing efficiency we are referring to the farmers ability to pick only ripe fruits and give trees enough time to reach maximum capacity.  Without this intervention, we currently believe a considerable number of farmers use ineffective techniques.  This will hopefully increase the number of our final product: the baobab fruit pulp.  


#Our organization will be conducting this randomized control trial in Ghana.  Fortunately we have the financial capacity to conduct this throughout each county in Ghana.  We expect the experiment to take 8-12 months for complete implementation and collection of data  We will first have the MHT administered through community baobab participants (CBP).  Ghana has a population of 32,830,000 people currently and the main industries are agriculture, mining, oil and gas.  We chose Ghana given its exposure to the agricultural sector and potential as a leading baobab pulp exporting country.  If we can mitigate the negative variables and maximize the positive variables, we hope to boost the overall output of baobab fruit pulp, a staple of this economy.  Although optimizing the baobab fruit may appear trivial, it has a larger relative influence in Ghana given the consumption habits and potential for boosting exports.


#### Fig 1: Flowchart diagram from Baobab Tree to Baobab Pulp

library(pdftools)
library(magick)

flowchart <- "/Users/johnward/Desktop/courses/ECON\ 134/baobab_pulp_flowchart.png"
# Embed the image directly into the document
cat("![](", flowchart, ")\n", sep="")
knitr::include_graphics(flowchart)


#This flowchart is provided to visualize the process that we are trying to improve.  Through understanding the various components, we can get a better idea of where we want to allocate our investments. 

#### Fig 2: Boxplot of Fruits per Tree and Pulps by MHT

summary(df_all)
# Box plot of Outcome by Treatment Group
boxplot(fruits_per_tree ~ MHT, data = df_all,
        xlab = "Treatment Group", 
        ylab = "Fruits per Tree",
        main = "Boxplot of Fruits per Tree by MHT (Treatment Group)")

# Box plot of Education Level by Treatment Group
boxplot(baobab_pulps ~ MHT, data = df_all,
        xlab = "Treatment Group", 
        ylab = "Baobab Pulps",
        main = "Boxplot of Baobab Pulps by MHT (Treatment Group)")



#There appears to be a wider distribution in the untreated group in the fruits per tree boxplot, however the mean appears to be slightly below the mean in the treated.  In the treated category, the average fruits per tree is roughly 20, if not slightly above.

#### Fig 3: Geographic Distribution of Baobab Pulps Pre/Post Treatment


library(tmap)
library(sf)

# Read Ghana shapefile
ghana <- st_read("gadm41_GHA_shp")

# Assuming df1 and df2 are defined somewhere else in your script
ghana$baobab_pulps_1 <- df1$baobab_pulps
ghana$baobab_pulps_2 <- df2$baobab_pulps

# Calculate the overall range of 'baobab_pulps' from both data frames
overall_range <- range(c(df1$baobab_pulps, df2$baobab_pulps))

# Define breaks for the color scale
breaks <- pretty(overall_range, n = 8)

# Define a color palette with a single color ranging from light to dark
color_palette <- colorRampPalette(c("lightblue", "blue"))

# Create the first map with the defined breaks
map1 <- tm_shape(ghana) +
  tm_fill(col = "baobab_pulps_1", palette = color_palette(length(breaks) - 1), 
          breaks = breaks, style = "cont", border.col = "black") +
  tm_layout(legend.outside = TRUE)

# Create the second map with the defined breaks
map2 <- tm_shape(ghana) +
  tm_fill(col = "baobab_pulps_2", palette = color_palette(length(breaks) - 1), 
          breaks = breaks, style = "cont", border.col = "black") +
  tm_layout(legend.outside = TRUE)

# Arrange the maps side by side
tmap_arrange(map1, map2, nrow = 1)




#This is provided as a visual on the geographical distribution of baobab fruit pulps per individual county in Ghana.  As the reader can clearly see, there appears to be an increase in the amount of fruit pulps post the MHT treatment intervention.

#### Fig 4: Scatterplot of Baobab Pulps vs. Processing Efficiency


# Scatterplot of Baobab Pulps vs. processing efficency with best fit line
plot(df_all$processing_efficiency, df_all$baobab_pulps,
     xlab = "Processing efficiency",
     ylab = "baobab pulps",
     main = "Scatterplot of efficiency vs. pulps",
     pch = 16)  # filled circles for data points

# best fit line
abline(lm(baobab_pulps ~ processing_efficiency, data = df_all), col = "red")




#### Fig 5: Regressions of a Variety of Variables against Outcome


# Regression 1: Outcome regressed on treatment
regression_1 <- lm(baobab_pulps ~ MHT, data = df_all)

# Regression 2: Outcome regressed on treatment and post
regression_2 <- lm(baobab_pulps ~ MHT + baobab_trees, data = df_all)

# Regression 3: Outcome regressed on treatment and post interaction
regression_3 <- lm(baobab_pulps ~ MHT * baobab_trees, data = df_all)

# Regression 4: Outcome regressed on treatment, post-treatment indicator, education, and female
regression_4 <- lm(baobab_pulps ~ MHT + processing_efficiency + popularity_of_bird_hunting, data = df_all)

# Regression 5: Outcome regressed on all variables
regression_5 <- lm(baobab_pulps ~ MHT + processing_efficiency + fruits_per_tree + popularity_of_bird_hunting + baobab_trees, data = df_all)

# Create a list of regression models
models_list <- list(
  regression_1,
  regression_2,
  regression_3,
  regression_4,
  regression_5
)

# Generate summary table
summary_table <- modelsummary(models_list, 
                               stars = TRUE)

# Print the summary table
summary_table


### Interpretation
#This table provides several regressions on relevant variables that can indicate the direction and significance of variables.  In the first regression, we estimate the effect of the MHT treatment on the number of baobab fruit pulps. There is a positive and statistically significant value of 11017 increase in baobab pulps for the counties that were administered the treatment.  This is initially revealing and a promising insight.  In the second regression, we regress baobab pulps on MHT and baobab_trees.  It seems that the MHT coefficient slightly decreased to 11014 but still remains statistically significant.  We also included the number of baobab_trees which we would expect that the number of trees would lead to a higher number of fruits, but this number is insignificant at the 0.05 level.  This coefficient was 0.89, which means that for every increase in baobab_trees there are 0.89 more pulps.  In the third regression, we include MHT, baobab_trees, and an interaction term between MHT and baobab_trees.  It was interesting to see how the MHT effect decreased substantially compared to the past regression, with a coefficient of 7478 (and statistically insignificant).  The baobab_trees variable was -4.97 and statistically insignificant.  For the interaction variable, we are essentially testing whether the effect of MHT on the outcome variable depends on the value of baobab_trees.  The value fo this was 11.7 and was statistically significant.  This value suggests that the relationship between MHT and baobab_pulps is stronger or different when baobab_trees are present compared to when they are not.  In the fourth regression, we measured the effect of both the MHT, processing_efficiency, and popularity_of_bird_hunting on the outcome variable.  It seems the MHT coefficient is statistically significant at 13963, the processing_efficiency variable was positive and significant at 645  This result indicates that as we increase the processing_efficiency by a percent (it is on a scale of 1-100), the number of baobab pulps increases by that value.  The popularity of bird hunting variable is also negative and significant indicating that as the ranking of bird hunting (1-10) increases by one ranking unit, the number of baobab fruit pulps decreases by 584.  In the fifth regression, we see the MHT variable is statistically significant at 13553.9.  We included the baobab_trees, processing_efficiency, popularity_of_bird_hunting, and fruits_per_tree variables in this regression as well.  Notable is that the coefficients are 16.7 (statistically significant), 648.2 (statistically significant, -51.2 (statistically significant), and 1197 (statistically significant).  These variables can be helpful when trying to fully comprehend the impact of different variables.



### Conclusion
#Through the model harvesting technique, we seek to increase the number of baobab fruit pulps.  Regression analysis revealed a significant increase in baobab pulp production associated with the MHT treatment, indicating a promising avenue for enhancing yield. Subsequent regressions incorporating additional variables, such as baobab tree presence, processing efficiency, and the popularity of bird hunting, provided further insights into the complex dynamics influencing pulp production.  The geographical distribution provided a visual insight into the effect of our treatment, furthering the case for the model harvesting technique.  A scatter plot of processing efficiency against the number of baobab fruit pulps revealed that higher processing efficiency appeared to be correlated with more pulps - which provides a direction into where we should be spending our time.  



#### References

#Eltahir, Muneer. 2020. "Baobab fruit production and factors affecting the productivity in North Kordofan, Sudan," Discovery Agriculture. pp. 12-17.

#Fischer, Sahrah. "The Baobab (Adansonia digitata L.) in Southern Kenya–A Study on Status, Distribution, Use and Importance in Taita–Taveta County" National Library of Medicine. passim

#Gustad, Gunnar. 2004. "Local management practices influence the viability of the baobab (Adansonia digitata Linn.) in different land use types, Cinzana, Mali" Science Direct. passim.

#Hounsou-Dindin, Guillaume. 2018. "Developing best agro-ecological practices for African baobab tree Adansonia digitata L. leaves production in smallholders farming systems in Benin" Republic of Benin University of Abomey-Calavi Faculty of Agronomic Sciences pp. 24-26

#Kaimba, George K. 2021. "Commercialization of underutilized fruits: Baobab pulp supply response to price and non-price incentives in Kenya" Science Direct. passim.

#Unknown Author. 2020. "Baobab tree: The majestic tree of life in the dry savannah", GhanaWeb. passim

#Vermeulen, Wessel Johannes. 2009. "The Sustainable Harvesting of non-Timber Forest Products From Natural Forests in the Souther Cape, South Africa: Development of Harvest Systems and Management Prescriptions" passim.

#Zwarts, Leo. "Effects on Birds of the Conversion of Savannah to Farmland in the Sahel: Habitats are Lost, But Not Everywhere and Not For All Species" BioOne Complete. pp. 255-257

