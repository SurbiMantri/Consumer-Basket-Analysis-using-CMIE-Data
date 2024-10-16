## Consumption Basket Analysis ##

setwd("C:\\Users\\admin\\Desktop\\Consumption Analysis")           # setting working directory
list.files()

library(readr)
a <- read_csv("consumption_pyramids_20220430_MS_rev.csv")          #importing April consumption pyramid data as 'a'
View(a)

m <- read_csv("consumption_pyramids_20220531_MS_rev.csv")          #importing May consumption pyramid data as 'm'
View(m)

j <- read_csv("consumption_pyramids_20220630_MS_rev.csv")          #importing June consumption pyramid data as 'j'
View(j)

#calling out the necessary packages:
library(dplyr)                                                
library(tidyr)
library(tidyverse)
library(ggplot2)


## data preprocessing ##

#reducing the data: 

# april 2022 data:
colnames(a)                                                      #list of columns in a

apr <- a %>%
  select("HH_ID", "STATE","HR", "DISTRICT", "REGION_TYPE", "PSU_ID", "MONTH", "AGE_GROUP",  "OCCUPATION_GROUP", "EDU_GROUP", "GENDER_GROUP", "SIZE_GROUP", "TOT_EXP",
         "M_EXP_FOOD", "M_EXP_BEVERAGES_N_WATER", "M_EXP_INTOXICANTS", "M_EXP_CLOTHING_N_FOOTWEAR", "M_EXP_GEMS_N_JEWELLERY",
         "M_EXP_APPLIANCES", "M_EXP_FOOD_IN_RESTAURANTS", "M_EXP_ENTMT", "M_EXP_HOUSE_RENT", "M_EXP_WATER_CHARGES", "M_EXP_POWER_N_FUEL", 
         "M_EXP_ELECTRICITY", "M_EXP_TRANSPORT", "M_EXP_HEALTH", "M_EXP_ALL_EMIS", "M_EXP_VACATION" )
View(apr)
sum(is.na(apr))


# may 2022 data:
may <- m %>%
  select("HH_ID", "STATE","HR", "DISTRICT", "REGION_TYPE", "PSU_ID", "MONTH", "AGE_GROUP",  "OCCUPATION_GROUP", "EDU_GROUP", "GENDER_GROUP", "SIZE_GROUP", "TOT_EXP",
         "M_EXP_FOOD", "M_EXP_BEVERAGES_N_WATER", "M_EXP_INTOXICANTS", "M_EXP_CLOTHING_N_FOOTWEAR", "M_EXP_GEMS_N_JEWELLERY",
         "M_EXP_APPLIANCES", "M_EXP_FOOD_IN_RESTAURANTS", "M_EXP_ENTMT", "M_EXP_HOUSE_RENT", "M_EXP_WATER_CHARGES", "M_EXP_POWER_N_FUEL", 
         "M_EXP_ELECTRICITY", "M_EXP_TRANSPORT", "M_EXP_HEALTH", "M_EXP_ALL_EMIS", "M_EXP_VACATION" )
View(may)

sum(is.na(may))

# june 2022 data:
june <- j %>%
  select("HH_ID", "STATE","HR", "DISTRICT", "REGION_TYPE", "PSU_ID", "MONTH", "AGE_GROUP",  "OCCUPATION_GROUP", "EDU_GROUP", "GENDER_GROUP", "SIZE_GROUP", "TOT_EXP",
         "M_EXP_FOOD", "M_EXP_BEVERAGES_N_WATER", "M_EXP_INTOXICANTS", "M_EXP_CLOTHING_N_FOOTWEAR", "M_EXP_GEMS_N_JEWELLERY",
         "M_EXP_APPLIANCES", "M_EXP_FOOD_IN_RESTAURANTS", "M_EXP_ENTMT", "M_EXP_HOUSE_RENT", "M_EXP_WATER_CHARGES", "M_EXP_POWER_N_FUEL", 
         "M_EXP_ELECTRICITY", "M_EXP_TRANSPORT", "M_EXP_HEALTH", "M_EXP_ALL_EMIS", "M_EXP_VACATION" )
View(june)

sum(is.na(june))

# joining income data - 

list.files()                                                      #list of files in our directory

ai <- read_csv("household_income_20220430_MS_rev.csv")            #importing April income pyramid data as 'ai'
mi <- read_csv("household_income_20220531_MS_rev.csv")            #importing May income pyramid data as 'mi'
ji <- read_csv("household_income_20220630_MS_rev.csv")            #importing June income pyramid data as 'ji'

colnames(ai)
class(ai$MONTH)
ai$MONTH <- as_factor(ai$MONTH)                                   #converting month to a factor
levels(ai$MONTH)

ai_1 <- ai %>%                                                    #reducing ai data
  select("HH_ID", "INC_OF_HH_FRM_ALL_SRCS")                       #variables= household id(primary key), income of households from all sources

apr_1 <- apr |>                                                   #joining ai data (april income) to apr (reduced april data)
  left_join(ai_1, join_by(HH_ID)) |>
  select(HH_ID, everything())


colnames(mi)
class(mi$MONTH)
mi$MONTH <- as_factor(mi$MONTH)                                   #converting month to a factor
levels(mi$MONTH)

mi_1 <- mi %>%                                                    #reducing mi data
  select("HH_ID", "INC_OF_HH_FRM_ALL_SRCS")

may_1 <- may |>                                                   #joining mi data (may income) to may (reduced may data)
  left_join(mi_1, join_by(HH_ID)) |>
  select(HH_ID, everything())


colnames(ji)
class(ji$MONTH)
ji$MONTH <- as_factor(ji$MONTH)                                   #converting month to a factor
levels(ji$MONTH)

ji_1 <- ji %>%                                                    #reducing ji data
  select("HH_ID", "INC_OF_HH_FRM_ALL_SRCS")

june_1 <- june |>                                                 #joining ji data (june income) to june (reduced june data)
  left_join(ji_1, join_by(HH_ID)) |>
  select(HH_ID, everything())

# stacking data
install.packages("janitor")
library(janitor)
compare_df_cols(apr_1, may_1, june_1)                             # comparing variables in all files/data sets. 

cp <- bind_rows(apr_1, may_1, june_1)                             # stacking them.
View(cp)
levels(cp$MONTH)
class(cp$MONTH)
cp$MONTH <- as_factor(cp$MONTH)



# converting charater to factor
cp_1$REGION_TYPE <- as.factor(cp_1$REGION_TYPE)
cp_1$STATE <- as.factor(cp_1$STATE)
cp_1$MONTH <- as.factor(cp_1$MONTH)
cp_1$AGE_GROUP <- as.factor(cp_1$AGE_GROUP)
cp_1$OCCUPATION_GROUP <- as.factor(cp_1$OCCUPATION_GROUP)
cp_1$EDU_GROUP <- as.factor(cp_1$EDU_GROUP)
cp_1$GENDER_GROUP <- as.factor(cp_1$GENDER_GROUP)
cp_1$SIZE_GROUP <- as.factor(cp_1$SIZE_GROUP)



#taking 0 as missing value
cp_1 <- cp %>%
  mutate(INC_OF_HH_FRM_ALL_SRCS  =
           if_else(INC_OF_HH_FRM_ALL_SRCS == 0, NA,
                   INC_OF_HH_FRM_ALL_SRCS))

cp_1 <- cp_1%>%
  filter(!is.na(INC_OF_HH_FRM_ALL_SRCS))

#dividing income into 5 categories
cp_1 <- cp_1 %>%
  mutate(
    cut_inc = cut(
      INC_OF_HH_FRM_ALL_SRCS,
      breaks = 5,
      labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    )
  )


# Export a data frame to CSV
write.csv(cp_1, "cp_1.csv", row.names = FALSE)

 
cp_2 <- cp_1 %>%                                    #reducing / filtering data
  mutate(REGION = case_when(
    STATE %in% c("Jammu and Kashmir", "Punjab", "Haryana", "Delhi", "Uttar Pradesh", "Himachal Pradesh", "Uttarakhand") ~ "North",
    STATE %in% c("Kerala", "Tamil Nadu", "Andhra Pradesh", "Telangana", "Karnataka") ~ "South",
    STATE %in% c("Bihar", "West Bengal", "Jharkhand", "Odisha") ~ "East",
    STATE %in% c("Rajasthan", "Maharashtra", "Gujarat", "Madhya Pradesh", "Chhattisgarh") ~ "West",
    TRUE ~ "Other")) # If the state does not fall in any category
table(cp_2$REGION)
cp_2$REGION <- as.factor(cp_2$REGION)

cp_2 <- cp_2 %>%                                  #renaming missing values as NA
  mutate(GENDER_GROUP = case_when(
    GENDER_GROUP == "Data Not Available" ~ NA,
    TRUE ~ GENDER_GROUP)) %>%                     # Keeps other values as they are
  mutate(OCCUPATION_GROUP = case_when(
    OCCUPATION_GROUP == "Data Not Available" ~ NA,
    TRUE ~ OCCUPATION_GROUP))


summary(cp_2$M_EXP_FOOD)

#cp_3 <- cp_2 %>%
# filter(EDU_GROUP %in% c("Graduates majority household","Households of all literates", 
#                           "Graduates minority household", "Households of all illiterates") &
#          !is.na(cut_inc)&
#          REGION_TYPE=="URBAN" &
#           SIZE_GROUP %in% c("3 Members", "4 Members", "5 Members"))



## plots  ##

## Jitter Plot

ggplot(cp_2, aes(x = REGION, y = TOT_EXP, color = GENDER_GROUP)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  labs(title = "Jittered Plot of Total Expenditure by Region and Gender", x = "Region", y = "Total Expenditure") +
  theme_minimal()


#scatterplot

ggplot(cp_2, aes(x = M_EXP_FOOD, y = TOT_EXP)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Food Expenditure vs Total Expenditure", 
       x = "Monthly Food Expenditure", 
       y = "Total Expenditure") +
  theme_minimal()

#boxplot
ggplot(cp_2, aes(x = REGION, y = TOT_EXP)) +
  geom_boxplot() +
  labs(title = "Household Expenditure by Region", 
       x = "Region", 
       y = "Total Expenditure") +
  theme_minimal()

#histogram

ggplot(cp_2, aes(x = TOT_EXP)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(title = "Distribution of Total Expenditure", 
       x = "Total Expenditure", 
       y = "Frequency") +
  theme_minimal()

#bar plot
ggplot(cp_2, aes(x = REGION, y = TOT_EXP, fill = REGION)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Total Expenditure by Region", x = "Region", y = "Average Total Expenditure") +
  theme_minimal()

cp_2 %>%
  group_by(REGION) %>%
  summarize(Food = sum(M_EXP_FOOD, na.rm = TRUE),
            Clothing = sum(M_EXP_CLOTHING_N_FOOTWEAR, na.rm = TRUE),
            Transport = sum(M_EXP_TRANSPORT, na.rm = TRUE)) %>%
  gather(key = "Category", value = "Expenditure", -REGION) %>%
  ggplot(aes(x = REGION, y = Expenditure, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Expenditure Composition by Region", x = "Region", y = "Expenditure") +
  theme_minimal() 


#facets
ggplot(cp_2,aes(cp_2$M_EXP_FOOD, cp_2$INC_OF_HH_FRM_ALL_SRCS, color=REGION)) +
  geom_point() +
  facet_grid(.~EDU_GROUP) +
  labs(title = "Total Income & Expenditure on Food by Region", x = "Food Expenditure", y = "Total Income")

#text overlayer

ggplot(cp_2, aes(x = M_EXP_FOOD, y = TOT_EXP)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = STATE), vjust = -1, size = 3, check_overlap = TRUE) +
  labs(title = "Food vs Total Expenditure with State Labels", x = "Food Expenditure", y = "Total Expenditure") +
  theme_minimal()

#Correlation Plot

library(ggcorrplot)
exp_data <- cp_2 %>% select(M_EXP_FOOD, M_EXP_TRANSPORT, M_EXP_HEALTH, M_EXP_CLOTHING_N_FOOTWEAR, TOT_EXP)
cor_matrix <- cor(exp_data, use = "complete.obs")
ggcorrplot(cor_matrix, lab=TRUE)



#animation

library(gganimate)
library(transformr)
library(gifski)

p <- ggplot(cp_2, aes(x = REGION, y = TOT_EXP, fill = REGION)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = 'Average Total Expenditure by Region in {closest_state}', x = 'Region', y = 'Avg Total Expenditure') +
  theme_minimal()

anim <- p + 
  transition_states(MONTH, transition_length = 2, state_length = 1) +
  ease_aes('linear')

animate(anim)







