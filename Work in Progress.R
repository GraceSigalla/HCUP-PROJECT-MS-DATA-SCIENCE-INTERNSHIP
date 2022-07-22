## PULLING DATA FROM ACS 2015-2019 CAPTURING RACE, AGE GROUPS AND HOUSING BURDEN, FOR NY STATE

library(tidyverse)
library(tidycensus)
#census_api_key ("df0b1e647032e261b17a090dffbcf03a2d331fc4", install = TRUE)
#readRenviron("~/.Renviron")

## First step, getting list variables that we will be working with for this project
vars <- load_variables(2019, "acs5", cache =TRUE)
View(vars)

##Setting up race variables

race_vars <- c(
  white = "B03002_003",
  black = "B03002_004",
  native = "B03002_005",
  asian = "B03002_006",
  HIPI = "B03002_007",
  hispanic = "B03002_012"
)


##Setting up household burden variables, for percentage of population spending 30%
##or more of income on rent
housing_burden_vars <- c(
  less_20k = "B25106_028", 
  les_35k = "B25106_032", 
  less_50k = "B25106_036",  
  less_75k = "B25106_040",
  over_75k = "B25106_044"
)


#Pulling all race, age and housing burden separately

##Pulling housing burden data
ny_housing_burd <- get_acs(
  geography = "county",
  state = 36,
  variables = housing_burden_vars,
  year = 2019,
  output = "wide"
)
ny_housing_burd
view(ny_housing_burd)


#Pulling agegroup, race, sex and ethnicity
ny_est <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("AGEGROUP", "RACE" ,"SEX","HISP"),
  breakdown_labels = TRUE,
  state = 36,
  year = 2019,
  output = "wide"
)
View(ny_est)


joined <- merge(ny_housing_burd, ny_est, by = "GEOID", all = TRUE)
view(joined)


#PULLING DATA USING ZCTA 


#Pulling race data
race_info <- get_acs(
  geography = "zcta",
  state = 36,
  variables = race_vars,
  year = 2019,
  output = "wide"
)
race_info
view(race_info)

##Pulling housing burden data
burden <- get_acs(
  geography = "zcta",
  state = 36,
  variables = housing_burden_vars,
  year = 2019,
  output = "wide"
)
burden
view(burden)

#Pulling median age, race & housing burden
ny_info <- get_acs(
  geography = "zcta",
  state = 36,
  variables = c(housing_burden_vars,race_vars, median_age = "B01002_001"),
  year = 2019,
  output = "wide"
)
ny_info
view(ny_info)


my_info1<-my_info%>%
#IMPORTING EXCEL FILE TO R - ZIPCODE CROSSWALK

zcta_df <- data.table(pipe("pbpaste"), sep = "\t", header = TRUE)
zcta_df
view(zcta_df)

#EXERCISE*data.table - easier to save file as csv. or fread...TRY and get the same answer using the crosswalks, left join on GEOID (allocation factor), groub by ZCTA. 


#Merge ny_info with zcta_df

zip_df <- merge(ny_info, zcta_df, by.x ="GEOID", by.y= "ZCTA", all = TRUE)
view(both_df)










#Pulling agegroup, race, sex and ethnicity
#ny_est <- get_estimates(
 # geography = "county",
  #product = "characteristics",
  #breakdown = c("AGEGROUP", "RACE" ,"SEX","HISP"),
  #breakdown_labels = TRUE,
  #state = 36,
  #year = 2019,
  #output = "wide"
#)
#iew(ny_est)




zip_df <- merge(ny_info, zct by = "GEOID", all = TRUE)
view(both_df)


##Setting up age variables (can't use this, brings out an error)
#age_vars <- c(
#  male_under_5 = "B01001_003", 
#  male_5_9 = "B01001_004", 
#  male_10_14 = "B01001_005",  
#  male_15_19 = sum(c("B01001_06", "B01001_07")), 
#  male_20_24 = sum(c("B01001_08", "B01001_09", "B01001_010")),
#  female_under_5 = "B01001_027",
#  female_5_9 = "B01001_028", 
#  female_10_14 = "B01001_029",
#  female_15_19 = sum(c("B01001_030", "B01001_031")),
#  female_20_24 = sum(c("B01001_032", "B01001_033", "B01001_034"))
#)

## Pulling age data in groups of five, for under below 25 (This code pulls for 
##individual genders, but doesn't add up values)

#ny_age <- get_acs(
# state = 36,
#  variables = age_vars,
#  year = 2019,
#  output = "wide"
#)
#view(ny_age)

## Pulling race data

#ny_race <- get_acs(
#  geography = "tract",
#  state = 36,
#  variables = c(race = race_vars),
#  year = 2019,
#  output = "wide"
#)

#ny_housing_products <- get_estimates(
#  geography = "county",
#  product = "housing",
#  breakdown = c("SEX","AGEGROUP", "RACE" ,"HISP"),
#  breakdown_labels = TRUE,
#  state = 36,
#  year = 2019
#)
#View(ny_housing_products)


#practise1 <- ny_age %>%
#  filter(variable != "B01001_001") %>%
#  mutate(agegroup = case_when(
#    variable < "B01001_004" ~ "below5", 
#    variable < "B01001_005" ~ "below10", 
#    variable < "B01001_006" ~ "below15",
#    variable < "B01001_008" ~ "bw20",
#    variable < "B01001_010" ~ "bw25",
#    TRUE ~ "above25k"
#)) 

## This is not very neccesary Pulling median age, median income, from NY with ethnicity groups into account
#ny_race_inc_age <- get_acs(
#  geography = "tract",
#  state = 36,
#  variables = c(med_inc = "B19013_001",
#               med_age = "B01002_001",
#               race = race_vars),
#  year = 2019,
#  output = "wide"
#)
#ny_race_inc_age
#view(ny_race_inc_age)

## Practicing with get_estimates and get_flows

#ny_components <- get_estimates(
#  geography = "county",
# product = "components",
#  state = 36,
# year = 2019
#)
#View(ny_components)

#ny_age_test <- get_acs(geography = "tract", 
 #                      product = "characteristics", 
 #                      variables = c("AGEGROUP", "RACE" ,"SEX"),  
 #                      breakdown_labels = TRUE, 
 #                      state = "36",
 #                      output = "wide"
#)
#ny_age_test
#view(ny_age_test)

#ny_test1 <- get_acs(geography = "county", 
 #                   product = "characteristics", 
  #                  variables = c("AGEGROUP", "HISP" ,"SEX"),  
  #                  breakdown_labels = TRUE, 
 #                   state = "36",
#                    year = 2019
#)
#ny_test1
#view(ny_test1)


#separate(
 # ny_est,
  #NAME,
  #into = c("county", "state"),
  #sep = ", "
#)


#ny_df <- bind_rows(ny_est, ny_housing_burd)
#view(ny_df)

##Age by sex?



