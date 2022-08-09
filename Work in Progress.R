# PULLING DATA FROM ACS 2015-2019 CAPTURING RACE, AGE GROUPS AND HOUSING BURDEN, FOR NY STATE

library(tidyverse)
library(tidycensus)
# census_api_key ("df0b1e647032e261b17a090dffbcf03a2d331fc4", install = TRUE)
# readRenviron("~/.Renviron")

## First step, getting list variables that we will be working with for this project
vars <- load_variables(2019, "acs5", cache =TRUE)
View(vars)

# Setting up total population variable

total_population <- "B01001_001" 

# Setting up age variables 
age_vars <- c(
  male_under_5 = "B01001_003", 
  male_5_9 = "B01001_004", 
  male_10_14 ="B01001_005",  
  male_15_17 = "B01001_006", 
  male_18_19 = "B01001_007",
  male_20 = "B01001_008",
  male_21 = "B01001_009",
  male_22_24 ="B01001_010",
  male_25_29 = "B01001_011",
  male_30_34 = "B01001_012",
  male_35_39 = "B01001_013",
  male_40_44 = "B01001_014",
  male_45_49 = "B01001_015",
  male_50_54 = "B01001_016",
  male_55_59 = "B01001_017",
  male_60_61 = "B01001_018",
  male_62_64 = "B01001_019",
  male_65_66 = "B01001_020",
  male_67_69 = "B01001_021",
  male_70_74 = "B01001_022",
  male_75_79 = "B01001_023",
  male_80_84 = "B01001_024",
  male_85_over = "B01001_025",
  female_under_5 = "B01001_027",
  female_5_9 = "B01001_028", 
  female_10_14 = "B01001_029",
  female_15_17 = "B01001_030", 
  female_18_19 =  "B01001_031",
  female_20 = "B01001_032",
  female_21 = "B01001_033", 
  female_22_24 = "B01001_034",
  female_25_29 = "B01001_035",
  female_30_34 = "B01001_036",
  female_35_39 = "B01001_037",
  female_40_44 = "B01001_038",
  female_45_49 = "B01001_039",
  female_50_54 = "B01001_040",
  female_55_59 = "B01001_041",
  female_60_61 = "B01001_042",
  female_62_64 = "B01001_043",
  female_65_66 = "B01001_044",
  female_67_69 = "B01001_045",
  female_70_74 = "B01001_046",
  female_75_79 = "B01001_047",
  female_80_84 = "B01001_048",
  female_85_over = "B01001_049"
)


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
##or more of income on rent and those who own a house
housing_burden_vars <- c(
  owner_less_20k = "B25106_006",
  owner_less_35k = "B25106_010",
  owner_less_50k = "B25106_014", 
  owner_less_75k = "B25106_018",
  owner_over_75k = "B25106_022",
  renter_less_20k = "B25106_028",
  renter_less_35k = "B25106_032",
  renter_less_50k = "B25106_036", 
  renter_less_75k = "B25106_040",
  renter_over_75k = "B25106_044"
)

ny_info_data <- get_acs(
  geography = "tract",
  state = 36,
  variables =c(
    male_under_5 = "B01001_003", 
    male_5_9 = "B01001_004", 
    male_10_14 ="B01001_005",  
    male_15_17 = "B01001_006", 
    male_18_19 = "B01001_007",
    male_20 = "B01001_008",
    male_21 = "B01001_009",
    male_22_24 ="B01001_010",
    male_25_29 = "B01001_011",
    male_30_34 = "B01001_012",
    male_35_39 = "B01001_013",
    male_40_44 = "B01001_014",
    male_45_49 = "B01001_015",
    male_50_54 = "B01001_016",
    male_55_59 = "B01001_017",
    male_60_61 = "B01001_018",
    male_62_64 = "B01001_019",
    male_65_66 = "B01001_020",
    male_67_69 = "B01001_021",
    male_70_74 = "B01001_022",
    male_75_79 = "B01001_023",
    male_80_84 = "B01001_024",
    male_85_over = "B01001_025",
    female_under_5 = "B01001_027",
    female_5_9 = "B01001_028", 
    female_10_14 = "B01001_029",
    female_15_17 = "B01001_030", 
    female_18_19 =  "B01001_031",
    female_20 = "B01001_032",
    female_21 = "B01001_033", 
    female_22_24 = "B01001_034",
    female_25_29 = "B01001_035",
    female_30_34 = "B01001_036",
    female_35_39 = "B01001_037",
    female_40_44 = "B01001_038",
    female_45_49 = "B01001_039",
    female_50_54 = "B01001_040",
    female_55_59 = "B01001_041",
    female_60_61 = "B01001_042",
    female_62_64 = "B01001_043",
    female_65_66 = "B01001_044",
    female_67_69 = "B01001_045",
    female_70_74 = "B01001_046",
    female_75_79 = "B01001_047",
    female_80_84 = "B01001_048",
    female_85_over = "B01001_049",
    white = "B03002_003",
    black = "B03002_004",
    native = "B03002_005",
    asian = "B03002_006",
    HIPI = "B03002_007",
    hispanic = "B03002_012",
    owner_less_20k = "B25106_006",
    owner_less_35k = "B25106_010",
    owner_less_50k = "B25106_014", 
    owner_less_75k = "B25106_018",
    owner_over_75k = "B25106_022",
    renter_less_20k = "B25106_028",
    renter_less_35k = "B25106_032",
    renter_less_50k = "B25106_036", 
    renter_less_75k = "B25106_040",
    renter_over_75k = "B25106_044"),
  year = 2019,
  output = "wide"
)


#Merging the variables to make the table smaller and more readable
pivot_wider(id_cols = c(GEOID, NAME), names_from=variable,
            values_from=estimate) %>%
ny_info_data2 <- str(ny_info_data%>%
  mutate(age_under5 = male_under_5 + female_under_5,
         age_5_9 = male_5_9 + female_5_9,
         age_10_14 = male_10_14 + female_10_14,
         age_15_19 = male_15_17 + male_18_19 + female_15_17 + female_18_19,
         age_20_24 = male_20 + male_21 + male_22_24 + female_20 + female_21 + female_22_24,
         age_25_29 = male_25_29 + female_25_29,
         age_30_34 = male_30_34 + female_30_34,
         age_35_39 = male_35_39 + female_35_39,
         age_40_44 = male_40_44 + female_40_44,
         age_45_49 = male_45_49 + female_45_49,
         age_50_54 = male_50_54 + female_50_54,
         age_55_59 = male_55_59 + female_55_59,
         age_60_64 = male_60_61 + male_62_64 + female_60_61 + female_62_64,
         age_65_69 = male_65_66 + male_67_69 + female_65_66 + female_67_69,
         age_70_74 = male_70_74 + female_70_74,
         age_75_79 =  male_75_79 + female_75_79,
         age_80_84 =  male_80_84 + female_80_84,
         age_85_over = male_85_over + female_85_over,
         owner_housing_burden = owner_less_20k + owner_less_35k + owner_less_50k +owner_less_75k + owner_over_75k,
         renter_housing_burden = renter_less_20k + renter_less_35k + renter_less_50k + renter_less_75k + renter_over_75k))







# Sum of owners and renters for housing burden

practice1$owner_housing_burden = practice1$B25106_006E + practice1$B25106_010E + practice1$B25106_014E + practice1$B25106_018E + practice1$B25106_022E
practice1$renter_housing_burden <-  sum(as.numeric("renter_less_20k"), as.numeric("renter_less_35k"), as.numeric("renter_less_50k"), as.numeric("renter_less_75k"),as.numeric("renter_less_75k"))



##Pulling housing burden data
#ny_housing_burd <- get_acs(
 # geography = "county",
 # state = 36,
  #variables = housing_burden_vars,
  #year = 2019,
  #output = "wide"
#)
#ny_housing_burd
#view(ny_housing_burd)


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

#PULLING DATA USING ZCTA 


#Pulling race data
#race_info <- get_acs(
 # geography = "zcta",
  #state = 36,
  #variables = race_vars,
  #year = 2019,
  #total = total_population,
  #output = "wide"
#)
#race_info
#view(race_info)

##Pulling housing burden data
#burden <- get_acs(
#  geography = "zcta",
 # state = 36,
  #variables = housing_burden_vars,
  #year = 2019,
  #output = "wide"
#)
#burden
#iew(burden)

#Pulling median age, race & housing burden
#ny_info <- get_acs(
 # geography = "zcta",
 # state = 36,
 # variables = c(housing_burden_vars,race_vars, median_age = "B01002_001"),
  #year = 2019,
 # output = "wide"
#)
#ny_info
#view(ny_info)


#my_info1<-my_info%>%


#IMPORTING EXCEL FILE TO R - ZIPCODE CROSSWALK

#zcta_df <- data.table(pipe("pbpaste"), sep = "\t", header = TRUE)
#zcta_df
#view(zcta_df)

#EXERCISE*data.table - easier to save file as csv. or fread...TRY and get the same answer using the crosswalks, left join on GEOID (allocation factor), groub by ZCTA. 


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

#zip_df <- merge(ny_info, zct by = "GEOID", all = TRUE)
#view(both_df)

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



