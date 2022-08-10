# PULLING DATA FROM ACS 2015-2019 CAPTURING RACE, AGE GROUPS AND HOUSING BURDEN, FOR NY STATE

library(tidyverse)
library(tidycensus)

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
ny_info_data
view(ny_info_data)

#Merging the variables to make the table smaller and more readable without pivot_wider

ny_info_data2 <- ny_info_data%>%
  mutate(age_under5 = male_under_5E + female_under_5E,
         age_5_9 = male_5_9E + female_5_9E,
         age_10_14 = male_10_14E + female_10_14E,
         age_15_19 = male_15_17E + male_18_19E + female_15_17E + female_18_19E,
         age_20_24 = male_20E + male_21E + male_22_24E + female_20E + female_21E + female_22_24E,
         age_25_29 = male_25_29E + female_25_29E,
         age_30_34 = male_30_34E + female_30_34E,
         age_35_39 = male_35_39E + female_35_39E,
         age_40_44 = male_40_44E + female_40_44E,
         age_45_49 = male_45_49E + female_45_49E,
         age_50_54 = male_50_54E + female_50_54E,
         age_55_59 = male_55_59E + female_55_59E,
         age_60_64 = male_60_61E + male_62_64E + female_60_61E + female_62_64E,
         age_65_69 = male_65_66E + male_67_69E + female_65_66E + female_67_69E,
         age_70_74 = male_70_74E + female_70_74E,
         age_75_79 =  male_75_79E + female_75_79E,
         age_80_84 =  male_80_84E + female_80_84E,
         age_85_over = male_85_overE + female_85_overE,
         owner_housing_burden = owner_less_20kE + owner_less_35kE + owner_less_50kE +owner_less_75kE + owner_over_75kE,
         renter_housing_burden = renter_less_20kE + renter_less_35kE + renter_less_50kE + renter_less_75kE + renter_over_75kE)%>%
  select(GEOID, NAME, whiteE, blackE, nativeE, asianE, HIPIE, age_under5, age_5_9, age_10_14, age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44,age_45_49,
         age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, age_75_79, age_80_84, age_85_over, owner_housing_burden, renter_housing_burden)
view(ny_info_data2)

#Merging the variables to make the table smaller and more readable with pivot_wider

ny_info_data_tst <- get_acs(
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
  year = 2019
)
view(ny_info_data_tst)

ny_info_wide <- ny_info_data_tst %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from=variable,
              values_from=estimate)

ny_info_wider <- ny_info_wide %>%
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
         renter_housing_burden = renter_less_20k + renter_less_35k + renter_less_50k + renter_less_75k + renter_over_75k)%>%
  select(GEOID, NAME, white, black, native, asian, HIPI, age_under5, age_5_9, age_10_14, age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44,age_45_49,
         age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, age_75_79, age_80_84, age_85_over, owner_housing_burden, renter_housing_burden)
ny_info_wider
view(ny_info_wider)






