# PULLING DATA FROM ACS 2015-2019 CAPTURING RACE, AGE GROUPS AND HOUSING BURDEN,
# FOR NY STATE

library(tidyverse)
library(tidycensus)
library(ggplot2)

##First step, get list variables that we will be working with for this project
vars <- load_variables(2019, "acs5", cache =TRUE)
View(vars)

# Setting up total population variable

total_pop <- "B01001_001" 

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
  HIPI = "B03002_007"
)

ethnicity_vars <- c(
  tot_nothisp = "B03002_002",
  nothisp_white = "B03002_003",
  nothisp_Black = "B03002_004",
  nothisp_native = "B03002_005",
  nothisp_asian = "B03002_006",
  nothisp_HIPI = "B03002_007",
  nothisp_other = sum("B03002_008","B03002_009", "B03002_010", "B03002_011"),
  hisp = "B03002_012",
  hisp_white = "B03002_013",
  hisp_Black = "B03002_014",
  hisp_native = "B03002_015",
  hisp_asian = "B03002_016",
  hisp_HIPI = "B03002_017",
  hisp_other = sum("B03002_018","B03002_019", "B03002_020", "B03002_021")
)

##Set up household burden variables, for percentage of population spending 30%
##or more of income on rent and those who own a house
housing_burd_vars <- c(
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

#Merge the variables so the table is more readable, use pivot_wider, calculating
#variables using the percentage of total population

ny_info_data <- get_acs(
  geography = "zcta",
  state = 36,
  variables =c(
    total_pop = "B01001_001", 
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
    nothisp = "B03002_002",
    nothisp_white = "B03002_003",
    nothisp_black = "B03002_004",
    nothisp_native = "B03002_005",
    nothisp_asian = "B03002_006",
    nothisp_HIPI = "B03002_007",
    nothisp_other1 = "B03002_008",
    nothisp_other2 = "B03002_009",
    nothisp_other3 = "B03002_010",
    nothisp_other4 = "B03002_011",
    hisp = "B03002_012",
    hisp_white = "B03002_013",
    hisp_black = "B03002_014",
    hisp_native = "B03002_015",
    hisp_asian = "B03002_016",
    hisp_HIPI = "B03002_017",
    hisp_other1 = "B03002_018",
    hisp_other2 = "B03002_019",
    hisp_other3 = "B03002_020",
    hisp_other4 = "B03002_021",
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
view(ny_info_data)

ny_info_wide <- ny_info_data %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from=variable,
              values_from=estimate)

ny_data <- ny_info_wide %>%
  mutate(age_under5 = 100 * ((male_under_5 + female_under_5)/total_pop),
         age_5_9 = 100 * ((male_5_9 + female_5_9)/total_pop),
         age_10_14 = 100 * ((male_10_14 + female_10_14)/total_pop),
         age_15_19 = 100 * ((male_15_17 + male_18_19 + female_15_17 + female_18_19)/total_pop),
         age_20_24 = 100 * ((male_20 + male_21 + male_22_24 + female_20 + female_21 + 
           female_22_24)/total_pop),
         age_25_29 = 100 * ((male_25_29 + female_25_29)/total_pop),
         age_30_34 = 100 * ((male_30_34 + female_30_34)/total_pop),
         age_35_39 = 100 * ((male_35_39 + female_35_39)/total_pop),
         age_40_44 = 100 * ((male_40_44 + female_40_44)/total_pop),
         age_45_49 = 100 * ((male_45_49 + female_45_49)/total_pop),
         age_50_54 = 100 * ((male_50_54 + female_50_54)/total_pop),
         age_55_59 = 100 * ((male_55_59 + female_55_59)/total_pop),
         age_60_64 = 100 * ((male_60_61 + male_62_64 + female_60_61 + female_62_64)/total_pop),
         age_65_69 = 100 * ((male_65_66 + male_67_69 + female_65_66 + female_67_69)/total_pop),
         age_70_74 = 100 * ((male_70_74 + female_70_74)/total_pop),
         age_75_79 =  100 * ((male_75_79 + female_75_79)/total_pop),
         age_80_84 =  100 * ((male_80_84 + female_80_84)/total_pop),
         age_85_over = 100 * ((male_85_over + female_85_over)/total_pop),
         owner_house_burd = 100 * ((owner_less_20k + owner_less_35k + owner_less_50k + 
           owner_less_75k + owner_over_75k)/total_pop),
         renter_house_burd = 100 * ((renter_less_20k + renter_less_35k + 
           renter_less_50k + renter_less_75k + renter_over_75k)/total_pop),
         nothisp1 = 100 * (nothisp/total_pop),
         nothisp_wht = 100 * (nothisp_white/total_pop),
         nothisp_blk = 100 * (nothisp_black/total_pop),
         nothisp_ntv = 100 * (nothisp_native/total_pop),
         nothisp_asn = 100 * (nothisp_asian/total_pop),
         nothisp_hp = 100 * (nothisp_HIPI/total_pop),
         nothisp_other = 100 * ((nothisp_other1 + nothisp_other2 + nothisp_other3 + 
           nothisp_other4)/total_pop),
         hisp1 = 100 * (hisp/total_pop),
         hisp_wht = 100 * (hisp_white/total_pop),
         hisp_blk = 100 * (hisp_black/total_pop),
         hisp_ntv = 100 * (hisp_native/total_pop),
         hisp_asn = 100 * (hisp_asian/total_pop),
         hisp_hp = 100 * (hisp_HIPI/total_pop),
         hisp_other = 100 * ((hisp_other1 + hisp_other2 + hisp_other3 + hisp_other4)/total_pop))%>%
  select(GEOID, NAME, nothisp1, nothisp_wht, nothisp_blk, 
         nothisp_ntv, nothisp_asn, nothisp_hp, nothisp_other, hisp1,
         hisp_wht,hisp_blk, hisp_ntv, hisp_asn, hisp_hp, 
         hisp_other, age_under5, age_5_9, age_10_14, 
         age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44, 
         age_45_49, age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, 
         age_75_79, age_80_84, age_85_over, owner_house_burd, renter_house_burd)
ny_data
view(ny_data)

#Exploratory Data Analysis

# Histogram

#not hispanic population
ggplot(ny_data, aes(x = nothisp1)) + 
  geom_histogram()

# hispanic population
ggplot(ny_data, aes(x = hisp1)) + 
  geom_histogram(bins = 100)

# Scatterplot

ggplot(ny_data, aes(x = nothisp_wht, y = age_25_29)) + 
  geom_point()

#Box Plot
ggplot(data= ny_data, mapping = aes(x = NAME, y = nothisp_wht)) + 
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Bar plot
ggplot(ny_data) + 
  geom_col(mapping = aes(x = variable, y = estimate))



