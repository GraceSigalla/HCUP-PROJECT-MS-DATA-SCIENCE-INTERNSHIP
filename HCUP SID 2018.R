# First is to install the library haven for reading the dta file

library(readstata13)
library(tidyverse)

# set working directory
setwd("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_HCUP/HCUP Data_NY")

# Read the DTA file

ny_sid18 <- read.dta13("NY_SID_2018_CORE.dta")
View(ny_sid18)


# Working with just a sample of the data

ny_sid1 <- sample_n(ny_sid18,60000)

view(ny_sid1)

# null values

sum(is.na(ny_sid1))

# EDA

# First, transform the dataset my adding age groups, and relabeling variables for died,
# race and ethnicity, gender and payment 

ny_sid_mod <- ny_sid1 %>% mutate(
  age_grp = case_when(
    AGE < 5 ~ "00-04",
    AGE >= 5 & AGE <= 9 ~ "05-09",
    AGE >= 10 & AGE <= 14 ~ "10-14",
    AGE >= 15 & AGE <= 19 ~ "15-19",
    AGE >= 20 & AGE <= 24 ~ "20-24",
    AGE >= 25 & AGE <= 29 ~ "25-29",
    AGE >= 30 & AGE <= 34 ~ "30-34",
    AGE >= 35 & AGE <= 39 ~ "35-39",
    AGE >= 40 & AGE <= 44 ~ "40-44",
    AGE >= 45 & AGE <= 49 ~ "45-49",
    AGE >= 50 & AGE <= 54 ~ "50-54",
    AGE >= 55 & AGE <= 59 ~ "55-59",
    AGE >= 60 & AGE <= 64 ~ "60-64",
    AGE >= 65 & AGE <= 69 ~ "65-69",
    AGE >= 70 & AGE <= 74 ~ "70-74",
    AGE >= 75 & AGE <= 79 ~ "75-79",
    AGE >= 80 & AGE <= 84 ~ "80-84",
    AGE >= 85 ~ "85-Over"),
  died_vars = case_when(
    DIED == 0 ~ "Lived",
    DIED == 1 ~ "Died"),
  race_ethnicity = case_when(
    RACE == 1 & HISPANIC == 0 ~ "nonhisp_wht",
    RACE == 2 & HISPANIC == 0 ~ "nonhisp_blk",
    RACE == 4 & HISPANIC == 0 ~ "nonhisp_other", 
    RACE == 5 & HISPANIC == 0 ~ "nonhisp_other",
    RACE == 6 & HISPANIC == 0 ~ "nonhisp_other",
    RACE == 1 & HISPANIC == 1 ~ "hisp",
    RACE == 2 & HISPANIC == 2 ~ "hisp",
    RACE == 3 & HISPANIC == 3~ "hisp",
    RACE == 4 & HISPANIC == 3 ~ "hisp",
    RACE == 5 & HISPANIC == 3 ~ "hisp",
    RACE == 6 & HISPANIC == 4 ~ "hisp"),
  gender = case_when(
    FEMALE == 0 ~ "Male",
    FEMALE == 1 ~ "Female"),
  payment = case_when(
    PAY1 == 1  ~ "Medicare",
    PAY1 == 2  ~ "Medicaid",
    PAY1 == 3  ~ "Private Insurance",
    PAY1 == 4  ~ "Self-pay",
    PAY1 == 5  ~ "No Charge",
    PAY1 == 6  ~ "Other"))

view(ny_sid_mod)

# To view the modified dataset 
write.csv(ny_sid_mod,"//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_HCUP/HCUP Data_NY//2017 Modified Data.csv", row.names = TRUE)


# Analysis on Age

age_var <- ny_sid_mod %>% mutate( 
  age_perc = (AGE/sum(AGE))*100)

view(age_var)

# Age Graph

ggplot(age_var, aes(age_grp, age_perc)) + 
  geom_bar(stat = "identity") 

# Analysis Based on Death Status

died_var <- ny_sid_mod %>% group_by(died_vars) %>% summarise(
  percent = 100 * n()/nrow(ny_sid_mod))
view(died_var)


# Analysis on Diagnosis Related Group (DRG)

drg_var <- ny_sid_mod%>% 
  mutate(total_drg=sum(DRG), 
         perc_drg=(DRG/total_drg)*100) 
view(drg_var)

# DRG Graph 

ggplot(drg_var, aes(DRG, perc_drg)) + 
  geom_bar(stat = "identity")

# Analysis on Race

# race-ethnicity percentage
raceth_per <- ny_sid_mod %>% group_by(race_ethnicity) %>% summarise(
  percent = 100 * n()/nrow(ny_sid_mod))

# Race Ethnicity Graph

ggplot(raceth_per, aes(race_ethnicity, percent)) + 
  geom_bar(stat = "identity")


# Analysis on Gender

gender_var <- ny_sid_mod %>% group_by(gender) %>% summarise(
  percent = 100 * n()/nrow(ny_sid_mod))

view(gender_var)


# Analysis on Payment Options

pay_var <- ny_sid_mod %>% group_by(payment) %>% summarise(
  percent = 100 * n()/nrow(ny_sid_mod))

view(pay_var)



# SUMMARY

# According to the 2017 NY SID core data collected, majority of the population 
# who visited the community hopsitals were between the age of between 60-75,
# a majority being white non-hispanic, and as for the gender, most of the 
# population is females. Most of those who visited the community hospitals in NY
# their services were payed either via medicare, medicaid and private insurance. 
#  the majority of inpatients population survived (didn't die).



# CREATING A CHART SHOWING DISTRIBUTION OF ILI AND ANY ILI PERCENTAGES BY MONTH  

# create a data frame pulling only necessary variables (no deleting or imputing NA's)

diagnosis_info <- select(ny_sid_mod, I10_DX_Admitting, I10_DX1, I10_DX2, AMONTH)
view(diagnosis_info)

flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

# Creating a month_year and diagnosis variables

diagnosis_vars <- diagnosis_info %>% mutate(date = as.factor(as.Date(with(diagnosis_info, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                            diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                                   ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_vars)

# Group by Date and Diagnosis Var

diagnosis_vars_group <- diagnosis_vars %>% group_by(date, diagnosis_var) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_vars_group)


# Calculating Percentage of Diagnosis per Month

diagnosis_vars_perc <- diagnosis_vars_group %>% group_by(date)  %>% mutate(
  percent = total / sum(total)* 100) %>% arrange(desc(date))


view(diagnosis_vars_perc)

# Filter out "other" diagnosis to Create the Chart

diagnosis_vars_fil <- diagnosis_vars_perc %>% filter(diagnosis_var != "OTHER")

view(diagnosis_vars_fil)


# Plot the ILI and ANY ILI Chart

ggplot(data=diagnosis_vars_fil, aes(date,percent, colour = diagnosis_var)) + 
  geom_point(aes(shape = diagnosis_var), size = 4)+
  geom_line(aes(group = diagnosis_var))+ xlab("Month and Year") +
  ylab("ILI Diagnosis Percentage") + ggtitle("Inpatient visits with an ILI diagnosis 2017 New York") 


# Recreating ILI Table

# Those with diagnosed with both, first and any ILI (This was calculated manually via excel)


# Analysis on AGE

age_var <- select(ny_sid1, I10_DX_Admitting, AMONTH, AYEAR, DMONTH, AGE)
view(age_var)

age_groups <- age_var %>% mutate(
  age_group = case_when(
    AGE < 18 ~ "Under 18",
    AGE >= 18 & AGE <= 64 ~ "18-64",
    AGE >= 65 ~ "65 and over")
)

view(age_groups)

diagnosis_age <- age_groups %>% mutate(date = as.factor(as.Date(with(age_groups, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                       diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                              ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_age)

# Group diagnosis var and age group

diagnosis_age_grp <- diagnosis_age %>% group_by(diagnosis_var,age_group) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_age_grp)

# Analysis on Race
race_var <- select(ny_sid1, I10_DX_Admitting, AMONTH, AYEAR, DMONTH, RACE)
view(race_var)

race_groups <- race_var %>% mutate(
  race_ethnicity = case_when(
    RACE == 1 ~ "white",
    RACE ==  2 ~ "black",
    RACE ==  3 ~ "hispanic",
    RACE == 4 ~ "asian or pacific islander",
    RACE == 5 ~ "native",
    RACE ==  6 ~ "other" )
)

view(race_groups)


diagnosis_race <- race_groups %>% mutate(date = as.factor(as.Date(with(race_groups, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                         diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                                ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_race)

# Group by diagnosis var and race ethnicity

diagnosis_race_grp <- diagnosis_race %>% group_by(diagnosis_var,race_ethnicity) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_race_grp)


# Analysis on Pay

payment_var <- select(ny_sid1, I10_DX_Admitting, AMONTH, AYEAR, DMONTH, PAY1)
view(pay_var)

pay_groups <- payment_var %>% mutate(
  payment = case_when(
    PAY1 == 1  ~ "Medicare",
    PAY1 == 2  ~ "Medicaid",
    PAY1 == 3  ~ "Private Insurance",
    PAY1 == 4  ~ "Self-pay",
    PAY1 == 5  ~ "No Charge",
    PAY1 == 6  ~ "Other")
)
view(pay_groups)

diagnosis_pay <- pay_groups %>% mutate(date = as.factor(as.Date(with(pay_groups, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                       diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                              ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_pay)

# Group by diagnosis var and payment variables

diagnosis_pay_grp <- diagnosis_pay %>% group_by(diagnosis_var,payment) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_pay_grp)


 # Analysis on Location of Residence

loc <- select(ny_sid1, I10_DX_Admitting, AMONTH, AYEAR, DMONTH, PL_UR_CAT4)

view(loc)

loc_groups <- loc %>% mutate(
  patient_location = case_when(
    PL_UR_CAT4 == 1 ~ "Adjacent",
    PL_UR_CAT4 ==  2 ~ "Adjacent",
    PL_UR_CAT4 ==  3 ~ "non-adjacent",
    PL_UR_CAT4 == 4 ~ "Rural Remote" )
)

view(loc_groups)


diagnosis_loc <- loc_groups %>% mutate(date = as.factor(as.Date(with(loc_groups, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                       diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                              ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))

view(diagnosis_loc)

# Group by Diagnosis Var and Patient Location Variables

diagnosis_loc_grp <- diagnosis_loc %>% group_by(diagnosis_var,patient_location) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_loc_grp)


# Analysis on Medium Income

med_inc <- select(ny_sid1, I10_DX_Admitting, AMONTH, AYEAR, DMONTH, MEDINCSTQ)

view(med_inc)

med_inc_var <- med_inc %>% mutate(
  med_income = case_when(
    MEDINCSTQ == 1 ~ "First quartile",
    MEDINCSTQ ==  2 ~ "Second quartile",
    MEDINCSTQ ==  3 ~ "Third quartile",
    MEDINCSTQ == 4 ~ "Fourth quartile" )
)

view(med_inc_var)


diagnosis_medinc <- med_inc_var %>% mutate(date = as.factor(as.Date(with(med_inc_var, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                           diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                                  ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_medinc)

# Group by diagnosis var and med income variables

diagnosis_medinc_grp <- diagnosis_medinc %>% group_by(diagnosis_var,med_income) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_medinc_grp)



# Linking ny_sid_mod with AHA File

# Read the AHA DTA file

AHA_sid18 <- read.dta13("NY_SID_2018_AHAL.dta")
View(AHA_sid17)

# Linking the two files

sid_aha18 <- left_join(ny_sid_mod, AHA_sid18, by = c("DSHOSPID" = "DSHOSPID"))

View(sid_aha18)


# LINKING SID_AHA17 FILE WITH ZIP_CROSSWALK FILE

# First, pull census data

# PULLING DATA FROM ACS 2015-2019 CAPTURING RACE, AGE GROUPS AND HOUSING BURDEN,
# FOR NY STATE


# Import libraries needed for this project. ie. ggplot, tidyverse and tidy census

library(tidyverse)
library(tidycensus)
library(ggplot2)

##First step, get list variables that we will be working with for this project

vars <- load_variables(2019, "acs5", cache =TRUE)

View(vars)


#Merge the variables so the table is more readable, use pivot_wider, calculating
#variables using the percentage of total population

ny_info_data <- get_acs(
  geography = "zcta",
  state = 36,
  variables =c(
    total_pop = "B01001_001",
    total_housing_burd = "B25106_001",
    total_hisp_lat = "B03002_001",
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
    nonhisp = "B03002_002",
    nonhisp_white = "B03002_003",
    nonhisp_black = "B03002_004",
    nonhisp_native = "B03002_005",
    nonhisp_asian = "B03002_006",
    nonhisp_HIPI = "B03002_007",
    nonhisp_other1 = "B03002_008",
    nonhisp_other2 = "B03002_009",
    nonhisp_other3 = "B03002_010",
    nonhisp_other4 = "B03002_011",
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
         age_15_19 = 100 * ((male_15_17 + male_18_19 + female_15_17 + 
                               female_18_19)/total_pop),
         age_20_24 = 100 * ((male_20 + male_21 + male_22_24 + female_20 + 
                               female_21 + 
                               female_22_24)/total_pop),
         age_25_29 = 100 * ((male_25_29 + female_25_29)/total_pop),
         age_30_34 = 100 * ((male_30_34 + female_30_34)/total_pop),
         age_35_39 = 100 * ((male_35_39 + female_35_39)/total_pop),
         age_40_44 = 100 * ((male_40_44 + female_40_44)/total_pop),
         age_45_49 = 100 * ((male_45_49 + female_45_49)/total_pop),
         age_50_54 = 100 * ((male_50_54 + female_50_54)/total_pop),
         age_55_59 = 100 * ((male_55_59 + female_55_59)/total_pop),
         age_60_64 = 100 * ((male_60_61 + male_62_64 + female_60_61 + 
                               female_62_64)/total_pop),
         age_65_69 = 100 * ((male_65_66 + male_67_69 + female_65_66 + 
                               female_67_69)/total_pop),
         age_70_74 = 100 * ((male_70_74 + female_70_74)/total_pop),
         age_75_79 =  100 * ((male_75_79 + female_75_79)/total_pop),
         age_80_84 =  100 * ((male_80_84 + female_80_84)/total_pop),
         age_85_over = 100 * ((male_85_over + female_85_over)/total_pop),
         owner_house_burd = 100 * ((owner_less_20k + owner_less_35k + 
                                      owner_less_50k + owner_less_75k + 
                                      owner_over_75k)/total_housing_burd),
         renter_house_burd = 100 * ((renter_less_20k + renter_less_35k + 
                                       renter_less_50k + renter_less_75k + renter_over_75k)/total_housing_burd),
         nonhisp_wht = 100 * (nonhisp_white/total_hisp_lat),
         nonhisp_blk = 100 * (nonhisp_black/total_hisp_lat),
         nonhisp_other = 100 * ((nonhisp_native + nonhisp_asian + nonhisp_HIPI + nonhisp_other1 + nonhisp_other2 + 
                                   nonhisp_other3 + nonhisp_other4)/total_hisp_lat),
         hisp = 100 * ((hisp_white + hisp_black + hisp_native + hisp_asian + hisp_HIPI + hisp_other1 + hisp_other2 + hisp_other3 + 
                          hisp_other4)/total_hisp_lat))%>%
  select(GEOID, NAME, nonhisp_wht, nonhisp_blk, nonhisp_other,hisp, age_under5, age_5_9, age_10_14, 
         age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44, 
         age_45_49, age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, 
         age_75_79, age_80_84, age_85_over, owner_house_burd, renter_house_burd)

view(ny_data)

ny_data_mod <- separate(
  ny_data,
  NAME,
  into = c("label", "ZCTA")
)

View(ny_data_mod)


# Read crosswalk csv before linking with hcup file

library("readxl")

cross17 <- read_xlsx("ZIP-ZCTACrosswalk2017.xlsx")

view(cross17)

cross17_ny <- cross17 %>% filter(STATE == "NY")

view(cross17_ny)


# Merge HCUP and cross walk for NY

Zip_cross17 <- left_join(sid_aha17, cross17_ny, by = c("ZIP" = "ZIP_CODE"))

view(Zip_cross17)

# Merge the zip_cross17 file with ACS data from census using ZCTA

Zcta_data17 <- left_join(Zip_cross17, ny_data_mod, by = c("ZCTA" = "ZCTA"))

view(Zip_cross17)

# Calculate population rate per per month for ILI and ANY ILI


hosp_tab <- select(Zcta_data17, AYEAR, AMONTH, ZCTA, I10_DX_Admitting)


flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

# Creating a month_year and diagnosis variables

hosp_vars <- hosp_tab %>% mutate(date = as.factor(as.Date(with(hosp_tab, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                 diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                                                        ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))



# Group by Date and Diagnosis Var

hosp_group <- hosp_vars %>% group_by(date, diagnosis_var) %>%
  summarize(total= n()) %>% ungroup()

view(hosp_group)


# Calculating Percentage of ILI and ANY ILI per county

# denominator for 2015-2019 acs population is 	19572319, attained from code below


# Total population for ACS(5) from 2015-2019

total_pop <- get_acs(
  geography = "state",
  variables = "B01001_001",
  state = 36,
  year = 2019
)
view(total_pop)

# calculated population rate (per 100,000)

hosp_perc <- hosp_group %>% group_by(date)  %>% mutate(
  pop_rate = total /	19572319 * 100000) %>% arrange(desc(date))


view(hosp_perc)

# Filter out "other" diagnosis to Create the Chart

hosp_fil <- hosp_perc %>% filter(diagnosis_var != "OTHER")

view(hosp_fil)



# Calculate population rate per county for ILI and ANY ILI


county_tab <- select(Zcta_data17,GEOID, STATE, I10_DX_Admitting)


flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

# Creating a month_year and diagnosis variables

county_vars <- county_tab %>% mutate(
  diagnosis_var = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                         ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI","OTHER")))



# Group by Date and Diagnosis Var

county_group <- county_vars %>% group_by(GEOID, diagnosis_var) %>%
  summarize(total= n()) %>% ungroup()

view(county_group)


# Calculating Percentage of ILI and ANY ILI per county

# denominator for 2015-2019 acs population is 	19572319

county_perc <- county_group %>% group_by(GEOID,STATE)  %>% mutate(
  rate = total /	19572319 * 100,000)


view(county_perc)

# Filter out "other" diagnosis to Create the Chart

county_fil <- county_perc %>% filter(diagnosis_var != "OTHER")

view(county_fil)


