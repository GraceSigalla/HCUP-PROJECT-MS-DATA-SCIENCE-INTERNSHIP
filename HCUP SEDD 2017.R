# Install the libraries needed to complete this project 

# options("install.lock"=FALSE)

library(readstata13)

library(tidyverse)


# SET WORKING DIRECTORY

setwd("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_HCUP/HCUP Data_NY")

# Read the DTA file

ny_sedd <- read.dta13("NY_SEDD_2017_CORE.dta")

View(ny_sedd)

# Working with just a sample of the data 

ny_sedd1 <- sample_n(ny_sedd,30000)


view(ny_sedd1)

# null values

sum(is.na(ny_sedd1))

# EDA

# First, transform the dataset my adding age groups, and relabeling variables for died,
# race and ethnicity, gender and payment 

ny_sedd_mod <- ny_sedd1 %>% mutate(
  age_grp = case_when(
    AGE < 5 ~ "00-04",
    AGE >= 5 &  AGE <= 9 ~ "05-09",
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

view(ny_sedd_mod)

# Analysis on Age

age_var <- ny_sedd_mod %>% mutate( 
  age_perc = (AGE/sum(AGE))*100)

view(age_var)

# Age Graph

ggplot(age_var, aes(age_grp, age_perc)) +  
  geom_bar(stat = "identity")

# Analysis Based on Death Status

died_var <- ny_sedd_mod %>% group_by(died_vars) %>% summarise(
  percent = 100 * n()/nrow(ny_sedd_mod))

view(died_var)

# Analysis on Race

# race-ethnicity percentage
raceth_per <- ny_sedd_mod %>% group_by(race_ethnicity) %>% summarise(
  percent = 100 * n()/nrow(ny_sedd_mod))

# Race Ethnicity Graph

ggplot(raceth_per, aes(race_ethnicity, percent)) + 
  geom_bar(stat = "identity")

# Analysis on Gender

gender_var <- ny_sedd_mod %>% group_by(gender) %>% summarise(
  percent = 100 * n()/nrow(ny_sedd_mod))

view(gender_var)


# Analysis on Payment Options

pay_var <- ny_sedd_mod %>% group_by(payment) %>% summarise(
  percent = 100 * n()/nrow(ny_sedd_mod))

view(pay_var)



# SUMMARY

# According to the 2017 NY SEDD core data collected, majority of the population 
# who visited the state's emergency department were between mostly new borns and
#those in their 30's. A majority of the population 
# a majority being white non-hispanic, and as for the gender, most of the 
# population is females. Most of those who visited the community hospitals in NY
# their services were payed either via medicare, medicaid and private insurance. 
#  the majority of emergency department population survived (didn't die).



# CREATING A CHART SHOWING DISTRIBUTION OF ILI AND ANY ILI PERCENTAGES BY MONTH  

# create a data frame pulling only necessary variables (no deleting or imputing NA's)

diagnosis_info <- select(ny_sedd_mod, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, I10_DX1,
                         I10_DX2, AMONTH, AYEAR)

view(diagnosis_info)

flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
         'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

# Creating a month_year and diagnosis variables

diagnosis_vars <- diagnosis_info %>% mutate(date = as.factor(as.Date(with(diagnosis_info, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                      diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                         ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_vars)

# Group

diagnosis_vars_group <- diagnosis_vars %>% group_by(date, diagnosis_var) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_vars_group)

# Calculating percentage of diagnosis per month

diagnosis_vars_perc <- diagnosis_vars_group %>% group_by(date)  %>% mutate(
  percent = total / sum(total)* 100) %>% arrange(desc(date))


view(diagnosis_vars_perc)


# Filter out "other" diagnosis

diagnosis_vars_fil <- diagnosis_vars_perc %>% filter(diagnosis_var != "OTHER")

view(diagnosis_vars_fil)


#Chart

ggplot(diagnosis_vars_fil, aes(date,percent, colour = diagnosis_var)) + 
  geom_point(aes(shape = diagnosis_var), size = 4)+
  geom_line(aes(group = diagnosis_var))+ xlab("Month and Year") +
  ylab("ILI Diagnosis Percentage") + ggtitle("ED visits with an ILI diagnosis 2017 New York") 


# Recreating ILI Table


# Those with diagnosed with both, first and any ILI (This code didn't work)

flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

diagnosis_tab <- diagnosis_info %>% mutate(
  dates = as.factor(as.Date(with(diagnosis_info, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
  diagnosis_vars = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                          ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI",
                                 ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX1 %in% flu_diag & 
                                          I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX1 %in% flu_like_diag, "BOTH", "OTHER"))))


view(diagnosis_tab)

# Group


# Analysis on Age
age_var <- select(ny_sedd1, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, 
                   AMONTH, AYEAR, DMONTH, AGE)
view(age_var)

age_groups <- age_var %>% mutate(
  age_group = case_when(
    AGE < 18 ~ "Under 18",
    AGE >= 18 & AGE <= 64 ~ "18-64",
    AGE >= 65 ~ "65 and over")
)

view(age_groups)

diagnosis_age <- age_groups %>% mutate(date = as.factor(as.Date(with(age_groups, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                      diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                             ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_age)

# Group diagnosis var and age group

diagnosis_age_grp <- diagnosis_age %>% group_by(diagnosis_var,age_group) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_age_grp)

# Analysis on Race

race_var <- select(ny_sedd1, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, 
                   AMONTH, AYEAR, DMONTH, RACE)
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


diagnosis_race <- race_groups %>% mutate(date = as.factor(as.Date(with(race_groups, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                        diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                               ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_race)

# Group by diagnosis var and race ethnicity

diagnosis_race_grp <- diagnosis_race %>% group_by(diagnosis_var,race_ethnicity) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_race_grp)


# Analysis on Pay

payment_var <- select(pay_var, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, 
                   AMONTH, AYEAR, DMONTH, payment)
view(payment_var)


diagnosis_pay <- payment_var %>% mutate(date = as.factor(as.Date(with(payment_var, paste(AYEAR,AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                          diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                                 ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_pay)

# Group by diagnosis var and payment variables

diagnosis_pay_grp <- diagnosis_pay %>% group_by(diagnosis_var,payment) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_pay_grp)

# Location of residence

loc <- select(ny_sedd1, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, 
                    AMONTH, AYEAR, DMONTH, PL_UR_CAT4)

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
                                               diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                                      ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_loc)

# Group by Diagnosis Var and Patient Location

diagnosis_loc_grp <- diagnosis_loc %>% group_by(diagnosis_var,patient_location) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_loc_grp)

# Analysis on Medium Income

med_inc <- select(ny_sedd1, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2, 
                  AMONTH, AYEAR, DMONTH, MEDINCSTQ)

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
                                           diagnosis_var = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                                                                  ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))


view(diagnosis_medinc)

# Group by Diagnosis Var and Med Income Variables

diagnosis_medinc_grp <- diagnosis_medinc %>% group_by(diagnosis_var,med_income) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_medinc_grp)


# Linking ny_sid_mod with AHA File

# Read the AHA DTA file

AHA_sedd17 <- read.dta13("NY_SEDD_2017_AHAL.dta")
View(AHA_sedd17)

# Linking the two files

sedd_aha17 <- left_join(ny_sedd_mod, AHA_sedd17, by = c("DSHOSPID" = "DSHOSPID"))

View(sedd_aha17)


# Count of Hospitalizations per Hospitals per Year for ILI and Any ILI

hosp_tab <- select(sedd_aha17, AYEAR, HOSPID, I10_DX_Visit_Reason1, I10_DX_Visit_Reason2)

hospitalizations <- hosp_tab %>% mutate(
  diagnosis_col = ifelse(I10_DX_Visit_Reason1 %in% flu_diag | I10_DX_Visit_Reason2 %in% flu_diag, "ILI",
                         ifelse(I10_DX_Visit_Reason1 %in% flu_like_diag | I10_DX_Visit_Reason2 %in% flu_like_diag, "ANY ILI","OTHER")))

view(hospitalizations)

