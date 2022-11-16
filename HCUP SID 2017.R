# First is to install the library haven for reading the dta file

library(readstata13)
library(tidyverse)

# set working directory
setwd("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_HCUP/HCUP Data_NY")

# Read the DTA file

ny_sid <- read.dta13("NY_SID_2017_CORE.dta")
View(ny_sid)


# Working with just a sample of the data

ny_sid1 <- sample_n(ny_sid,60000)

view(ny_sid1)

# null values

sum(is.na(ny_sid1))

# EDA

# Age count

age_var <- ny_sid1 %>% mutate( 
  age_perc = (AGE/sum(AGE))*100,
  age_grp = case_when(
    AGE < 5 ~ "under 5",
    AGE >= 5 & AGE<= 9 ~ "5-9",
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
    AGE >= 85 ~ "85-Over"))



view(age_var)

# plot graph

ggplot(age_var, aes(age_grp, age_perc)) + 
  geom_bar(stat = "identity") 

# People who died

died_var <- ny_sid1 %>% group_by(DIED) %>% summarise(
  percent = 100 * n()/nrow(ny_sid1)) %>% mutate(DIED = case_when(
    DIED == 0 ~ "Lived",
    DIED == 1 ~ "Died"))

view(died_var)


# Diagnosis Related Group (DRG)

drg_var <- ny_sid1 %>% 
  mutate(total_drg=sum(DRG), 
         perc_drg=(DRG/total_drg)*100) 
view(drg_var)

ggplot(drg_var, aes(DRG, perc_drg)) + 
  geom_bar(stat = "identity")

# Race

# Race and ethnicity - join

race_eth <- ny_sid1 %>% mutate(
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
    RACE == 6 & HISPANIC == 4 ~ "hisp")
)
view(race_eth)

# race-ethnicity percentage
raceth_per <- race_eth %>% group_by(race_ethnicity) %>% summarise(
  percent = 100 * n()/nrow(ny_sid1))

ggplot(raceth_per, aes(race_ethnicity, percent)) + 
  geom_bar(stat = "identity")


# Gender

gender_var <- ny_sid1 %>% group_by(FEMALE) %>% summarise(
  percent = 100 * n()/nrow(ny_sid1)) %>% mutate(FEMALE = case_when(
    FEMALE == 0 ~ "Male",
    FEMALE == 1 ~ "Female"
  ))

view(gender_var)


# Pay Percentages

pay_var <- ny_sid1 %>% group_by(PAY1) %>% summarise(
  percent = 100 * n()/nrow(ny_sid1)) %>% mutate(PAY1 = case_when(
    PAY1 == 1  ~ "Medicare",
    PAY1 == 2  ~ "Medicaid",
    PAY1 == 3  ~ "Private Insurance",
    PAY1 == 4  ~ "Self-pay",
    PAY1 == 5  ~ "No Charge",
    PAY1 == 6  ~ "Other")
)
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

diagnosis_info <- select(ny_sid1, I10_DX_Admitting, I10_DX1, I10_DX2, AMONTH, DMONTH)
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

# Group

diagnosis_vars_group <- diagnosis_vars %>% group_by(date, diagnosis_var) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_vars_group)


# Calculating percentage of diagnosis per month

diagnosis_vars_perc <- diagnosis_vars_group %>% group_by(date)  %>% mutate(
  percent = total / sum(total)* 100) %>% arrange(desc(date))


view(diagnosis_vars_perc)

# Filter out "other" diagnosis

diagnosis_vars_fil <- diagnosis_vars_perc %>% filter(diagnosis_var != "Other")

view(diagnosis_vars_fil)


#Chart

ggplot(data=diagnosis_vars_fil, aes(date,percent, colour = diagnosis_var)) + 
  geom_point(aes(shape = diagnosis_var), size = 4)+
  geom_line(aes(group = diagnosis_var))+ xlab("Month and Year") +
  ylab("ILI Diagnosis Percentage") + ggtitle("Inpatient visits with an ILI diagnosis 2017 New York") 


# Recreating ILI Table


# ****Those with diagnosed with both, first and any ILI****

flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189', 'J09X1','J09X2','J09X3','J09X9')


flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

diagnosis_tab <- diagnosis_info %>% mutate(
  dates = as.factor(as.Date(with(diagnosis_info, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
  diagnosis_vars = ifelse(I10_DX_Admitting %in% flu_diag, "ILI",
                          ifelse(I10_DX_Admitting %in% flu_like_diag, "ANY ILI", "OTHER")))
                                 #ifelse(I10_DX_Admitting %in% flu_diag | I10_DX1 %in% flu_diag & 
                                          #I10_DX_Admitting %in% flu_like_diag | I10_DX1 %in% flu_like_diag, "BOTH", "OTHER"))))


view(diagnosis_tab)


# Group

diagnosis_tab_grp <- diagnosis_tab %>% group_by(diagnosis_vars) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_tab_grp)

# To manually look for observations with "both" diagnosis of ILI and/or ANY ILI in the reason and primary and secondary diagnosis
write.csv(diagnosis_info,"//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_HCUP/HCUP Data_NY//2017 SID2 Diagnosis Information.csv", row.names = TRUE)


# AGE
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

# Group

diagnosis_age_grp <- diagnosis_age %>% group_by(diagnosis_var,age_group) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_age_grp)

# RACE
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

# Group

diagnosis_race_grp <- diagnosis_race %>% group_by(diagnosis_var,race_ethnicity) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_race_grp)


# PAY
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

# Group

diagnosis_pay_grp <- diagnosis_pay %>% group_by(diagnosis_var,payment) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_pay_grp)


# Location of residence

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

# Group

diagnosis_loc_grp <- diagnosis_loc %>% group_by(diagnosis_var,patient_location) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_loc_grp)


# Medium Income

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

# Group

diagnosis_medinc_grp <- diagnosis_medinc %>% group_by(diagnosis_var,med_income) %>%
  summarize(total= n()) %>% ungroup()

view(diagnosis_medinc_grp)





























# SECOND WAY BY IMPUTING THE NULL VALUES USING THE MEAN   

# Replacing null values with mean of the column

library(imputeTS)
diagnosis_info_sid1 <- na_mean(diagnosis_info_sid)
view(diagnosis_info_sid1)

# Then round up 
diag_info_sid <- diagnosis_info_sid1 %>% 
  mutate_if(is.numeric, round)

view(diag_info_sid)

# Create variables for influenza and influenza like diseases
flu_like_diag <- c('J069', 'J399', 'J200', 'J201','J202', 'J203', 'J204', 'J205',
                   'J206', 'J207', 'J208', 'J209', 'J210', 'J211', 'J218', 'J219', 'J40','B012',
                   'B052', 'B0681', 'B250', 'J120', 'J121', 'J122', 'J123', 'J1281', 'J1289', 'J129',
                   'J440', 'J441', 'J470', 'J471')

flu_diag <- c('J1000', 'J1001', 'J1008','J101','J102','J1081','J1082', 'J1083','J1089',
              'J1100','J1108', 'J111','J112','J1181','J1182','J1183','J1189')

both_diag <- c('J09X1','J09X2','J09X3','J09X9')


diagnosis_cat_sid <- diag_info_sid %>% mutate(Dates = as.factor(as.Date(with(diag_info_sid, paste("2017",AMONTH,"01",sep="-")),"%Y-%m-%d")),
                                           Diagnosis = ifelse(I10_DX_Admitting %in% flu_sid, "ILI",
                                                              ifelse(I10_DX_Admitting %in% flu_like_sid, "ANY ILI","Other")))

view(diagnosis_cat_sid)


# Calculate percentage

#Group and summarize with count
diag_cat_sid1 <- diagnosis_cat_sid %>% group_by(Dates, Diagnosis) %>%
  summarize(total= n()) %>% ungroup()

view(diag_cat_sid1)


# Calculating percentage of diagnosis per month

diag_cat_sid2 <- diag_cat_sid1 %>% group_by(Dates)  %>% mutate(
  percentage = total / sum(total)* 100) %>% arrange(desc(Dates))


view(diag_cat_sid2)

# Filter out "other" diagnosis

diag_sid_ili <- diag_cat_sid2 %>% filter(Diagnosis != "Other")
view(diag_sid_ili)


#Chart

ggplot(data=diag_sid_ili, aes(Dates,percentage, colour = Diagnosis)) + 
  geom_point(aes(shape = Diagnosis), size = 4)+
  geom_line(aes(group = Diagnosis))+ xlab("Month and Year") +
  ylab("ILI Diagnosis Percentage") + ggtitle("Inpatient visits with an ILI diagnosis 2017 New York")

