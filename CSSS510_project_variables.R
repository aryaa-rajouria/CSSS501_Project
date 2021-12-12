
# READ DATA AND MERGE--------------------------------

library(readxl)
library(tidyverse)
library(labelled) #this package allows us to give variables full labels
library(data.table)
library(dplyr)

#Adding Nios Data 
Data1 <- read.csv("https://www.dol.gov/sites/dolgov/files/ETA/naws/pdfs/NAWS_NIOSH_2009_2010.csv")

#Adding NAWS Data questionnaire variables in alphabetical order from "A" through "E" covering the period 1989 through 2018 
Data2 <- read.csv("https://www.dol.gov/sites/dolgov/files/ETA/naws/pdfs/NAWS_A2E191.csv")

#questionnaire variables in alphabetical order from "F" through "Y" covering the period 1989 through 2018
Data3 <- read.csv("https://www.dol.gov/sites/dolgov/files/ETA/naws/pdfs/NAWS_F2Y191.csv")


# MERGE DATA (Nov. 8, 2021) [Aryaa]

# NAWS data with NIOSH data aka Final Data
#merged data 1 and two by FWID
merge1 <- merge(Data1, Data2, by="FWID")
#merged the above data with Data 3 (Note obs for both merges are < than for original)
fdata <- merge(merge1, Data3, by="FWID")

# TIMELINE CHECK-IN (Nov. 8, 2021) [Aryaa & Aminah] Update the group


# CLEAN DATA ---------------------------------------------------


#  CLEAN DATA & CREATE OUTCOME VARIABLES (check for missing, NAs)
# (Nov. 12, 2021)

# Self-rated health (ordered logit) [Aminah]
# MG1 In general, how would you describe your health? Would you say... --> categorical 
# . -> missing - 1 NA from 2010
# 1 -> Excellent - 747
# 2 -> Good - 2115
# 3 -> Fair - 811
# 4 -> Poor - 14
# 7 -> Don't know - 3 

fdata <- rename(fdata, srh = MG1) # Renaming the variable to reflect self-rated health (srh)

fdata$srh[fdata$srh == 7] <- NA # Recoding 3 "Don't Know" cases into NAs

fdata %>% count(srh) # Calling counts of each of the responses 

# Checking counts 
#  srh    n
#    1     747
#    2    2115
#    3    811
#    4     14
#   NA      4    

# make srh into factor with labels
fdata <- fdata %>% mutate(srh=factor(srh, labels = c("Excellent", "Good", "Fair", "Poor")))
var_label(fdata$srh) <- "Self rated health"




# elevated depressive symptoms (binary) [Aryaa]
#are we making an index? Just did a binary synopsis for var MC10 in NIOS
#Relates to MC10: In the past seven (7) days, have you felt depressed? 
#. = missing; 0 = No; 1 = Yes 
#<<<<<<< HEAD

#fdata <- rename(fdata, eds = MC10) - CKA changed 12/3, not correct

#counting up eds in the final merged data: note, NOT separated by years (2009/2010)
#fdata %>% count(eds)
#3 NA's, 2174 (No) 0s, and 514 (Yes) 1s
#are we getting rid of NAs? 
fdata$MC10
# df$Rate2 = df$Rate # there's no df, not sure what this does

# df$Marks[df$Names == "Sita"] <- 25 # there's no df, not sure what this does
# =======
fdata <- rename(fdata, eds = MC10)
#counting up eds in the final merged data: note, NOT seperated by years (2009/2010)
fdata %>% count(eds)
#3 NA's, 2174 (No) 0s, and 514 (Yes) 1s
#are we getting rid of NAs? 
# fdata$MC10

# # DF is not a data frame that exists, this causes an error
# df$Marks[df$Names == "Sita"] <- 25

#m=10
#for(i in 1:m){
#    fdata$new_mcdays[i] = fdata$MCDays[i]
# }
# I know this can be done with loops but none of them are working so doing it this way for now: will figure this out!!! 

fdata$new_mcdays1 = fdata$MCDays1
fdata$new_mcdays2 = fdata$MCDays2
fdata$new_mcdays3 = fdata$MCDays3
fdata$new_mcdays4 = fdata$MCDays4
fdata$new_mcdays5 = fdata$MCDays5
fdata$new_mcdays6 = fdata$MCDays6
fdata$new_mcdays7 = fdata$MCDays7
fdata$new_mcdays8 = fdata$MCDays8
fdata$new_mcdays9 = fdata$MCDays9
fdata$new_mcdays10 = fdata$MCDays10

#regrouping mcdays1   
fdata$new_mcdays1[fdata$new_mcdays1==2] <- 1
fdata$new_mcdays1[fdata$new_mcdays1==3] <- 2
fdata$new_mcdays1[fdata$new_mcdays1==4] <- 2
fdata$new_mcdays1[fdata$new_mcdays1==5] <- 3
fdata$new_mcdays1[fdata$new_mcdays1==6] <- 3
fdata$new_mcdays1[fdata$new_mcdays1==7] <- 3
fdata$new_mcdays1[is.na(fdata$new_mcdays1)] <- 0

#fdata$new_mcdays2 = fdata$MCDays2 
#iters <- 7
#output <- numeric(iters)
#for (i in 1:iters) {
# output[i] <- fdata$new_mcdays1[fdata$new_mcdays1==2]}

#regrouping mcdays2
fdata$new_mcdays2[fdata$new_mcdays2==2] <- 1
fdata$new_mcdays2[fdata$new_mcdays2==3] <- 2
fdata$new_mcdays2[fdata$new_mcdays2==4] <- 2
fdata$new_mcdays2[fdata$new_mcdays2==5] <- 3
fdata$new_mcdays2[fdata$new_mcdays2==6] <- 3
fdata$new_mcdays2[fdata$new_mcdays2==7] <- 3
fdata$new_mcdays2[is.na(fdata$new_mcdays2)] <- 0

#regrouping mcdays3
fdata$new_mcdays3[fdata$new_mcdays3==2] <- 1
fdata$new_mcdays3[fdata$new_mcdays3==3] <- 2
fdata$new_mcdays3[fdata$new_mcdays3==4] <- 2
fdata$new_mcdays3[fdata$new_mcdays3==5] <- 3
fdata$new_mcdays3[fdata$new_mcdays3==6] <- 3
fdata$new_mcdays3[fdata$new_mcdays3==7] <- 3
fdata$new_mcdays3[is.na(fdata$new_mcdays3)] <- 0

#regrouping mcdays4
fdata$new_mcdays4[fdata$new_mcdays4==2] <- 1
fdata$new_mcdays4[fdata$new_mcdays4==3] <- 2
fdata$new_mcdays4[fdata$new_mcdays4==4] <- 2
fdata$new_mcdays4[fdata$new_mcdays4==5] <- 3
fdata$new_mcdays4[fdata$new_mcdays4==6] <- 3
fdata$new_mcdays4[fdata$new_mcdays4==7] <- 3
fdata$new_mcdays4[is.na(fdata$new_mcdays4)] <- 0

#regrouping mcdays5
fdata$new_mcdays5[fdata$new_mcdays5==2] <- 1
fdata$new_mcdays5[fdata$new_mcdays5==3] <- 2
fdata$new_mcdays5[fdata$new_mcdays5==4] <- 2
fdata$new_mcdays5[fdata$new_mcdays5==5] <- 3
fdata$new_mcdays5[fdata$new_mcdays5==6] <- 3
fdata$new_mcdays5[fdata$new_mcdays5==7] <- 3
fdata$new_mcdays5[is.na(fdata$new_mcdays5)] <- 0

#regrouping mcdays6
fdata$new_mcdays6[fdata$new_mcdays6==2] <- 1
fdata$new_mcdays6[fdata$new_mcdays6==3] <- 2
fdata$new_mcdays6[fdata$new_mcdays6==4] <- 2
fdata$new_mcdays6[fdata$new_mcdays6==5] <- 3
fdata$new_mcdays6[fdata$new_mcdays6==6] <- 3
fdata$new_mcdays6[fdata$new_mcdays6==7] <- 3
fdata$new_mcdays6[is.na(fdata$new_mcdays6)] <- 0

#regrouping mcdays7
fdata$new_mcdays7[fdata$new_mcdays7==2] <- 1
fdata$new_mcdays7[fdata$new_mcdays7==3] <- 2
fdata$new_mcdays7[fdata$new_mcdays7==4] <- 2
fdata$new_mcdays7[fdata$new_mcdays7==5] <- 3
fdata$new_mcdays7[fdata$new_mcdays7==6] <- 3
fdata$new_mcdays7[fdata$new_mcdays7==7] <- 3
fdata$new_mcdays7[is.na(fdata$new_mcdays7)] <- 0

#regrouping mcdays8
fdata$new_mcdays8[fdata$new_mcdays8==2] <- 1
fdata$new_mcdays8[fdata$new_mcdays8==3] <- 2
fdata$new_mcdays8[fdata$new_mcdays8==4] <- 2
fdata$new_mcdays8[fdata$new_mcdays8==5] <- 3
fdata$new_mcdays8[fdata$new_mcdays8==6] <- 3
fdata$new_mcdays8[fdata$new_mcdays8==7] <- 3
fdata$new_mcdays8[is.na(fdata$new_mcdays8)] <- 0

#regrouping mcdays9
fdata$new_mcdays9[fdata$new_mcdays9==2] <- 1
fdata$new_mcdays9[fdata$new_mcdays9==3] <- 2
fdata$new_mcdays9[fdata$new_mcdays9==4] <- 2
fdata$new_mcdays9[fdata$new_mcdays9==5] <- 3
fdata$new_mcdays9[fdata$new_mcdays9==6] <- 3
fdata$new_mcdays9[fdata$new_mcdays9==7] <- 3
fdata$new_mcdays9[is.na(fdata$new_mcdays9)] <- 0

#regrouping mcdays10
fdata$new_mcdays10[fdata$new_mcdays10==2] <- 1
fdata$new_mcdays10[fdata$new_mcdays10==3] <- 2
fdata$new_mcdays10[fdata$new_mcdays10==4] <- 2
fdata$new_mcdays10[fdata$new_mcdays10==5] <- 3
fdata$new_mcdays10[fdata$new_mcdays10==6] <- 3
fdata$new_mcdays10[fdata$new_mcdays10==7] <- 3
fdata$new_mcdays10[is.na(fdata$new_mcdays10)] <- 0

# Adding new column based on the sum of other columns:
# fdata <- fdata %>% rowwise () %>% 
#   mutate(eds_total = 0)

fdata <- fdata %>% rowwise() %>%
  mutate(eds_total=0)

fdata <- fdata %>% rowwise() %>%
  mutate(eds_total = sum(c_across(new_mcdays1:new_mcdays10)))

fdata %>% count(eds_total) 
# eds_total     n
# <dbl> <int>
#   1         0   237
# 2         1   133
# 3         2   186
# 4         3  2802
# 5         4   249
# 6         5    58
# 7         6    26

##-------------------------------------------------------------------
# added 12/3 by Courtney
fdata <- fdata %>% rowwise() %>%
  mutate(eds_total2 = sum(across(starts_with("new_mcdays"))))

fdata %>% count(eds_total2) 


#eds binary variable created, 
fdata <- fdata %>% mutate(eds = case_when(
  eds_total2 < 10 ~ 0,
  eds_total2 >=10 ~ 1))
var_label(fdata$eds) <- "Elevated depressive symptoms"

# look at results, for some reason eds_total wasn't accurate
# fdata %>% select(new_mcdays1, new_mcdays2, new_mcdays3, new_mcdays4, new_mcdays5, new_mcdays6, new_mcdays7, new_mcdays8, new_mcdays9, new_mcdays10, eds_total2, eds_total) %>% view()
# 

##-------------------------------------------------------------------
# Note: below no longer matches count above
#eds_total     n
#<dbl> <int>
# 1        12     1
#2        14     3
#3        16     1
#4        20     1
#5        24     1
#6        28     1
#7        29     1
#8        30     1
#9        NA  3681

# fdata <- fdata %>% rowwise() %>%
#   mutate(eds_check = sum(c_across(MCDays1:MCDays10)))
# 
# fdata %>% count(eds_check) 



# elevated psychological demands (binary) [Courtney]
# this indicator sums up responses to MJ1 and MJ2
fdata <- fdata %>% mutate(
  MJ1b = case_when(
    MJ1 ==5 ~ NA_integer_,  # 5 = doesn't understand, change to NA
    MJ1 ==7 ~ NA_integer_,  # 7 = don't know, change to NA
    TRUE ~ MJ1),
  MJ2b = case_when(
    MJ2 ==5 ~ NA_integer_,  # 5 = doesn't understand, change to NA
    MJ2 ==7 ~ NA_integer_,  # 7 = don't know, change to NA
    TRUE ~ MJ2),
  epd = case_when(
    MJ1b + MJ2b >= 2 ~ 1,
    MJ1b + MJ2b  < 2 ~ 0))  # score of 2 or higher means epd = true

# check count
# epd    n
# 1   0 2540
# 2   1 1134
# 3  NA   17
var_label(fdata$epd) <- "Elevated psychological demand"



# low control (binary) [Katherine]
fdata <- data.frame(fdata)
smd <- fdata$MD1 + fdata$MD2 + fdata$MD3 + fdata$MD4 #new column with summed MD1,MD2,MD3,MD4 on decision-latitude
lowcont <- rep(0, length(smd)) #lowcont created as an empty shell of 0s, to be filled by following:
for (i in 1:length(smd)) { #for every element of smd,
  if (fdata$MD1[i] %in% c(5, 6, 7) | fdata$MD2[i] %in% c(5, 6, 7) | 
      fdata$MD3[i] %in% c(5, 6, 7) | fdata$MD4[i] %in% c(5, 6, 7)) {
    lowcont[i] <- NA; #changes responses of 5, 6, 7 to NAs for MD1-4
  } # if you implement check above then make below and else if statmenet
  else if (!is.na(smd[i])) {  #if element of smd is not na
    if(smd[i] <= 2) { #and if element of smd is less than or equal to 2
      if (all(fdata[i, c('MD1', 'MD2', 'MD3', 'MD4')] < 2)) { #or if MD1, MD2, MD3, and MD4 is less than 2
        lowcont[i] <- 1 #then it is coded as 1 for low control.
      }
    }
  } else { #if element of smd is na, then it is coded as na
    lowcont[i] <- NA
  }
}


fdata$lowcont <- lowcont
var_label(fdata$lowcont) <- "Low control"

# Above code effectively throws out observations where respondant 
# (5) didn't understand, (6) refused, or (7) didn't know

table(lowcont)
sum(is.na(lowcont))
# low control - 2909 0s (no), 536 1s (yes), 246 (na)
# missing (.) is coded as na

# job strain (binary) [Aminah]

# Job strain was coded as 1 if both the elevated psychological demands and low control variables were coded as 1. 
# Otherwise, job strain was coded as 0.
# Represents the other outcomes put together.

fdata <- fdata %>% mutate(
  job_strain = case_when(
    epd == 1 & lowcont == 1 ~ 1, # If epd and lowcont is 1, job strain is 1
    epd != 1 & lowcont != 1 ~ 0)) # If epd and lowcont are not equal to 1, job strain is 0 

fdata %>% count(job_strain) # Calling counts of each of the responses 

var_label(fdata$job_strain) <- "Job strain"

# below counts no longer match above


# Checking counts 
#    job_strain    n
#     0   2556
#     1   1134
#     NA    1   *What to do with single NA?* 


# CREATE COVARIATES (comment about the number of missing)

# Age - AGE - respondent age  [Aryaa]
# <<<<<<< HEAD

# fdata <- rename(fdata, age = AGE) # commented to preserve old AGE variable in case it provides a better fit 

# below is not totally correct, there's an NA which is presumably someone 90+ yo
# should change that line to TRUE ~ "60+"))

fdata <- fdata %>% mutate(age = case_when(AGE < 20 ~ "14-19",
                                          AGE < 30 ~ "20-29",
                                          AGE < 40 ~ "30-39",
                                          AGE < 50 ~ "40-49",
                                          AGE < 60 ~ "50-59",
                                          AGE < 90 ~ "60+"))
var_label(fdata$age) <- "Age"

# commenting out because this gets rid of original variable
  # fdata <- rename(fdata, age = AGE)

#recategorizing fdata

# This data seems to do the same thing, but the new variable
# fdata <- fdata %>% mutate(age = case_when(age < 20 ~ "14-19",
#                                           age < 30 ~ "20-29",
#                                           age < 40 ~ "30-39",
#                                           age < 50 ~ "40-49",
#                                           age < 60 ~ "50-59", 
#                                           age < 90 ~ "60+"))


# Creating a numeric variable for age called age_num

fdata <- fdata %>% mutate(age_num = case_when(AGE < 20 ~ 1,
                                              AGE < 30 ~ 2,
                                              AGE < 40 ~ 3,
                                              AGE < 50 ~ 4,
                                              AGE < 60 ~ 5,
                                              AGE < 90 ~ 6))

var_label(fdata$age_num) <- "Age numeric"

fdata %>% count(age) 
# age   n
# 14-19 162
# 20-29 986
# 30-39 994
# 40-49 824
# 50-59 491
# 60-69 205 (Collapsed 60+: 234)
# 70-79  27
# 80-89   1
#  <NA>   1


# Gender - GENDER - respondent gender [Courtney]
fdata <- fdata %>% mutate(
  gender = case_when(
    GENDER == 0 ~ "man",
    GENDER == 1 ~ "woman"))

var_label(fdata$gender) <- "Gender"

# Creating a numeric variable called gender_num 

fdata <- fdata %>% mutate(gender_num = GENDER)
table(fdata$gender_num)
sum(is.na(fdata$gender_num))

var_label(fdata$gender) <- "Gender numeric"


fdata %>% count(gender)


# check count
# gender    n
# man 3052
# woman  639


# English attainment: [Katherine]

# B03A - asks respondent whether she attended English/ESL classes or school in the U.S.

fdata <- fdata %>% mutate(
  esl_class = case_when(
    B03ex ==0 ~ "No",
    B03ex ==1 ~ "Yes"))

var_label(fdata$esl_class) <- "ESL class"

# Creates new numeric variable for esl_class_num

fdata <- fdata %>% mutate(esl_class_num = B03ax)
table(fdata$esl_class_num)
sum(is.na(fdata$esl_class_num))

var_label(fdata$esl_class) <- "ESL class numeric"

# attended esl classes - 3091 0s (no), 598 1s (yes), 2 (na)

# B07 - asks how well the respondent speaks English

fdata <- fdata %>% mutate(eng_speak = factor(B07, labels = c("Not at all", 
                                                             "A little", "Somewhat", "Well")))
var_label(fdata$eng_speak) <- "Speak English"

# Creates new numeric variable for eng_speak_num

fdata <- fdata %>% mutate(eng_speak_num = B07)
table(fdata$eng_speak_num)
sum(is.na(fdata$eng_speak_num))

# english speaking skill - 1223 1s (not at all), 1220 2s (a little), 355 3s (somewhat), 887 4s (well), 6 (na)

# B08 - asks how well the respondent reads English 

fdata <- fdata %>% mutate(eng_read = factor(B08, labels = c("Not at all", "A little", "Somewhat", "Well")))
var_label(fdata$eng_read) <- "Read English"

# Creates new numeric variable for eng_read_num

fdata <- fdata %>% mutate(eng_read_num = B08)
table(fdata$eng_read_num)
sum(is.na(fdata$eng_read_num))

#english reading skill - 1687 1s (not at all), 908 2s (a little), 241 3s (somewhat), 847 4s (well), 8 (na)

# B20 - asks about language spoken to respondent as a child 

# The 'setnames' keeps original variables, but not renaming because we do not use these

fdata <- setnames(fdata, old = c('B20a','B20b','B20c','B20d','B20e','B20f','B20z'),
                  new = c('ac_english','ac_spanish','ac_creole','ac_mixtec','ac_kanjobal',
                          'ac_zapotec','ac_other'))

sum(is.na(fdata$ac_english)) 
table(fdata$ac_english) 
# english - 2962 (false), 727 (true), 2 (na)
sum(is.na(fdata$ac_spanish))
table(fdata$ac_spanish) 
# spanish - 669 (false), 3022 (true), 0 (na)
sum(is.na(fdata$ac_creole))
table(fdata$ac_creole) 
# creole - 3679 (false), 12 (true), 0 (na)
sum(is.na(fdata$ac_mixtec))
table(fdata$ac_mixtec) 
# mixtec - 3617 (false), 74 (true), 0 (na)
sum(is.na(fdata$ac_kanjobal))
table(fdata$ac_kanjobal) 
# kanjobal - 3679 (false), 12 (true), 0 (na)
sum(is.na(fdata$ac_zapotec))
table(fdata$ac_zapotec)
# zapotec - 3660 (false), 31 (true), 0 (na)
sum(is.na(fdata$ac_other))
table(fdata$ac_other) 
# other - 3537 (false), 154 (true), 0 (na)

# B21 - asks what languages the respondent speaks as an adult 

fdata <- setnames(fdata, old = c('B21a','B21b','B21c','B21d','B21e','B21f','B21z'),
                  new = c('aa_english','aa_spanish','aa_creole','aa_mixtec','aa_kanjobal',
                          'aa_zapotec','aa_other'))

sum(is.na(fdata$aa_english)) 
table(fdata$aa_english) 
# english - 1243 (false), 2447 (true), 2 (na)
sum(is.na(fdata$aa_spanish))
table(fdata$aa_spanish) 
# spanish - 518 (false), 3171 (true), 2 (na)
sum(is.na(fdata$aa_creole))
table(fdata$aa_creole) 
# creole - 3677 (false), 14 (true), 0 (na)
sum(is.na(fdata$aa_mixtec))
table(fdata$aa_mixtec) 
# mixtec - 3616 (false), 75 (true), 0 (na)
sum(is.na(fdata$aa_kanjobal))
table(fdata$aa_kanjobal) 
# kanjobal - 3680 (false), 11 (true), 0 (na)
sum(is.na(fdata$aa_zapotec))
table(fdata$aa_zapotec) 
# zapotec - 3661 (false), 30 (true), 0 (na)
sum(is.na(fdata$aa_other))
table(fdata$aa_other) 
# other - 3537 (false), 154 (true), 0 (na)

# B24 - asks which language the respondent is most comfortable conversing in
# B24 doesn't exist

fdata <- fdata %>%
  mutate(dom_lang = factor(B24, labels = c("English", "Spanish", "Creole", "Mixtec", "Kanjobal", "Zapotec", "Other")))
var_label(fdata$dom_lang) <- "Dominant language"

# Creating numeric variable called dom_lang_num

fdata <- fdata %>% mutate(dom_lang_num = B24)
table(fdata$dom_lang_num)
sum(is.na(fdata$dom_lang_num))

var_label(fdata$dom_lang_num) <- "Dominant language numeric"


# Health insurance: [Aryaa]
# A21A - asks whether respondent has health insurance 
# fdata <- rename(fdata, health_in = A21a)
fdata <- fdata %>% mutate(health_in = case_when(
  A21a==0 ~ "No",
  A21a==1 ~ "Yes",
  A21a==95 ~ NA_character_))
var_label(fdata$health_in) <- "Health Insurance"

# Creating numeric variable called health_in_num

fdata <- fdata %>% mutate(health_in_num = A21a)
table(fdata$health_in_num)
sum(is.na(fdata$health_in_num))

var_label(fdata$health_in_num) <- "Health insurance numeric"

# Coding 95 as NA
fdata$health_in[fdata$health_in == 95] <- NA
fdata$health_in_num[fdata$health_in_num == 95] <- NA

# just making sure they match
table(fdata$health_in)
table(fdata$health_in_num)


#counting up health_in in the final merged data
fdata %>% count(health_in)


# Documented status: [Courtney]
# LEGAPPL - indicates status of legal application 

fdata <- fdata %>% mutate(
  doc_status = case_when(
    currstat ==1 ~ "Citizen",
    currstat ==2 ~ "Green card",
    currstat ==3 ~ "Other work authorization",
    currstat ==4 ~ "Unauthorized"))

# Creating numeric variable called doc_status_num

fdata <- fdata %>% mutate(doc_status_num = currstat)
table(fdata$doc_status_num)
sum(is.na(fdata$doc_status_num))

var_label(fdata$doc_status_num) <- "Doc status numeric"

# MIGTYPE - indicates type of migrant
fdata <- fdata %>% mutate(
  migrant = case_when(
    MIGRANT ==0 ~ "Non-migrant",
    MIGRANT ==1 ~ "Migrant"))

# Creating numeric variable called migrant_num

fdata <- fdata %>% mutate(migrant_num = MIGRANT)
table(fdata$migrant_num)
sum(is.na(fdata$migrant_num))

fdata <- fdata %>% mutate(
  migrant_1 = case_when(
    MIGTYPE ==0 ~ "Non-migrant",
    MIGTYPE ==1 ~ "FTC",
    MIGTYPE ==2 ~ "Shuttle"))

# Creating numeric variable called migrant_1_num

fdata <- fdata %>% mutate(migrant_1_num = MIGTYPE)
table(fdata$migrant_1_num)
sum(is.na(fdata$migrant_1_num))

var_label(fdata$migrant_1) <- "Migrant type 1 numeric"


fdata <- fdata %>% mutate(
  migrant_2 = case_when(
    MIGTYPE2 =="FTC" ~ "FTC",
    MIGTYPE2 =="NEWCOMER" ~ "Newcomer",
    MIGTYPE2 =="SETTLED" ~ "Settled",
    MIGTYPE2 =="SHUTTLE" ~ "Shuttle"))

# Creating numeric variable called migrant_2_num

# fdata <- fdata %>% mutate(migrant_2_num = MIGTYPE2)
# table(fdata$migrant_2_num)
# sum(is.na(fdata$migrant_2_num))

fdata <- fdata %>% mutate(
  migrant_2_num = case_when(
    MIGTYPE2 =="FTC" ~ 1,
    MIGTYPE2 =="NEWCOMER" ~ 2,
    MIGTYPE2 =="SETTLED" ~ 3,
    MIGTYPE2 =="SHUTTLE" ~ 4))


var_label(fdata$migrant_2_num) <- "Migrant type 2 numeric"


# ETHNICITY - 

# Not totally sure how we would interpret ethnicity as below
fdata <- fdata %>% mutate(
  ethnicity = case_when(
    B01 ==1 ~ "Mexican-American",
    B01 ==2 ~ "Mexican",
    B01 ==3 ~ "Mexican-American",
    B01 ==4 ~ "Other Hispanic",
    B01 ==5 ~ "Other Hispanic",
    B01 ==7 ~ "Not Hispianic or Latino"))  

# Creating numeric variable ethnicity_num

fdata <- fdata %>% mutate(ethnicity_num = B01)
table(fdata$ethnicity_num)
sum(is.na(fdata$ethnicity_num))

var_label(fdata$ethnicity_num) <- "Ethnicity numeric"


# RACE - 
fdata <- fdata %>% mutate(
  race = case_when(
    B02 ==1 ~ "White",
    B02 ==2 ~ "Black AA",
    B02 ==4 ~ "Am Indian, Indigenous",
    B02 ==5 ~ "Other", # 5 Asian cases
    B02 ==6 ~ "Other", # 11 Native HI P.I cases
    B02 ==7 ~ "Other")) 

# Creating numeric variable called race_num

fdata <- fdata %>% mutate(race_num = B02)
table(fdata$race_num)
sum(is.na(fdata$race_num))

var_label(fdata$race) <- "Race numeric"


# EDCUATION - Attended any HS or GED equivalent school in US
fdata <- fdata %>% mutate(
  edu_hs = case_when(
    B03ex ==0 ~ "No",
    B03ex ==1 ~ "Yes"))

# Creating numeric variable called edu_hs_num

fdata <- fdata %>% mutate(edu_hs_num = B03ex)
table(fdata$edu_hs_num)
sum(is.na(fdata$edu_hs_num))

var_label(fdata$edu_hs_num) <- "U.S. high school numeric"

# EDCUATION - Attended any college in US
fdata <- fdata %>% mutate(
  edu_coll = case_when(
    B03fx ==0 ~ "No",
    B03fx ==1 ~ "Yes"))

var_label(fdata$edu_coll) <- "U.S. college numeric"

# Creating numeric variable called edu_coll_num

fdata <- fdata %>% mutate(edu_coll_num = B03fx)
table(fdata$edu_coll_num)
sum(is.na(fdata$edu_coll_num))

var_label(fdata$edu_coll_num) <- "U.S. college numeric"


# EDCUATION - Attended any English classes in US
fdata <- fdata %>% mutate(
  edu_esl = case_when(
    B03ax ==0 ~ "No",
    B03ax ==1 ~ "Yes"))   

var_label(fdata$edu_esl) <- "ESL class"

# Creating numeric variable called edu_esl_num

fdata <- fdata %>% mutate(edu_esl_num = B03ax)
table(fdata$edu_esl_num)
sum(is.na(fdata$edu_esl_num))

var_label(fdata$edu_esl_num) <- "ESL class numeric"


# EDCUATION - Attended any job in US
fdata <- fdata %>% mutate(
  edu_job = case_when(
    B03dx ==0 ~ "No",
    B03dx ==1 ~ "Yes")) 

var_label(fdata$edu_job) <- "Job training"

# Creating numeric variable called edu_job_num

fdata <- fdata %>% mutate(edu_job_num = B03dx)
table(fdata$edu_job_num)
sum(is.na(fdata$edu_job_num))

var_label(fdata$edu_job_num) <- "Job training numeric"

# ANY EDUCATION IN UW
fdata <- fdata %>% mutate(edu_any_usa = "No")
fdata <- fdata %>% mutate(
  edu_any_usa = case_when(
    edu_esl ==1 ~ "Yes",
    edu_hs ==1 ~ "Yes",
    edu_job ==1 ~ "Yes",
    edu_coll ==1 ~ "Yes",
    TRUE ~ edu_any_usa))    

var_label(fdata$edu_any_usa) <- "Any education in US"

# Creating a numeric variable called edu_any_num

fdata <- fdata %>% mutate(edu_any_usa_num = 0)

fdata <- fdata %>% mutate(
  edu_any_usa_num = case_when(
    edu_esl_num ==1 ~ 1,
    edu_hs_num ==1 ~ 1,
    edu_job_num ==1 ~ 1,
    edu_coll_num ==1 ~ 1,
    TRUE ~ edu_any_usa_num))    
var_label(fdata$edu_any_usa_num) <- "Any education in US numeric"

table(fdata$edu_any_usa_num)
sum(is.na(fdata$edu_any_usa_num))

# highest grade finished
# 
# fdata <- fdata %>% mutate(
#   edu_highest = case_when(
#     A09 <6 ~ 1,
#     A09 >=6 & A09 <12 ~ 2,
#     A09 >=12 & A09 <16 ~ 3,                  
#     A09 >=16  ~ 4))
# var_label(fdata$edu_highest) <- "Highest education"
# fdata <- fdata %>% mutate(edu_highest = factor(edu_highest, labels = c("< Primary", "Primary", "High school", "College")))
# 
# var_label(fdata$edu_highest) <- "Highest education"
# 
# # Creating a numeric variable called edu_highest_num
# 
# fdata <- fdata %>% mutate(
#   edu_highest_num = case_when(
#     A09 <6 ~ 1,
#     A09 >=6 & A09 <12 ~ 2,
#     A09 >=12 & A09 <16 ~ 3,                  
#     A09 >=16  ~ 4))
# var_label(fdata$edu_highest_num) <- "Highest education numeric"
# 
# table(fdata$edu_highest)
# sum(is.na(fdata$edu_highest))

# Commented out the above portion. Below seems to be the most accurate. 

# highest grade finsihed

fdata <- fdata %>% mutate(
  edu_highest = case_when(
    A09 >12 ~ "Any College",                  
    A09 >=12 ~ "Secondary",
    A09 >=6  ~ "Primary",
    A09 <6 ~ "Less than primary"))

var_label(fdata$edu_highest) <- "Highest education"

# Creating numeric variable called edu_highest_num

fdata <- fdata %>% mutate(
  edu_highest_num = case_when(
    A09 >12 ~ 1,                  
    A09 >=12 ~ 2,
    A09 >=6  ~ 3,
    A09 <6 ~ 4))

var_label(fdata$edu_highest_num) <- "Highest education numeric"

table(fdata$edu_highest)
sum(is.na(fdata$edu_highest))


# INCOME - FAMILY'S TOTAL INCOME

fdata <- fdata %>% mutate(
  income_family = case_when(
    G03 <=3 ~ "$0-2,499",
    G03 >3 & G03 <=6  ~ "$2,500-9,999",
    G03 >6 & G03 <=9  ~ "$9,999-17,499",
    G03 >9 & G03 <=12  ~ "$17,499-29,999",
    G03 >12 & G03 <97  ~ "$30,000",
    G03==97 ~ NA_character_),income_family = factor(income_family,
                       levels = c("$0-2,499",
                                  "$2,500-9,999",
                                  "$9,999-17,499",
                                  "$17,499-29,999",
                                  "$30,000",
                                  "Don't know")))

var_label(fdata$income_family) <- "Family income"

# numeric version called income_family_num

fdata <- fdata %>% mutate(
  income_family_num = case_when(
    G03 <=3 ~ 1,
    G03 >3 & G03 <=6  ~ 2,
    G03 >6 & G03 <=9  ~ 3,
    G03 >9 & G03 <=12  ~ 4,
    G03 >12 & G03 <97  ~ 5))

# re-coding "don't know" to NA
fdata$income_family_num[fdata$income_family_num == 97] <- NA

var_label(fdata$income_family_num) <- "Family income numeric"

# checking if the same
table(fdata$income_family)
sum(is.na(fdata$income_family))

table(fdata$income_family_num)
sum(is.na(fdata$income_family_num))


# INCOME - personal total income
fdata <- fdata %>% mutate(
  income = case_when(
    G01 <=3 ~ "$0-2,499",
    G01 >3 & G01 <=6  ~ "$2,500-9,999",
    G01 >6 & G01 <=9  ~ "$9,999-17,499",
    G01 >9 & G01 <=12  ~ "$17,499-29,999",
    G01 >12 & G01 <95  ~ "$30,000",
    G01==95 ~ NA_character_),
  income = factor(income,
                  levels = c("$0-2,499",
                             "$2,500-9,999",
                             "$9,999-17,499",
                             "$17,499-29,999",
                             "$30,000",
                             "Don't know")))

var_label(fdata$income) <- "Income"

# Creating numeric variable called income_num

fdata <- fdata %>% mutate(
  income_num = case_when(
    G01 <=3 ~ 1,
    G01 >3 & G01 <=6  ~ 2,
    G01 >6 & G01 <=9  ~ 3,
    G01 >9 & G01 <=12  ~ 4,
    G01 >12 & G01 <95  ~ 5))

fdata$income_num[fdata$income_num == 95] <- NA

var_label(fdata$income_num) <- "Income numeric"

# checking if the same
table(fdata$income)
sum(is.na(fdata$income))

table(fdata$income_num)
sum(is.na(fdata$income_num))


# MIXEDFAM - indicates a respondent is not documented in the U.S. but has children who are U.S. citizens [Katherine]

fdata <- rename(fdata, mix_fam = MixedFam)
table(fdata$mix_fam)
sum(is.na(fdata$mix_fam))
# mixed fam - 3017 0s (no), 674 1s (yes), 0 missing

# NQ10L - indicates a respondent’s main difficulty accessing health care in the U.S. centers on being “undocumented” and “not treated well” as a result [Aminah]

# NQ10L When you want to get health care in the U.S. what are the main difficulties you face?
# I'm "undocumented' they don't treat me well

# This column doesn't appear in our data. What should we do? 

# G04 - Asks if anyone in their household has recieved the following social services in the last 2 years

fdata <- setnames(fdata, old = c('G04px','G04B','G04C','G04D','G04E','G04fx','G04gx','G04H','G04I','G04J',
                                 'G04K','G04L','G04M','G04N'), 
                  new = c('hh_TANF','hh_food_stamps','hh_dis_ins','hh_unemp_ins','hh_soc_sec',
                          'hh_vet_pay','hh_gen_as', 'hh_li_house', 'hh_phealth_cl','hh_medicaid',
                          'hh_wic', 'hh_dis_rel','hh_leg_ser','hh_other'))

# Changing responses with "don't know" (95) to NA

fdata$hh_dis_ins[fdata$hh_dis_ins == 95] <- NA
fdata$hh_unemp_ins[fdata$hh_unemp_ins == 95] <- NA
fdata$hh_soc_sec[fdata$hh_soc_sec == 95] <- NA
fdata$hh_vet_pay[fdata$hh_vet_pay == 95] <- NA
fdata$hh_gen_as[fdata$hh_gen_as == 95] <- NA
fdata$hh_li_house[fdata$hh_li_house == 95] <- NA
fdata$hh_gen_as[fdata$hh_gen_as == 95] <- NA
fdata$hh_other[fdata$hh_other == 95] <- NA


sum(is.na(fdata$hh_TANF)) 
table(fdata$hh_TANF) 
# TANF - 3676 (false), 13 (true), 2 (na)
sum(is.na(fdata$hh_food_stamps)) 
table(fdata$hh_food_stamps) 
# Food stamps - 3301 (0), 836 (1), 2 (na)
sum(is.na(fdata$hh_dis_ins)) 
table(fdata$hh_dis_ins)
# Disability insurance - 3620 (0), 67 (1), 4 (na)
sum(is.na(fdata$hh_unemp_ins)) 
table(fdata$hh_unemp_ins)
# Unemployment insurance - 3620 (0), 67 (1), 4 (na)
sum(is.na(fdata$hh_soc_sec)) 
table(fdata$hh_soc_sec)
# Social security - 3608 (0), 79 (1), 4 (na)
sum(is.na(fdata$hh_vet_pay)) 
table(fdata$hh_vet_pay)
# Veterans pay - 3678 (0), 9 (1), 4 (na)
sum(is.na(fdata$hh_gen_as)) 
table(fdata$hh_gen_as)
# General assistance/welfare - 3664 (0), 23 (1), 4 (na)
sum(is.na(fdata$hh_li_house)) 
table(fdata$hh_li_house)
# Low income housing - 3654 (0), 33 (1), 4 (na)
sum(is.na(fdata$hh_phealth_cl)) 
table(fdata$hh_phealth_cl)
# Public health clinic - 3517 (false), 172 (true), 2 (na)
sum(is.na(fdata$hh_medicaid)) 
table(fdata$hh_medicaid)
# Medicaid - 2503 (false), 1186 (true), 2 (na)
sum(is.na(fdata$hh_wic)) 
table(fdata$hh_wic)
# WIC - 3046 (false), 643 (true), 2 (na)
sum(is.na(fdata$hh_dis_rel)) 
table(fdata$hh_dis_rel)
# Disaster relief - 3672 (false), 17 (true), 2 (na)
sum(is.na(fdata$hh_leg_ser)) 
table(fdata$hh_leg_ser)
# Legal service - 3687 (false), 2 (true), 2 (na)
sum(is.na(fdata$hh_other)) 
table(fdata$hh_other)
# Other - 3660 (0), 27 (1), 4 (na)

#Creating new variable with 0/1 for whether they recieve ANY of the above social assistance
fdata <- fdata %>%
  mutate(hh_social_assist = ifelse(hh_TANF | hh_food_stamps==1 | hh_dis_ins==1 | hh_unemp_ins==1 |
                                     hh_soc_sec==1 | hh_vet_pay==1 | hh_gen_as==1 | hh_li_house==1 | hh_phealth_cl |
                                     hh_medicaid | hh_wic | hh_dis_rel | hh_leg_ser | hh_other==1, 1, 0))

sum(is.na(fdata$hh_social_assist)) 
table(fdata$hh_social_assist)
# Social assistance - 1882 (0), 1807 (1), 2 (na)

# Married/living together

fdata <- fdata %>% mutate(
  married.LT = case_when(
    A05 ==1 ~ "Single",
    A05 ==2 ~ "Married/LT",
    A05 ==3 ~ "Sep/Divorced"))

# creating numeric variable called married.LT_num

fdata <- fdata %>% mutate(
  married.LT_num = case_when(
    A05 ==1 ~ 1,
    A05 ==2 ~ 2,
    A05 ==3 ~ 3))

sum(is.na(fdata$married.LT)) 
table(fdata$married.LT)
sum(is.na(fdata$married.LT_num)) 
table(fdata$married.LT_num)

# 2337 married, 231 sep/divorced, 1122 single, 1 na


# write.csv(fdata, 'fdata10.csv')


#Creating a new dataset with only the selected covariates (created above)

# [TIMELINE CHECK-IN ON WED. NOV 10, 2021]

