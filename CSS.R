
# READ IN DATA  

library(readxl)

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

#---------------------------------------------------
#  CLEAN DATA & CREATE OUTCOME VARIABLES (check for missing, NAs)
# (Nov. 12, 2021)

  # Self-rated health (ordered logit) [Aminah]

  # elevated depressive symptoms (binary) [Aryaa]
    #are we making an index? Just did a binary synopsis for var MC10 in NIOS
    #Relates to MC10: In the past seven (7) days, have you felt depressed? 
      #. = missing; 0 = No; 1 = Yes 
    library(dplyr)
    fdata <- rename(fdata, eds = MC10)
    #counting up eds in the final merged data: note, NOT seperated by years (2009/2010)
    fdata %>% count(eds)
    #3 NA's, 2174 (No) 0s, and 514 (Yes) 1s
    #are we getting rid of NAs? 

  # elevated psychological demands (binary) [Courtney]
  # low control (binary) [Katherine]
  # job strain (binary) [Aminah]

# CREATE COVARIATES (comment about the number of missing)

# Age - AGE - respondent age  [Aryaa]
    fdata <- rename(fdata, age = AGE)
    #recategorizing fdata
    fdata <- fdata %>% mutate(age = case_when(age < 20 ~ "14-19",
                                     age < 30 ~ "20-29",
                                     age < 40 ~ "30-39",
                                     age < 50 ~ "40-49",
                                     age < 60 ~ "50-59", 
                                     age < 70 ~ "60-69",
                                     age < 80 ~ "70-79",
                                     age < 90 ~ "80-89"))
    fdata %>% count(age) 
   # age   n
   # 14-19 162
   # 20-29 986
   # 30-39 994
   # 40-49 824
   # 50-59 491
   # 60-69 205
   # 70-79  27
   # 80-89   1
   #  <NA>   1

# Gender - GENDER - respondent gender [Courtney]

# English attainment: [Katherine]
  # B03A - asks respondent whether she attended English/ESL classes or school in the U.S.
  # B07 - asks how well the respondent speaks English
  # B08 - asks how well the respondent reads English 
  # B20 - asks about language spoken to respondent as a child 
  # B21 - asks what languages the respondent speaks as an adult 
  # B24 - asks which language the respondent is most comfortable conversing in 

# Income - G01 - respondent’s total income in the past year in USD [Aminah]

# Health insurance: [Aryaa]
  # A21A - asks whether respondent has health insurance 
    fdata <- rename(fdata, health_in = A21A)
    #counting up health_in in the final merged data
    fdata %>% count(health_in)
  # A23A - indicates who pays for the respondent’s health insurance 

# Documented status: [Courtney]
  # LEGAPPL - indicates status of legal application 
  # MIGTYPE - indicates type of migrant 

# MIXEDFAM - indicates a respondent is not documented in the U.S. but has children who are U.S. citizens [Katherine]

# NQ10L - indicates a respondent’s main difficulty accessing health care in the U.S. centers on being “undocumented” and “not treated well” as a result [Aminah]

    
#Creating a new dataset with only the selected covariates (created above): 
    #Aryaa can do this on Wednesday either during or before our meeting
    
# [TIMELINE CHECK-IN ON WED. NOV 10, 2021]
