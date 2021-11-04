
# READ IN DATA  

library(readxl)

#Adding Nios Data 
Nios_Data <- read_excel("C:/Users/aryaa/OneDrive/CSSS510/Project/NAWS_NIOSH_2009_2010 (1)/Naws_Data.xlsx")
Data1 <- Nios_Data 

#Adding NAWS Data questionnaire variables in alphabetical order from "A" through "E" covering the period 1989 through 2018 
NAWS_A2E191 <- read_excel("C:/Users/aryaa/OneDrive/CSSS510/Project/NAWS_A2E191.xlsx")
Data2 <-NAWS_A2E191

#questionnaire variables in alphabetical order from "F" through "Y" covering the period 1989 through 2018
NAWS_F2Y191 <- read_excel("C:/Users/aryaa/OneDrive/CSSS510/Project/NAWS_F2Y191.xlsx")
View(NAWS_F2Y191)
Data3 <- NAWS_F2Y191

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
  # elevated psychological demands (binary) [Courtney]
  # low control (binary) [Katherine]
  # job strain (binary) [Aminah]

# CREATE COVARIATES (comment about the number of missing)

# Age - AGE - respondent age  [Aryaa]

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
  # A23A - indicates who pays for the respondent’s health insurance 

# Documented status: [Courtney]
  # LEGAPPL - indicates status of legal application 
  # MIGTYPE - indicates type of migrant 

# MIXEDFAM - indicates a respondent is not documented in the U.S. but has children who are U.S. citizens [Katherine]

# NQ10L - indicates a respondent’s main difficulty accessing health care in the U.S. centers on being “undocumented” and “not treated well” as a result [Aminah]

# [TIMELINE CHECK-IN ON WED. NOV 10, 2021]
