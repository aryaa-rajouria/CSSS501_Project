
# READ IN DATA  

library(readxl)
library(tidyverse)

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
    # MG1 In general, how would you describe your health? Would you say...
      # . -> missing *Decide whether to drop single NA for 2010*
      # 1 -> Excellent - 747
      # 2 -> Good - 2115
      # 3 -> Fair - 811
      # 4 -> Poor - 14
      # 7 -> Don't know - 3 
      fdata <- rename(fdata, srh = MG1) # Renaming the variable to reflect self-rated health (srh)
      fdata %>% count(srh) # Calling counts of each of the responses 

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
    
    
    
 # low control (binary) [Katherine]


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

# Above code effectively throws out observations where respondant 
# (5) didn't understand, (6) refused, or (7) didn't know

table(lowcont)
sum(is.na(lowcont))
# low control - 2909 0s (no), 536 1s (yes), 246 (na)
# missing (.) is coded as na

  # job strain (binary) [Aminah]

    # What exactly are we trying to capture with job strain, particularly as a binary outcome?
    # From what I've seen, job strain is typically operationalized using job demand and job control
    # We might be able to gauge job strain using excessive work as a proxy and collapsing it into a binary outcome
      # MJ2 In your current FW ..how often are you asked to do an excessive amount of work?
        # . -> Missing - 3   *Consider omitting as NAs*
        # 0 -> Never - 2868
        # 1 -> Sometimes - 722
        # 2 -> Very Often - 64
        # 3 -> Always - 23
        # 5 -> Does not understand - 3  *Consider omitting as NAs*
        # 7 -> Don't know - 8  *Consider omitting as NAs*
  library(dplyr)
  fdata <- rename(fdata, strain = MJ2) # Renaming the variable to reflect job strain 
  fdata %>% count(strain) # Calling counts of each of the responses 

  #fdata <- fdata %>% mutate(strain = case_when(   # Coercing strain into a binary, commented out for now because we can coerce it into a binary in our models as well 
  #strain == 0 ~ 0,
  #strain == 1 ~ 1,
  #strain == 2 ~ 1,
  #strain == 3 ~ 1)) 

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
  fdata <- fdata %>% mutate(
    gender = case_when(
      GENDER == 0 ~ "man",
      GENDER == 1 ~ "woman"))
    
  # check count
      # gender    n
      # man 3052
      # woman  639
    
    
# English attainment: [Katherine]

  # B03A - asks respondent whether she attended English/ESL classes or school in the U.S.

fdata <- rename(fdata, esl_class = B03ax)
table(fdata$esl_class)
sum(is.na(fdata$esl_class))
# attended esl classes - 3091 0s (no), 598 1s (yes), 2 (na)

  # B07 - asks how well the respondent speaks English

fdata <- rename(fdata, eng_speak = B07)
table(fdata$eng_speak)
sum(is.na(fdata$eng_speak))
# english speaking skill - 1223 1s (not at all), 1220 2s (a little), 355 3s (somewhat), 887 4s (well), 6 (na)

  # B08 - asks how well the respondent reads English 

fdata <- rename(fdata, eng_read = B08)
table(fdata$eng_read)
sum(is.na(fdata$eng_read))
#english reading skill - 1687 1s (not at all), 908 2s (a little), 241 3s (somewhat), 847 4s (well), 8 (na)

  # B20 - asks about language spoken to respondent as a child 

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
sum(is.na(fdata$ac_mixtex))
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
sum(is.na(fdata$aa_mixtex))
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

fdata <- rename(fdata, dom_lang = B24)
table(fdata$dom_lang)
sum(is.na(fdata$dom_lang))

# Income - G01 - respondent’s total income in the past year in USD [Aminah]

# Health insurance: [Aryaa]
  # A21A - asks whether respondent has health insurance 
    fdata <- rename(fdata, health_in = A21a)
    #counting up health_in in the final merged data
    fdata %>% count(health_in)
    
    
  # A23A1-A23A7 - indicates who pays for the respondent’s (farmworker) health insurance 
    #rename different types of health insurance payment info
    fdata <- rename(fdata, fwk_pays = A23a1, spouse_pays = A23a2, empl_pays = A23a3, semp_pays = A23a4,
                    gvt_pays = A23a5, other_pays = A23a6, parent_pays = A23a7)
    fdata %>% count(fwk_pays) #No: 901; Yes: 210; NA: 2571
    fdata %>% count (spouse_pays) #No, 1084; Yes: 36; NA: 2571
    fdata %>% count (empl_pays) #No: 573; Yes: 547, NA: 2571
    fdata %>% count (semp_pays) #No: 1017, Yes: 103, NA: 2571
    fdata %>% count (gvt_pays) #No: 820, Yes: 300; NA: 2571
    fdata %>% count (other_pays) #No: 1071; Yes: 49; NA: 2571
    fdata %>% count (parent_pays) #Yes: 18; NA: 3673
    #do we want to create a specific type of matrix with this data? What might be the best way to mutate this?
    
# Documented status: [Courtney]
  # LEGAPPL - indicates status of legal application 
  # MIGTYPE - indicates type of migrant 
    fdata <- fdata %>% mutate(
      doc_status = case_when(
        currstat ==1 ~ "Citizen",
        currstat ==2 ~ "Green card",
        currstat ==3 ~ "Other work authorization",
        currstat ==4 ~ "Unauthorized"))
    

# MIXEDFAM - indicates a respondent is not documented in the U.S. but has children who are U.S. citizens [Katherine]

fdata <- rename(fdata, mix_fam = MixedFam)
table(fdata$mix_fam)
sum(is.na(fdata$mix_fam))
# mixed fam - 3017 0s (no), 674 1s (yes), 0 missing

# NQ10L - indicates a respondent’s main difficulty accessing health care in the U.S. centers on being “undocumented” and “not treated well” as a result [Aminah]

  # NQ10L When you want to get health care in the U.S. what are the main difficulties you face?
  # I'm "undocumented' they don't treat me well

      # This column doesn't appear in our data. What should we do? 
    
#Creating a new dataset with only the selected covariates (created above)
    
# [TIMELINE CHECK-IN ON WED. NOV 10, 2021]
