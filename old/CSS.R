
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

# If we want the raw merged data
# write.csv(fdata, 'fdata_raw')

# TIMELINE CHECK-IN (Nov. 8, 2021) [Aryaa & Aminah] Update the group



# CLEAN DATA ---------------------------------------------------


#  CLEAN DATA & CREATE OUTCOME VARIABLES (check for missing, NAs)
# (Nov. 12, 2021)

# SRH ------
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
  
      
      
# EDS -------   
  # elevated depressive symptoms (binary) [Aryaa]
    #are we making an index? Just did a binary synopsis for var MC10 in NIOS
    #Relates to MC10: In the past seven (7) days, have you felt depressed? 
      #. = missing; 0 = No; 1 = Yes 
    
    #fdata <- rename(fdata, eds = MC10) - CKA changed 12/3, not correct
      
    #counting up eds in the final merged data: note, NOT separated by years (2009/2010)
    #fdata %>% count(eds)
    #3 NA's, 2174 (No) 0s, and 514 (Yes) 1s
    #are we getting rid of NAs? 
    fdata$MC10
    # df$Rate2 = df$Rate # there's no df, not sure what this does
    
# df$Marks[df$Names == "Sita"] <- 25 # there's no df, not sure what this does
    
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
    
    # added 12/3 by Courtney ************************************
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
    
    
    ##************************************

    # fdata <- fdata %>% rowwise() %>%
    #   mutate(eds_check = sum(c_across(MCDays1:MCDays10)))
    # 
    # fdata %>% count(eds_check) 
     
# EPD ---------------
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
    
    
# LOW CONTROL ----------------    
 # low control (binary) [Katherine]
    
    
    # previously this was a tibble and had row_wise operations so
    # things get weird, instead let's bring it back to being a data.frame
    
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

# JOB STRAIN -----------------
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
  

# CREATE COVARIATES --------------------------
  
# AGE -----------------------------------------
  # Age - AGE - respondent age  [Aryaa]
  
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

    
# GENDER -------------   
  # Gender - GENDER - respondent gender [Courtney]
  fdata <- fdata %>% mutate(
    gender = case_when(
      GENDER == 0 ~ "man",
      GENDER == 1 ~ "woman"))
  var_label(fdata$gender) <- "Gender"
  
  fdata %>% count(gender)
    
  # check count
      # gender    n
      # man 3052
      # woman  639
    

# ENGLISH    
  # English attainment: [Katherine]

# B03A - asks respondent whether she attended English/ESL classes or school in the U.S.

  # fdata <- rename(fdata, esl_class = B03ax)
  # table(fdata$esl_class)
  # sum(is.na(fdata$esl_class))
  # fdata <- fdata %>% mutate(esl_class = factor(esl_class, labels = c("No", "Yes")))
  # var_label(fdata$esl_class) <- "ESL class"

  # attended esl classes - 3091 0s (no), 598 1s (yes), 2 (na)
  
# B07 - asks how well the respondent speaks English
  
  fdata <- rename(fdata, eng_speak = B07)
  table(fdata$eng_speak)
  sum(is.na(fdata$eng_speak))
  fdata <- fdata %>% mutate(eng_speak = factor(eng_speak, labels = c("Not at all", "A little", "Somewhat", "Well")))
  var_label(fdata$eng_speak) <- "Speak English"

  # english speaking skill - 1223 1s (not at all), 1220 2s (a little), 355 3s (somewhat), 887 4s (well), 6 (na)
  
# B08 - asks how well the respondent reads English 
  
  fdata <- rename(fdata, eng_read = B08)
  table(fdata$eng_read)
  sum(is.na(fdata$eng_read))
  fdata <- fdata %>% mutate(eng_read = factor(eng_read, labels = c("Not at all", "A little", "Somewhat", "Well")))
  var_label(fdata$eng_read) <- "Read English"

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
  fdata <- fdata %>% mutate(dom_lang = factor(dom_lang, labels = c("English", "Spanish", "Creole", "Mixtec", "Kanjobal", "Zapotec", "Other")))
  var_label(fdata$dom_lang) <- "Dominant language"
  
  

# HEALTH INSURANCE ----------------------
  # Health insurance: [Aryaa]
  # A21A - asks whether respondent has health insurance 
    # fdata <- rename(fdata, health_in = A21a)
  fdata <- fdata %>% mutate(health_in = case_when(
    A21a==0 ~ "No",
    A21a==1 ~ "Yes",
    A21a==95 ~ NA_character_))
    var_label(fdata$health_in) <- "Health Insurance"
  
    #counting up health_in in the final merged data
    fdata %>% count(health_in)
    
    # health_in    n
    #    No 2534
    #    Yes 1125
    #    <NA>   32
    # 
    
# DOCUMENTED STATUS -------------------
    
  # Documented status: [Courtney]
  # LEGAPPL - indicates status of legal application 
 
    fdata <- fdata %>% mutate(
      doc_status = case_when(
        currstat ==1 ~ "Citizen",
        currstat ==2 ~ "Green card",
        currstat ==3 ~ "Other work authorization",
        currstat ==4 ~ "Unauthorized"))
    var_label(fdata$doc_status) <- "Documented Status"
    

# MIGRANT TYPES -------------------
    
    # MIGTYPE - indicates type of migrant
    fdata <- fdata %>% mutate(
      migrant = case_when(
        MIGRANT ==0 ~ "Non-migrant",
        MIGRANT ==1 ~ "Migrant"))
    var_label(fdata$migrant) <- "Migrant Type"
    
      fdata <- fdata %>% mutate(
        migrant_1 = case_when(
          MIGTYPE ==0 ~ "Non-migrant",
          MIGTYPE ==1 ~ "FTC",
          MIGTYPE ==2 ~ "Shuttle"))
      var_label(fdata$migrant_1) <- "Migrant Type"
      
            
        fdata <- fdata %>% mutate(
          migrant_2 = case_when(
            MIGTYPE2 =="FTC" ~ "FTC",
            MIGTYPE2 =="NEWCOMER" ~ "Newcomer",
            MIGTYPE2 =="SETTLED" ~ "Settled",
            MIGTYPE2 =="SHUTTLE" ~ "Shuttle"))
        var_label(fdata$migrant_2) <- "Migrant Type"
        

# RACE & ETHNICITY ---------------------------------
        
        # ETHNICITY - 
        fdata <- fdata %>% mutate(
          ethnicity = case_when(
            B01 ==1 ~ "Mexican-American",
            B01 ==2 ~ "Mexican",
            B01 ==3 ~ "Mexican-American",
            B01 ==4 ~ "Other Hispanic",
            B01 ==5 ~ "Other Hispanic",
            B01 ==7 ~ "Not Hispianic or Latino"))  
        var_label(fdata$ethnicity) <- "Ethnicity"
        
          
          
          # RACE - 
          fdata <- fdata %>% mutate(
            race = case_when(
              B02 ==1 ~ "White",
              B02 ==2 ~ "Black AA",
              B02 ==4 ~ "Am Indian, Indigenous",
              B02 ==5 ~ "Other", # 5 Asian cases
              B02 ==6 ~ "Other", # 11 Native HI P.I cases
              B02 ==7 ~ "Other")) 
          var_label(fdata$race) <- "Race"
          

# EDUCATION  ---------------------------------
          
          # EDCUATION - Attended any HS or GED equivalent school in US
          fdata <- fdata %>% mutate(
            edu_hs = case_when(
              B03ex ==0 ~ "No",
              B03ex ==1 ~ "Yes"))
          var_label(fdata$edu_hs) <- "High school in US"
          
          # EDCUATION - Attended any college in US
          fdata <- fdata %>% mutate(
            edu_coll = case_when(
              B03fx ==0 ~ "No",
              B03fx ==1 ~ "Yes"))
          var_label(fdata$edu_coll) <- "College in US"
          

      
          # EDCUATION - Attended any English classes in US
          
          # B03ax doesn't exist because it was coded before as esl_class
          
          fdata <- fdata %>% mutate(
            edu_esl = case_when(
              B03ax ==0 ~ "No",
              B03ax ==1 ~ "Yes"))
          var_label(fdata$edu_esl) <- "ESL class"

          
          # EDCUATION - Attended any job in US
          fdata <- fdata %>% mutate(
            edu_job = case_when(
              B03dx ==0 ~ "No",
              B03dx ==1 ~ "Yes")) 
          var_label(fdata$edu_job) <- "Job training"
          
          
        # ANY EDUCATION IN US

      fdata <- fdata %>% mutate(edu_any_usa = "No")

      # edu_esl doesn't exist anymore because it was recoded earlier
      # instead, using esl_class now
      
      fdata <- fdata %>% mutate(
        edu_any_usa = case_when(
          edu_esl =="Yes" ~ "Yes",
          edu_hs =="Yes" ~ "Yes",
          edu_job =="Yes" ~ "Yes",
          edu_coll =="Yes" ~ "Yes",
          TRUE ~ edu_any_usa))    
      var_label(fdata$edu_any_usa) <- "Any education in US"
      

    # # Highest education # this overwrites the previous encoding, maybe delete one or the other?

          
          # highest grade finished
          
          fdata <- fdata %>% mutate(
            edu_highest = case_when(
              A09 <6 ~ 1,
              A09 >=6 & A09 <12 ~ 2,
              A09 >=12 & A09 <16 ~ 3,                  
              A09 >=16  ~ 4))
          var_label(fdata$edu_highest) <- "Highest education"
          fdata <- fdata %>% mutate(edu_highest = factor(edu_highest, labels = c("< Primary", "Primary", "High school", "College")))
          
          
                 
# INCOME --------------------------------

  # Income - G01 - respondent’s total income in the past year in USD [Aminah]

          
                    # INCOME - FAMILY'S TOTAL INCOME
          fdata <- fdata %>% mutate(
            income_family = case_when(
              G03 <=3 ~ 1,
              G03 >3 & G03 <=6  ~ 2,
              G03 >6 & G03 <=9  ~ 3,
              G03 >9 & G03 <=12  ~ 4,
              G03 >12 & G03 <97  ~ 5,
              G03==97 ~ NA_real_))
          
          fdata <- fdata %>% mutate(
            income_family = factor(income_family,
                                   labels = c("$0-2,499",
                                              "$2,500-9,999",
                                              "$9,999-17,499",
                                              "$17,499-29,999",
                                              "$30,000")))
          
          var_label(fdata$income_family) <- "Family income"
          
          
          # INCOME - personal total income
          fdata <- fdata %>% mutate(
            income = case_when(
              G01 <=3 ~ 1,
              G01 >3 & G01 <=6  ~ 2,
              G01 >6 & G01 <=9  ~ 3,
              G01 >9 & G01 <=12  ~ 4,
              G01 >12 & G01 <95  ~ 5,
              G01==95 ~ NA_real_))
          fdata <- fdata %>% mutate(
            income = factor(income,
                                   labels = c("$0-2,499",
                                              "$2,500-9,999",
                                              "$9,999-17,499",
                                              "$17,499-29,999",
                                              "$30,000")))
          
          var_label(fdata$income) <- "Income"
          
          
          
 # MIXED FAMILY -----------------
          
  # MIXEDFAM - indicates a respondent is not documented in the U.S. but has children who are U.S. citizens [Katherine]
  
  fdata <- rename(fdata, mix_fam = MixedFam)
  table(fdata$mix_fam)
  sum(is.na(fdata$mix_fam))
  fdata <- fdata %>% mutate(mix_fam = factor(mix_fam, labels = c("No", "Yes")))
  var_label(fdata$mix_fam) <- "Mixed family"
  # mixed fam - 3017 0s (no), 674 1s (yes), 0 missing

  # NQ10L - indicates a respondent’s main difficulty accessing health care in the U.S. centers on being “undocumented” and “not treated well” as a result [Aminah]
  
    # NQ10L When you want to get health care in the U.S. what are the main difficulties you face?
    # I'm "undocumented' they don't treat me well
  
        # This column doesn't appear in our data. What should we do? 
  
  # G04 - Asks if anyone in their household has recieved the following social services in the last 2 years

# SOCIAL BENEFITS ---------------------------
  
  
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
    
    # should each of these include "==1"? I added here (courtney)
    
  fdata <- fdata %>%
    mutate(hh_social_assist = ifelse(hh_TANF==1 | 
                                       hh_food_stamps==1 | 
                                       hh_dis_ins==1 | 
                                       hh_unemp_ins==1 |
                                       hh_soc_sec==1 | 
                                       hh_vet_pay==1 | 
                                       hh_gen_as==1 | 
                                       hh_li_house==1 | 
                                       hh_phealth_cl==1 |
                                       hh_medicaid ==1 | 
                                       hh_wic==1 | 
                                       hh_dis_rel==1 | 
                                       hh_leg_ser==1 | 
                                       hh_other==1, 1, 0))
  
  sum(is.na(fdata$hh_social_assist)) 
  table(fdata$hh_social_assist)
  fdata <- fdata %>% mutate(hh_social_assist = factor(hh_social_assist,
                                                      labels = c("No", "Yes")))
  # Social assistance - 1882 (0), 1807 (1), 2 (na)
  var_label(fdata$hh_social_assist) <- "Social assistance"
  
 
# MARRIED -------------------------------------
  
       # Married/living together
    
    fdata <- fdata %>% mutate(
      married.LT = case_when(
        A05 ==1 ~ "Single",
        A05 ==2 ~ "Married/LT",
        A05 ==3 ~ "Sep/Divorced"))
    
    sum(is.na(fdata$married.LT)) 
    table(fdata$married.LT)
    var_label(fdata$married.LT) <- "Married"
    # 2337 married, 231 sep/divorced, 1122 single, 1 na

# CHILDREN ---------------------------------------
    
  # Number of children in the household (under 17)
  
  fdata <- rename(fdata, children = A15B)
    var_label(fdata$children) <- "Children"
    
      
  #Creating a new dataset with only the selected covariates (created above)
      
  # [TIMELINE CHECK-IN ON WED. NOV 10, 2021]

# SAVE DATA ------------------------
    
saveRDS(fdata, "fdata.rds")

    