
library(brant)
library(MASS)
library(simcf)
library(VGAM)
library(RColorBrewer)
library(verification)
library(tile)
library(ggplot2)
library(tidyr)
library(dplyr)
library(here)
library(gtsummary)
library(modelsummary)
library(labelled)
library(kableExtra)


# data --------
fdata <- readRDS(here("./fdata.rds"))
options(scipen = 999)
#fdata <- fdata %>% rename(esl_class=edu_esl)

var_label(fdata$edu_highest) <- "Highest education"


# EDS -------------

# reordered covariates to be organized

m2a <- glm(eds ~ doc_status + age + gender + race + 
            hh_social_assist + income + married.LT +
             edu_highest + eng_read +edu_esl +  migrant_2, 
           data=fdata, 
           family=binomial)


summary(m2a)
# odds ratios
#exp(coef(m2a))

# preparing for pp
pe.glm <- m2a$coefficients
vc.glm <- vcov(m2a) 
ll.glm <- logLik(m2a)




# EPD ----------

m2b <- glm(epd ~ doc_status + age + gender + race + 
             hh_social_assist + income + married.LT +
             edu_highest + eng_read +edu_esl +  migrant_2,
           data=fdata, 
           family=binomial)



# LOW CONTROL  --------

m2c <- glm(lowcont ~ doc_status + age + gender + race + 
             hh_social_assist + income + married.LT +
             edu_highest + eng_read +edu_esl +  migrant_2,
           data=fdata, 
           family=binomial)


# predicted estimates ---------

# code from HW3
# confint(m2a)



# Table of Model A
# model_list <- list("Model A" = m2a)
# msummary(model_list, statistic = 'p.value', )

list("tbl_regression-chr:tidy_columns" = c("estimate", "std.error", "p.value")) %>%
  set_gtsummary_theme()

m2a_table <- tbl_regression(m2a) %>% 
  bold_labels()  


# m2a abbreviated table 2
m2a_table_2 <- 
  tbl_regression(m2a, 
                 include = c("doc_status",
                             "age", "gender",
                             "race", 
                             "hh_social_assist",
                             "income")) %>% 
  bold_labels() 

# m2a abbreviated table 3
m2a_table_3 <- 
  tbl_regression(m2a,
                 include = c("married.LT",
                             "edu_highest",
                             "eng_read", 
                             "edu_esl",
                             "migrant_2")) %>% 
  bold_labels()  






# m2b full table
m2b_table <- tbl_regression(m2b) %>% 
  bold_labels()

# m2b abbreviated table 2
m2b_table_2 <- 
  tbl_regression(m2b, 
                 include = c("doc_status",
                             "age", "gender",
                             "race", 
                             "hh_social_assist",
                             "income")) %>% 
  bold_labels() 

# m2b abbreviated table 3
m2b_table_3 <- 
  tbl_regression(m2b,
                 include = c("married.LT",
                             "edu_highest",
                             "eng_read", 
                             "edu_esl",
                             "migrant_2")) %>% 
  bold_labels()  





# m2c full table
m2c_table <- tbl_regression(m2c) %>% 
  bold_labels()

# m2a abbreviated table 2
m2c_table_2 <- 
  tbl_regression(m2c, 
                 include = c("doc_status",
                             "age", "gender",
                             "race", 
                             "hh_social_assist",
                             "income")) %>% 
  bold_labels() 

# m2a abbreviated table 3
  m2c_table_3 <- 
    tbl_regression(m2c,
                   include = c("married.LT",
                               "edu_highest",
                               "eng_read", 
                               "edu_esl",
                               "migrant_2")) %>% 
    bold_labels()  



# save full
final_table_full <-
  tbl_merge(
    tbls = list(m2a_table, m2b_table, m2c_table),
    tab_spanner = c("**Elevated Depression Symptoms**", "**Elevated Psychological Distress**", "**Low Control**"))

final_table_full <- final_table %>%
  as_gt %>%
  gt::gtsave(filename="analysis_summary.png")

# final_table_full_kable <- final_table %>%
#   as_kable_extra() %>%
#   kable_styling(
#     bootstrap_options = c("striped", "condensed"), #popular bootstrap styles
#     font_size = 10,
#     full_width = FALSE) %>%
#   save_kable(file="analysis_summary2.png")
#     
    


# save part1
final_table_part1 <-
  tbl_merge(
    tbls = list(m2a_table_2, m2b_table_2, m2c_table_2),
    tab_spanner = c("**Elevated Depression Symptoms**", "**Elevated Psychological Distress**", "**Low Control**"))


final_table_part1 <- final_table_part1 %>%
  as_gt %>%
  gt::gtsave(filename="analysis_summary1.png")




# save part2
final_table_part2 <-
  tbl_merge(
    tbls = list(m2a_table_3, m2b_table_3, m2c_table_3),
    tab_spanner = c("**Elevated Depression Symptoms**", "**Elevated Psychological Distress**", "**Low Control**"))


final_table_part2 <- final_table_part2 %>%
  as_gt %>%
  gt::gtsave(filename="analysis_summary2.png")


           