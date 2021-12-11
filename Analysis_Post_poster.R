# ANALYSIS 

library(brant)
library(MASS)
library(simcf)
library(VGAM)
library(RColorBrewer)
library(verification)
library(tile)
library(ggplot2)
library(tidyr)
library(dplyr) # when you load MASS and dplyr select is overwritten

options(scipen = 999)
fdata <- read.csv('/Users/katherine/Desktop/CSSS510_group_project/CSSS510_Project/fdata9.csv')


# _____________________________________________________________________________________________

# WHAT FACTORS (ESPECIALLY DOC_STATUS) ARE ASSOCIATED WITH POOR MENTAL HEALTH? 

# _____________________________________________________________________________________________

# EDS

# summed social assistance

m2a <- glm(eds ~ hh_social_assist + esl_class + age + gender + doc_status + doc_status*hh_social_assist
           + doc_status*esl_class + income + married.LT + race 
           + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)


summary(m2a)
# odds ratios
exp(coef(m2a))

# disaggregated social assistance

m2aa <- glm(eds ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_dis_ins + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + gender*hh_wic + doc_status
            + doc_status*esl_class + doc_status*health_in + income + married.LT + race 
            + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)

summary(m2aa)
# odds ratios
exp(coef(m2aa))


# testing goodness of fit

pe.m1 <- m2a$par
ll.m1 <- logLik(m2a)

pe.m2 <- m2aa$par
ll.m2 <- logLik(m2aa)

k.m1 <- length(pe.m1)
k.m2 <- length(pe.m2)

aic.m1 <- 2*k.m1 - 2*ll.m1
aic.m2 <- 2*k.m2 - 2*ll.m2
aic.test <- aic.m2 - aic.m1
aic.test

# Because log Lik is more than 2 units less (less than -2), we know that model 2 is a better fit.
# Although, there are some types of social assistance with very few participants. 
# Therefore, worried that estimates would be biased. 

# _____________________________________________________________________________________________

# ATTEMPT AT EXPECTED VALUES WITH SIMCF ON EDS

# Currently not working, see 'xyhp' issue below

model <- (eds ~ hh_social_assist + esl_class_num + age_num + gender_num + doc_status_num + 
            doc_status_num*hh_social_assist + doc_status_num*esl_class + income_num + married.LT_num 
          + race_num + edu_highest_num)
m2a <- glm(formula=model, data=fdata, family=binomial)

pe.glm <- m2a$coefficients
vc.glm <- vcov(m2a) 
ll.glm <- logLik(m2a)

used_columns <- c('eds', 'hh_social_assist', 'esl_class_num', 'age_num', 'gender_num', 'doc_status_num',
                  'income_num', 'married.LT_num', 'race_num', 'edu_highest_num')
summary(fdata[,used_columns])
sims <- 1e4
simbetas <- mvrnorm(sims, pe.glm, vc.glm)

# yields warning message - filled with NAs
xhyp <- cfMake(model, fdata, nscen=4)

# Make all 4 scenarios for all citizenship categories
for (i in 1:5) { xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=i, scen=i) }

# run simulations
eds_results <- data.frame(simcf::logitsimev(xhyp, simbetas, ci=.95))
eds_results

# _____________________________________________________________________________________________

# EPD

m2b <- glm(epd ~ hh_social_assist + esl_class + age + gender + doc_status + doc_status*hh_social_assist
           + doc_status*esl_class + income + married.LT + race 
           + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)

summary(m2b)
exp(coef(m2b))

#disaggregated social assistance

m2bb <- glm(epd ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + gender*hh_wic + doc_status
            + doc_status*esl_class + doc_status*health_in + income + married.LT + race 
            + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)

summary(m2bb)
exp(coef(m2bb))

# Testing goodness of fit

pe.m1 <- m2b$par
ll.m1 <- logLik(m2b)

pe.m2 <- m2bb$par
ll.m2 <- logLik(m2bb)

k.m1 <- length(pe.m1)
k.m2 <- length(pe.m2)

aic.m1 <- 2*k.m1 - 2*ll.m1
aic.m2 <- 2*k.m2 - 2*ll.m2
aic.test <- aic.m2 - aic.m1
aic.test

# Again, dissagregated model is a better fit

# _____________________________________________________________________________________________

# LOW CONTROL 

m2c <- glm(lowcont ~ hh_social_assist + esl_class + age + gender + doc_status + doc_status*hh_social_assist
           + doc_status*esl_class + income + married.LT + race 
           + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)

summary(m2c)
exp(coef(m2c))

# disaggregated social assistance

m2cc <- glm(lowcont ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_dis_ins + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + gender*hh_wic + doc_status
            + doc_status*esl_class + doc_status*health_in + income + married.LT + race 
            + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)

summary(m2cc)
exp(coef(m2cc))

# goodness of fit test

pe.m1 <- m2c$par
ll.m1 <- logLik(m2c)

pe.m2 <- m2cc$par
ll.m2 <- logLik(m2cc)

k.m1 <- length(pe.m1)
k.m2 <- length(pe.m2)

aic.m1 <- 2*k.m1 - 2*ll.m1
aic.m2 <- 2*k.m2 - 2*ll.m2
aic.test <- aic.m2 - aic.m1
aic.test

# disaggregated is a better fit

# _____________________________________________________________________________________________

# FIGURES

# _____________________________________________________________________________________________

# ODDS RATIO FOR MENTAL HEALTH OF UNAUTHORIZED WORKERS

# Chris said that we should convert these to relative risks. Once, we figure out how to do that, 
# the following code can be modified to remake these tables with relative risk


# EDS
# computing CI
ci_m2a <- confint(m2a)
m2a_res <- round(exp(cbind(OR = (coef(m2a)), ci_m2a)),3)
m2a_res
or_eds <- m2a_res[12,]
or_eds

#EPD
# computing CI
ci_m2b <- confint(m2b)
m2b_res <- round(exp(cbind(OR = (coef(m2b)), ci_m2b)),3)
m2b_res
or_epd <- m2b_res[12,]
or_epd

# LOW CONTROL 
# computing CI
ci_m2c <- confint(m2c)
m2c_res <- round(exp(cbind(OR = (coef(m2c)), ci_m2c)),3)
m2c_res
or_lowcont <- m2c_res[12,]
or_lowcont

fig1 <- as.data.frame(rbind(or_eds, or_epd, or_lowcont))
fig1$model <- c('elev. depress. symp.', 'elev. pysch. demands', 'low control')
colnames(fig1) <- c('OR', 'LOWER', 'UPPER', 'model')
ggplot(fig1,
       aes(x=model, y=OR, ymin=LOWER, ymax=UPPER,
           color=model)) + 
  geom_pointrange() +
  xlab("Pyscho-social factors") +
  ylab('Odds ratio (95% CI)') +
  guides(color='none') +
  geom_hline(yintercept = 1, linetype='dashed') +
  ggtitle('Poor mental health for unauthorized')

# _____________________________________________________________________________________________

# COUNT OF FWs WHOSE HOUSEHOLD RECIEVES EACH TYPE OF SOCIAL ASSISTANCE (REGARDLESS OF DOC STATUS)

# bar plot
cols_to_include <- c('hh_dis_ins', 'hh_dis_rel', 'hh_food_stamps', 'hh_gen_as', 'hh_soc_sec', 
                     'hh_vet_pay', 'hh_li_house', 'hh_other', 'hh_phealth_cl','hh_unemp_ins',
                     'hh_medicaid','hh_TANF', 'hh_wic')
fdata %>% mutate(obs_idx=row_number()) %>%
  dplyr::select(obs_idx, all_of(cols_to_include)) %>% 
  pivot_longer(cols = all_of(cols_to_include)) %>%filter(!is.na(value)) %>%
  group_by(name) %>% summarize(n=sum(value), .groups='drop') %>% arrange(-n) %>% 
  ggplot(aes(x=factor(name, levels=unique(name)), y=n)) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.title.x = element_blank()) +
  ylab("Counts")


# _____________________________________________________________________________________________

# EXPLORTORY ANALYSES

# _____________________________________________________________________________________________


# PROPORTION OF UNAUTHORIZED WORKERS WHOSE HOUSEHOLDS RECIEVE VARIOUS TYPES OF SOCIAL ASSISTANCE

# medicaid
prop.1 <- with(fdata, table(doc_status, hh_medicaid)) %>%
  prop.table(margin = 1)
prop.1

# wic
prop.2 <- with(fdata, table(doc_status, hh_wic)) %>%
  prop.table(margin = 1)
prop.2

# unemployment insurance
prop.3 <- with(fdata, table(doc_status, hh_unemp_ins)) %>%
  prop.table(margin = 1)
prop.3

# food stamps
prop.4 <- with(fdata, table(doc_status, hh_food_stamps)) %>%
  prop.table(margin = 1)
prop.4

# public health clinic
prop.5 <- with(fdata, table(doc_status, hh_phealth_cl)) %>%
  prop.table(margin = 1)
prop.5

# disaster relief
prop.6 <- with(fdata, table(doc_status, hh_dis_rel)) %>%
  prop.table(margin = 1)
prop.6

# TANF
prop.7 <- with(fdata, table(doc_status, hh_TANF)) %>%
  prop.table(margin = 1)
prop.7

# social security
prop.8 <- with(fdata, table(doc_status, hh_soc_sec)) %>%
  prop.table(margin = 1)
prop.8

# any social assistance
prop.9 <- with(fdata, table(doc_status, hh_social_assist)) %>%
  prop.table(margin = 1)
prop.9




















