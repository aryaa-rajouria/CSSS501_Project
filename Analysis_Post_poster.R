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
library(dplyr)
library(here)

# when you load MASS and dplyr select is overwritten

fdata <- readRDS(here("./fdata.rds"))

options(scipen = 999)
# fdata <- read.csv('/Users/katherine/Desktop/CSSS510_group_project/CSSS510_Project/fdata10.csv')


# _____________________________________________________________________________________________

# WHAT FACTORS (ESPECIALLY DOC_STATUS) ARE ASSOCIATED WITH POOR MENTAL HEALTH?

# NOTES: 
# Taking out interaction terms, because would be more interpretable if we can calculate predicted probabilities
# In non-simcf models, not using numeric version variable because not sure if interperetation is same
# _____________________________________________________________________________________________
fdata <- fdata %>% rename(esl_class=edu_esl)

# EDS
# summed social assistance

m2a <- glm(eds ~ hh_social_assist + edu_esl + age + gender + doc_status
           + income + married.LT + race + edu_highest + eng_read + migrant_2, data=fdata, family=binomial)


summary(m2a)
# odds ratios
exp(coef(m2a))

# preparing for pp
pe.glm <- m2a$coefficients
vc.glm <- vcov(m2a) 
ll.glm <- logLik(m2a)

# disaggregated social assistance

m2aa <- glm(eds ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_dis_ins + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + doc_status + income + married.LT + race 
            + edu_highest + eng_read + migrant_2 + edu_esl, data=fdata, family=binomial)

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

# EXPECTED VALUES OF EDS FOR EACH DOCUMENTATION STATUS 

model2 <- (eds ~ hh_social_assist + doc_status_num + edu_highest_num + age_num 
           + gender_num + income_num + married.LT_num + race_num + eng_read_num 
           + migrant_2_num + edu_esl_num)
           
m2a_test <- glm(formula=model2, data=fdata, family=binomial)
summary(m2a_test)
pe.glm <- m2a_test$coefficients
vc.glm <- vcov(m2a_test) 
ll.glm <- logLik(m2a_test)

mdata <- extractdata(model2, fdata, na.rm=TRUE)

# checking that all variables look ok
used_columns <- c('eds', 'hh_social_assist', 'age_num', 'edu_esl_num', 'gender_num', 'doc_status_num',
                  'income_num', 'married.LT_num', 'race_num', 'edu_highest_num', 'migrant_2_num')
summary(fdata[,used_columns])

sims <- 1e4
simbetas <- mvrnorm(sims, pe.glm, vc.glm)

# Making a scenario for each type of doc status
xhyp <- cfMake(model2, mdata, nscen=4)

xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=1, scen=1) 
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=2, scen=2)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=3, scen=3)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=4, scen=4)

# run simulations
eds_results <- data.frame(simcf::logitsimev(xhyp, simbetas, ci=.95))

eds_results

# PREDICTED PROBABILITIES WITH SOCIAL ASSISTANCE AT 1 AND 0

hh_social_assist <- c(0, 1)
doc_status_num <- c(1:4)
simVars <- expand.grid(doc_status_num=doc_status_num, hh_social_assist=hh_social_assist)
nscen <- nrow(simVars)

diff_sa_ds <- cfMake(model2, mdata, nscen)
for (i in 1:nscen) {
  diff_sa_ds <- simcf::cfChange(diff_sa_ds, "doc_status_num", x=simVars$doc_status_num[i], scen=i)
  diff_sa_ds <- simcf::cfChange(diff_sa_ds, "hh_social_assist", x=simVars$hh_social_assist[i], scen=i)
}
simbetas[1:4,]
# Simulate expected probabilities for all scenarios
diff_sa_ds_sims <- simcf::logitsimev(diff_sa_ds, simbetas, ci=0.95)

pp_sa_ds_results <- cbind(simVars, data.frame(diff_sa_ds_sims))

pp_sa_ds_results

# FIGURE - PREDICTED PROB OF EDS BY DOC STATUS AND SOCIAL ASSISTANCE (NOT RIGHT YET)

pp_sa_ds_results %>%
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('doc_status') + ylab('Probability of EDS') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  facet_grid(. ~ factor(hh_social_assist, levels=c(0,1), labels=c('No social assist', "Yes social assist"))) +
  xlab('Documentation status') +
  NULL

# FIRST DIFFERENCES BETWEEN RECIEVING/NOT RECIEVING SOCIAL ASSIST FOR EACH DOC STATUS

diff_sa_ds_fd <- cfMake(model2, mdata, 4)
diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "doc_status_num", x=1, xpre=1, scen=1)
diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "hh_social_assist", xpre=0, x=1, scen=1)

diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "doc_status_num", x=2, xpre=2, scen=2)
diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "hh_social_assist", xpre=0, x=1, scen=2)

diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "doc_status_num", x=3, xpre=3, scen=3)
diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "hh_social_assist", xpre=0, x=1, scen=3)

diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "doc_status_num", x=4, xpre=4, scen=4)
diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "hh_social_assist", xpre=0, x=1, scen=4)

# # This last one is for all participants, regardless of doc status
# If we use this, we'll need to change ,4 to ,5 in the first line
# diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd, "hh_social_assist", xpre=0, x=1, scen=5)

diff_sa_ds_fd_sims <- simcf::logitsimfd(diff_sa_ds_fd, simbetas, ci=0.95)
diff_sa_ds_fd_sims
diff_sa_ds_fd_sims_df <- as.data.frame(diff_sa_ds_fd_sims)


# FIGURE - FIRST DIFFERENCES

diff_sa_ds_fd_sims_df %>%
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('doc_status') +
  ylab('First diff of EDS w/wo Social Assist') + xlab('Documentation status') +
  geom_hline(yintercept=0, linetype='dashed')

# _____________________________________________________________________________________________

# EPD

m2b <- glm(epd ~ hh_social_assist + age + gender + doc_status + income + married.LT + race 
           + edu_highest + eng_read + migrant_2 + edu_esl, data=fdata, family=binomial)

summary(m2b)
exp(coef(m2b))

#disaggregated social assistance

m2bb <- glm(epd ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + doc_status + income + married.LT + race 
            + edu_highest + eng_read + migrant_2 + edu_esl, data=fdata, family=binomial)

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

# EXPECTED VALUES OF EPD FOR EACH DOCUMENTATION STATUS 

model3 <- (epd ~ hh_social_assist + doc_status_num + edu_highest_num + age_num 
           + gender_num + income_num + married.LT_num + race_num + eng_read_num 
           + migrant_2_num + edu_esl_num)


m2b_test <- glm(formula=model3, data=fdata, family=binomial)
summary(m2b_test)
pe.glm <- m2b_test$coefficients
vc.glm <- vcov(m2b_test) 
ll.glm <- logLik(m2b_test)

mdata <- extractdata(model3, fdata, na.rm=TRUE)

# checking that all variables look ok
used_columns <- c('epd', 'hh_social_assist', 'age_num', 'edu_esl_num', 'gender_num', 'doc_status_num',
                  'income_num', 'married.LT_num', 'race_num', 'edu_highest_num', 'migrant_2_num')
summary(fdata[,used_columns])

sims <- 1e4
simbetas <- mvrnorm(sims, pe.glm, vc.glm)

# Making a scenario for each type of doc status
xhyp <- cfMake(model3, mdata, nscen=4)

xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=1, scen=1) 
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=2, scen=2)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=3, scen=3)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=4, scen=4)

# run simulations
epd_results <- data.frame(simcf::logitsimev(xhyp, simbetas, ci=.95))

epd_results

# PREDICTED PROBABILITIES WITH SOCIAL ASSISTANCE AT 1 AND 0

hh_social_assist <- c(0, 1)
doc_status_num <- c(1:4)
simVars <- expand.grid(doc_status_num=doc_status_num, hh_social_assist=hh_social_assist)
nscen <- nrow(simVars)

diff_sa_ds_epd <- cfMake(model3, mdata, nscen)
for (i in 1:nscen) {
  diff_sa_ds_epd <- simcf::cfChange(diff_sa_ds_epd, "doc_status_num", x=simVars$doc_status_num[i], scen=i)
  diff_sa_ds_epd <- simcf::cfChange(diff_sa_ds_epd, "hh_social_assist", x=simVars$hh_social_assist[i], scen=i)
}
simbetas[1:4,]
# Simulate expected probabilities for all scenarios
diff_sa_ds_epd_sims <- simcf::logitsimev(diff_sa_ds_epd, simbetas, ci=0.95)

pp_sa_ds_epd_results <- cbind(simVars, data.frame(diff_sa_ds_epd_sims))

pp_sa_ds_epd_results

# FIGURE - PREDICTED PROB OF EPD BY DOC STATUS AND SOCIAL ASSISTANCE 

pp_sa_ds_epd_results %>%
  # ggplot(aes(x=factor(era), y=pe, ymin=lower, ymax=upper, color=factor(winpct))) +
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('Documentation status') + ylab('Probability of EPD') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  facet_grid(. ~ factor(hh_social_assist, levels=c(0,1), labels=c('No social assist', "Yes social assist"))) +
  xlab('Documentation status') +
  NULL 

# FIRST DIFFERENCES BETWEEN RECIEVING/NOT RECIEVING SOCIAL ASSIST FOR EACH DOC STATUS

diff_sa_ds_fd_epd <- cfMake(model3, mdata, 4)
diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "doc_status_num", x=1, xpre=1, scen=1)
diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "hh_social_assist", xpre=0, x=1, scen=1)

diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "doc_status_num", x=2, xpre=2, scen=2)
diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "hh_social_assist", xpre=0, x=1, scen=2)

diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "doc_status_num", x=3, xpre=3, scen=3)
diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "hh_social_assist", xpre=0, x=1, scen=3)

diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "doc_status_num", x=4, xpre=4, scen=4)
diff_sa_ds_fd_epd <- simcf::cfChange(diff_sa_ds_fd_epd, "hh_social_assist", xpre=0, x=1, scen=4)

# # This last one is for all participants, regardless of doc status
# If we use this, we'll need to change ,4 to ,5 in the first line
# diff_sa_ds_fd <- simcf::cfChange(diff_sa_ds_fd_epd, "hh_social_assist", xpre=0, x=1, scen=5)

diff_sa_ds_fd_epd_sims <- simcf::logitsimfd(diff_sa_ds_fd_epd, simbetas, ci=0.95)
diff_sa_ds_fd_epd_sims_df <- as.data.frame(diff_sa_ds_fd_epd_sims)


# FIGURE - FIRST DIFFERENCES

diff_sa_ds_fd_epd_sims_df %>%
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('doc_status') +
  ylab('First diff of EPD with/without Social Assist') + xlab('Documentation status') +
  geom_hline(yintercept=0, linetype='dashed')

# _____________________________________________________________________________________________

# LOW CONTROL 

m2c <- glm(lowcont ~ hh_social_assist + age + gender + doc_status + income + married.LT + race 
           + edu_highest + eng_read + migrant_2 + edu_esl, data=fdata, family=binomial)

summary(m2c)
exp(coef(m2c))

# disaggregated social assistance

m2cc <- glm(lowcont ~ hh_medicaid + hh_wic + hh_unemp_ins + hh_food_stamps + hh_phealth_cl
            + hh_dis_rel + hh_TANF + hh_soc_sec + hh_dis_ins + hh_li_house + hh_other
            + hh_li_house + hh_gen_as + hh_vet_pay + age + doc_status
            + income + married.LT + race + edu_highest + eng_read 
            + migrant_2 + edu_esl, data=fdata, family=binomial)

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

# EXPECTED VALUES OF LOW CONTROL FOR EACH DOCUMENTATION STATUS 

model4 <- (lowcont ~ hh_social_assist + doc_status_num + edu_highest_num + age_num 
           + gender_num + income_num + married.LT_num + race_num + eng_read_num 
           + migrant_2_num + edu_esl_num)


m2c_test <- glm(formula=model4, data=fdata, family=binomial)
summary(m2c_test)
pe.glm <- m2c_test$coefficients
vc.glm <- vcov(m2c_test) 
ll.glm <- logLik(m2c_test)

mdata <- extractdata(model4, fdata, na.rm=TRUE)

# checking that all variables look ok
used_columns <- c('lowcont', 'hh_social_assist', 'age_num', 'edu_esl_num', 'gender_num', 'doc_status_num',
                  'income_num', 'married.LT_num', 'race_num', 'edu_highest_num', 'migrant_2_num')
summary(fdata[,used_columns])

sims <- 1e4
simbetas <- mvrnorm(sims, pe.glm, vc.glm)

# Making a scenario for each type of doc status
xhyp <- cfMake(model4, mdata, nscen=4)

xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=1, scen=1) 
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=2, scen=2)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=3, scen=3)
xhyp <- simcf::cfChange(xhyp, "doc_status_num", x=4, scen=4)

# run simulations
lowcont_results <- data.frame(simcf::logitsimev(xhyp, simbetas, ci=.95))

lowcont_results

# PREDICTED PROBABILITIES WITH SOCIAL ASSISTANCE AT 1 AND 0

hh_social_assist <- c(0, 1)
doc_status_num <- c(1:4)
simVars <- expand.grid(doc_status_num=doc_status_num, hh_social_assist=hh_social_assist)
nscen <- nrow(simVars)

diff_sa_ds_lowcont <- cfMake(model4, mdata, nscen)
for (i in 1:nscen) {
  diff_sa_ds_lowcont <- simcf::cfChange(diff_sa_ds_lowcont, "doc_status_num", x=simVars$doc_status_num[i], scen=i)
  diff_sa_ds_lowcont <- simcf::cfChange(diff_sa_ds, "hh_social_assist", x=simVars$hh_social_assist[i], scen=i)
}
simbetas[1:4,]
# Simulate expected probabilities for all scenarios
diff_sa_ds_lowcont_sims <- simcf::logitsimev(diff_sa_ds_lowcont, simbetas, ci=0.95)

pp_sa_ds_lowcont_results <- cbind(simVars, data.frame(diff_sa_ds_lowcont_sims))

pp_sa_ds_lowcont_results

# FIGURE - PREDICTED PROB OF EDS BY DOC STATUS AND SOCIAL ASSISTANCE (NOT RIGHT YET)

pp_sa_ds_lowcont_results %>%
  # ggplot(aes(x=factor(era), y=pe, ymin=lower, ymax=upper, color=factor(winpct))) +
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('doc_status') + ylab('Probability of Low control') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  facet_grid(. ~ factor(hh_social_assist, levels=c(0,1), labels=c('No social assist', "Yes social assist"))) +
  xlab('Documentation status') +
  NULL

# FIRST DIFFERENCES BETWEEN RECIEVING/NOT RECIEVING SOCIAL ASSIST FOR EACH DOC STATUS

diff_sa_ds_fd_lowcont <- cfMake(model4, mdata, 4)
diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "doc_status_num", x=1, xpre=1, scen=1)
diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "hh_social_assist", xpre=0, x=1, scen=1)

diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "doc_status_num", x=2, xpre=2, scen=2)
diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "hh_social_assist", xpre=0, x=1, scen=2)

diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "doc_status_num", x=3, xpre=3, scen=3)
diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "hh_social_assist", xpre=0, x=1, scen=3)

diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "doc_status_num", x=4, xpre=4, scen=4)
diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "hh_social_assist", xpre=0, x=1, scen=4)

# # This last one is for all participants, regardless of doc status
# If we use this, we'll need to change ,4 to ,5 in the first line
# diff_sa_ds_fd_lowcont <- simcf::cfChange(diff_sa_ds_fd_lowcont, "hh_social_assist", xpre=0, x=1, scen=5)

diff_sa_ds_fd_lowcont_sims <- simcf::logitsimfd(diff_sa_ds_fd_lowcont, simbetas, ci=0.95)
diff_sa_ds_fd_lowcont_sims
diff_sa_ds_fd_lowcont_sims_df <- as.data.frame(diff_sa_ds_fd_lowcont_sims)


# FIGURE - FIRST DIFFERENCES

diff_sa_ds_fd_lowcont_sims_df %>%
  ggplot(aes(x=factor(doc_status_num, levels=1:4,
                      label=c('Citizen', 'Green card', 'Other work auth', 'Unauthorized')),
             y=pe, ymin=lower, ymax=upper)) +
  geom_pointrange() + xlab('doc_status') +
  ylab('First diff of Low Control w/wo Social Assist') + xlab('Documentation status') +
  geom_hline(yintercept=0, linetype='dashed')


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




















