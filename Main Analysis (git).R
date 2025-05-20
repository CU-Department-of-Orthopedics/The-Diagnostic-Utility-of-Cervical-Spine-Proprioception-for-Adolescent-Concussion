## JPE Concussion/Control

library(readxl) 
library(psych)
library(tidyverse)
library(table1)
library(cutpointr)
library(pROC)
library(ggplot2)

dat <- ...

str(dat$Group_code)
str(dat$Group)

dat$Group_code <- as.factor(dat$Group_code)

dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
dat$conc_hx_yn <- factor(dat$conc_hx_yn, labels = c("No", "Yes"))
dat$Race <- factor(dat$Race, labels = c("Black or African-American", "White", "More than 1 race reported", "Unknown or not reported"))
dat$ethnicity <- factor(dat$ethnicity, labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown or not reported"))
dat$history_injury_yn <- factor(dat$history_injury_yn, labels = c("No", "Yes"))
dat$migraine_his <- factor(dat$migraine_his, labels = c("No", "Yes"))
dat$add_his <- factor(dat$add_his, labels = c("No", "Yes"))
dat$anx_his <- factor(dat$anx_his, labels = c("No", "Yes"))
dat$dep_his <- factor(dat$dep_his, labels = c("No", "Yes"))

## Table 1

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}

pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- fisher.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

label(dat$age_visit) <- "Age (years)"
label(dat$sex) <- "Sex"
label(dat$ethnicity) <- "Ethnicity"
label(dat$doi_to_visit1) <- "Days since concussion"
label(dat$pcsi_score_current_1) <- "Symptom Severity (Post-Concussion Symptom Inventory Score)"
label(dat$doi_to_clear) <- "Return to play time (days from concussion)"
label(dat$dhi_total_1) <- "Dizziness Handicap Inventory Score"
label(dat$conc_hx_yn) <- "Prior concussion history"
label(dat$migraine_his) <- "Migraine history"
label(dat$add_his) <- "Attention deficit disorder history"
label(dat$anx_his) <- "Anxiety history"
label(dat$dep_his) <- "Depression history"

table1(
  ~ age_visit + sex + Race + ethnicity + conc_hx_yn + pcsi_score_current_1 
  + migraine_his + add_his + dep_his + anx_his | Group,
  data = dat,
  render.continuous = render.cont,
  render.categorical = render.cat,
  extra.col = list(`P-value`= pvalue),
  overall = F,
  caption = "Table 1: Participant characteristics stratified by control and concussion groups."
)

dat_concussion <- dat[dat$Group_code == '1', ] 

mean(dat_concussion$doi_to_visit1,na.rm=TRUE) # days since injury
sd(dat_concussion$doi_to_visit1,na.rm=TRUE)

mean(dat_concussion$doi_to_clear,na.rm=TRUE) # RTP clearance time (days)
sd(dat_concussion$doi_to_clear,na.rm=TRUE)

# Logistic Regression Model

dat$conc_hx_yn <- factor(dat$conc_hx_yn, labels = c("0", "1"))

LRM <- glm(Group_code ~ JPE_deg_avg_1 + conc_hx_yn, data = dat, family = binomial(link = "logit")) 

summary(LRM) # regression parameter estimates
confint(LRM) # confidence intervals

exp(coef(LRM)) # Odds ratios

#Odds ratios with 95% CI
exp(summary(LRM)$coef["JPE_deg_avg_1",1]+           
      qnorm(c(0.025,0.5,0.975)) * summary(LRM)$coef["JPE_deg_avg_1",2])

exp(summary(LRM)$coef["conc_hx_yn",1]+           
      qnorm(c(0.025,0.5,0.975)) * summary(LRM)$coef["conc_hx_yn",2])

# AUC (AUC = 0.81)
auc <- roc(dat$Group_code, predict(LRM))$auc
cat("AUC = ", auc)

# ROC curve
predicted <- roc(dat$Group_code, predict(LRM))
plot(predicted, main = "ROC", legacy.axes = T) # legacy.axes = T plots '1-Specificity'

# Cutpoints

opt_cut <- cutpointr(
  data = dat,
  x = JPE_deg_avg_1, 
  class = Group_code, 
  pos_class = "1",
  neg_class = "0", 
  metric = youden,
  method = maximize_metric
  )


summary(opt_cut)
plot(opt_cut)
plot_metric(opt_cut)
