# --------------
# Author: Kaiwen Wang
# Date: 2021-07-24
# Purpose: Survival analysis 
# Requires:
# Output: 
# --------------


library(survival)
library(ggpubr)

#===============================================================#
# Univariate cox regression ----
#===============================================================#
# log rank test
survdiff(formula =Surv(os_time,os_status)~adjuvant_management,data=clinical_df)

# univariate cox regression 
uni_cox <- coxph(formula= Surv(os_time,os_status)~adjuvant_management,data=clinical_df)


surv_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(time=os_time,event=os_status) ~ adjuvant_management, data = clinical_df), 
  xlab = "Years", 
  ylab = "Overall survival probability",
  # risk.table = T,
  pval=T,
  pval.method=T,
  # cumevents = T,
  break.time.by = 1,
  surv.median.line = "v",  # add the median survival pointer.
  legend.labs = c("Adjuvant chemotherapy", "Surveillence"),
  pval.coord=c(0,0.55),
  pval.method.coord=c(0,0.6),
  palette = 'Dark2',
  ylim = c(0.5, 1)
)


#===============================================================#
# Multivariate cox regression ----
#===============================================================#

multi_cox <- coxph(formula= Surv(os_time,os_status)~adjuvant_management+,data=clinical_df)
