# --------------
# Author: Kaiwen Wang
# Date: 2021-07-24
# Purpose: Survival analysis 
# Requires:
# Output: 
# --------------


library(survival)
library(ggpubr)
library(flextable)

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



# convert to flextable

multi_cox_results_summary_tbl <- flextable(multi_cox_results_summary_tbl)

multi_cox_results_summary_tbl %<>%
  bold(i= ~p_value <0.05, j = ~ p_value) %>%
  hline(i = ~predictor =='n_communities',border=officer::fp_border(width=1.6))%>%
  set_header_labels(predictor='Predictor',
                    p_value = "p value", 
                    `exp(coef)` = "Hazard ratio",
                    cell_type = "Cell type",
                    `lower_95 CI` = "Lower 95% C.I.",
                    `upper_95 CI` = "Upper 95% C.I.") %>%
  colformat_double(digits=3) %>%
  autofit() 

save_as_docx(multi_cox_results_summary_tbl, path = here('output',"uni_cox summary.docx"))


