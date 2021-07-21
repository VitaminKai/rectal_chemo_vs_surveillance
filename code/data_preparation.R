# --------------
# Author: Kaiwen Wang
# Date: 2021-07-20
# Purpose: Prepares data 
# Requires:
# Output: 
# --------------

source(here::here('code','initiation.R'))
library(readxl)

#===============================================================#
#  ----
#===============================================================#


df <- data.table(read_excel(here('input','rectal_dataset_full.xlsx')))

# Rename columns

# Prognostic markers at baseline


setnames(df,
         old="Age at diagnosis",
         new='age_at_diagnosis')

setnames(df,
         old="T stage at baseline",
         new='T_stage_baseline')

setnames(df,
         old="N stage baseline",
         new='N_stage_baseline')


setnames(df,
         old="Disease at baseline grade( e.g G1 or G2)",
         new='histological_grade_baseline')


setnames(df,
         old="Extra mural venous invasion at baseline (EMVI) yes or no",
         new='EMVI')

setnames(df,
         old="Circumferential resection margin at baseline (CRM) ( in cm)",
         new='CRM')


setnames(df,
         old="Distance from anal verge at baseline (cm)",
         new='distance_anal_verge')

setnames(df,
         old="Neoadjuant chemo yes or no",
         new='neoadjuvant_chemo_status')

setnames(df,
         old="What neodjuvant chemo regimen?",
         new='neoadjuvant_chemo_regimen')

# prognosis markers post RT/surgery
setnames(df,
         old="ypT staging at Surgery(post-op)",
         new='ypT_stage')

setnames(df,
         old="ypN staging",
         new='ypN_stage')


setnames(df,
         old="ypM staging",
         new='ypM_stage')


setnames(df,
         old="TRG status on MRI , post-RT E.g. TRG2, TRG3",
         new='TRG_status')


setnames(df,
         old="R status at surgery(R0 or R1)",
         new='R_status')


setnames(df,
         old="Adjuvant chemo or surveillance",
         new='adjuvant_management')

setnames(df,
         old="If adjuvant, what chemo regimen and how many cycles( free text)",
         new='adjuvant_chemo_regimen')

setnames(df,
         old="Is patient still alive or deceased?(Y/N)",
         new='os_status')

setnames(df,
         old="Recurrence free survival (RFS)",
         new='rfs')


                              

col_to_include <- c('age_at_diagnosis','os','T_stage_baseline','N_stage_baseline'
                    'histological_grade_baseline')
  
df <- df