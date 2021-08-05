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


#===============================================================#
# Rename columns ----
#===============================================================#

# Prognostic markers at baseline

setnames(df,
         old="Age at diagnosis",
         new='age_at_diagnosis')

setnames(df,
         old="T stage at baseline",
         new='T_stage_baseline')

setnames(df,
         old="N stage at baseline",
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
         old="EAS Diagnosis date",
         new='date_of_diagnosis')

setnames(df,
         old="If deceased, date of death?",
         new='date_of_death')


setnames(df,
         old="Date of Surgery",
         new='date_of_surgery')

setnames(df,
         old="Recurrence Date",
         new='date_of_recurrence')
                              

col_to_include <- c('age_at_diagnosis','T_stage_baseline','N_stage_baseline',
                    'ypT_stage','ypN_stage','ypM_stage','TRG_status','R_status',
                    'EMVI','CRM','distance_anal_verge','adjuvant_management','adjuvant_chemo_regimen',
                    'histological_grade_baseline','os_status','date_last_follow_up',
                    'date_of_diagnosis','date_of_death','date_of_surgery','date_of_recurrence')
  
clinical_df <- df[,..col_to_include]

clinical_df[,`:=`(date_of_diagnosis=as.Date(date_of_diagnosis),
                  date_last_follow_up=as.Date(date_last_follow_up),
                  date_of_death=as.Date(as.numeric(date_of_death),origin="1900-01-01"),
                  date_of_surgery=as.Date(as.numeric(date_of_surgery),origin="1900-01-01"),
                  date_of_recurrence=as.Date(as.numeric(date_of_recurrence),origin="1900-01-01")
                  )]

clinical_df[is.na(date_last_follow_up),date_last_follow_up:=as.Date('2021-06-29')]


clinical_df[,os_status:=ifelse(os_status=='Alive',0,
                               ifelse(os_status=='Dead',1,NA))]

# format rfs status
clinical_df[,rfs_status := ifelse(is.na(date_of_recurrence)==TRUE, '0','1')]


# format os time into years
clinical_df[,os_time := ifelse(os_status=='0',(date_last_follow_up-date_of_diagnosis)/365,
                               ifelse(os_status=='1',(date_of_death-date_of_diagnosis)/365,NA))]

# format rfs time into years
clinical_df[,rfs_time := ifelse(rfs_status=='0',(date_last_follow_up-date_of_diagnosis)/365,
                               ifelse(rfs_status=='1',(date_of_recurrence-date_of_diagnosis)/365,NA))]

# format adjuvant management column
clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'.*surv.*|.*Surv.*','surveillence')]

clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'Declined.*','surveillence')]

clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'adj.*|Adj.*','adjuvant_chemo')]

clinical_df[adjuvant_management!='adjuvant_chemo' &adjuvant_management!='surveillence', adjuvant_management:=NA]

clinical_df[,table(adjuvant_management)]

# format prognostic factors EMVI
clinical_df[,table(EMVI)]

clinical_df[str_detect(EMVI,'yes|Yes|positive'),EMVI:='pos']

clinical_df[str_detect(EMVI,'No\\b$|no\\b$|neg'),EMVI:='neg']

clinical_df[EMVI!='pos' &EMVI!='neg', EMVI:=NA]

# format prognostic factors: CRM

clinical_df[,table(CRM)]

clinical_df[str_detect(CRM,regex('Involved.*|Positive|threaten.{1,2}$',ignore_case = T)),CRM:='pos']

clinical_df[str_detect(CRM,regex('clear.*|negative',ignore_case = T)),CRM:='neg']


# format prognostic factors: TRG status

clinical_df[,table(TRG_status)]

clinical_df[,TRG_status:=str_extract(TRG_status,regex('^TRG\\s?[0-9]$'))]
clinical_df[,TRG_status:=str_replace(TRG_status,' ','')]

# format prognostic factors: R status
clinical_df[,table(R_status)]
clinical_df[,R_status]
clinical_df[,R_status:=str_extract(R_status,'^R[0-9]')]

# format baseline histological grade

clinical_df[,table(histological_grade_baseline)]
clinical_df[,histological_grade_baseline:=str_extract(histological_grade_baseline,'G[0-9|x]')]

# format baseline grade
clinical_df[,table(ypT_stage)]
clinical_df[,ypT_stage:=str_replace(ypT_stage,regex('ypt',ignore_case = T),'ypT')]

clinical_df[,table(ypN_stage)]


clinical_df[,table(ypM_stage)]
library(tidyverse)

#### convert to stages #### 
clinical_df %>% count(T_stage_baseline, N_stage_baseline)

clinical_df %>% 
  mutate(N_stage_baseline_new = 
           str_extract(N_stage_baseline, "N\\d")
           ) %>% 
  count(T_stage_baseline, N_stage_baseline_new) %>% 
  filter(N_stage_baseline_new == "N0") # all T stage baseline > T2


clinical_df <- clinical_df %>% 
  mutate(N_stage_baseline_new = 
           str_extract(N_stage_baseline, "N\\d")
  ) %>% 
  mutate(
    cancer_staging_baseline = 
      case_when(
        N_stage_baseline_new %in% c("N2", "N1") ~ "stage_3",
        N_stage_baseline_new == "N0" ~
          "stage_2"
      )
  ) 

clinical_df <- clinical_df %>% 
  mutate(ypM_stage = "M0") %>% 
  mutate(
    ypN_stage = str_extract(ypN_stage, "N\\d"),
    ypT_stage = str_extract(ypT_stage, "T\\d")
  ) %>%  
  mutate(cancer_stage_yp = 
           case_when(
             ypN_stage %in% c("N2", "N1") ~ "stage_3",
             ypT_stage %in% c("T3", "T4") ~ "stage_2",
             ypT_stage %in% c("T1", "T2") ~ "stage_1",
             ypT_stage %in% c("T0") ~ "stage_0"
           )
  )

clinical_df <- clinical_df %>% 
  mutate(distance_anal_verge_unit_tmp = 
           str_extract(distance_anal_verge, "[a-z].*"),
         distance_anal_verge_final =
           case_when(
             distance_anal_verge_unit_tmp == "cm" ~ as.numeric(str_extract(distance_anal_verge, "\\d")),
             distance_anal_verge_unit_tmp == "mm" ~ as.numeric(str_extract(distance_anal_verge, "\\d"))/10
           )
         ) %>%
  select(-c(distance_anal_verge_unit_tmp, distance_anal_verge))
  
clinical_df <- clinical_df %>% 
  mutate(
    CRM = if_else(
      CRM %in% c("pos", "neg"), CRM, NA_character_
    )
  )

clinical_df_multi <- clinical_df %>% 
  select(-c(ends_with("stage_baseline"), ends_with("stage"), adjuvant_chemo_regimen)) %>% 
  relocate(os_time, os_status, adjuvant_management, .before = everything())
