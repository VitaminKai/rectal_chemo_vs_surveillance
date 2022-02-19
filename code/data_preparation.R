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
         old="R number",
         new='patient_id')

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


setnames(df,
         old="Time Delay between RT and surgery (days)",
         new='time_interval_RT_surgery')


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
         old="yT staging post RT",
         new='ypT_RT')


setnames(df,
         old="yN stage post RT",
         new='ypN_RT')


setnames(df,
         old="yM stage post RT",
         new='ypM_RT')

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


col_to_include <- c('patient_id','age_at_diagnosis','T_stage_baseline','N_stage_baseline',
                    'ypT_stage','ypN_stage','ypM_stage','ypT_RT','ypN_RT','ypM_RT',
                    'TRG_status','R_status','EMVI','CRM','distance_anal_verge',
                    'adjuvant_management','adjuvant_chemo_regimen',
                    'histological_grade_baseline','time_interval_RT_surgery',
                    'os_status','date_last_follow_up',
                    'date_of_diagnosis','date_of_death','date_of_surgery','date_of_recurrence')

clinical_df <- df[!is.na(patient_id),..col_to_include]



clinical_df[,`:=`(date_of_diagnosis=as.Date(date_of_diagnosis),
                  date_last_follow_up=as.Date(date_last_follow_up),
                  date_of_death=as.Date(as.numeric(date_of_death),origin="1900-01-01"),
                  date_of_surgery=as.Date(as.numeric(date_of_surgery),origin="1900-01-01"),
                  date_of_recurrence=as.Date(as.numeric(date_of_recurrence),origin="1900-01-01")
)]




###=============================================###
# Add date of last follow up
###=============================================###

# Add default date of last follow up for all patients
clinical_df[is.na(date_last_follow_up),date_last_follow_up:=as.Date('2021-06-29')]

# For patients with unknown dead/alive status on the excel sheet, search through 
# record and put last follow up date as the last date where their record was updated
clinical_df[patient_id=='R654092',date_last_follow_up:=as.Date('2021-03-01')]
clinical_df[patient_id=='R654144',date_last_follow_up:=as.Date('2020-07-12')]
clinical_df[patient_id=='R655268',date_last_follow_up:=as.Date('2019-03-25')]
clinical_df[patient_id=='R656852',date_last_follow_up:=as.Date('2020-11-18')]
clinical_df[patient_id=='R649248',date_last_follow_up:=as.Date('2016-01-19')]
clinical_df[patient_id=='R648961',date_last_follow_up:=as.Date('2016-12-03')]
clinical_df[patient_id=='R644471',date_last_follow_up:=as.Date('2017-01-07')]
clinical_df[patient_id=='R661768',date_last_follow_up:=as.Date('2016-09-29')]
clinical_df[patient_id=='R675352',date_last_follow_up:=as.Date('2018-01-31')]


###=============================================###
# Extract survival status
###=============================================###

clinical_df[,table(date_of_surgery)]
clinical_df[patient_id=='R654092',os_status]

# Classify os_status as 1 if dead and 0 if otherwise including both alive and NA (i.e. lost to follow-up)
clinical_df[,os_status:=ifelse(os_status=='Dead',1,0)]
clinical_df[is.na(os_status),os_status:=0]


# format rfs status, there is only date info in this column, therefore, recurred if column is not empty
clinical_df[,rfs_status := ifelse(is.na(date_of_recurrence)==TRUE, 0,1)]


###=============================================###
# Calculate survival time
###=============================================###

# Choose the start date of survival
survival_start_date = 'date_of_surgery'

# calculate os time and format os time into years
clinical_df[,os_time := ifelse(os_status=='0',(date_last_follow_up-get(survival_start_date))/365,
                               ifelse(os_status=='1',(date_of_death-get(survival_start_date))/365,NA))]

# calculate rfs time and format rfs time into years
clinical_df[,rfs_time := ifelse(rfs_status=='0',os_time,
                                ifelse(rfs_status=='1',(date_of_recurrence-get(survival_start_date))/365,NA))]

# format adjuvant management column
clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'.*surv.*|.*Surv.*','surveillence')]

clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'Declined.*','surveillence')]

clinical_df[,adjuvant_management:=str_replace(adjuvant_management,'adj.*|Adj.*','adjuvant_chemo')]

clinical_df[adjuvant_management!='adjuvant_chemo' &adjuvant_management!='surveillence', adjuvant_management:=NA]

clinical_df[,table(adjuvant_management)]

# Format time interval between RT and surgery
clinical_df[,time_interval_RT_surgery:= as.numeric(time_interval_RT_surgery)]

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

clinical_df[,TRG_status:= fct_relevel(TRG_status,'TRG3')] 


# format prognostic factors: R status
clinical_df[,table(R_status)]
clinical_df[,R_status]
clinical_df[,R_status:=str_extract(R_status,'^R[0-9]')]

# format baseline histological grade

clinical_df[,table(histological_grade_baseline)]
clinical_df[,histological_grade_baseline:=str_extract(histological_grade_baseline,'G[0-9]')]

clinical_df[,histological_grade_baseline:=factor(histological_grade_baseline)]

clinical_df[,histological_grade_baseline:= fct_relevel(histological_grade_baseline,'G3')]

# format baseline grade
clinical_df[,table(ypT_stage)]
clinical_df[,ypT_stage:=str_replace(ypT_stage,regex('ypt',ignore_case = T),'ypT')]

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
    cancer_stage_baseline = 
      case_when(
        N_stage_baseline_new %in% c("N2", "N1") ~ "stage_3",
        N_stage_baseline_new == "N0" ~
          "stage_2"
      )
  ) 

# Format post RT staging
clinical_df[,ypT_RT:=str_extract(ypT_RT,'T\\d')]
clinical_df[,ypN_RT:=str_extract(ypN_RT,'N\\d')]
clinical_df[,ypM_RT:=str_extract(ypM_RT,'M\\d')]

clinical_df[,cancer_stage_yp_RT := case_when(ypM_RT == 'M1' ~ 'stage_4',
                                             ypN_RT %in% c("N2", "N1") ~ "stage_3",
                                             ypT_RT %in% c("T3", "T4") ~ "stage_2",
                                             ypT_RT %in% c("T1", "T2") ~ "stage_1",
                                             ypT_RT %in% c("T0") ~ "stage_0")]

clinical_df[,cancer_stage_yp_RT:= fct_relevel(cancer_stage_yp_RT,'stage_3')] 

# Format post surgery staging
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

clinical_df[,cancer_stage_yp:= fct_relevel(cancer_stage_yp,'stage_3')] 

# Format distance from anal verge
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

clinical_df[!(is.na(distance_anal_verge_final)),
            distance_anal_verge_category:=ifelse(distance_anal_verge_final<5,'<5cm',ifelse(
              distance_anal_verge_final>=5 &distance_anal_verge_final<=10,'5-10cm',ifelse(
                distance_anal_verge_final>10 &distance_anal_verge_final<=15,'10-15cm','>15cm')))]


# format CRM
clinical_df <- clinical_df %>% 
  mutate(
    CRM = if_else(
      CRM %in% c("pos", "neg"), CRM, NA_character_
    )
  )

clinical_df[,CRM:= fct_relevel(CRM,'pos')] 

#=============================Exclude patient groups with small numbers ===============================#

# Exclude R2, with only 2 patients out of 181
clinical_df <- clinical_df[!(patient_id %in% c('R681653','R662302-'))]

# Exclude histological graade 1, with only 1 patients out of 181
clinical_df <- clinical_df[patient_id != 'R614383']

# Exclude cancer stage grade 0 and 4 after RT, with only 1 patients (stage 4) and 4 (stage 0) out of 181
clinical_df <- clinical_df[!(cancer_stage_yp_RT %in% c('stage_0','stage_4'))]
clinical_df <- clinical_df[,cancer_stage_yp_RT:= fct_drop(cancer_stage_yp_RT)]

# Drop unused factor levels for histological grade and R status
clinical_df[,histological_grade_baseline:=fct_drop(histological_grade_baseline)]
clinical_df[,R_status:=fct_drop(R_status)]

#============================================================#
multi_var <- c('patient_id','os_time','os_status','rfs_time','rfs_status','age_at_diagnosis',
               'CRM',"R_status",'TRG_status','distance_anal_verge_final',
               'distance_anal_verge_category','EMVI','adjuvant_management',
               'time_interval_RT_surgery','histological_grade_baseline',
               'cancer_stage_yp','cancer_stage_yp_RT')

clinical_multi_df <- clinical_df[,..multi_var]
# # 
# library(VIM)
# library(mice)
# mice_plot <- aggr(clinical_df_multi, col=c('navyblue','yellow'),
#                     numbers=TRUE, sortVars=TRUE,
#                     labels=names(clinical_df_multi), cex.axis=.7,
#                     gap=3, ylab=c("Missing data","Pattern"))
# # 
# # 


