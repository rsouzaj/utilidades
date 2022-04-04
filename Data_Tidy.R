library(tidyverse)


# Data import -------------------------------------------------------------

My_data <- readr::read_delim("data.csv", 
                             delim = "|")

My_data <- readr::read_csv("data.csv")


# Variables names ---------------------------------------------------------

names(Lead_redcap)


# Data frame structure, dimension, and head --------------------------------

str(Lead_redcap)
dim(Lead_redcap)
head(Lead_redcap, 11) # there is 9 levels of repeated instrument


# Which is the Outcome? ---------------------------------------------------

## Sleep disturbance

## Measures: ISI and PQSI



# Multiple data with same ID ----------------------------------------------

## Demography

cadastro_tb <- all_table %>% 
  filter(is.na(redcap_repeat_instance)) %>%
  mutate(idade = as.numeric(round(difftime(data_cadastro, nascimento,
                                           units = "days") / 365)),
         etnia = recode(etnia, "1" = "White" , "2" = "Brown", "3" = "Black",
                        "4" = "Other", "5" = "Other"),
         
         civil = recode(civil, "1" = "Married", "2" = "Married", "3" = "Single", "4" = "Divorced",
                        "5" = NULL),
         
         instrucao = recode(instrucao, "1" = "Low", "2" = "Low", # anos de estudo
                            "3" = "Intermediate", "4" = "High", "5" = "High",
                            "6" = "High"),
         
         rendimento = recode(rendimento, "1" = "up to 2", "2" = "2-5",
                             "3" = "5-10", "4" = ">10")
  ) %>% 
  select(record_id, idade, etnia:rendimento)


## Anamnese

anamnese_tb <- all_table %>% 
  filter(redcap_repeat_instrument == "anamnese_geral") %>% 
  mutate(
    impacto_endo = recode(impacto_endo, 
                          "1" = "Infertility",
                          "2" = "Pain",
                          "3" = "Pain and Infertility",
                          "4" = "Pain and Infertility",
                          "5" = "Pain and Infertility",
                          "6" = "Image finding",
                          "7" = "Image finding"),
    exercicio = recode(exercicio,
                       "0" = "Rarely",
                       "1" = "1 to 2 times/week",
                       "2" = "More than 3 times/week",
                       "3" = "More than 3 times/week"
    ),
    
    fuma = if_else(fuma_anos == 0, "No", "Yes"),
    depressao = if_else(depressao == 0, "No", "Yes"),
    sono = recode(sono,
                  "Good sleep",
                  "Regular sleep",
                  "Poor sleep"),
    neuromodulador = if_else(trat_dor___5 == 0, "No", "Yes"),
    antidepressivo = if_else(trat_dor___6 == 0, "No", "Yes")
    
  ) %>% 
  select(record_id, impacto_endo : depressao, exercicio, fuma, sono, neuromodulador, antidepressivo)


## Diagnostic/treatment


diag_tratamento <- all_table |> 
  filter(redcap_repeat_instrument == "diagnosticotratamento") |> 
  mutate(endo_comp_ant = if_else(diag___3 == 0, "No", "Yes")) |> 
  select(record_id, endo_comp_ant)

## Pain


dor_tb <- all_table %>% 
  filter(redcap_repeat_instrument == "dor" & redcap_repeat_instance == 1) %>% 
  mutate(intensidade_dor = case_when(dor2 <= 0 ~ "No pain",
                                     dor2 >= 1 & dor2 <= 4 ~ "Mild pain",
                                     dor2 > 4 & dor2 <= 7 ~ "Moderate pain",
                                     dor2 > 7 ~ "Severe pain"),
         
         dor0 = case_when(dor0 == 0 ~ "No",
                          dor0 == 1 ~ "Yes"),
         dor_cat = if_else(dor2 > 4, "Moderate/High", "No/mild")
  ) %>% 
  select(record_id, starts_with("dor"),intensidade_dor, -dor_meses)

# dor0 = menstruate (0= no,  1 = yes)
# dor1 = dysmenorrea
# dor2 = pelvic pain
# dor9 = back pain
# dor4 = shoulder or dorsal pain
# dor5 = sexual activity (0= no,  1 = yes)
# dor6 = dyspareunia
# dor3 = leg pain 
# dor7 = dyschezia
# dor8 = dysuria

## O´Leary questionnaire

oleary_tb <- all_table %>% 
  filter(redcap_repeat_instrument == "oleary") %>% 
  mutate(ole_escore = oleary_q1 + oleary_q2 + oleary_q3 + oleary_q4 +
           oleary_q5 + oleary_q6 + oleary_q7 + oleary_q8,
         ole_cat    = case_when(ole_escore < 12 ~ "No painful bladder", 
                                ole_escore >= 12 ~ "Painful bladder")
  ) %>% 
  select(record_id,  starts_with("ole"))

## Insomnia Severity Index questionnaire

isi_tb <- all_table %>% 
  filter(redcap_repeat_instrument == "isi_sono") %>% 
  mutate(isi_escore = isi_q1 + isi_q2 + isi_q3 + isi_q4
         + isi_q5 + isi_q6 + isi_q7,
         isi_cat = case_when(isi_escore > 14 ~ "Poor sleep",
                             isi_escore <= 14 ~ "Good sleep"),
         isi_cat_4 = case_when(isi_escore <= 7 ~ "No insomnia",
                               isi_escore >= 8 & isi_escore <=14 ~ "Subthreshold insomnia",
                               isi_escore >= 15 & isi_escore <= 21 ~ "Moderate insomnia",
                               isi_escore >= 22 ~ "Severe insomnia")
  ) %>%  
  select(record_id, isi_escore, isi_cat, isi_cat_4) 

## Pittsburgh questionnaire (Pittsburgh Sleep Quality Questionnarie)


pitt_tb <- all_table %>% 
  filter(redcap_repeat_instrument == "pittsburgh") %>% 
  select(record_id, starts_with("pitt"), -pittsburgh_complete)

### Score and variables in another script