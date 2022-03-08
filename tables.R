
# NA-table ----------------------------------------------------------------

banco  %>%
  summarise(N = n()) %>%
  tibble(Variáveis = colnames(banco),
         Missing =  colSums(is.na(banco)),
         "Porcentagem(%)" = (colSums(is.na(banco))*100/N)
  ) %>%
  select(Variáveis:"Porcentagem(%)") %>%
  xtable::xtable() %>%
  flextable::as_flextable()


# demographics-table ------------------------------------------------------




banco %>%
  select(idade, imc, etnia, civil, instrucao, rendimento, exercicio, fuma,
         dor0,  depressao,
         neuromodulador, antidepressivo,
         impacto_endo, endo_comp_ant, isi_cat, pitt_cat, intensidade_dor, dor_meses, ole_cat) %>%
  gtsummary::tbl_summary(
    missing = "no",
    label = c(
      dor0 ~ "Menstruation",
      idade ~ "Age (years)",
      imc ~ "BMI",
      etnia ~ "Ethnicity",
      civil ~ "Marital status",
      instrucao ~ "Education level",
      rendimento ~ "House incoming",
      endo_comp_ant ~ "DEAC", #Deep Endometriosis in Anterior Compartment
      impacto_endo ~ "Main complaint",
      dor_meses ~ "Pain duration (months)",
      depressao ~ "Depression",
      exercicio ~ "Exercise",
      fuma ~ "Smoking",
      neuromodulador ~ "Neuromodulator",
      antidepressivo ~ "Antidepressant",
      pitt_cat ~ "PSQI",
      isi_cat ~ "ISI",
      intensidade_dor ~ "Pain intensity",
      ole_cat ~ "PBS"
    )
  ) %>%
  gtsummary::bold_labels() %>%
  gtsummary::as_flex_table() %>%
  # flextable::set_caption(caption = "Table 1 - demographic distribution") %>%
  flextable::bold(part = "header") %>%
  flextable::add_footer_lines(
    values =  "BMI: body mass index; smoking = more than 1 cigarette/day; main complaint = reason for reference to specialized service; pain duration = length of time feeling pain; menstrual cycles = present")



# table - cross -----------------------------------------------------------

banco %>%
  select(isi_cat, pitt_cat) %>%
  gtsummary::tbl_cross(percent = "cell",
                       missing = "no",
                       label = c(
                         isi_cat ~ "ISI",
                         pitt_cat ~ "PSQI"
                       )
  ) %>%
  gtsummary::modify_header(
    update = list(label ~ "**Sleep quality**")
  ) %>%
  gtsummary::as_flex_table() %>%
  # flextable::set_caption(caption = "Table 2 - ISI vs PSQI") %>%
  flextable::bold(i = 1, part = "header") %>%
  flextable::bold(i = 2, j= c(1,4), part = "header") %>%
  flextable::add_footer_lines(
    values =  "ISI = Insomnia Severity Index questionaire; PSQI = Pittsburgh Sleep Quality Index")



# p-value and effect-size -------------------------------------------------

## p_valor
ole_comp7 <- rstatix::kruskal_test(data = banco, comp7 ~ ole_cat)

## effectsize

eff_ole_comp7 <- rstatix::kruskal_effsize(data = banco, comp7 ~ ole_cat)

print(ole_comp7)

print(eff_ole_comp7)

## Tabela estatística O'Leary vs componentes

list(ole_comp1, ole_comp2, ole_comp3, ole_comp4,
     ole_comp5, ole_comp6, ole_comp7) %>%
  purrr::reduce(full_join) -> table_ole_comp

## statistics table

table_stat_ole_comp <- table_ole_comp %>%
  left_join(table_eff_ole_comp, by= ".y.") %>%
  select(.y., statistic, df, p, effsize, magnitude)
#
#  table_stat_ole_comp %>%
#   xtable::xtable() %>%
#   flextable::as_flextable()#%>%
#   #  flextable::set_header_labels(
#    .y. = "Componentes do sono"
#  )

flextable::flextable(
  table_stat_ole_comp
) %>%
  flextable::set_header_labels(
    .y. = "Componentes do sono"
  ) %>%
  flextable::add_header(statistic= "Sintomas urinários", df= "Sintomas urinários", p= "Sintomas urinários", effsize= "Sintomas urinários", magnitude = "Sintomas urinários") %>%
  flextable::merge_h(part = "header") %>%
  flextable::align( align = "center", part = "header") %>%
  flextable::colformat_double(digits = 4)
