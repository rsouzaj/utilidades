# União dos bancos --------------------------------------------------------

list(cadastro_tb,diag_tratamento, anamnese_tb, dor_tb, oleary_tb, pitt_tb, isi_tb) %>%
  purrr::reduce(left_join, by = "record_id") %>%
  filter(!is.na(ole_cat))-> banco

readr::write_csv(banco, "banco.csv")
