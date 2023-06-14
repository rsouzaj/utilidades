# Normalizar variáveis ----------------------------------------------------

## Log scale (Não funciona com números negativos ou caso tenha ZEROS)
## pode usar um truque: adicionar uma constante a todos os números, o menor núrero da escala +1)
##  exemplo: nesse caso, menor número é ZERO, então 0 + 1 = 1)

banco |> 
  mutate(
    log_pitt = log(pitt_escore + 1),
    log_isi = log(isi_escore + 1),
    log_ole = log(ole_escore + 1)
  ) -> banco

hist(banco$log_ole)

## Scale
## Scale returns a matrix; we have to inform that it's numeric

banco |> 
  filter(!is.na(pitt_escore & !is.na(ole_escore) & !is.na(isi_escore))) |> 
  mutate(norm_ole_escore = scale(ole_escore),      ## norm_ole_escore = as.numeric(scale(ole_escore))
         norm_isi_escore = scale(isi_escore),
         norm_pitt_escore = scale(pitt_escore)
  ) -> banco



summary(banco$norm_ole_escore)


## Min_max_scale

min_max_function <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

banco |> 
  # filter(!is.na(pitt_escore & !is.na(ole_escore) & !is.na(isi_escore)))
  mutate(min_max_ole = min_max_function(ole_escore),
         min_max_isi = min_max_function(isi_escore),
         min_max_pitt = min_max_function(pitt_escore)
  ) -> banco

summary(banco$min_max_isi)

plot(banco$min_max_ole, banco$dor2)


## Z-score 
### idêntico ao scale

z_score_fun <- function(x){
  (x - mean(x, na.rm = T))/ sd(x, na.rm = T) # exatamente = função scale
}

banco |> 
  filter(!is.na(pitt_escore & !is.na(ole_escore) & !is.na(isi_escore))) |> 
  mutate(
    z_score_isi = z_score_fun(isi_escore),
    z_score_pitt = z_score_fun(pitt_escore),
    z_score_ole = z_score_fun((ole_escore))
  ) -> banco

banco |> 
  select(starts_with("min"), starts_with("z_score"), starts_with("norm"),
         starts_with("log")) |> view()
