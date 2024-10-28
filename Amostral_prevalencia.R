
# Prevalência -------------------------------------------------------------


# Parâmetros
prevalencia_estimada <- 0.35 # Estimativa inicial da prevalência
nivel_confianca <- 0.80
z_score <- qnorm(1 - (1 - nivel_confianca) / 2)
margem_erro <- 0.05

# Cálculo do tamanho da amostra
tamanho_amostra <- (z_score^2 * prevalencia_estimada * (1 - prevalencia_estimada)) / margem_erro^2

# Arredondando para cima
tamanho_amostra <- ceiling(tamanho_amostra)

# Resultado
tamanho_amostra
