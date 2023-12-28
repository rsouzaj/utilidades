
# Missing values ----------------------------------------------------------
library(readxl)
banco <- read_excel("C:/Users/rsouz/OneDrive/Pesquisas/MESTRADO - Ricardo/banco.xlsx")

## VErificando quantidade de NA

is.na(banco)  ### missing value (NA) a parece como TRUE e c?lulas preenchidas como FALSE

sum(is.na(banco))  ### n?mero de dados NA

na.omit(banco)    ### retira todas as observa??es com NA (linhas)

mean(banco$idade, na.rm = TRUE)  


banco2 = banco



# Instal??o do pacote mice ------------------------------------------------


install.packages("mice")


library("mice")

banco$etnia = as.factor(banco$etnia)

summary(banco)



# Simple imputation -------------------------------------------------------
### trocando as variáveis NA pela média. 

banco$imc[which(is.na(banco$imc))] = mean(banco$imc, na.rm = TRUE)



banco$idade[which(is.na(banco$idade))] = mean(banco$idade, na.rm = TRUE) 

banco$idade<- round(banco$idade, digits = 0)  ### arredonda, caso queria n?meros inteiros.



# MICE imputation ---------------------------------------------------------

### Quando não sabemos o valor de uma varável (ex: 1-Sim, 2-Não), não ha´como fazer uma média ou imputar um valor.

## methods (mice) - para ver as possibilidades do "mice"


banco2 = select(banco2, idade:imc)

banco2$etnia = as.factor(banco2$etnia)

my_imp = mice(banco2, m=5, method = c("pmm", "polyreg", "pmm", "pmm", "pmm"), maxit =20) ###métods tem que ser na mesma
##quantidade de colunas do banco de dados
### pmm predictive mean matching
### loreg ( regressão logística) - usada somente para variáveis categóricas com duas respostas.
### polyreg ( regressão logística para mais de duas respotas)
### podemos usar um parâmetro para avaliar a média de distribuição. Vou usar o imc

summary(banco2$imc)  ### mean = 293.1

my_imp$imp$imc   ### série de 5 colunas com médias de imc. Escolher a coluna com valores mais próximos da média banco2$imc


banco_final<- complete(my_imp, 1)  ## vai completar randomicamente a coluna "etnia", mas respeitando uma distribuição
## parecida com imc.
