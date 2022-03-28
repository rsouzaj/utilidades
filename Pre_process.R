
# Carregar pacotes uteis --------------------------------------------------
install.packages()
library()

# Carregar banco de dados -------------------------------------------------
read.csv()

# Estrutura dos dados -----------------------------------------------------

str()
dim()
head()


# Definir o desfecho ------------------------------------------------------

# Ex:
pbcclas<- pbc %>% filter(status==0 | status==2)
table(pbcclas$status) #vendo os valores do status

pbcclas$status<-factor(pbcclas$status, levels=c(2,0), labels =c("Obito","Vivo"))


# Transformação de variáveis ----------------------------------------------

# ex:
str(pbcclas)
pbcclas$age<-as.numeric(pbcclas$age)
pbcclas$bili<-as.numeric(pbcclas$bili)
pbcclas$chol<-as.numeric(pbcclas$chol)


# Analise exploratoria ----------------------------------------------------



summary(pbc)

#Boxplots
g1<-ggplot(pbc, aes(status, age)) +
  geom_boxplot()
plot(g1)

g2<-ggplot(pbc, aes(status, bili)) +
  geom_boxplot()
plot(g2)

g3<-ggplot(pbc, aes(status, chol)) +
  geom_boxplot()
plot(g3)

g4<-ggplot(pbc, aes(status, alk.phos)) +
  geom_boxplot()
plot(g4)

library(patchwork) #arrumando os gr�ficos

g1 + g2 + g3 + g4


#dispers�o
g5<-ggplot(pbc, aes(age, bili)) +
  geom_point()
plot(g5)

g6<-ggplot(pbc, aes(age, chol)) +
  geom_point()
plot(g6)

#arrumando os gr�ficos
g5+g6


# Dados faltantes ---------------------------------------------------------


# preeencher com a média, por exemplo.

pbc$chol[is.na(pbc$chol)]<-mean(pbc$chol,na.rm = TRUE)



# Outliers ----------------------------------------------------------------

#valores discrepantes - Outliers
boxage<-ggplot(pbc, aes(y=age)) +
  geom_boxplot()
plot(boxage)

#teste para detectar outlier
library(EnvStats)
test <- rosnerTest(pbc$age,
                   k = 3)
test$all.stats


# padronização de variáveis numéricas -------------------------------------

pbc$chol<- scale(pbc$chol, center = T)
pbc$bili<- scale(pbc$bili, center = T)



# Correlação entre variáveis numéricas ------------------------------------

library(corrplot)
pbcnum<- pbc %>% select("age","bili","chol","albumin","platelet","copper",
                        "alk.phos","ast")
k <- cor(pbcnum)
corrplot(k, method = "circle")

library(corrgram)
corrgram(pbcnum, lower.panel = panel.pts, upper.panel= panel.conf, diag.panel = panel.density)


# variáveis categoricas

chisq.test()

# variaveis numericas vs categoricas

t.test()













