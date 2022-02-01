#amostra simples1
amostra =sample(c(0,1), 150, replace = T, prob=c(0.5,0.5))
amostra
summary(as.factor(amostra))

amostra2 = sample(c('Vini', 'Mamis'), 150, replace = T, prob = c(0.5,0.5))
amostra2
summary(as.factor(amostra2))

amostra = sample(c(0,1), 150, replace = T, prob = c(0.7,0.3))
summary(as.factor(amostra))

amostra = sample(c(1:1000), 150, replace = F)
summary(as.factor(amostra))

amostra =sample(c(0,1), 150, replace = T, prob=c(0.7,0.3))
amostra
amotrairis = iris[amostra ==1,]
amotrairis
dim(amotrairis)

set.seed(2345)
sample(c(1000),1)
set.seed(2346)
sample(c(1000), 1)

#Amostragem sistemática

install.packages("TeachingSampling")
library(TeachingSampling)

amostra = S.SY(150,10)
amostra
dim(amostra)

amostrairis = iris[amostra,]
amostrairis

#amostragem Estratificada
install.packages('sampling')
library(sampling)

propoçao = 25

#metodos:
#Srswor = Amostra simples sem reposição
#srswr =  Amostra simples COM reposição
#poisson 
#systematc

amostrairis2 = strata(data = iris,
                      stratanames = c('Species'), size = c(rep(propoçao, 3)), method = 'srswor')
amostrairis2
summary(amostrairis2)
summary(iris$Species)

summary(infert)

amostrainfert = strata(data = infert,
                       stratanames = c('education'), size = c(5,48,47), method = 'srswor')
amostrainfert
summary(amostrainfert)

#medidas de centralidade e variabilidade

jogadores = c(40000, 18000, 12000, 250000, 30000, 140000, 300000, 40000, 800000)

#media
mean(jogadores)
#mediana
median(jogadores)
#Quartis
quartis = quantile(jogadores)
#3º Quartil
quartis[3]
#desvio padrão
sd(jogadores)
summary(jogadores)

#Teorema Central do limite
install.packages('semTools')
library(semTools)
z = rep(0,500)

for (i in 1:500){
  m = mvrnonnorm(1000, c(1,2), matrix(c(10,2,2,5), 2,2),
  skewness = c(5,2), kurtosis = c(3,3))
  z[i] = mean(m)
  if (i<4){
    hist(m,breaks = 50, main = paste0('Histograma', i))
  }
}
