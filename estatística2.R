#disttribuicão T de Student
pt(1.5,8)
pt(1.5,8, lower.tail = F)
pt(1.5,8) + pt(1.5,8, lower.tail = F)
#distribuiçao Binomial
dbinom(x=3,size = 5,prob = 0.5)
pbinom(q = 4, size = 4, prob = 0.25)
dbinom(x =7, size = 12, prob = 0.25)
pbinom(q = 2, size = 4, prob = 0.25)
#forma 1
dbinom(x = 3, size = 4, prob = 0.25) + dbinom(x = 4, size = 4, prob = 0.25)
#forma2
1 - pbinom(q = 2, size = 4, prob = 0.25)
#distribuiçao poisson

dpois(3,lambda = 2)

#acumulado (0,1,2,3,4,5)
ppois(3,2)
#metodo1
ppois(2,2)
#metodo2
dpois(0,2) + dpois(1,2) + dpois(2,2)
#Mais que 3
ppois(3,2,lower.tail = F)

#Quiquadrado

novela = matrix(c(19,43,6,32),nrow = 2)
row.names(novela) = c('Masculino', 'Feminino')
colnames(novela) = c('Assiste', 'Não assiste')
novela
chisq.test(novela)
novela2 <- matrix(c(22,3,43,32), nrow = 2, byrow = T)
row.names(novela2) = c('Masculino', 'Feminino')
colnames(novela2) = c('assiste', 'nao assiste')
novela2
chisq.test(novela2)

#anova
library(readxl)
tratamento = read_excel('C:/Users/Intel/Desktop/Formação Cientista De Dados/Dados/anova.xlsx')
tratamento
boxplot(tratamento$Horas ~ tratamento$Remedio)

an = aov(Horas ~ Remedio, data = tratamento)
summary(an
        )
a = aov(Horas ~ Remedio * Sexo, data = tratamento)
summary(a)

tukey = TukeyHSD(a)
tukey
plot(tukey)

