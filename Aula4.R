# Aula 4

# Carrega o dataset
p <- read.csv("PIB.csv", sep=";", dec=",")

# Separa o dataset
# até dez/14 linha 132
treino <- p[1:132,]
teste <- p[133:138,]

#Regressão linear simples
mod <- lm(PIB~BRL,data=treino)

#Visualiza as informações
summary(mod)

#precisão do pib
prev <- predict(mod, newdata=teste)

#comparar bases
cbind(prev, teste$PIB, prev - teste$PIB)

#calcular a média do erro quadrado
sse<-sum((prev - teste$PIB)^2)


#Regressão linear Múltipla
mod <- lm(PIB~BRL+BRP,data=treino)
summary(mod)
prev <- predict(mod, newdata=teste)
cbind(prev, teste$PIB, prev - teste$PIB)
sse<-sum((prev - teste$PIB)^2)

#RLM + Dummies
mod <- lm(PIB~BRL+BRP+D2+D5+D6+D7,data=treino)
summary(mod)
prev <- predict(mod, newdata=teste)
cbind(prev, teste$PIB, prev - teste$PIB)
sse<-sum((prev - teste$PIB)^2)


#Auto Regressivo AR
mod <- lm(PIB~PIBi1+PIBi2+PIBi12,data=treino)
summary(mod)
prev <- predict(mod, newdata=teste)
cbind(prev, teste$PIB, prev - teste$PIB)
sse<-sum((prev - teste$PIB)^2)

#FSBDS23 Rodolfo
#https://www.kaggle.com/c/titanic
#Chance de sobrevivência do disastre, exercício para essa semana
#Survived variável base tranin
#Na base teste não existe a tabela survied
#Regressão linear
