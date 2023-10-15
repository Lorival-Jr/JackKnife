
# Regressão Logística Binária ---------------------------------------------


# Lendo e entendendo o banco de dados -------------------------------------


dados <- read.csv('facerecognition.dat', sep = ' ')
dados

# Entendendo o banco
str(dados)

dados$match <- as.factor(dados$match)

str(dados)

summary(dados)

levels(dados$match) # Não deu match = categoria de referência


# Checagem de pressupostos ------------------------------------------------

## Variável match (y ou dependente)
## 1. Variável dependente dicotômica (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

# A variável match segue isso

## Construindo o modelo
names(dados)
fit0 <- glm(match ~ eyediff + nosecheekdiff + variabilityratio,
            family = binomial(link = 'logit'), data = dados)

## 3. Ausência de outliers

plot(fit0, which =5) 
# Todos os pontos estão dentro do intervalo
# Mas aquele 895 é preocupante


# usando pacote
summary(MASS::stdres(fit0))         # ta fugindo um pouco do -3 e 3

## 4. Ausência de multicolinearidade

cor(dados[,2:4]) # OK, correlações baixas 0.3, 0.18 e 0.19

pairs(dados)

car::vif(fit0)   # Vif é bem baixo, usando um valor seria preocupante acima de 10

## 5. Relaçao linear entre cada Var independente contínua e o lofito da Var dependente

 ### Interação entre a VI contínua e o seu log não significativa (teste de Box-Tidwell)

intlog_eyediff <- dados$eyediff          * log(dados$eyediff)
intlog_noseche <- dados$nosecheekdiff    * log(dados$nosecheekdiff)
intlog_variabi <- dados$variabilityratio * log(dados$variabilityratio)

dados2 <- dados
dados2$int_eye <- intlog_eyediff 
dados2$int_nos <- intlog_noseche
dados2$int_var <- intlog_variabi

fit_interacao <- glm(match ~ ., 
                     family = binomial(link = 'logit'), data = dados2)

summary(fit_interacao)
# O Pressuposto foi atendido para as variáveis eyediff e nosecheekdiff, já para variabilityratio a interação foi significativa para o modelo            

# Opção 2: cálculo do logito
# é calculado como:
                  # prob   <- predict(mod, type = 'response')
                  # logito <- log(prob/(1 - prob))

# Ou pega direto do modelo criado
logito <- fit0$linear.predictors

plot(x = logito, y = dados$eyediff)          # Tem uma linearidade 
plot(x = logito, y = dados$nosecheekdiff)    # Tem uma linearidade
plot(x = logito, y = dados$variabilityratio) # Não tem linearidade



# Modelo 2 ----------------------------------------------------------------

fit2 <- glm(match ~ eyediff + nosecheekdiff,
            family = binomial(link = 'logit'), data = dados)

plot(fit2, which =5)         # Os pressupostos testados se mantém
summary(MASS::stdres(fit2))  
cor(dados[,2:4])
car::vif(fit2) 



# Análise do modelo -------------------------------------------------------

## Efeitos gerais
# Anova(fit2, type = 'II', test = 'Wald')

## Efeitos específicos

summary(fit2) 
# Todas variáveis são muito significativas, tendo ***
# Lembrando que a relação é ao não match

## Razões de chance (odds-ratio)

# Via log-likelihood, com IC de 95%
exp(cbind(Odds= coef(fit2), confint(fit2)))

# Via erro padrão, com IC de 95%
exp(cbind(Odds = coef(fit2), confint.default(fit2)))

#Pelas Odds
# há  uma diminuição da chance de estar na categoria match = 1
# quando o eyediff e nosecheekdiff aumentam, visto que são < 1


# Desempenho do modelo ----------------------------------------------------

# AIC
AIC(fit2, fit0)
# Testei para ambos mesmo o fit0 não respeitando os pressupostos
# Iveram AIC igual à 1007

# BIC
BIC(fit2, fit0)
# Modelos tiveram BIC semelhante, o fit2 teve BIC de 1022

# se ambos atendessem os pressupostos, poderiamos testar via anova se eles tem diferença

anova(fit2, fit0, test='Chisq') 
# Tivemos um P-valor = 0.1669, não sendo significativo

# Tabela de classificação
# install.packages('QuantPsyc')
QuantPsyc::ClassLog(fit2, dados$match)


# Resumindo

summary(fit2)

# As variáveis eyediff e nosecheekdiff são previsores para a variável match
# O aumento de ambas variáveis fazem diminuir a chance de match ser 1
# Eyediff       - Odds: 1.466857e-04, Ic 95%: [4.8710e-06, 4.2845e-03]
# Nosecheekdiff - Odds: 1.925857e-06, Ic 95%: [1.6574e-07, 2.2376e-05]
