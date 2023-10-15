dados <- read.csv('facerecognition.dat', sep = ' ')

dados

# i-ésimo pseudovalor JK theta_i <- n*theta - (n-1)*thetai
# erro erro = (n-1)*(thetai - theta), thetai snedo = sum(theta_i)/n

# Sobre o banco
# match, 0 ou 1, representa a correspondencia entre o predito e o real
# Match é a var de interesse

# P1
# Utilizando o método de Jackknife, crie uma rotina no ambiente
# R que retorne os valores ótimos dos β que maximizam a função de verossimilhança acima com
# 
# base no banco de dados facerecognition.dat. Além disso, sua rotina deve retornar os erros-
#   padrões e, também, os intervalos de 95% de confiança para os β’s ajustados.

n <- 20
set.seed(11)
x <- dados$eyediff

## Jackknife
jack <- numeric(length(x) - 1)
pseudo <- numeric(length(x))
for(i in 1:length(x))
{
  
  for(j in 1:length(x))
  {
    
    if(j < i) jack[j] <- x[j] else if(j > i) jack[j-1] <- x[j]
    
  }
  pseudo[i] <- length(x)*sample(x,replace=T)-(length(x)-1)*sample(jack,replace=T)
  
}

hist(jack, col = "light green", main = "Histogram of Jackknife ReSampling")
hist(dados$eyediff)


mean(jack)
mean(dados$eyediff)

var(jack)
var(dados$eyediff)
