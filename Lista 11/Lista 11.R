install.packages("tidyverse")
library(tidyverse)

dados <- read_csv2('Lista 11/dados.csv')
glimpse(dados)

dados %>% 
  ggplot(aes(x = TAMANHO_COLIGACAO, y = PROP_VOTOS)) +
  geom_point()

media_colg <- mean(dados$TAMANHO_COLIGACAO)
media_voto <- mean(dados$PROP_VOTOS)

teste <- (dados$TAMANHO_COLIGACAO - media_colg) * (dados$PROP_VOTOS - media_voto)

b_cima <- sum(teste)  

teste2 <- (dados$TAMANHO_COLIGACAO - media_colg)
(teste2)

sum((dados$TAMANHO_COLIGACAO - media_colg)*(dados$PROP_VOTOS - media_voto))

dados$TAMANHO_COLIGACAO
mean(dados$TAMANHO_COLIGACAO)
teste2

dados$TAMANHO_COLIGACAO
((dados$TAMANHO_COLIGACAO - mean(dados$TAMANHO_COLIGACAO))^2)

sum((dados$TAMANHO_COLIGACAO - mean(dados$TAMANHO_COLIGACAO))^2)

lmTamanho = lm(PROP_VOTOS~TAMANHO_COLIGACAO, data = dados)

summary(lmTamanho)

sum(lmTamanho$residuals^2)
