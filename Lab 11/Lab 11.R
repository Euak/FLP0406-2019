library(tidyverse)
library(readxl)

dados <- read_excel('Lab 11/dados.xlsx')
glimpse(dados)

med_cresc <- mean(na.omit(dados$Crescimento_PIB_per_capita))
med_vot <- mean(na.omit(dados$Vot_partido_incumbente))

sd(na.omit(dados$Crescimento_PIB_per_capita))
sd(na.omit(dados$Vot_partido_incumbente))

X-U <- (dados$Crescimento_PIB_per_capita - med_cresc)

dados <- dados %>% 
  mutate('X-U' = Crescimento_PIB_per_capita - med_cresc,
         'Y-U' = Vot_partido_incumbente - med_vot)

dados <- dados %>% 
  mutate('XY' = `X-U` * `Y-U`)

sum(na.omit(dados$XY))/6
dados

cov(na.omit(dados$Crescimento_PIB_per_capita), na.omit(dados$Vot_partido_incumbente))


dados %>% 
  na.omit() %>% 
  cov(x = Crescimento_PIB_per_capita, y = Vot_partido_incumbente)


var(na.omit(dados$Crescimento_PIB_per_capita))
var(na.omit(dados$Vot_partido_incumbente))


cor(na.omit(dados$Crescimento_PIB_per_capita), na.omit(dados$Vot_partido_incumbente))

dados %>% 
  na.omit() %>% 
  ggplot(aes(x = Crescimento_PIB_per_capita, y = Vot_partido_incumbente)) + 
  geom_point()

lm_vot <- lm(na.omit(dados$Crescimento_PIB_per_capita) ~ na.omit(dados$Vot_partido_incumbente))

summary(lm_vot)
