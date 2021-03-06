#Lab 4
library(tidyverse)
library(readxl)

#Recolhendo a amostra n0
populacao <- read_excel('Lab 5/Banco_Lab 05.xlsx', col_names = T)

amostra <- populacao %>% 
  subset(ID < 33)

# a)
stats_votos <- amostra %>% 
  summarise("M�dia" = mean(Num_votos), "Mediana" = median(Num_votos), "Desvio-padr�o" = sd(Num_votos))

stats_finan <- amostra %>% 
  summarise("M�dia" = mean(Financiamento), "Mediana" = median(Financiamento), "Desvio-padr�o" = sd(Financiamento))

# b)
correlacao <- cor(amostra$Financiamento, amostra$Num_votos)

# Recolhendo amostra n1
amostra1 <- populacao %>% 
  subset(ID %in% sample(populacao$ID, size = 32))

# c)
stats_votos1 <- amostra1 %>% 
  summarise("M�dia" = mean(Num_votos), "Mediana" = median(Num_votos), "Desvio-padr�o" = sd(Num_votos))

stats_finan1 <- amostra1 %>% 
  summarise("M�dia" = mean(Financiamento), "Mediana" = median(Financiamento), "Desvio-padr�o" = sd(Financiamento))

# d)
correlacao1 <- cor(amostra1$Financiamento, amostra1$Num_votos)
