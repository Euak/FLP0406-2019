#Lista 6
library(tidyverse)
library(readxl)
install.packages("datapasta")
library(clipr)

#Importando a planilha
CPDS <- read_excel("Lista 6/CPDS-1960-2016.xlsx")

#Exerc�cio A)
CPDS_AMOSTRA_5 <- CPDS %>% 
  group_by(AMOSTRA_5) %>% 
  na.omit() %>% 
  summarise("M�dia" = mean(effpar_ele), "Vari�ncia" = var(effpar_ele), "Desvio-Padr�o" = sd(effpar_ele))

#Exerc�cio C)
MEDIA_AMOSTRA_5 <- mean(CPDS_AMOSTRA_5$M�dia)
MEDIA_TOTAL <- mean(na.omit(CPDS$effpar_ele))

#Exerc�cio D)
CPDS_AMOSTRA_99 <- CPDS %>% 
  subset(AMOSTRA_100 == 99) %>% 
  na.omit() %>% 
  summarise("M�dia" = mean(effpar_ele), "Vari�ncia" = var(effpar_ele), "Desvio-Padr�o" = sd(effpar_ele))

CPDS_AMOSTRA_100 <- CPDS %>% 
  subset(AMOSTRA_100 != 99) %>% 
  na.omit() %>% 
  summarise("M�dia" = mean(effpar_ele), "Vari�ncia" = var(effpar_ele), "Desvio-Padr�o" = sd(effpar_ele))
