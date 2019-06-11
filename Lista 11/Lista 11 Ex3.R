library(tidyverse)
library(readxl)

install.packages('smss')
library(smss)

dados <- read_excel('Lista 11/Banco_Horas_Leitura.xlsx')
dados <- na.omit(dados)


dados %>% 
  group_by(Nivel) %>% 
  summarise(media = mean(`Horas de leitura`), dp = sd(`Horas de leitura`), n = n()) %>% 
  view()

data("crime2005")
mean(crime2005$PO)

