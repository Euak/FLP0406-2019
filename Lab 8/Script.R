sample(1:66, 5,replace = )


library(tidyverse)
library(readxl)

dados <- read_excel('Lab 8/Banco_Lab 08.xlsx')

amostra <- dados %>% 
  subset(Obs %in% c(56, 4, 53, 40, 45))


mean(amostra$P2)
sd(amostra$P2)

mean(dados$P2)
