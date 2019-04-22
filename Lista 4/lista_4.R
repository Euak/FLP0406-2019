library(tidyverse)

#Tópicos da Aula 3

#Exercício C
library(readxl)
CPDS_2016 <- read_excel("Lista 4/CPDS_2016.xlsx")
#bic
CPDS_2016 %>% 
  ggplot(aes(x = bic)) + 
  geom_bar() +
  labs(y = "Números de casos")
#judreview
CPDS_2016 %>% 
  ggplot(aes(x = judrev)) + 
  geom_bar() +
  labs(y = "Números de casos")

#Exercício D
#womenpar
CPDS_2016 %>% 
  ggplot(aes(x = womenpar)) + 
  geom_histogram(binwidth = 3) +
  labs(y = "Números de casos")

#Exercício E e F
#womenpar
CPDS_2016 %>% 
  ggplot(aes(y = womenpar)) + 
  geom_boxplot() +
  labs(y = "Porcentagem de mulheres no parlamento")

#Exercício G
# Criando a função.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Obtendo a moda de bic
getmode(CPDS_2016$bic)
#Obtendo a moda de judreview
getmode(CPDS_2016$judrev)

mean(CPDS_2016$womenpar)

var(CPDS_2016$womenpar)
sd(CPDS_2016$womenpar)