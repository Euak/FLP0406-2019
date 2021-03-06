#Lab 4
#carregar pacotes
library(tidyverse)
library(readxl)
install.packages("gridExtra")
library(gridExtra)

#Exerc�cio D)
bd <- read_xlsx('Lab 4/Banco de dados.xlsx', col_names = T)

bd_filtered <- bd %>% 
  select(country, enep, enpp, seats, legislative_type) %>% 
  na.omit %>% 
  subset(enep > 0 & enpp > 0 & seats > 0)

# Valor M�dio enep - N�mero efetivo de partidos na elei��o
some_enep <- 0
for (i in 1:nrow(bd_filtered)) {
  some_enep <- some_enep + bd_filtered$enep[i]
}
media_enep <- some_enep/26
mediana_enep <- sort(bd_filtered$enep)[26/2]

# Vari�ncia enep - N�mero efetivo de partidos na elei��o
some_var_enep <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_enep <- some_var_enep + (bd_filtered$enep[i]-media_enep)^2
}
var_enep <- some_var_enep/(26-1)

# Desvio-padr�o enep - N�mero efetivo de partidos na elei��o
dp_enep <- sqrt(var_enep)

# Valor M�dio enpp - N�mero efetivo de partidos no parlamento
some_enpp <- 0
for (i in 1:nrow(bd_filtered)) {
  some_enpp <- some_enpp + bd_filtered$enpp[i]
}
media_enpp <- some_enpp/26
mediana_enpp <- sort(bd_filtered$enpp)[26/2]

# Vari�ncia enpp - N�mero efetivo de partidos no parlamento
some_var_enpp <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_enpp <- some_var_enpp + (bd_filtered$enpp[i]-media_enpp)^2
}
var_enpp <- some_var_enpp/(26-1)

# Desvio-padr�o enpp - N�mero efetivo de partidos no parlamento
dp_enpp <- sqrt(var_enpp)

# Valor M�dio seats - n�mero de cadeiras em disputa
some_seats <- 0
for (i in 1:nrow(bd_filtered)) {
  some_seats <- some_seats + bd_filtered$seats[i]
}
media_seats <- some_seats/26
mediana_seats <- sort(bd_filtered$seats)[26/2]

# Vari�ncia seats - n�mero de cadeiras em disputa
some_var_seats <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_seats <- some_var_seats + (bd_filtered$seats[i]-media_seats)^2
}
var_seats <- some_var_seats/(26-1)

# Desvio-padr�o seats - n�mero de cadeiras em disputa
dp_seats <- sqrt(var_seats)

# Moda legislative type - Sistema eleitoral
moda_legis_type <- 2

# Exerc�cio E
# Histograma enep
hist_enep <- bd_filtered %>% 
  ggplot(aes(x = enep)) +
  geom_histogram()

# Histograma enpp
hist_enpp <- bd_filtered %>% 
  ggplot(aes(x = enpp)) +
  geom_histogram()

# Histograma seats
hist_seats <- bd_filtered %>% 
  ggplot(aes(x = seats)) +
  geom_histogram(bins = 4)

# Histograma legislative_type
hist_legis_type <- bd_filtered %>% 
  ggplot(aes(x = legislative_type)) +
  geom_histogram()

grid.arrange(hist_enep, hist_enpp, hist_seats, hist_legis_type)

# Exerc�cio H
bd_filtered %>% 
    ggplot(aes(x = enep, y = enpp)) +
    geom_point()

#Corre��o (legislativo x n� de partidos efetivos)
bd_filtered %>% 
  ggplot(aes(y = enep)) +
  geom_boxplot() +
  facet_wrap(~legislative_type)

bd_filtered %>% 
  ggplot(aes(x = seats, y = enep)) +
  geom_point() +
  facet_wrap(~legislative_type)
