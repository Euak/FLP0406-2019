#Lab 4
#carregar pacotes
library(tidyverse)
library(readxl)
install.packages("gridExtra")
library(gridExtra)

#Exercício D)
bd <- read_xlsx('Lab 4/Banco de dados.xlsx', col_names = T)

bd_filtered <- bd %>% 
  select(country, enep, enpp, seats, legislative_type) %>% 
  na.omit %>% 
  subset(enep > 0 & enpp > 0 & seats > 0)

# Valor Médio enep - Número efetivo de partidos na eleição
some_enep <- 0
for (i in 1:nrow(bd_filtered)) {
  some_enep <- some_enep + bd_filtered$enep[i]
}
media_enep <- some_enep/26
mediana_enep <- sort(bd_filtered$enep)[26/2]

# Variância enep - Número efetivo de partidos na eleição
some_var_enep <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_enep <- some_var_enep + (bd_filtered$enep[i]-media_enep)^2
}
var_enep <- some_var_enep/(26-1)

# Desvio-padrão enep - Número efetivo de partidos na eleição
dp_enep <- sqrt(var_enep)

# Valor Médio enpp - Número efetivo de partidos no parlamento
some_enpp <- 0
for (i in 1:nrow(bd_filtered)) {
  some_enpp <- some_enpp + bd_filtered$enpp[i]
}
media_enpp <- some_enpp/26
mediana_enpp <- sort(bd_filtered$enpp)[26/2]

# Variância enpp - Número efetivo de partidos no parlamento
some_var_enpp <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_enpp <- some_var_enpp + (bd_filtered$enpp[i]-media_enpp)^2
}
var_enpp <- some_var_enpp/(26-1)

# Desvio-padrão enpp - Número efetivo de partidos no parlamento
dp_enpp <- sqrt(var_enpp)

# Valor Médio seats - número de cadeiras em disputa
some_seats <- 0
for (i in 1:nrow(bd_filtered)) {
  some_seats <- some_seats + bd_filtered$seats[i]
}
media_seats <- some_seats/26
mediana_seats <- sort(bd_filtered$seats)[26/2]

# Variância seats - número de cadeiras em disputa
some_var_seats <- 0
for (i in 1:nrow(bd_filtered)) {
  some_var_seats <- some_var_seats + (bd_filtered$seats[i]-media_seats)^2
}
var_seats <- some_var_seats/(26-1)

# Desvio-padrão seats - número de cadeiras em disputa
dp_seats <- sqrt(var_seats)

# Moda legislative type - Sistema eleitoral
moda_legis_type <- 2

# Exercício E
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

# Exercício H
bd_filtered %>% 
    ggplot(aes(x = enep, y = enpp)) +
    geom_point()

#Correção (legislativo x nº de partidos efetivos)
bd_filtered %>% 
  ggplot(aes(y = enep)) +
  geom_boxplot() +
  facet_wrap(~legislative_type)

bd_filtered %>% 
  ggplot(aes(x = seats, y = enep)) +
  geom_point() +
  facet_wrap(~legislative_type)
