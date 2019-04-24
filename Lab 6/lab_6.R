# Laboratório 6 - Probabilidades
library(tidyverse)

# Questão 2
quest2 <- sample(1:6, 3, replace=T)

# Questão 3
quest2_media <- mean(quest2)
quest2_dp <- sd(quest2)

# Amostra de 600 jogadas
quest3 <- sample(1:6, 600, replace=T)
hist(quest3)

as.data.frame(table(quest3))

mean(quest3)
var(quest3)

# Questão 7
quest7_6 <- sample(1:6, 6, replace=T)
media_q7_6 <- mean(quest7_6)
dp_q7_6 <- sd(quest7_6)

quest7_60 <- sample(1:6, 60, replace=T)
media_q7_60 <- mean(quest7_60)
dp_q7_60 <- sd(quest7_60)

quest7_600 <- sample(1:6, 600, replace=T)
media_q7_600 <- mean(quest7_600)
dp_q7_600 <- sd(quest7_600)

quest7_6000 <- sample(1:6, 6000, replace=T)
media_q7_6000 <- mean(quest7_6000)
dp_q7_6000 <- sd(quest7_6000)

as.data.frame(table(quest7_6000))
