#Lista 5
library(tidyverse)
library(readxl)

# Obtendo os dados
dados <- read_delim('Lista 5/dados.csv', col_names = T, quote = "\"", delim = ';')
dados$PROP_VOTOS <- as.numeric(sub(",", ".", dados$PROP_VOTOS, fixed = TRUE))

# Exercício 2 
# A)
stats <- dados %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise("Média" = mean(PROP_VOTOS), "Variância" = var(PROP_VOTOS) ,"Desvio-padrão" = sd(PROP_VOTOS))

dados %>% 
  subset(SIGLA_PARTIDO == "PSDB") %>% 
  select(PROP_VOTOS) %>% 
  print.data.frame()

# B)
#Medianas
dados %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise("Mediana" = median(PROP_VOTOS))

#Quartis TAMANHO_COLIGACAO - PT
quartis_pt <- dados %>% 
  subset(SIGLA_PARTIDO == "PT") %>% 
  .$TAMANHO_COLIGACAO %>% 
  quantile()

#Quartis TAMANHO_COLIGACAO - PSDB
quartis_psdb <- dados %>% 
  subset(SIGLA_PARTIDO == "PSDB") %>% 
  .$TAMANHO_COLIGACAO %>% 
  quantile()

# Função para classificar o quartil
getQuartile <- function(x, vector) {
  if (x <= vector["25%"]){
    return(1)
  } else if (x <= vector["50%"]){
    return(2)
  } else if (x <= vector["75%"]){
    return(3)
  } else {
    return(4)
  }
}

# Criando a variável TAMANHO_COLIGACAO_CAT
dados <- ungroup(dados)
dados_col <- dados %>% 
  rowwise %>% 
  mutate("TAMANHO_COLIGACAO_CAT" = if_else(SIGLA_PARTIDO == "PT", true = getQuartile(TAMANHO_COLIGACAO, quartis_pt),
                                           false = getQuartile(TAMANHO_COLIGACAO, quartis_psdb)))

# Criando a tabela por Categoria de Quartil e Sigla
dados_coligacao <- dados_col %>%
  group_by(SIGLA_PARTIDO, TAMANHO_COLIGACAO_CAT) %>% 
  summarise("PROP_VOTOS" = mean(PROP_VOTOS))

# Exercício C
dados_colig_norm <- dados_coligacao %>% 
  mutate("PROP_VOTOS_NORM" =  if_else(SIGLA_PARTIDO == "PT", true = (PROP_VOTOS - stats$Média[stats$SIGLA_PARTIDO == "PT"]) / stats$`Desvio-padrão`[stats$SIGLA_PARTIDO == "PT"],
                                      false = (PROP_VOTOS - stats$Média[stats$SIGLA_PARTIDO == "PSDB"]) / stats$`Desvio-padrão`[stats$SIGLA_PARTIDO == "PSDB"]))
