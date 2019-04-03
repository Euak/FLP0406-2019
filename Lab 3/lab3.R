library(tidyverse)

#Procedimento 1
#Importação da base
library(readxl)
Base_de_dados <- read_excel("Lab 3/Base de dados.xlsx")
View(Base_de_dados)

#Exercício B
latin_america <- c("Caribbean", "South America", "Central America")
  
democracy_2008_LA <- Base_de_dados %>% 
  subset(year == 2008 &
         un_region_name %in% latin_america) %>% 
  select(ctryname, democracy)

write.table(democracy_2008_LA, "Lab 3/democracy_2008_LA.csv", sep = ",", col.names = NA,
            qmethod = "double", quote = FALSE)

#Exercício B
democracy_1980_LA <- Base_de_dados %>% 
  subset(year == 1980 &
           un_region_name %in% latin_america) %>% 
  select(ctryname, democracy)

write.table(democracy_1980_LA, "Lab 3/democracy_1980_LA.csv", sep = ",", col.names = NA,
            qmethod = "double", quote = FALSE)

democracy_1980_LA %>% 
  ggplot(aes(x = democracy)) +
    geom_bar()

#Procedimento 2
democracy_1980_2008_LA <- Base_de_dados %>% 
  subset(year %in% c(1980, 2008) &
           un_region_name %in% latin_america) %>% 
  select(ccdcodelet, ctryname, year, democracy)

write.table(democracy_1980_2008_LA, "Lab 3/democracy_1980_2008_LA_lab1.csv", sep = ";", col.names = NA,
            quote = FALSE)

#Procedimento 3 e 4
#Agrupando o ano
WB_tided <- WB %>% 
   gather("Year", "Indicator Value", '1960':'2018')

#Filtrando o banco com o indicador
WB_filtered <-  WB_tided %>% 
  subset(Year %in% c(1980, 2008) &
         `Country Code` %in% unique(democracy_1980_2008_LA$ccdcodelet) &
           `Indicator Name` %in% c("Government expenditure on education, total (% of GDP)",
                                   "")) %>% 
  select(-X64, -`Country Name`, -`Indicator Code`, -Year)

#Juntando as tabelas
dados <- inner_join(democracy_1980_2008_LA, WB_filtered, by = c("ccdcodelet" = "Country Code"))

#Procedimento 6
dados %>% 
  subset(year == 1980) %>% 
  na.omit() %>% 
    ggplot(aes(x = `Indicator Value`, y = democracy)) +
    geom_point()


