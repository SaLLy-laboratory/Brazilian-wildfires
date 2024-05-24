library(tidyverse)

biomas = read.csv("Fires_Biomes.csv")
estados = read.csv("Fires_States.csv")
municipios = read.csv("Fires_Mun.csv")

## Biomas

biomas %>% mutate(mes_ano = str_sub(Dia, 1, 7)) %>% group_by(mes_ano) %>%
  select(-Dia) %>% summarise_all(sum) %>% data.frame() %>%
  write.csv("Fires_Month_Biomes.csv", row.names = F)

## Estados

estados %>% mutate(mes_ano = str_sub(Dia, 1, 7)) %>% group_by(mes_ano) %>%
  select(-Dia) %>% summarise_all(sum) %>% data.frame() %>%
  write.csv("Fires_Month_States.csv", row.names = F)

## Municipios

municipios %>% mutate(mes_ano = str_sub(date, 1, 7)) %>% group_by(mes_ano) %>%
  select(-date) %>% summarise_all(sum) %>% data.frame() %>%
  write.csv("Fires_Month_Mun.csv", row.names = F)
