# PACKAGES

library(tidyverse)

# DATA TABLES

Controle = read.csv("Control.csv")
Link = dir(pattern = ".csv")

# FIRESPOTS BY BIOME

bioma = function(BASE, ANO, CONTROLE = Controle, EX = F){
  
  BIOMAS = CONTROLE %>% select(BIOMA) %>% table() %>% names()         # NAME OF BRAZILIAN BIOMES
  BR = data.frame(date = seq(as.Date(paste0(ANO,"-01-01")),
                             as.Date(paste0(ANO,"-12-31")), "day"))   # DATES TO LINK BIOMES
  
  for(i in 1:6){
    FOCOS = BASE %>% select(datahora, bioma) %>%
      mutate(datahora = as.Date(str_sub(datahora, 1, 10))) %>%
      group_by(datahora, bioma) %>% filter(bioma == BIOMAS[i]) %>%
      select(datahora) %>% count() %>% data.frame() %>% 
      select(data = datahora, focos = n) %>% suppressMessages()
    
    BR = left_join(BR, FOCOS, by = c("date" = "data"))                # UNION OF BIOMES
    
    cat(paste0(round((i/6)*100, 2), "% \n"))                          # EVOLUTION FOLLOW-UP
  }
  
  names(BR) = c("Dia", BIOMAS)                                        # RENAMING VARIABLES WITH BIOMES NAMES
  BR[is.na(BR)] = 0                                                   # DAYS WITHOUT FOCUS: NA = 0

  if(EX){write.csv(BR, file = paste0("Focos_Biome_", ANO, ".csv"),
                   row.names = F)}                                    # EXPORT
  
  cat(paste0(ANO, " FINALIZADO. \n"))
  
  return(BR)
 
}

rbind(bioma(read.csv(Link[2]), "2011"), bioma(read.csv(Link[3]), "2012"),
      bioma(read.csv(Link[4]), "2013"), bioma(read.csv(Link[5]), "2014"),
      bioma(read.csv(Link[6]), "2015"), bioma(read.csv(Link[7]), "2016"),
      bioma(read.csv(Link[8]), "2017"), bioma(read.csv(Link[9]), "2018"),
      bioma(read.csv(Link[10]), "2019"), bioma(read.csv(Link[11]), "2020"),
      bioma(read.csv(Link[12]), "2021"), bioma(read.csv(Link[13]), "2022")) %>%
  data.frame() %>% write.csv("Fires_Biomes.csv", row.names = F) # EXPORT DATABASE

# FIRESPOTS BY STATE

estados = function(BASE, ANO, CONTROLE = Controle, EX = F){
  
  ESTs = CONTROLE %>% select(NM_UF) %>% table() %>% names()           # NAME OF BRAZILIAN BIOMES
  BR = data.frame(date = seq(as.Date(paste0(ANO,"-01-01")),
                             as.Date(paste0(ANO,"-12-31")), "day"))   # DATES TO LINK BIOMES
  
  for(i in 1:27){
    FOCOS = BASE %>% select(datahora, estado) %>%
      mutate(datahora = as.Date(str_sub(datahora, 1, 10))) %>%
      group_by(datahora, estado) %>% filter(estado == ESTs[i]) %>%
      select(datahora) %>% count() %>% data.frame() %>% 
      select(data = datahora, focos = n) %>% suppressMessages()
    
    BR = left_join(BR, FOCOS, by = c("date" = "data"))                # UNION OF BIOMES
    
    cat(paste0(round((i/27)*100, 2), "% \n"))                          # EVOLUTION FOLLOW-UP
  }
  
  names(BR) = c("Dia", ESTs)                                        # RENAMING VARIABLES WITH BIOMES NAMES
  BR[is.na(BR)] = 0                                                   # DAYS WITHOUT FOCUS: NA = 0
  
  if(EX){write.csv(BR, file = paste0("Focos_State_", ANO, ".csv"),
                   row.names = F)}                                    # EXPORT
  
  cat(paste0(ANO, " FINALIZADO. \n"))
  
  return(BR)
  
}

rbind(estados(read.csv(Link[2]), "2011"), estados(read.csv(Link[3]), "2012"),
      estados(read.csv(Link[4]), "2013"), estados(read.csv(Link[5]), "2014"),
      estados(read.csv(Link[6]), "2015"), estados(read.csv(Link[7]), "2016"),
      estados(read.csv(Link[8]), "2017"), estados(read.csv(Link[9]), "2018"),
      estados(read.csv(Link[10]), "2019"), estados(read.csv(Link[11]), "2020"),
      estados(read.csv(Link[12]), "2021"), estados(read.csv(Link[13]), "2022")) %>%
  data.frame() %>% write.csv("Fires_States.csv", row.names = F) # EXPORT DATABASE

# FIRESPOTS BY MUNICIPALITY

municipio = function(BASE, ANO, CONTROLE = Controle, EX = F){
  ESTs = CONTROLE %>% select(NM_UF) %>% table() %>% names()         # NAME OF BRAZILIAN STATES
  BR = data.frame(date = seq(as.Date(paste0(ANO, "-01-01")),
                             as.Date(paste0(ANO, "-12-31")),
                             "day"))                                # DATES TO LINK STATES
  
  for(j in 1:27){
    MUNs = CONTROLE %>% filter(NM_UF == ESTs[j]) %>%
      select(COD_IBGE, NM_MUN_S) %>% unique()                       # NAME OF ALL CITIES IN STATE j
    estado = BASE %>% filter(estado == ESTs[j]) %>%
      mutate(datahora = as.Date(str_sub(datahora, 1, 10)))          # SELECTION OF STATE FIRESPOTS j
    muns = table(estado$municipio) %>% data.frame() %>%
      mutate(MUN = Var1) %>% select(MUN) %>%
      left_join(MUNs, by = c("MUN" = "NM_MUN_S"))                   # NAME OF CITIES WITH FIRESPOTS
    
    dt = data.frame(date = seq(as.Date(paste0(ANO, "-01-01")),
                               as.Date(paste0(ANO, "-12-31")),
                               "day"))                              # DATES TO LINK CITIES
    
    for(l in 1:nrow(muns)){
      municipio = estado[estado$municipio == muns$MUN[l],]
      mun = data.frame(table(municipio$datahora)) %>%
        mutate(date = as.Date(Var1)) %>% select(-Var1)
      
      dt = left_join(dt, mun, by = "date")                          # UNION OF CITIES
    }
    
    names(dt) = c("Dia", muns$COD_IBGE)                             # RENAMING VARIABLES WITH IBGE CODES
    
    if(nrow(muns) == nrow(MUNs)){
      dt[is.na(dt)] = 0
      BR = left_join(BR, dt, by = c("date" = "Dia"))
    }                               # ALL CITIES HAVE FIRESPOTS
    else{
      A = t(dt) %>% data.frame()
      A = A[-1,] %>% mutate(cidade = as.integer(row.names(A)[-1])) %>%
        suppressMessages()
      A = left_join(MUNs, A, by = c("COD_IBGE" = "cidade")) %>%
        select(-COD_IBGE, -NM_MUN_S) %>% t() %>% data.frame() %>%
        mutate_at(paste0("X", seq(1, nrow(MUNs))), as.numeric) %>%
        mutate(date = dt$Dia)
      
      names(A) = c(MUNs$COD_IBGE, "Dia")
      A[is.na(A)] = 0
      BR = left_join(BR, A, by = c("date" = "Dia"))
    }                                                       # SOME CITIES HAVEN'T FIRESPOTS
    
    cat("Estado:", ESTs[j], "Finalizado!! \n")
    
  }
  
  if(EX){write.csv(BR, file = paste0("Focos_Mun_", ANO, ".csv"),
                   row.names = F)}                                  # EXPORT
  
  cat(paste0(ANO, " FINALIZADO. \n"))
  
  return(BR)
}

rbind(municipio(read.csv(Link[2]), "2011"), municipio(read.csv(Link[3]), "2012"),
      municipio(read.csv(Link[4]), "2013"), municipio(read.csv(Link[5]), "2014"),
      municipio(read.csv(Link[6]), "2015"), municipio(read.csv(Link[7]), "2016"),
      municipio(read.csv(Link[8]), "2017"), municipio(read.csv(Link[9]), "2018"),
      municipio(read.csv(Link[10]), "2019"), municipio(read.csv(Link[11]), "2020"),
      municipio(read.csv(Link[12]), "2021"), municipio(read.csv(Link[13]), "2022")) %>%
  data.frame() %>% write.csv("Fires_Mun.csv", row.names = F) # EXPORT DATABASE

## WHEN USING THIS CODE IT IS NECESSARY TO BE CAREFUL WITH LINE 9 "dir(pattern = ".csv")".
## FOR CORRECT FUNCTIONING, IN THE DIRECTORY WHERE THE SCRIPT SHOULD CONTAIN ONLY THE FILE
## "Control.csv" AND THE FILES WITH THE FIRESPOTS, ONE FILE FOR EACH YEAR. WHEN FINISHING
## THE GROUP PROCESS PER DAY, THE NEW FILE WILL BE SAVED IN THE SAME SOURCE FOLDER, AND IF
## THE CODE ON LINE 9 IS RUN AGAIN, THE NUMBER OF FILES IN THE SOURCE DIRECTORY WILL BE
## CHANGED, MAY CAUSE ERRORS IN THE REST OF THE CODE.
