library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)


inter_sivep_20 <- fread("bases/INFLUD-24-05-2021.csv")
inter_sivep_21 <- fread("bases/INFLUD21-24-05-2021.csv")
pib_municipios <- read_excel("bases/pib_municipios.xlsx")


################# Base de Internação ############### 

inter_sivep_20$DT_INTERNA <- as.Date (inter_sivep_20$DT_INTERNA, format = '%d/%m/%Y' )
inter_sivep_20$DT_NASC <- as.Date (inter_sivep_20$DT_NASC, format = '%d/%m/%Y' )
inter_sivep_20$IDADE = as.numeric ((inter_sivep_20$DT_INTERNA - inter_sivep_20$DT_NASC)/365.25)


################### Base com dados por idade e somente residentes Brasil ############################ 

inter_sivep_20 <- filter(inter_sivep_20, IDADE < 19)
inter_sivep_20 <- subset(inter_sivep_20,inter_sivep_20$ID_PAIS == "BRASIL")

########### ANALISE COM DESFECHO (5-SRAG por COVID-19)

### 0-19 anos ###
inter_sivep_20 <- subset(inter_sivep_20, inter_sivep_20$CLASSI_FIN
                         == 5)
inter_sivep_20$CASO <-1


################# Base de Internação ############### 

inter_sivep_21$DT_INTERNA <- as.Date (inter_sivep_21$DT_INTERNA, format = '%d/%m/%Y' )
inter_sivep_21$DT_NASC <- as.Date (inter_sivep_21$DT_NASC, format = '%d/%m/%Y' )
inter_sivep_21$IDADE = as.numeric ((inter_sivep_21$DT_INTERNA - inter_sivep_21$DT_NASC)/365.25)


################### Base com dados por idade e somente residentes Brasil ############################ 

inter_sivep_21 <- filter(inter_sivep_21, IDADE < 19)
inter_sivep_21 <- subset(inter_sivep_21,inter_sivep_21$ID_PAIS == "BRASIL")

########### ANALISE COM DESFECHO (4-SRAG não especificado 5-SRAG por COVID-19)

### 0-19 anos ###
inter_sivep_21 <- subset(inter_sivep_21, inter_sivep_21$CLASSI_FIN
                         == 5)
inter_sivep_21$CASO <-1

##Junção base 2020 com 2021
base_inter <- bind_rows (inter_sivep_20, inter_sivep_21)

base_inter <- base_inter %>% select(DT_SIN_PRI,
                                    DT_INTERNA,
                                    CO_MUN_RES,
                                    ID_MN_RESI,
                                    AMOSTRA,
                                    UTI,
                                    SUPORT_VEN,
                                    DT_RAIOX,
                                    DT_TOMO,
                                    EVOLUCAO,
                                    IDADE,
                                    CASO)



pib_municipios <- pib_municipios %>% select(CO_MUN_RES = codmun, pib)
pib_municipios <- mutate(pib_municipios, "Decil " = ntile(pib_municipios$pib,10)) #Coluna com quantil

#Ajustando código das cidades satélites
##Código do DF é 530010

base_inter$CO_MUN_RES <- ifelse(substr(base_inter$CO_MUN_RES,1,2) == "53", 530010, base_inter$CO_MUN_RES)

base_inter <- merge(base_inter, pib_municipios, by = "CO_MUN_RES", all = T)


#Excluindo pacientes sem municípios 
base_inter <- subset(base_inter, !is.na(base_inter$CO_MUN_RES))

#Excluindo pacientes sem evolução 
base_inter <- subset(base_inter, !is.na(base_inter$EVOLUCAO))

#Escrevendo base trabalhada
write.csv(base_inter, "bases/base_inter_decil.csv", row.names = T)
