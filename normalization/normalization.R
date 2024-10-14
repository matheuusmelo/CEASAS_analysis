# Carrega pacotes
library(scales)
library(tidyverse) 
library(readxl)
library(openxlsx)
library(dplyr)

setwd("C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_Agosto_2023/Normalizar")

# Read the data from Excel file
banco_p_normalizar <- read_excel("banco_para_normalizar.xlsx")

ceasa_nome <- banco_p_normalizar$CEASAS

ceasa_ID <- banco_p_normalizar$ID_CEASA

# Remover colunas pois a normalização só ocorre com coluna de dados numéricos
banco_p_normalizar <- banco_p_normalizar[,-c(1,2)]

head(banco_p_normalizar)

# Crie uma função para normalizar uma coluna pelo mínimo e máximo
normalizar_coluna <- function(coluna) {
  minimo <- min(coluna)
  maximo <- max(coluna)
  coluna_normalizada <- (coluna - minimo) / (maximo - minimo)
  return(coluna_normalizada)
}

# Aplique a função a todas as colunas do dataframe 'dados'
colunas_normalizadas <- lapply(banco_p_normalizar, normalizar_coluna)

# Crie um novo dataframe com as colunas normalizadas
dados_normalizados <- as.data.frame(colunas_normalizadas)

# Exiba o dataframe com as colunas normalizadas
print(dados_normalizados)

#Introduzir O nome das CEASAS de novo no banco
dados_normalizados <- dados_normalizados %>%
  mutate(CEASAS = ceasa_nome)

#Introduzir O ID das CEASAS de novo no banco
dados_normalizados <- dados_normalizados %>%
  mutate(ID_CEASA = ceasa_ID)


#Reorganizar para que CEASAS seja a primeira coluna e ID_CEASA seja a segunda coluna do banco
dados_normalizados <- dados_normalizados %>%
  select(CEASAS, ID_CEASA, everything())

head(dados_normalizados)

writexl::write_xlsx(dados_normalizados,"banco_normalizado_set_2023.xlsx")
