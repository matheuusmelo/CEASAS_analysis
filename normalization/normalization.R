#Loading packages
library(scales)
library(tidyverse) 
library(readxl)
library(openxlsx)
library(dplyr)

setwd("C:/path/to/your/project")

# Read the data from Excel file
banco_p_normalizar <- read_excel("bd_to_normalizate.xlsx")
ceasa_nome <- banco_p_normalizar$CEASAS
ceasa_ID <- banco_p_normalizar$ID_CEASA

# Remove columns because normalization only occurs with numeric data columns
banco_p_normalizar <- banco_p_normalizar[,-c(1,2)]

head(banco_p_normalizar)

# Create a function to normalize a column by minimum and maximum
normalizar_coluna <- function(coluna) {
  minimo <- min(coluna)
  maximo <- max(coluna)
  coluna_normalizada <- (coluna - minimo) / (maximo - minimo)
  return(coluna_normalizada)
}

# Apply function to all columns of dataframe 'data'
colunas_normalizadas <- lapply(banco_p_normalizar, normalizar_coluna)

# Create a new dataframe with the normalized columns
dados_normalizados <- as.data.frame(colunas_normalizadas)

# Display the dataframe with normalized columns
print(dados_normalizados)

# Enter the CEASAS name back into the database
dados_normalizados <- dados_normalizados %>%
  mutate(CEASAS = ceasa_nome)

# Enter the CEASAS ID back into the database
dados_normalizados <- dados_normalizados %>%
  mutate(ID_CEASA = ceasa_ID)

# Reorganize so that CEASAS is the first column and ID_CEASA is the second column in the database
dados_normalizados <- dados_normalizados %>%
  select(CEASAS, ID_CEASA, everything())

head(dados_normalizados)

#Export database normalized
writexl::write_xlsx(dados_normalizados,"normalized_bd.xlsx")
