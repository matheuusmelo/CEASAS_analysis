library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(deflateBR)


setwd("C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias")

# Read the data from Excel file
prohort_comdist3 <- read_excel("prohort_comdist3_set_2023.xlsx")

############################
#Distâncias

# Banco de dados com distância média por Ceasa por Ano
dist_ano <- prohort_comdist3%>%
  group_by(munceasa, Ano) %>% 
  summarise(media_km = mean(c(dist_km), na.rm = T))

writexl::write_xlsx(dist_ano,"medias_dist_ceasas_anos_set2023.xlsx")








# Banco de dados com distância média por Ceasa em todos os anos
dist_ano_2 <- prohort_comdist3%>%
  group_by(munceasa) %>% 
  summarise(media_km = mean(c(dist_km), na.rm = T))

writexl::write_xlsx(dist_ano_2,"medias_dist_ceasas_set2023.xlsx")







# Banco de dados com distância média por grupo por ano por ceasa
dist_grupo_ano <- prohort_comdist3%>%
  group_by(munceasa,Ano,grupo) %>% 
  summarise(mediasimples = mean(c(dist_km), na.rm = T))


# Informações de unidade e grupo passam para as colunas
dist_grupo_ano_pivot <- dist_grupo_ano %>%
  tidyr::pivot_wider(names_from = c(grupo), values_from = c(mediasimples)) 

writexl::write_xlsx(dist_grupo_ano_pivot,"medias_dist_grupos_anos_set2023.xlsx")

# Banco de dados com distância média por grupo por Ceasa em todos os anos
dist_grupo_ano_2 <- prohort_comdist3%>%
  group_by(munceasa,grupo) %>% 
  summarise(media_km = mean(c(dist_km), na.rm = T))


dist_grupo_ano_2_pivot <- dist_grupo_ano_2 %>%
  tidyr::pivot_wider(names_from = c(grupo), values_from = c(media_km))

writexl::write_xlsx(dist_grupo_ano_2_pivot,"medias_dist_grupos_set2023.xlsx")






###########################
# Valor

# Banco de dados com kg e rs média por Ceasa por Ano
mov_ano <- prohort_comdist3%>%
  group_by(munceasa, Ano, unid) %>% 
  summarise(media_valor = mean(c(valor), na.rm = T))

writexl::write_xlsx(mov_ano,"medias_mov_ceasas_anos_set2023.xlsx")


# Banco de dados com kg e rs média por Ceasa em todos os anos
mov_ano_2 <- prohort_comdist3%>%
  group_by(munceasa, unid) %>% 
  summarise(media_valor = mean(c(valor), na.rm = T))

writexl::write_xlsx(mov_ano_2,"medias_mov_ceasas_set2023.xlsx")

# Banco de dados com com kg e rs média por grupo por ano por ceasa
mov_grupo_ano <- prohort_comdist3%>%
  group_by(munceasa,Ano,grupo, unid) %>% 
  summarise(media_valor = mean(c(valor), na.rm = T))

# Informações de unidade e grupo passam para as colunas
mov_grupo_ano_pivot <- mov_grupo_ano %>%
  tidyr::pivot_wider(names_from = c(grupo, unid), values_from = c(media_valor)) 

writexl::write_xlsx(mov_grupo_ano_pivot,"medias_mov_grupos_anos_set2023.xlsx")



###############################
#Ajustes banco dag março 24

# Read the data from Excel file
mov_grupo_ano_pivot3 <- read_excel("medias_mov_grupos_anos_set2023.xlsx")

# Transposição dos dados
dados_transpostos <- mov_grupo_ano_pivot3 %>%
  pivot_wider(
    names_from = Ano,
    values_from = c(frutas_Rs, frutas_kg, hort_folhas_Rs, hort_folhas_kg, hort_frutos_Rs, hort_frutos_kg, hort_tuberculos_Rs, hort_tuberculos_kg))

# Exibir os dados transpostos
dados_transpostos

writexl::write_xlsx(dados_transpostos,"medias_mov_grupos_fev24.xlsx")

head(dados_transpostos)


# Deflacionar os valores 
#Frutas 2017

frutas_17 <- select(dados_transpostos, frutas_Rs_2017)

actual_dates17 <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_17 <- ipca(frutas_17, actual_dates17, "12/2022")
write.xlsx(resultado_frutas_17, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_17_Deflacionado.xlsx")


#Frutas 2018

frutas_18 <- select(dados_transpostos, frutas_Rs_2018)

actual_dates18 <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_18 <- ipca(frutas_18, actual_dates18, "12/2022")
write.xlsx(resultado_frutas_18, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_18_Deflacionado.xlsx")


#Frutas 2019
frutas_19 <- select(dados_transpostos, frutas_Rs_2019)

actual_dates19 <- seq.Date(from = as.Date("2019-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_19 <- ipca(frutas_19, actual_dates19, "12/2022")
write.xlsx(resultado_frutas_19, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_19_Deflacionado.xlsx")


#Frutas 2020
frutas_20 <- select(dados_transpostos, frutas_Rs_2020)

actual_dates20 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_20 <- ipca(frutas_20, actual_dates20, "12/2022")
write.xlsx(resultado_frutas_20, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_20_Deflacionado.xlsx")

#Frutas 2121
frutas_21 <- select(dados_transpostos, frutas_Rs_2021)

actual_dates21 <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_21 <- ipca(frutas_21, actual_dates21, "12/2022")
write.xlsx(resultado_frutas_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_21_Deflacionado.xlsx")

#Frutas 2022
frutas_22 <- select(dados_transpostos, frutas_Rs_2022)

actual_dates22 <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_frutas_22 <- ipca(frutas_22, actual_dates21, "12/2022")
write.xlsx(resultado_frutas_22, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/frutas_22_Deflacionado.xlsx")


# Folhas

#hort_folhas 2017

hort_folhas_17 <- select(dados_transpostos, hort_folhas_Rs_2017)

actual_dates17 <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_17 <- ipca(hort_folhas_17, actual_dates17, "12/2022")
write.xlsx(resultado_hort_folhas_17, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_17_Deflacionado.xlsx")


#hort_folhas 2018

hort_folhas_18 <- select(dados_transpostos, hort_folhas_Rs_2018)

actual_dates18 <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_18 <- ipca(hort_folhas_18, actual_dates18, "12/2022")
write.xlsx(resultado_hort_folhas_18, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_18_Deflacionado.xlsx")


#hort_folhas 2019
hort_folhas_19 <- select(dados_transpostos, hort_folhas_Rs_2019)

actual_dates19 <- seq.Date(from = as.Date("2019-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_19 <- ipca(hort_folhas_19, actual_dates19, "12/2022")
write.xlsx(resultado_hort_folhas_19, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_19_Deflacionado.xlsx")


#hort_folhas 2020
hort_folhas_20 <- select(dados_transpostos, hort_folhas_Rs_2020)

actual_dates20 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_20 <- ipca(hort_folhas_20, actual_dates20, "12/2022")
write.xlsx(resultado_hort_folhas_20, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_20_Deflacionado.xlsx")

#hort_folhas 2121
hort_folhas_21 <- select(dados_transpostos, hort_folhas_Rs_2021)

actual_dates21 <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_21 <- ipca(hort_folhas_21, actual_dates21, "12/2022")
write.xlsx(resultado_hort_folhas_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_21_Deflacionado.xlsx")

#hort_folhas 2022
hort_folhas_22 <- select(dados_transpostos, hort_folhas_Rs_2022)

actual_dates22 <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_22 <- ipca(hort_folhas_22, actual_dates21, "12/2022")
write.xlsx(resultado_hort_folhas_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_22_Deflacionado.xlsx")

#Frutos

#hort_folhas 2017

hort_folhas_17 <- select(dados_transpostos, hort_folhas_Rs_2017)

actual_dates17 <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_17 <- ipca(hort_folhas_17, actual_dates17, "12/2022")
write.xlsx(resultado_hort_folhas_17, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_17_Deflacionado.xlsx")


#hort_folhas 2018

hort_folhas_18 <- select(dados_transpostos, hort_folhas_Rs_2018)

actual_dates18 <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_18 <- ipca(hort_folhas_18, actual_dates18, "12/2022")
write.xlsx(resultado_hort_folhas_18, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_18_Deflacionado.xlsx")


#hort_folhas 2019
hort_folhas_19 <- select(dados_transpostos, hort_folhas_Rs_2019)

actual_dates19 <- seq.Date(from = as.Date("2019-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_19 <- ipca(hort_folhas_19, actual_dates19, "12/2022")
write.xlsx(resultado_hort_folhas_19, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_19_Deflacionado.xlsx")


#hort_folhas 2020
hort_folhas_20 <- select(dados_transpostos, hort_folhas_Rs_2020)

actual_dates20 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_20 <- ipca(hort_folhas_20, actual_dates20, "12/2022")
write.xlsx(resultado_hort_folhas_20, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_20_Deflacionado.xlsx")

#hort_folhas 2121
hort_folhas_21 <- select(dados_transpostos, hort_folhas_Rs_2021)

actual_dates21 <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_21 <- ipca(hort_folhas_21, actual_dates21, "12/2022")
write.xlsx(resultado_hort_folhas_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_21_Deflacionado.xlsx")

#hort_folhas 2022
hort_folhas_22 <- select(dados_transpostos, hort_folhas_Rs_2022)

actual_dates22 <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_folhas_22 <- ipca(hort_folhas_22, actual_dates21, "12/2022")
write.xlsx(resultado_hort_folhas_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_folhas_22_Deflacionado.xlsx")


#Frutos

#hort_frutos 2017

hort_frutos_17 <- select(dados_transpostos, hort_frutos_Rs_2017)

actual_dates17 <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_17 <- ipca(hort_frutos_17, actual_dates17, "12/2022")
write.xlsx(resultado_hort_frutos_17, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_17_Deflacionado.xlsx")


#hort_frutos 2018

hort_frutos_18 <- select(dados_transpostos, hort_frutos_Rs_2018)

actual_dates18 <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_18 <- ipca(hort_frutos_18, actual_dates18, "12/2022")
write.xlsx(resultado_hort_frutos_18, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_18_Deflacionado.xlsx")


#hort_frutos 2019
hort_frutos_19 <- select(dados_transpostos, hort_frutos_Rs_2019)

actual_dates19 <- seq.Date(from = as.Date("2019-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_19 <- ipca(hort_frutos_19, actual_dates19, "12/2022")
write.xlsx(resultado_hort_frutos_19, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_19_Deflacionado.xlsx")


#hort_frutos 2020
hort_frutos_20 <- select(dados_transpostos, hort_frutos_Rs_2020)

actual_dates20 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_20 <- ipca(hort_frutos_20, actual_dates20, "12/2022")
write.xlsx(resultado_hort_frutos_20, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_20_Deflacionado.xlsx")

#hort_frutos 2121
hort_frutos_21 <- select(dados_transpostos, hort_frutos_Rs_2021)

actual_dates21 <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_21 <- ipca(hort_frutos_21, actual_dates21, "12/2022")
write.xlsx(resultado_hort_frutos_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_21_Deflacionado.xlsx")

#hort_frutos 2022
hort_frutos_22 <- select(dados_transpostos, hort_frutos_Rs_2022)

actual_dates22 <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_frutos_22 <- ipca(hort_frutos_22, actual_dates21, "12/2022")
write.xlsx(resultado_hort_frutos_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_frutos_22_Deflacionado.xlsx")


#Tubérculos

#hort_tuberculos 2017

hort_tuberculos_17 <- select(dados_transpostos, hort_tuberculos_Rs_2017)

actual_dates17 <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_17 <- ipca(hort_tuberculos_17, actual_dates17, "12/2022")
write.xlsx(resultado_hort_tuberculos_17, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_17_Deflacionado.xlsx")


#hort_tuberculos 2018

hort_tuberculos_18 <- select(dados_transpostos, hort_tuberculos_Rs_2018)

actual_dates18 <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_18 <- ipca(hort_tuberculos_18, actual_dates18, "12/2022")
write.xlsx(resultado_hort_tuberculos_18, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_18_Deflacionado.xlsx")


#hort_tuberculos 2019
hort_tuberculos_19 <- select(dados_transpostos, hort_tuberculos_Rs_2019)

actual_dates19 <- seq.Date(from = as.Date("2019-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_19 <- ipca(hort_tuberculos_19, actual_dates19, "12/2022")
write.xlsx(resultado_hort_tuberculos_19, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_19_Deflacionado.xlsx")


#hort_tuberculos 2020
hort_tuberculos_20 <- select(dados_transpostos, hort_tuberculos_Rs_2020)

actual_dates20 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_20 <- ipca(hort_tuberculos_20, actual_dates20, "12/2022")
write.xlsx(resultado_hort_tuberculos_20, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_20_Deflacionado.xlsx")

#hort_tuberculos 2121
hort_tuberculos_21 <- select(dados_transpostos, hort_tuberculos_Rs_2021)

actual_dates21 <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_21 <- ipca(hort_tuberculos_21, actual_dates21, "12/2022")
write.xlsx(resultado_hort_tuberculos_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_21_Deflacionado.xlsx")

#hort_tuberculos 2022
hort_tuberculos_22 <- select(dados_transpostos, hort_tuberculos_Rs_2022)

actual_dates22 <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-01"), by = "year")
resultado_hort_tuberculos_22 <- ipca(hort_tuberculos_22, actual_dates21, "12/2022")
write.xlsx(resultado_hort_tuberculos_21, "C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/medias/hort_tuberculos_22_Deflacionado.xlsx")




#
#
# converter pra dolar


# Read the data from Excel file
banco_p_uss <- read_excel("ajustado_dag_def_mar24.xlsx")

banco_rs_p_kusd <- select(banco_p_uss, frutas_Rs_2017,	frutas_Rs_2018,	frutas_Rs_2019,	frutas_Rs_2020,	frutas_Rs_2021,	frutas_Rs_2022,	hort_folhas_Rs_2017,	hort_folhas_Rs_2018,	hort_folhas_Rs_2019,	hort_folhas_Rs_2020,	hort_folhas_Rs_2021,	hort_folhas_Rs_2022,	hort_frutos_Rs_2017,	hort_frutos_Rs_2018,	hort_frutos_Rs_2019,	hort_frutos_Rs_2020,	hort_frutos_Rs_2021,	hort_frutos_Rs_2022,	hort_tuberculos_Rs_2017,	hort_tuberculos_Rs_2018,	hort_tuberculos_Rs_2019,	hort_tuberculos_Rs_2020,	hort_tuberculos_Rs_2021,	hort_tuberculos_Rs_2022)

# converter todos os valores pra dólar
dados_convertidos_uss <- banco_rs_p_kusd / 5.16



dados_convertidos_em_mil_uss <- dados_convertidos_uss / 1000


# Exibir os dados resultantes
print(dados_convertidos_em_mil_uss)

# Defina o nome do arquivo de saída
nome_arquivo <- "dados_convertidos_kusd.xlsx"

# Escrever os dados divididos para um arquivo Excel
write.xlsx(dados_convertidos_em_mil_uss, nome_arquivo, rowNames = FALSE)



#
#
#converter de kg para mil toneladas

banco_kg_p_kt <- select(banco_p_uss, frutas_kg_2017,	frutas_kg_2018,	frutas_kg_2019,	frutas_kg_2020,	frutas_kg_2021,	frutas_kg_2022,	hort_folhas_kg_2017,	hort_folhas_kg_2018,	hort_folhas_kg_2019,	hort_folhas_kg_2020,	hort_folhas_kg_2021,	hort_folhas_kg_2022,	hort_frutos_kg_2017,	hort_frutos_kg_2018,	hort_frutos_kg_2019,	hort_frutos_kg_2020,	hort_frutos_kg_2021,	hort_frutos_kg_2022,	hort_tuberculos_kg_2017,	hort_tuberculos_kg_2018,	hort_tuberculos_kg_2019,	hort_tuberculos_kg_2020,	hort_tuberculos_kg_2021,	hort_tuberculos_kg_2022)


dados_convertidos_em_mil_t <- banco_kg_p_kt / 1000000

# Exibir os dados resultantes
print(dados_convertidos_em_mil_t)


# Defina o nome do arquivo de saída
nome_arquivo <- "dados_convertidos_kt.xlsx"

# Escrever os dados divididos para um arquivo Excel
write.xlsx(dados_convertidos_em_mil_t, nome_arquivo, rowNames = FALSE)



##################################
# Organizar de novo os dados

banco_baseado_organizar <- read_excel("medias_mov_grupos_anos_set2023.xlsx")

banco_p_organizar <- read_excel("dados_convertidos.xlsx")


dados_long <- banco_p_organizar %>%
  pivot_longer(
    cols = -munceasa,
    names_to = c(".value", "ano"),
    names_pattern = "(\\w+)_(\\d+)"
  ) %>%
  arrange(ano, munceasa)

print(dados_long)


dados_long2 <- banco_p_organizar %>%
  pivot_longer(
    cols = -munceasa,
    names_to = c(".value", "ano"),
    names_pattern = "(\\w+)_(\\d+)"
  ) %>%
  arrange(munceasa, as.numeric(ano))

print(dados_long2)

writexl::write_xlsx(dados_long2,"banco_organizado_final_mar24.xlsx")


######################################
# Banco de dados com kg e rs por grupo por Ceasa em todos os anos
mov_grupo_ano_2 <- prohort_comdist3%>%
  group_by(munceasa,grupo, unid) %>% 
  summarise(media_valor = mean(c(valor), na.rm = T))

mov_grupo_ano_2_pivot <- mov_grupo_ano_2 %>%
  tidyr::pivot_wider(names_from = c(grupo, unid), values_from = c(media_valor)) 


writexl::write_xlsx(mov_grupo_ano_2_pivot,"medias_mov_grupos_set2023.xlsx")

##################################################################################
# Organizar de novo os dados (Teste T - Igor Lima)

banco_p_organizar <- read_excel("banco_organizado_final_mar24.xlsx")

# Banco de dados com kg e rs média por Ceasa por Ano
banco_igor <- banco_p_organizar%>%
  group_by(Cluster) %>% 
  summarise(media_valor = mean(c(dist_media_km), na.rm = T))


banco_p_organizar

# Organizar o banco de dados em ordem alfabética a partir da coluna Cluster
dados_ordenados <- arrange(banco_p_organizar, Cluster)

# Mostrar o banco de dados ordenado
dados_ordenados


writexl::write_xlsx(dados_ordenados,"banco_organizado_igor_abril24.xlsx")





