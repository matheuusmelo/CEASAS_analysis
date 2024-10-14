library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(FactoMineR)
library(Factoshiny)
library(stats)
library(dplyr)
library(pastecs)
library(psych)
library(ggcorrplot)
library(readxl)
library(openxlsx)
library(tidyr)
library(NbClust)
library(fpc)
library(clValid)
library(ggplot2)

setwd("C:/Users/mathe/OneDrive/Documentos/Profissional/Artigos/Em Produção/Dag/Artigo_1/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023")

# Read the data from Excel file
#banco <- read_excel("banco_norm_nome_ingles.xlsx")
banco <- read_excel("banco_norm_nome_ingles_ajuste.xlsx")

ceasa_nome <- banco$CEASAS

ceasa_ID <- banco$ID_CEASA

# Remove unwanted columns
banco <- banco[,-c(1,2)]

# Create a vector with the desired row names
siglas_ceasas <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18",
  "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29")

# Convert tibble to data frame and set row names
banco <- as.data.frame(banco)
rownames(banco) <- siglas_ceasas
head(banco)

#banco <- read.table(
#  file = 'banco_julho_2023.csv',
#  header = TRUE,
#  sep = ',',
#  skip = 0,
#  fill = TRUE,
#  encoding = "UTF-8")

# Dissimilarity matrix
d <- dist(banco, method = "euclidean")
#matriz_d <- as.matrix(d)


####################################################### 
#KMEANS

fviz_nbclust(banco, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method KMEANS
fviz_nbclust(banco, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

nb <- NbClust(banco, diss = NULL, distance = "euclidean", 
              min.nc = 2, max.nc = 10, 
              method = "kmeans", index = "all")

# Trace o gráfico para visualizar os índices de validação
fviz_nbclust(nb)

clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(banco, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
##
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(banco, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

set.seed(123)
fviz_nbclust(banco, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


gap_stat <- clusGap(banco, FUN = hcut, K.max = 10, B = 10)
fviz_gap_stat(gap_stat)


# K-means clustering
km.res <- eclust(banco, "kmeans", k = 3, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


#################################

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

hc.res <- eclust(banco, "hclust", k = 3, hc_metric = "euclidean",
                 hc_method = "ward.D2", graph = FALSE)


p <- fviz_cluster(hc.res, c("point", "text"), 
                  ellipse.type = "norm",
             palette = "jco", 
             ggtheme = theme_minimal(),
             main = "",
             xlab = "Dim1 (51%)",
             ylab = "Dim2 (34.1%)",
             show.clust.cent = FALSE,
             repel = TRUE,
             labelsize = 12,
             outlier.labelsize = labelsize
             #ellipse.alpha = 0
             )

print(p)

p2 <- p + theme(
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(face = "bold"))

# Visualize o gráfico com os rótulos dos eixos em negrito
print(p2)



nb <- NbClust(banco, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")

nb$All.index

fviz_silhouette(hc.res, palette = "jco",
                ggtheme = theme_classic())

# Cut tree into k groups
sub_grp <- cutree(hc5, k = 3)

# Number of members in each cluster
table(sub_grp)

banco %>%
  mutate(cluster = sub_grp) %>%
  head

#dendrograma clássico
plot(hc5, cex = 0.6, hang = -1)
rect.hclust(hc5, k = 3, border = 2:5)

#cluster plot
max.overlaps = Inf
fviz_cluster(list(data = banco, cluster = sub_grp),
             geom = c("point","text"),
             repel = TRUE,
             pointsize = 1.0,
             labelsize = 8,
             max.overlaps = Inf,
             palette = "Set2", ggtheme = theme_minimal())


# Silhouette information
silinfo <- hc.res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 29)

# Average silhouette width of each cluster
med_si <- as.data.frame(silinfo$clus.avg.widths) 
write.xlsx(med_si, "~/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/med_silhouette_cada_cluster.xlsx")

# The total average (mean of all individual silhouette widths)
med_si_2 <-  as.data.frame(silinfo$avg.width)
med_si_2
# The size of each clusters
hc.res$size

# Statistics for clustering
km_stats <- cluster.stats(dist(banco), hc.res$cluster)
km_stats

# Dun index
km_stats$dunn
km_stats$pearsongamma
km_stats$corrected.rand

#Rand index
sp <- as.numeric(banco)

km_stats$corrected.rand

# Compute clValid

clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(banco, nClust = 2:10,
                  clMethods = clmethods, validation = "internal", method = "ward")
# Summary
summary(intern)

# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(banco, nClust = 2:10, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

##################################################
#exportar planilha com os dados de clusterização
final <- data.frame(banco, Cluster = sub_grp)
view(final)

#Introduzir O nome das CEASAS de novo no banco
final <- final %>%
  mutate(CEASAS = ceasa_nome)

#Introduzir O ID das CEASAS de novo no banco
final <- final %>%
  mutate(ID_CEASA = ceasa_ID)

#Reorganizar para que CEASAS seja a primeira coluna e ID_CEASA seja a segunda coluna do banco
final <- final %>%
  select(CEASAS, ID_CEASA, Cluster, everything())

view(final)

write.xlsx(final, "~/Profissional/Artigos/Em Produção/Dag/Dados excel/Estatisticas_CEASAS/Cluster/R/Novo/Novo_set_2023/clusterizacao.xlsx")

####################################################################
#FIM DO CLUSTER E COMEÇO DO PCA
#analise descritiva


# Read the data from Excel file
banco2 <- read_excel("clusterizacao_ajuste.xlsx")
# Remove unwanted columns
banco2 <- banco2[,-c(1,2,3)]

describe(banco2) # média, desvio padrão, mínimo e máximo
boxplot(banco2)

#matriz de correlações
matcor <- round(cor(banco2), 2)
matcor

ggcorrplot(matcor, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"),
           ggtheme=theme_bw)


# teste de Bartlett - testar se as variâncias são homogêneas

Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
Bartlett.sphericity.test(banco2)

KMO(banco2)

res.pca <- PCA(banco2, graph = TRUE) #PCA
res.pca2 <- PCA(final, graph = TRUE) #PCA

round(res.pca$eig,3) #autovalores com 3 casas decimais



round(res.pca$svd$V,3) #autovetores

res.pca$eig # A proporção de variação retida pelos componentes principais (CP) pode ser extraída da seguinte forma


round(res.pca$var$cor^2,4) # baseando-se na matriz de cor


round(res.pca$var$cor,4)

round(res.pca$var$cos2,4)

res.desc <- dimdesc(res.pca, axes = c(1,2))
res.desc$Dim.1

# Matriz rotacionada dos 3 Componentes Principais
pc <- principal(r=banco, nfactors=3, rotate="varimax", scores=T)
pc$loadings


#REALIZANDO PCA
pca_res <- prcomp(banco, scale = TRUE)
summary(pca_res) # standard deviation, proportioN of variance, cumulative proportion
names(pca_res)
pca_res$rotation
pca_res$sdev
pca_res$x



res.hcpc <- HCPC(banco, graph = TRUE) #Hierarquical clustering on principal components

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)



PCA(banco, scale.unit = TRUE, ncp = 3, graph = TRUE)
print(res.pca)
res.pca$var$cos2

get_eig(res.pca) # Extract eigenvalues/variances

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50)) # Visualize eigenvalues/variances

# Extract the results for variables
var <- get_pca_var(res.pca)
var

# Contribution of variables
head(var$contrib)
view(var)

# Contribution of variables
head(var$cos2)
view(var)

# Variables on dimensions 1 and 2
fviz_pca_var(res.pca, axes = c(1, 2))
# Individuals on dimensions 1 and 2
fviz_pca_ind(res.pca, axes = c(1, 2)) 

# Coordinates of variables
head(var$coord)


# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")


# PCA VARIAVEL COLORIDO
pca_print <- fviz_pca_var(res.pca, col.var="cos2",
             gradient.cols = c("red", "blue", "#006400"),
             title = "",
             repel = TRUE) + xlab("Comercialization (k ton. and k USD)") +   ylab("Distance (Km)")

pca_print <- pca_print +
  labs(color = "Contribution")

print(pca_print)



#PCA INDIVIDUAL COLORIDO
pca_indiv_plot <- fviz_pca_ind(res.pca, col.ind = "cos2", geom = c("point", "text"), 
                               repel = TRUE, gradient.cols = c("red", "blue", "#006400"), title = "") + xlab("Comercialization (k ton. and k USD)") + ylab("Distance (Km)") +  theme_minimal() + theme(axis.text.x = element_text(face = "bold"))

# Altere a legenda para "contribution"
pca_indiv_plot <- pca_indiv_plot +
  labs(color = "Contribution")

print(pca_indiv_plot)



# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                title = NULL) + labs(x = "Comercialization (k t and k USD)", y = "Distance (Km)")
                

fviz_pca_biplot(res.pca,
                label = "all",
                geom.ind = c("point", "text"), 
                geom.var = c("arrow"),
                col.ind = "cos2",
                alpha.ind = 1,
                pointshape = 19, 
                pointsize = 2,
                gradient.cols = c("red", "blue", "#006400"),
                col.var = "black",
                repel = TRUE,
                title = NULL) + labs(x = "Comercialization (k t and k USD)", y = "Distance (Km)")

fviz_pca_biplot(res.pca,
                label = "all",
                geom.ind = c("point", "text"), 
                geom.var = c("arrow"),
                col.ind = "cos2",
                alpha.ind = 1,
                pointshape = 19, 
                pointsize = 2,
                col.var = "black",
                repel = TRUE,
                title = NULL) + labs(x = "Comercialization (k t and k USD)", y = "Distance (Km)")

# Altere a legenda para "contribution"
fviz_pca_biplot <- fviz_pca_biplot +
  labs(color = "Contribution")


# Supondo que você tenha um vetor de clusters para cada indivíduo chamado "clusters"
clusters <- c("Cluster 1", "Cluster 2", "Cluster 2",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 2",	"Cluster 2",	"Cluster 1",	"Cluster 2",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 1",	"Cluster 2",	"Cluster 2",	"Cluster 3",	"Cluster 2",	"Cluster 1",	"Cluster 2",	"Cluster 2",	"Cluster 1",	"Cluster 2")

# Crie um vetor de cores correspondente aos clusters
cluster_colors <- c("Cluster 1" = "Cluster 1", "Cluster 2" = "Cluster 2", "Cluster 3" = "Cluster 3")

# Atribua cores aos indivíduos com base nos clusters
col.individuals <- cluster_colors[clusters]


q <- fviz_pca_biplot(res.pca,
                     label = "all",
                     geom.ind = c("point", "text"), 
                     geom.var = c("arrow", "text"),
                     col.ind = col.individuals, # Use as cores atribuídas aos indivíduos
                     alpha.ind = 1,
                     pointshape = 19, 
                     pointsize = 2,
                     gradient.cols = c("red", "blue", "#006400"),
                     col.var = "black",
                     repel = TRUE,
                     title = NULL,
                     addEllipses = TRUE,
                     show.clust.cent = FALSE)

q <- q + labs(color = "Cluster")

q



fviz_pca_biplot(res.pca, geom=c("point", "text", "arrows"), 
                label = "var", 
                alpha.var ="contrib", col.var="contrib",
                repel=TRUE, gradient.cols = c("red", "blue", "#006400")) + 
  scale_shape_manual(values=c(18,17,16,15))


fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = final$Cluster, col.ind = "white",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdBu") + labs(fill = "Cluster", color = "Contrib", alpha = "Contrib") 

res.desc <- dimdesc(res.pca.cor, axes = c(1,2), proba = 0.05)
res.desc <- dimdesc(res.pca.cor, axes = c(1,3), proba = 0.05)


#"#00AFBB", "#E7B800", "#FC4E07"

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
#fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)


# Contributions of individuals to PC1
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 10)
# Contributions of individuals to PC2
fviz_contrib(res.pca, choice = "ind", axes = 2, top = 10)
3# Contributions of individuals to PC3
fviz_contrib(res.pca, choice = "ind", axes = 3, top = 10)

fviz_contrib(res.pca, choice="ind", axes = 1,
             fill = "lightgray", color = "black") +
  theme_minimal() + theme(axis.text.x = element_text(face = "bold")) #theme(axis.text.x = element_text(angle=45))


# Visualize variable with cos2 >= 0.6
fviz_pca_var(res.pca, select.var = list(cos2 = 0.6))

# Top 5 active variables with the highest cos2
fviz_pca_var(res.pca, select.var= list(cos2 = 5))

# Select by names
#name <- list(name = c("AF_17", "AF_18", "AF_19", "AF_20", "AF_21"))
#fviz_pca_ind(res.pca, select.ind = name)

# top 5 contributing individuals and variable
fviz_pca_biplot(res.pca, select.ind = list(contrib = 5), 
                select.var = list(contrib = 5),
                ggtheme = theme_minimal())

# Scree plot
scree.plot <- fviz_eig(res.pca)
# Plot of individuals
ind.plot <- fviz_pca_ind(res.pca)
# Plot of variables
var.plot <- fviz_pca_var(res.pca)

pdf("PCA.pdf") # Create a new pdf device
print(scree.plot)
print(ind.plot)
print(var.plot)
dev.off() # Close the pdf device

# Export into a CSV file
write.infile(res.pca, "pca.csv", sep = ";")

# Especifique o nome do arquivo XLSX que você deseja criar
nome_arquivo_xlsx <- "pca.xlsx"

# Exporte os resultados para o arquivo XLSX
write_xlsx(res.pca, nome_arquivo_xlsx)

# Print scree plot to a png file
#png("pca-scree-plot.png")
#print(scree.plot)
#dev.off()
# Print individuals plot to a png file
#png("pca-variables.png")
#print(var.plot)
#dev.off()
# Print variables plot to a png file
#png("pca-individuals.png")
#print(ind.plot)
#dev.off()

# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind

head(ind$coord)
view(ind)

head(ind$contrib)
view(ind$contrib)

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
options(ggrepel.max.overlaps = Inf)

#PLOT PCA1 e PCA2
fviz_pca_ind(res.pca, 
             axes = c(1,2),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, axes = c(1, 2), geom.ind = c("point", "text"),
             label = "all", invisible = "none", labelsize = 4,
             pointsize = 2, habillage = "none",
             addEllipses = FALSE, ellipse.level = 0.95, 
             col.ind = "black", col.ind.sup = "blue", alpha.ind = 1,
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             jitter = list(what = "label", width = NULL, height = NULL))

#PLOT PCA1 e PCA3
#fviz_pca_ind(res.pca, 
 #            axes = c(1,3),
  #           col.ind = "cos2",
  #           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
   #          repel = TRUE # Avoid text overlapping (slow if many points)
#)


#fviz_pca_ind(res.pca, label="none", habillage=final$Cluster)



# Description of dimensions CORRELAÇÃO
res.desc$Dim.1
res.desc$Dim.2
res.desc$Dim.3

#Usando o pacote Factoshiny
res.shiny=PCAshiny(banco)


################################################################## 
#Análise descritiva do banco bruto 
banco2 <- read_excel("Banco_Bruto_Set_2023.xlsx")

# Remove unwanted columns
banco2 <- banco2[,-c(1,2)]

# Create a vector with the desired row names
#siglas_ceasas2 <- c(
  # "11", "12", "13", "14", "15", "16", "21", "22", "23", "24", "25", "26", "31", "32", "33", "34", "35", "36",
  #  "41", "42", "43", "44", "45", "46", "51", "52", "53", "54", "55", "56", "61", "62", "63", "64", "65", "66",
  #  "71", "72", "73", "74", "75", "76", "81", "82", "83", "84", "85", "86", "91", "92", "93", "94", "95", "96",
  # "101", "102", "103", "104", "105", "106", "111", "112", "113", "114", "115", "116", "121", "122", "123", "124",
  #"125", "126", "131", "132", "133", "134", "135", "136", "141", "142", "143", "144", "145", "146", "151", "152",
  #"153", "154", "155", "156", "161", "162", "163", "164", "165", "166", "171", "172", "173", "174", "175", "176",
  #"181", "182", "183", "184", "185", "186", "191", "192", "193", "194", "195", "196", "201", "202", "203", "204",
  #"205", "206", "211", "212", "213", "214", "215", "216", "221", "222", "223", "224", "225", "226", "231", "232",
  #"233", "234", "235", "236", "241", "242", "243", "244", "245", "246", "251", "252", "253", "254", "255", "256",
  #"261", "262", "263", "264", "265", "266", "271", "272", "273", "274", "275", "276", "281", "282", "283", "284",
  #"285", "286")

# Convert tibble to data frame and set row names
#banco2 <- as.data.frame(banco2)
#rownames(banco2) <- siglas_ceasas2
#head(banco2)
#summary(banco2)


# Média, desvio padrão e variância gerais de cada cluster

banco3 <- banco2 %>%
  group_by(Cluster) %>% 
  summarise(
    medias = mean(c(
      Distancia_km, Distancia_Folha, Distancia_Fruta, Distancia_Fruto,
      Distancia_Tuberculo, Frutas_Financeiro, Folhas_Financeiro,
      Frutos_Financeiro, Tuberculos_Financeiro, Frutas_Volume,
      Folhas_Volume, Frutos_Volume, Tuberculos_Volume
    ), na.rm = TRUE),
    desvio_padrao = sd(c(
      Distancia_km, Distancia_Folha, Distancia_Fruta, Distancia_Fruto,
      Distancia_Tuberculo, Frutas_Financeiro, Folhas_Financeiro,
      Frutos_Financeiro, Tuberculos_Financeiro, Frutas_Volume,
      Folhas_Volume, Frutos_Volume, Tuberculos_Volume
    ), na.rm = TRUE),
    variancia = var(c(
      Distancia_km, Distancia_Folha, Distancia_Fruta, Distancia_Fruto,
      Distancia_Tuberculo, Frutas_Financeiro, Folhas_Financeiro,
      Frutos_Financeiro, Tuberculos_Financeiro, Frutas_Volume,
      Folhas_Volume, Frutos_Volume, Tuberculos_Volume
    ), na.rm = TRUE)
  )


head(banco3)

writexl::write_xlsx(banco4,"estatistica_descritiva_geral_clusters.xlsx")


df2 <- banco2 %>% 
  group_by(Cluster) %>% 
  summarise(med_Distancia_km = mean(Distancia_km),
            var_Distancia_km = var(Distancia_km),
            sd_Distancia_km = sd(Distancia_km),
            med_Distancia_Folha = mean(Distancia_Folha),
            var_Distancia_Folha = var(Distancia_Folha),
            sd_Distancia_Folha = sd(Distancia_Folha),
            med_Distancia_Fruta = mean(Distancia_Fruta),
            var_Distancia_Fruta = var(Distancia_Fruta),
            sd_Distancia_Fruta = sd(Distancia_Fruta),
            med_Distancia_Fruto = mean(Distancia_Fruto),
            var_Distancia_Fruto = var(Distancia_Fruto),
            sd_Distancia_Fruto = sd(Distancia_Fruto),
            mean_Distancia_Tuberculo = mean(Distancia_Tuberculo),
            var_Distancia_Tuberculo = var(Distancia_Tuberculo),
            sd_Distancia_Tuberculo = sd(Distancia_Tuberculo),
            mean_Frutas_Financeiro = mean(Frutas_Financeiro),
            var_Frutas_Financeiro = var(Frutas_Financeiro),
            sd_Frutas_Financeiro = sd(Frutas_Financeiro),
            mean_Frutas_Volume = mean(Frutas_Volume),
            var_Frutas_Volume = var(Frutas_Volume),
            sd_Frutas_Volume = sd(Frutas_Volume),
            mean_Folhas_Financeiro = mean(Folhas_Financeiro),
            var_Folhas_Financeiro = var(Folhas_Financeiro),
            sd_Folhas_Financeiro = sd(Folhas_Financeiro),
            mean_Folhas_Volume = mean(Folhas_Volume),
            var_Folhas_Volume = var(Folhas_Volume),
            sd_Folhas_Volume = sd(Folhas_Volume),
            mean_Frutos_Financeiro = mean(Frutos_Financeiro),
            var_Frutos_Financeiro = var(Frutos_Financeiro),
            sd_Frutos_Financeiro = sd(Frutos_Financeiro),
            mean_Frutos_Volume = mean(Frutos_Volume),
            var_Frutos_Volume = var(Frutos_Volume),
            sd_Frutos_Volume = sd(Frutos_Volume),
            mean_Tuberculos_Financeiro = mean(Tuberculos_Financeiro),
            var_Tuberculos_Financeiro = var(Tuberculos_Financeiro),
            sd_Tuberculos_Financeiro = sd(Tuberculos_Financeiro),
            mean_Tuberculos_Volume = mean(Tuberculos_Volume),
            var_Tuberculos_Volume = var(Tuberculos_Volume),
            sd_Tuberculos_Volume = sd(Tuberculos_Volume))

df2


writexl::write_xlsx(df2,"estatistica_descritiva_variaveis_clusters.xlsx")


