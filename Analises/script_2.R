# Instalar Pacote
#install.packages("BiocManager")
#BiocManager::install("EBImage")
#install.packages("metagear")

# Carregar Pacote
library(metagear)


# Ajustar dados bibliográficos em acordo com o pacote Metagear
dados.biblio$author <- as.factor(dados.biblio$author)
dados.biblio$title <- as.factor(dados.biblio$title)
dados.biblio$source  <- as.factor(dados.biblio$source )
dados.biblio$doi <- as.factor(dados.biblio$doi)
dados.biblio$abstract <- as.factor(dados.biblio$abstract)
dados.biblio$year <- as.integer(dados.biblio$year)
dados.biblio$volume <- as.integer(dados.biblio$volume)
dados.biblio$start_page <- as.integer(dados.biblio$start_page)
dados.biblio$end_page <- as.integer(dados.biblio$end_page)
names(dados.biblio) <- c("AUTHORS","YEAR","TITLE","JOURNAL","VOLUME","LPAGES","UPAGES","DOI","ABSTRACT")

#Distribuir o esforço do screening dos artigos selecionados
effort_distribute(dados.biblio, initialize = TRUE, reviewers = "laporta", save_split = TRUE)

#Iniciar o screening dos artigos selecionados por Título e Resumo 
abstract_screener("effort_laporta.csv", aReviewer = "laporta")

#Preparar planilha com para busca de artigos completos
select.biblio <- read.table("effort_laporta.csv", header = T, sep = ",")
select.biblio <- subset(select.biblio , subset = select.biblio$INCLUDE == "YES")
write.table(select.biblio[,-12], "select.biblio.csv", sep = ",", col.names = NA,
            qmethod = "double")






