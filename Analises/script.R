# Instalar Pacote
#install.packages("synthesisr")

# Carregar Pacote
library(synthesisr)

# Listar os arquivos contendo as buscas das bases de dados PubMed, Web of Science, Scopus e Embase
arquivos.buscas.bases.de.dados <- list.files("C:\\Users\\gabri\\Downloads\\Meta Analise\\An trans-vertical\\buscas",full.names = TRUE)


# Importar arquivos de buscas das bases de dados
arquivos.importados <- read_refs(filename = arquivos.buscas.bases.de.dados, return_df = TRUE)

# Remover artigos duplicados por título com o método 'string_distance'
results <- deduplicate(arquivos.importados, match_by = "title", method = "string_osa", rm_punctuation = TRUE, to_lower = TRUE)

# Construir uma planilha com dados bibliográficos (título, resumo, periódico) para análise posterior
dados.biblio <- results[,c("author", "year", "title", "source", "volume", "start_page", "end_page", "doi", "abstract")]







