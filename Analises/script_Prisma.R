# Instalar Pacote
#install.packages("BiocManager")
#BiocManager::install("EBImage")
#install.packages("metagear")

# Carregar Pacote
library(metagear)

phases <- c(
"START_PHASE: 2061 estudos identificados nas bases de dados",
"233 estudos duplicados removidos",
"1828 estudos analisados por título e resumo",
"EXCLUDE_PHASE: 1718 estudos excluídos",
"110 estudos completos analisados para avaliar elegibilidade",
"EXCLUDE_PHASE: 87 estudos completos excluídos por não aderir aos critérios de elegibilidade",
"seleção final de 23 estudos incluídos na síntese quantitativa (meta-análise)"
)

png("prisma.png", height = 180, width = 360, units = "mm", res = 600)
plot_PRISMA(phases, design = "vintage")
dev.off()
