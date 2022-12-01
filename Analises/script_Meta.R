# Instalar Pacote
#install.packages("meta")

# Carregar Pacote
library(meta)

# Importar base de dados
baseM <- read.table("base_Meta.csv", sep = ";", header = T)

# Estatísticas descritivas
#pop geral
sum(baseM[1:33,'cases'])
sum(baseM[1:33,'sample'])
mean(baseM[1:33,'prevalence']);sd(baseM[1:33,'prevalence']);range(baseM[1:33,'prevalence'])

#mulheres
sum(baseM[34:46,'cases'])
sum(baseM[34:46,'sample'])
mean(baseM[34:46,'prevalence']);sd(baseM[34:46,'prevalence']);range(baseM[34:46,'prevalence'])

#mulheres em idade fértil
sum(baseM[47:55,'cases'])
sum(baseM[47:55,'sample'])
mean(baseM[47:55,'prevalence']);sd(baseM[47:55,'prevalence']);range(baseM[47:55,'prevalence'])

#mulheres grávidas
sum(baseM[56:58,'cases'])
sum(baseM[56:58,'sample'])
mean(baseM[56:58,'prevalence']);sd(baseM[56:58,'prevalence']);range(baseM[56:58,'prevalence'])

#binômio mãe-filho
sum(baseM[59:82,'cases'])
sum(baseM[59:82,'sample'])
mean(baseM[59:82,'prevalence']);sd(baseM[59:82,'prevalence']);range(baseM[59:82,'prevalence'])

# Forest & Funnel plots
#pop geral
m.pop<-metagen(TE=prevalence, seTE=SE, studlab=Study, data=baseM, subset=subgroup=="TabelaS2")
png("forest-mpop.png", units = "mm", width = 250, height = 215, res = 1200)
forest(m.pop, layout = "meta", addrow.overall = T, ff.study.label = "italic", xlim = c(-0.01,0.3), addrows.below.overall = 1, colgap.left = "10 mm", colgap.forest.left = "10 mm", print.tau2 = F, 
col.diamond = "lightblue", col.square="darkblue", big.mark=",")
dev.off()

png("funnel-pop.png", units = "mm", width = 160, height = 130, res = 1200)
par(mar=c(4,4,0.5,0.1))
funnel(m.pop, pch=c(8), cex=2, col = c(1),lty.common=1, lwd.common=1.2, lwd.random=2, col.common=8, col.random=2,xlab='',
ylab='')
mtext("Estimativa de Prevalência",side=1,cex=1.5,line=3)
mtext("Erro Padrão",side=2,cex=1.5,line=2)
legend(0.12,0.04, legend = c("População geral"),pch=c(8), cex=1.5, col = c(1), bty ='n')
dev.off()

#mulheres
m.mul<-metagen(TE=prevalence, seTE=SE, studlab=Study, data=baseM, subset=subgroup=="TabelaS3")
png("forest-mul.png", units = "mm", width = 250, height = 115, res = 1200)
forest(m.mul, layout = "meta", addrow.overall = T, ff.study.label = "italic", xlim = c(-0.01,0.1), addrows.below.overall = 1, colgap.left = "10 mm", colgap.forest.left = "10 mm", print.tau2 = F, 
col.diamond = "lightblue", col.square="darkblue", big.mark=",")
dev.off()

png("funnel-mul.png", units = "mm", width = 160, height = 130, res = 1200)
par(mar=c(4,4,0.5,0.1))
funnel(m.mul, pch=c(19), cex=2, col = c(5),lty.common=1, lwd.common=1.2, lwd.random=2, col.common=8, col.random=2,xlab='',
ylab='')
mtext("Estimativa de Prevalência",side=1,cex=1.5,line=3)
mtext("Erro Padrão",side=2,cex=1.5,line=2)
legend(0.05,0.008, legend = c("Mulheres"),pch=c(19), cex=1.5, col = c(5), bty ='n')
dev.off()

#mulheres em idade fértil
m.mulF<-metagen(TE=prevalence, seTE=SE, studlab=Study, data=baseM, subset=subgroup=="TabelaS4")
png("forest-mulF.png", units = "mm", width = 250, height = 95, res = 1200)
forest(m.mulF, layout = "meta", addrow.overall = T, ff.study.label = "italic", xlim = c(-0.01,0.05), addrows.below.overall = 1, colgap.left = "10 mm", colgap.forest.left = "10 mm", print.tau2 = F, 
col.diamond = "lightblue", col.square="darkblue", big.mark=",", digits = 3)
dev.off()

png("funnel-mulF.png", units = "mm", width = 160, height = 130, res = 1200)
par(mar=c(4,4,0.5,0.1))
funnel(m.mulF, pch=c(17), cex=2, col = c(6),lty.common=1, lwd.common=1.2, lwd.random=2, col.common=8, col.random=2,xlab='',
ylab='')
mtext("Estimativa de Prevalência",side=1,cex=1.5,line=3)
mtext("Erro Padrão",side=2,cex=1.5,line=2)
legend(0.009,0.006, legend = c("Mulheres em idade fértil"),pch=c(17), cex=1.5, col = c(6), bty ='n')
dev.off()

#mulheres grávidas
m.mulG<-metagen(TE=prevalence, seTE=SE, studlab=Study, data=baseM, subset=subgroup=="TabelaS5")
png("forest-mulG.png", units = "mm", width = 270, height = 60, res = 1200)
forest(m.mulG, layout = "meta", addrow.overall = T, ff.study.label = "italic", xlim = c(-0.001,0.003), addrows.below.overall = 1, colgap.left = "10 mm", colgap.forest.left = "10 mm", print.tau2 = F, 
col.diamond = "lightblue", col.square="darkblue", big.mark=",", digits = 4)
dev.off()

png("funnel-mulG.png", units = "mm", width = 160, height = 130, res = 1200)
par(mar=c(4,4,0.5,0.1))
funnel(m.mulG, pch=c(15), cex=2, col = c(7),lty.common=1, lwd.common=1.2, lwd.random=2, col.common=8, col.random=2,xlab='',
ylab='')
mtext("Estimativa de Prevalência",side=1,cex=1.5,line=3)
mtext("Erro Padrão",side=2,cex=1.5,line=2)
legend(0.0002,0.0002, legend = c("Mulheres grávidas"),pch=c(15), cex=1.5, col = c(7), bty ='n')
dev.off()

#binômio mãe-filho
m.binMF<-metagen(TE=prevalence, seTE=SE, studlab=Study, data=baseM, subset=subgroup=="TabelaS6")
png("forest-binMF.png", units = "mm", width = 250, height = 175, res = 1200)
forest(m.binMF, layout = "meta", addrow.overall = T, ff.study.label = "italic", xlim = c(-0.1,0.3), addrows.below.overall = 1, colgap.left = "10 mm", colgap.forest.left = "10 mm", print.tau2 = F, 
col.diamond = "lightblue", col.square="darkblue", big.mark=",")
dev.off()

png("funnel-binMF.png", units = "mm", width = 160, height = 130, res = 1200)
par(mar=c(4,4,0.5,0.1))
funnel(m.binMF,  pch=c(18), cex=2, col = c(8), lty.common=1, lwd.common=1.2, lwd.random=2, col.common=8, col.random=2,xlab='',
ylab='')
mtext("Estimativa de Prevalência",side=1,cex=1.5,line=3)
mtext("Erro Padrão",side=2,cex=1.5,line=2)
legend(-0.115,0.06, legend = c("Binômio mãe-filho"), cex=1.5, pch=c(18),  col = c(8), bty ='n')
dev.off()



