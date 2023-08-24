#instalar pacotes
install.packages(c("mgcv"))
install.packages(c("MuMIn"))

#carregar pacotes
library(MASS)
library(foreign)
library(mgcv)
library(MuMIn)

#importar base de dados
baseTDCApop2<-read.dbf("baseTDCApop2.dbf", as.is=T)

#ver estrutura da base de dados
str(baseTDCApop2)

#padronização de variáveis
str(baseTDCApop2)
baseS<-as.matrix(baseTDCApop2[,-c(1,2,3,5,27,29)])
baseS[1:10,1:23]
baseS.2<-scale(baseS[,-c(16,17,18,19,20)])
baseS.3 <- as.data.frame(baseS.2)
str(baseS.3)
baseS.4 <- as.data.frame<-cbind(baseS.3,baseS[,c(16,17,18,19,20)])
str(baseS.4)

#Subset
basePrevT<-subset(baseS.4, subset = baseS.4$PrevT>=0)
str(basePrevT)
hist(basePrevT$PrevT)

###Modelos
#1 linear univariado
m1.prevT<-lm(PrevT ~ Indice+X+Y+AREA_KM2+PopM, data = basePrevT)
m2.prevT<-lm(PrevT ~ Chagas+X+Y+AREA_KM2+PopM, data = basePrevT)
m3.prevT<-lm(PrevT ~ Sentinela+X+Y+AREA_KM2+PopM, data = basePrevT)
m4.prevT<-lm(PrevT ~ Acesso+X+Y+AREA_KM2+PopM, data = basePrevT)
m5.prevT<-lm(PrevT ~ SIAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m6.prevT<-lm(PrevT ~ MortJ+X+Y+AREA_KM2+PopM, data = basePrevT)
m7.prevT<-lm(PrevT ~ MortV+X+Y+AREA_KM2+PopM, data = basePrevT)
m8.prevT<-lm(PrevT ~ IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m9.prevT<-lm(PrevT ~ IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m10.prevT<-lm(PrevT ~ MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m11.prevT<-lm(PrevT ~ CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m12.prevT<-lm(PrevT ~ Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m13.prevT<-lm(PrevT ~ ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m14.prevT<-lm(PrevT ~ AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

#2 linear bivariado
m15.prevT<-lm(PrevT ~ Chagas+Sentinela+X+Y+AREA_KM2+PopM, data = basePrevT)
m16.prevT<-lm(PrevT ~ Chagas+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT)
m17.prevT<-lm(PrevT ~ Sentinela+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT)

m18.prevT<-lm(PrevT ~ SIAB+MortJ+X+Y+AREA_KM2+PopM, data = basePrevT)
m19.prevT<-lm(PrevT ~ SIAB+MortV+X+Y+AREA_KM2+PopM, data = basePrevT)
m20.prevT<-lm(PrevT ~ SIAB+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m21.prevT<-lm(PrevT ~ SIAB+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m22.prevT<-lm(PrevT ~ SIAB+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m23.prevT<-lm(PrevT ~ SIAB+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m24.prevT<-lm(PrevT ~ SIAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m25.prevT<-lm(PrevT ~ SIAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m26.prevT<-lm(PrevT ~ SIAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m27.prevT<-lm(PrevT ~ MortJ+MortV+X+Y+AREA_KM2+PopM, data = basePrevT)
m28.prevT<-lm(PrevT ~ MortJ+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m29.prevT<-lm(PrevT ~ MortJ+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m30.prevT<-lm(PrevT ~ MortJ+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m31.prevT<-lm(PrevT ~ MortJ+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m32.prevT<-lm(PrevT ~ MortJ+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m33.prevT<-lm(PrevT ~ MortJ+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m34.prevT<-lm(PrevT ~ MortJ+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m35.prevT<-lm(PrevT ~ MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m36.prevT<-lm(PrevT ~ MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m37.prevT<-lm(PrevT ~ MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m38.prevT<-lm(PrevT ~ MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m39.prevT<-lm(PrevT ~ MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m40.prevT<-lm(PrevT ~ MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m41.prevT<-lm(PrevT ~ MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m42.prevT<-lm(PrevT ~ IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m43.prevT<-lm(PrevT ~ IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m44.prevT<-lm(PrevT ~ IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m45.prevT<-lm(PrevT ~ IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m46.prevT<-lm(PrevT ~ IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m47.prevT<-lm(PrevT ~ IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m48.prevT<-lm(PrevT ~ IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m49.prevT<-lm(PrevT ~ IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m50.prevT<-lm(PrevT ~ IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m51.prevT<-lm(PrevT ~ IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m52.prevT<-lm(PrevT ~ IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m53.prevT<-lm(PrevT ~ MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m54.prevT<-lm(PrevT ~ MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m55.prevT<-lm(PrevT ~ MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m56.prevT<-lm(PrevT ~ MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m57.prevT<-lm(PrevT ~ CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m58.prevT<-lm(PrevT ~ CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m59.prevT<-lm(PrevT ~ CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m60.prevT<-lm(PrevT ~ Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m61.prevT<-lm(PrevT ~ Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m62.prevT<-lm(PrevT ~ ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

#3 linear multivariado
m63.prevT<-lm(PrevT ~ Chagas+Sentinela+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT)

m64.prevT<-lm(PrevT ~ SIAB+MortJ+MortV+X+Y+AREA_KM2+PopM, data = basePrevT)
m65.prevT<-lm(PrevT ~ SIAB+MortJ+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m66.prevT<-lm(PrevT ~ SIAB+MortJ+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m67.prevT<-lm(PrevT ~ SIAB+MortJ+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m68.prevT<-lm(PrevT ~ SIAB+MortJ+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m69.prevT<-lm(PrevT ~ SIAB+MortJ+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m70.prevT<-lm(PrevT ~ SIAB+MortJ+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m71.prevT<-lm(PrevT ~ SIAB+MortJ+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m72.prevT<-lm(PrevT ~ SIAB+MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m73.prevT<-lm(PrevT ~ SIAB+MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m74.prevT<-lm(PrevT ~ SIAB+MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m75.prevT<-lm(PrevT ~ SIAB+MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m76.prevT<-lm(PrevT ~ SIAB+MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m77.prevT<-lm(PrevT ~ SIAB+MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m78.prevT<-lm(PrevT ~ SIAB+MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m79.prevT<-lm(PrevT ~ SIAB+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m80.prevT<-lm(PrevT ~ SIAB+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m81.prevT<-lm(PrevT ~ SIAB+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m82.prevT<-lm(PrevT ~ SIAB+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m83.prevT<-lm(PrevT ~ SIAB+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m84.prevT<-lm(PrevT ~ SIAB+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m85.prevT<-lm(PrevT ~ SIAB+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m86.prevT<-lm(PrevT ~ SIAB+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m87.prevT<-lm(PrevT ~ SIAB+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m88.prevT<-lm(PrevT ~ SIAB+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m89.prevT<-lm(PrevT ~ SIAB+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m90.prevT<-lm(PrevT ~ SIAB+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m91.prevT<-lm(PrevT ~ SIAB+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m92.prevT<-lm(PrevT ~ SIAB+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m93.prevT<-lm(PrevT ~ SIAB+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m94.prevT<-lm(PrevT ~ SIAB+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m95.prevT<-lm(PrevT ~ SIAB+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m96.prevT<-lm(PrevT ~ SIAB+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m97.prevT<-lm(PrevT ~ SIAB+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m98.prevT<-lm(PrevT ~ SIAB+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m99.prevT<-lm(PrevT ~ SIAB+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m100.prevT<-lm(PrevT ~ MortJ+MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT)
m101.prevT<-lm(PrevT ~ MortJ+MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m102.prevT<-lm(PrevT ~ MortJ+MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m103.prevT<-lm(PrevT ~ MortJ+MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m104.prevT<-lm(PrevT ~ MortJ+MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m105.prevT<-lm(PrevT ~ MortJ+MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m106.prevT<-lm(PrevT ~ MortJ+MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m107.prevT<-lm(PrevT ~ MortJ+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m108.prevT<-lm(PrevT ~ MortJ+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m109.prevT<-lm(PrevT ~ MortJ+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m110.prevT<-lm(PrevT ~ MortJ+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m111.prevT<-lm(PrevT ~ MortJ+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m112.prevT<-lm(PrevT ~ MortJ+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m113.prevT<-lm(PrevT ~ MortJ+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m114.prevT<-lm(PrevT ~ MortJ+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m115.prevT<-lm(PrevT ~ MortJ+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m116.prevT<-lm(PrevT ~ MortJ+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m117.prevT<-lm(PrevT ~ MortJ+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m118.prevT<-lm(PrevT ~ MortJ+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m119.prevT<-lm(PrevT ~ MortJ+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m120.prevT<-lm(PrevT ~ MortJ+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m121.prevT<-lm(PrevT ~ MortJ+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m122.prevT<-lm(PrevT ~ MortJ+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m123.prevT<-lm(PrevT ~ MortJ+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m124.prevT<-lm(PrevT ~ MortJ+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m125.prevT<-lm(PrevT ~ MortJ+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m126.prevT<-lm(PrevT ~ MortJ+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m127.prevT<-lm(PrevT ~ MortJ+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m128.prevT<-lm(PrevT ~ MortV+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT)
m129.prevT<-lm(PrevT ~ MortV+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m130.prevT<-lm(PrevT ~ MortV+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m131.prevT<-lm(PrevT ~ MortV+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m132.prevT<-lm(PrevT ~ MortV+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m133.prevT<-lm(PrevT ~ MortV+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m134.prevT<-lm(PrevT ~ MortV+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m135.prevT<-lm(PrevT ~ MortV+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m136.prevT<-lm(PrevT ~ MortV+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m137.prevT<-lm(PrevT ~ MortV+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m138.prevT<-lm(PrevT ~ MortV+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m139.prevT<-lm(PrevT ~ MortV+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m140.prevT<-lm(PrevT ~ MortV+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m141.prevT<-lm(PrevT ~ MortV+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m142.prevT<-lm(PrevT ~ MortV+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m143.prevT<-lm(PrevT ~ MortV+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m144.prevT<-lm(PrevT ~ MortV+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m145.prevT<-lm(PrevT ~ MortV+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m146.prevT<-lm(PrevT ~ MortV+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m147.prevT<-lm(PrevT ~ MortV+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m148.prevT<-lm(PrevT ~ MortV+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m149.prevT<-lm(PrevT ~ IntDC+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT)
m150.prevT<-lm(PrevT ~ IntDC+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m151.prevT<-lm(PrevT ~ IntDC+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m152.prevT<-lm(PrevT ~ IntDC+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m153.prevT<-lm(PrevT ~ IntDC+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m154.prevT<-lm(PrevT ~ IntDC+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m155.prevT<-lm(PrevT ~ IntDC+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m156.prevT<-lm(PrevT ~ IntDC+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m157.prevT<-lm(PrevT ~ IntDC+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m158.prevT<-lm(PrevT ~ IntDC+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m159.prevT<-lm(PrevT ~ IntDC+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m160.prevT<-lm(PrevT ~ IntDC+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m161.prevT<-lm(PrevT ~ IntDC+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m162.prevT<-lm(PrevT ~ IntDC+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m163.prevT<-lm(PrevT ~ IntDC+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m164.prevT<-lm(PrevT ~ IntIC+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT)
m165.prevT<-lm(PrevT ~ IntIC+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m166.prevT<-lm(PrevT ~ IntIC+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m167.prevT<-lm(PrevT ~ IntIC+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m168.prevT<-lm(PrevT ~ IntIC+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m169.prevT<-lm(PrevT ~ IntIC+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m170.prevT<-lm(PrevT ~ IntIC+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m171.prevT<-lm(PrevT ~ IntIC+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m172.prevT<-lm(PrevT ~ IntIC+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m173.prevT<-lm(PrevT ~ IntIC+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m174.prevT<-lm(PrevT ~ MortS+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT)
m175.prevT<-lm(PrevT ~ MortS+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m176.prevT<-lm(PrevT ~ MortS+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m177.prevT<-lm(PrevT ~ MortS+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m178.prevT<-lm(PrevT ~ MortS+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m179.prevT<-lm(PrevT ~ MortS+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m180.prevT<-lm(PrevT ~ CobAB+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT)
m181.prevT<-lm(PrevT ~ CobAB+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m182.prevT<-lm(PrevT ~ CobAB+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

m183.prevT<-lm(PrevT ~ Proc+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT)

#4 glm univariado com distribuição gama
basePrevT2<-subset(baseS.4, subset = baseS.4$PrevT>0)
str(basePrevT2)

m1a.prevT<-glm(PrevT ~ Indice+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m2a.prevT<-glm(PrevT ~ Chagas+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m3a.prevT<-glm(PrevT ~ Sentinela+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m4a.prevT<-glm(PrevT ~ Acesso+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m5a.prevT<-glm(PrevT ~ SIAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m6a.prevT<-glm(PrevT ~ MortJ+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m7a.prevT<-glm(PrevT ~ MortV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m8a.prevT<-glm(PrevT ~ IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m9a.prevT<-glm(PrevT ~ IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m10a.prevT<-glm(PrevT ~ MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m11a.prevT<-glm(PrevT ~ CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m12a.prevT<-glm(PrevT ~ Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m13a.prevT<-glm(PrevT ~ ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m14a.prevT<-glm(PrevT ~ AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

#5 glm bivariado com distribuição gama
m15a.prevT<-glm(PrevT ~ Chagas+Sentinela+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m16a.prevT<-glm(PrevT ~ Chagas+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m17a.prevT<-glm(PrevT ~ Sentinela+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m18a.prevT<-glm(PrevT ~ SIAB+MortJ+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m19a.prevT<-glm(PrevT ~ SIAB+MortV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m20a.prevT<-glm(PrevT ~ SIAB+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m21a.prevT<-glm(PrevT ~ SIAB+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m22a.prevT<-glm(PrevT ~ SIAB+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m23a.prevT<-glm(PrevT ~ SIAB+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m24a.prevT<-glm(PrevT ~ SIAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m25a.prevT<-glm(PrevT ~ SIAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m26a.prevT<-glm(PrevT ~ SIAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m27a.prevT<-glm(PrevT ~ MortJ+MortV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m28a.prevT<-glm(PrevT ~ MortJ+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m29a.prevT<-glm(PrevT ~ MortJ+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m30a.prevT<-glm(PrevT ~ MortJ+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m31a.prevT<-glm(PrevT ~ MortJ+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m32a.prevT<-glm(PrevT ~ MortJ+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m33a.prevT<-glm(PrevT ~ MortJ+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m34a.prevT<-glm(PrevT ~ MortJ+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m35a.prevT<-glm(PrevT ~ MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m36a.prevT<-glm(PrevT ~ MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m37a.prevT<-glm(PrevT ~ MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m38a.prevT<-glm(PrevT ~ MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m39a.prevT<-glm(PrevT ~ MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m40a.prevT<-glm(PrevT ~ MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m41a.prevT<-glm(PrevT ~ MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m42a.prevT<-glm(PrevT ~ IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m43a.prevT<-glm(PrevT ~ IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m44a.prevT<-glm(PrevT ~ IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m45a.prevT<-glm(PrevT ~ IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m46a.prevT<-glm(PrevT ~ IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m47a.prevT<-glm(PrevT ~ IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m48a.prevT<-glm(PrevT ~ IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m49a.prevT<-glm(PrevT ~ IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m50a.prevT<-glm(PrevT ~ IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m51a.prevT<-glm(PrevT ~ IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m52a.prevT<-glm(PrevT ~ IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m53a.prevT<-glm(PrevT ~ MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m54a.prevT<-glm(PrevT ~ MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m55a.prevT<-glm(PrevT ~ MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m56a.prevT<-glm(PrevT ~ MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m57a.prevT<-glm(PrevT ~ CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m58a.prevT<-glm(PrevT ~ CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m59a.prevT<-glm(PrevT ~ CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m60a.prevT<-glm(PrevT ~ Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m61a.prevT<-glm(PrevT ~ Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m62a.prevT<-glm(PrevT ~ ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

#6 glm multivariado com distribuição gama
m63a.prevT<-glm(PrevT ~ Chagas+Sentinela+Acesso+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m64a.prevT<-glm(PrevT ~ SIAB+MortJ+MortV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m65a.prevT<-glm(PrevT ~ SIAB+MortJ+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m66a.prevT<-glm(PrevT ~ SIAB+MortJ+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m67a.prevT<-glm(PrevT ~ SIAB+MortJ+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m68a.prevT<-glm(PrevT ~ SIAB+MortJ+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m69a.prevT<-glm(PrevT ~ SIAB+MortJ+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m70a.prevT<-glm(PrevT ~ SIAB+MortJ+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m71a.prevT<-glm(PrevT ~ SIAB+MortJ+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m72a.prevT<-glm(PrevT ~ SIAB+MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m73a.prevT<-glm(PrevT ~ SIAB+MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m74a.prevT<-glm(PrevT ~ SIAB+MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m75a.prevT<-glm(PrevT ~ SIAB+MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m76a.prevT<-glm(PrevT ~ SIAB+MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m77a.prevT<-glm(PrevT ~ SIAB+MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m78a.prevT<-glm(PrevT ~ SIAB+MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m79a.prevT<-glm(PrevT ~ SIAB+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m80a.prevT<-glm(PrevT ~ SIAB+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m81a.prevT<-glm(PrevT ~ SIAB+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m82a.prevT<-glm(PrevT ~ SIAB+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m83a.prevT<-glm(PrevT ~ SIAB+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m84a.prevT<-glm(PrevT ~ SIAB+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m85a.prevT<-glm(PrevT ~ SIAB+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m86a.prevT<-glm(PrevT ~ SIAB+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m87a.prevT<-glm(PrevT ~ SIAB+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m88a.prevT<-glm(PrevT ~ SIAB+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m89a.prevT<-glm(PrevT ~ SIAB+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m90a.prevT<-glm(PrevT ~ SIAB+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m91a.prevT<-glm(PrevT ~ SIAB+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m92a.prevT<-glm(PrevT ~ SIAB+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m93a.prevT<-glm(PrevT ~ SIAB+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m94a.prevT<-glm(PrevT ~ SIAB+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m95a.prevT<-glm(PrevT ~ SIAB+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m96a.prevT<-glm(PrevT ~ SIAB+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m97a.prevT<-glm(PrevT ~ SIAB+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m98a.prevT<-glm(PrevT ~ SIAB+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m99a.prevT<-glm(PrevT ~ SIAB+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m100a.prevT<-glm(PrevT ~ MortJ+MortV+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m101a.prevT<-glm(PrevT ~ MortJ+MortV+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m102a.prevT<-glm(PrevT ~ MortJ+MortV+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m103a.prevT<-glm(PrevT ~ MortJ+MortV+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m104a.prevT<-glm(PrevT ~ MortJ+MortV+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m105a.prevT<-glm(PrevT ~ MortJ+MortV+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m106a.prevT<-glm(PrevT ~ MortJ+MortV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m107a.prevT<-glm(PrevT ~ MortJ+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m108a.prevT<-glm(PrevT ~ MortJ+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m109a.prevT<-glm(PrevT ~ MortJ+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m110a.prevT<-glm(PrevT ~ MortJ+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m111a.prevT<-glm(PrevT ~ MortJ+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m112a.prevT<-glm(PrevT ~ MortJ+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m113a.prevT<-glm(PrevT ~ MortJ+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m114a.prevT<-glm(PrevT ~ MortJ+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m115a.prevT<-glm(PrevT ~ MortJ+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m116a.prevT<-glm(PrevT ~ MortJ+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m117a.prevT<-glm(PrevT ~ MortJ+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m118a.prevT<-glm(PrevT ~ MortJ+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m119a.prevT<-glm(PrevT ~ MortJ+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m120a.prevT<-glm(PrevT ~ MortJ+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m121a.prevT<-glm(PrevT ~ MortJ+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m122a.prevT<-glm(PrevT ~ MortJ+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m123a.prevT<-glm(PrevT ~ MortJ+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m124a.prevT<-glm(PrevT ~ MortJ+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m125a.prevT<-glm(PrevT ~ MortJ+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m126a.prevT<-glm(PrevT ~ MortJ+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m127a.prevT<-glm(PrevT ~ MortJ+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m128a.prevT<-glm(PrevT ~ MortV+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m129a.prevT<-glm(PrevT ~ MortV+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m130a.prevT<-glm(PrevT ~ MortV+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m131a.prevT<-glm(PrevT ~ MortV+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m132a.prevT<-glm(PrevT ~ MortV+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m133a.prevT<-glm(PrevT ~ MortV+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m134a.prevT<-glm(PrevT ~ MortV+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m135a.prevT<-glm(PrevT ~ MortV+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m136a.prevT<-glm(PrevT ~ MortV+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m137a.prevT<-glm(PrevT ~ MortV+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m138a.prevT<-glm(PrevT ~ MortV+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m139a.prevT<-glm(PrevT ~ MortV+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m140a.prevT<-glm(PrevT ~ MortV+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m141a.prevT<-glm(PrevT ~ MortV+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m142a.prevT<-glm(PrevT ~ MortV+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m143a.prevT<-glm(PrevT ~ MortV+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m144a.prevT<-glm(PrevT ~ MortV+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m145a.prevT<-glm(PrevT ~ MortV+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m146a.prevT<-glm(PrevT ~ MortV+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m147a.prevT<-glm(PrevT ~ MortV+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m148a.prevT<-glm(PrevT ~ MortV+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m149a.prevT<-glm(PrevT ~ IntDC+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m150a.prevT<-glm(PrevT ~ IntDC+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m151a.prevT<-glm(PrevT ~ IntDC+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m152a.prevT<-glm(PrevT ~ IntDC+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m153a.prevT<-glm(PrevT ~ IntDC+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m154a.prevT<-glm(PrevT ~ IntDC+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m155a.prevT<-glm(PrevT ~ IntDC+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m156a.prevT<-glm(PrevT ~ IntDC+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m157a.prevT<-glm(PrevT ~ IntDC+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m158a.prevT<-glm(PrevT ~ IntDC+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m159a.prevT<-glm(PrevT ~ IntDC+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m160a.prevT<-glm(PrevT ~ IntDC+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m161a.prevT<-glm(PrevT ~ IntDC+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m162a.prevT<-glm(PrevT ~ IntDC+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m163a.prevT<-glm(PrevT ~ IntDC+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m164a.prevT<-glm(PrevT ~ IntIC+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m165a.prevT<-glm(PrevT ~ IntIC+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m166a.prevT<-glm(PrevT ~ IntIC+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m167a.prevT<-glm(PrevT ~ IntIC+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m168a.prevT<-glm(PrevT ~ IntIC+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m169a.prevT<-glm(PrevT ~ IntIC+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m170a.prevT<-glm(PrevT ~ IntIC+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m171a.prevT<-glm(PrevT ~ IntIC+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m172a.prevT<-glm(PrevT ~ IntIC+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m173a.prevT<-glm(PrevT ~ IntIC+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m174a.prevT<-glm(PrevT ~ MortS+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m175a.prevT<-glm(PrevT ~ MortS+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m176a.prevT<-glm(PrevT ~ MortS+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m177a.prevT<-glm(PrevT ~ MortS+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m178a.prevT<-glm(PrevT ~ MortS+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m179a.prevT<-glm(PrevT ~ MortS+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m180a.prevT<-glm(PrevT ~ CobAB+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())
m181a.prevT<-glm(PrevT ~ CobAB+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m182a.prevT<-glm(PrevT ~ CobAB+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())

m183a.prevT<-glm(PrevT ~ Proc+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT2, family=Gamma())


#7 gam univariado 
m1g.prevT<-gam(PrevT ~ s(Indice)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m2g.prevT<-gam(PrevT ~ s(Chagas)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m3g.prevT<-gam(PrevT ~ s(Sentinela)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m4g.prevT<-gam(PrevT ~ s(Acesso)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m5g.prevT<-gam(PrevT ~ s(SIAB)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m6g.prevT<-gam(PrevT ~ s(MortJ)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m7g.prevT<-gam(PrevT ~ s(MortV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m8g.prevT<-gam(PrevT ~ IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m9g.prevT<-gam(PrevT ~ s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m10g.prevT<-gam(PrevT ~ s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m11g.prevT<-gam(PrevT ~ CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m12g.prevT<-gam(PrevT ~ s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m13g.prevT<-gam(PrevT ~ s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m14g.prevT<-gam(PrevT ~ AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

#8 gam bivariado
m15g.prevT<-gam(PrevT ~ s(Chagas)+s(Sentinela)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m16g.prevT<-gam(PrevT ~ s(Chagas)+s(Acesso)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m17g.prevT<-gam(PrevT ~ s(Sentinela)+s(Acesso)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m18g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m19g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m20g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m21g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m22g.prevT<-gam(PrevT ~ s(SIAB)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m23g.prevT<-gam(PrevT ~ s(SIAB)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m24g.prevT<-gam(PrevT ~ s(SIAB)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m25g.prevT<-gam(PrevT ~ s(SIAB)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m26g.prevT<-gam(PrevT ~ s(SIAB)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m27g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m28g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m29g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m30g.prevT<-gam(PrevT ~ s(MortJ)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m31g.prevT<-gam(PrevT ~ s(MortJ)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m32g.prevT<-gam(PrevT ~ s(MortJ)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m33g.prevT<-gam(PrevT ~ s(MortJ)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m34g.prevT<-gam(PrevT ~ s(MortJ)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m35g.prevT<-gam(PrevT ~ s(MortV)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m36g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m37g.prevT<-gam(PrevT ~ s(MortV)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m38g.prevT<-gam(PrevT ~ s(MortV)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m39g.prevT<-gam(PrevT ~ s(MortV)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m40g.prevT<-gam(PrevT ~ s(MortV)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m41g.prevT<-gam(PrevT ~ s(MortV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m42g.prevT<-gam(PrevT ~ IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m43g.prevT<-gam(PrevT ~ IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m44g.prevT<-gam(PrevT ~ IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m45g.prevT<-gam(PrevT ~ IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m46g.prevT<-gam(PrevT ~ IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m47g.prevT<-gam(PrevT ~ IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m48g.prevT<-gam(PrevT ~ s(IntIC)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m49g.prevT<-gam(PrevT ~ s(IntIC)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m50g.prevT<-gam(PrevT ~ s(IntIC)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m51g.prevT<-gam(PrevT ~ s(IntIC)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m52g.prevT<-gam(PrevT ~ s(IntIC)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m53g.prevT<-gam(PrevT ~ s(MortS)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m54g.prevT<-gam(PrevT ~ s(MortS)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m55g.prevT<-gam(PrevT ~ s(MortS)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m56g.prevT<-gam(PrevT ~ s(MortS)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m57g.prevT<-gam(PrevT ~ CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m58g.prevT<-gam(PrevT ~ CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m59g.prevT<-gam(PrevT ~ CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m60g.prevT<-gam(PrevT ~ s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m61g.prevT<-gam(PrevT ~ s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m62g.prevT<-gam(PrevT ~ s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

#9 gam multivariado
m63g.prevT<-gam(PrevT ~ s(Chagas)+s(Sentinela)+s(Acesso)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m64g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+s(MortV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m65g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m66g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m67g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m68g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m69g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m70g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m71g.prevT<-gam(PrevT ~ s(SIAB)+s(MortJ)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m72g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m73g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m74g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m75g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m76g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m77g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m78g.prevT<-gam(PrevT ~ s(SIAB)+s(MortV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m79g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m80g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m81g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m82g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m83g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m84g.prevT<-gam(PrevT ~ s(SIAB)+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m85g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m86g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m87g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m88g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m89g.prevT<-gam(PrevT ~ s(SIAB)+s(IntIC)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m90g.prevT<-gam(PrevT ~ s(SIAB)+s(MortS)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m91g.prevT<-gam(PrevT ~ s(SIAB)+s(MortS)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m92g.prevT<-gam(PrevT ~ s(SIAB)+s(MortS)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m93g.prevT<-gam(PrevT ~ s(SIAB)+s(MortS)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m94g.prevT<-gam(PrevT ~ s(SIAB)+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m95g.prevT<-gam(PrevT ~ s(SIAB)+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m96g.prevT<-gam(PrevT ~ s(SIAB)+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m97g.prevT<-gam(PrevT ~ s(SIAB)+s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m98g.prevT<-gam(PrevT ~ s(SIAB)+s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m99g.prevT<-gam(PrevT ~ s(SIAB)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m100g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+IntDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m101g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+s(IntIC)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m102g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m103g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m104g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m105g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m106g.prevT<-gam(PrevT ~ s(MortJ)+s(MortV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m107g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m108g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m109g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m110g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m111g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m112g.prevT<-gam(PrevT ~ s(MortJ)+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m113g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m114g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m115g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m116g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m117g.prevT<-gam(PrevT ~ s(MortJ)+s(IntIC)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m118g.prevT<-gam(PrevT ~ s(MortJ)+s(MortS)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m119g.prevT<-gam(PrevT ~ s(MortJ)+s(MortS)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m120g.prevT<-gam(PrevT ~ s(MortJ)+s(MortS)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m121g.prevT<-gam(PrevT ~ s(MortJ)+s(MortS)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m122g.prevT<-gam(PrevT ~ s(MortJ)+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m123g.prevT<-gam(PrevT ~ s(MortJ)+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m124g.prevT<-gam(PrevT ~ s(MortJ)+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m125g.prevT<-gam(PrevT ~ s(MortJ)+s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m126g.prevT<-gam(PrevT ~ s(MortJ)+s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m127g.prevT<-gam(PrevT ~ s(MortJ)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m128g.prevT<-gam(PrevT ~ s(MortV)+IntDC+IntIC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m129g.prevT<-gam(PrevT ~ s(MortV)+IntDC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m130g.prevT<-gam(PrevT ~ s(MortV)+IntDC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m131g.prevT<-gam(PrevT ~ s(MortV)+IntDC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m132g.prevT<-gam(PrevT ~ s(MortV)+IntDC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m133g.prevT<-gam(PrevT ~ s(MortV)+IntDC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m134g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+s(MortS)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m135g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m136g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m137g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m138g.prevT<-gam(PrevT ~ s(MortV)+s(IntIC)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m139g.prevT<-gam(PrevT ~ s(MortV)+s(MortS)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m140g.prevT<-gam(PrevT ~ s(MortV)+s(MortS)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m141g.prevT<-gam(PrevT ~ s(MortV)+s(MortS)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m142g.prevT<-gam(PrevT ~ s(MortV)+s(MortS)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m143g.prevT<-gam(PrevT ~ s(MortV)+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m144g.prevT<-gam(PrevT ~ s(MortV)+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m145g.prevT<-gam(PrevT ~ s(MortV)+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m146g.prevT<-gam(PrevT ~ s(MortV)+s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m147g.prevT<-gam(PrevT ~ s(MortV)+s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m148g.prevT<-gam(PrevT ~ s(MortV)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m149g.prevT<-gam(PrevT ~ IntDC+IntIC+MortS+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m150g.prevT<-gam(PrevT ~ IntDC+IntIC+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m151g.prevT<-gam(PrevT ~ IntDC+IntIC+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m152g.prevT<-gam(PrevT ~ IntDC+IntIC+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m153g.prevT<-gam(PrevT ~ IntDC+IntIC+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m154g.prevT<-gam(PrevT ~ IntDC+MortS+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m155g.prevT<-gam(PrevT ~ IntDC+MortS+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m156g.prevT<-gam(PrevT ~ IntDC+MortS+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m157g.prevT<-gam(PrevT ~ IntDC+MortS+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m158g.prevT<-gam(PrevT ~ IntDC+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m159g.prevT<-gam(PrevT ~ IntDC+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m160g.prevT<-gam(PrevT ~ IntDC+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m161g.prevT<-gam(PrevT ~ IntDC+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m162g.prevT<-gam(PrevT ~ IntDC+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m163g.prevT<-gam(PrevT ~ IntDC+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m164g.prevT<-gam(PrevT ~ s(IntIC)+s(MortS)+CobAB+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m165g.prevT<-gam(PrevT ~ s(IntIC)+s(MortS)+s(Proc)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m166g.prevT<-gam(PrevT ~ s(IntIC)+s(MortS)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m167g.prevT<-gam(PrevT ~ s(IntIC)+s(MortS)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m168g.prevT<-gam(PrevT ~ s(IntIC)+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m169g.prevT<-gam(PrevT ~ s(IntIC)+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m170g.prevT<-gam(PrevT ~ s(IntIC)+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m171g.prevT<-gam(PrevT ~ s(IntIC)+s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m172g.prevT<-gam(PrevT ~ s(IntIC)+s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m173g.prevT<-gam(PrevT ~ s(IntIC)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m174g.prevT<-gam(PrevT ~ s(MortS)+CobAB+Proc+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m175g.prevT<-gam(PrevT ~ s(MortS)+CobAB+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m176g.prevT<-gam(PrevT ~ s(MortS)+CobAB+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m177g.prevT<-gam(PrevT ~ s(MortS)+s(Proc)+s(ProcV)+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m178g.prevT<-gam(PrevT ~ s(MortS)+s(Proc)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m179g.prevT<-gam(PrevT ~ s(MortS)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m180g.prevT<-gam(PrevT ~ CobAB+Proc+ProcV+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")
m181g.prevT<-gam(PrevT ~ CobAB+Proc+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m182g.prevT<-gam(PrevT ~ CobAB+ProcV+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")

m183g.prevT<-gam(PrevT ~ s(Proc)+s(ProcV)+AIHICDC+X+Y+AREA_KM2+PopM, data = basePrevT, method="REML")





#Selecao de modelos
selec1<-AICc(
m1.prevT,
m2.prevT,
m3.prevT,
m4.prevT,
m5.prevT,
m6.prevT,
m7.prevT,
m8.prevT,
m9.prevT,
m10.prevT,
m11.prevT,
m12.prevT,
m13.prevT,
m14.prevT,
m15.prevT,
m16.prevT,
m17.prevT,
m18.prevT,
m19.prevT,
m20.prevT,
m21.prevT,
m22.prevT,
m23.prevT,
m24.prevT,
m25.prevT,
m26.prevT,
m27.prevT,
m28.prevT,
m29.prevT,
m30.prevT,
m31.prevT,
m32.prevT,
m33.prevT,
m34.prevT,
m35.prevT,
m36.prevT,
m37.prevT,
m38.prevT,
m39.prevT,
m40.prevT,
m41.prevT,
m42.prevT,
m43.prevT,
m44.prevT,
m45.prevT,
m46.prevT,
m47.prevT,
m48.prevT,
m49.prevT,
m50.prevT,
m51.prevT,
m52.prevT,
m53.prevT,
m54.prevT,
m55.prevT,
m56.prevT,
m57.prevT,
m58.prevT,
m59.prevT,
m60.prevT,
m61.prevT,
m62.prevT,
m63.prevT,
m64.prevT,
m65.prevT,
m66.prevT,
m67.prevT,
m68.prevT,
m69.prevT,
m70.prevT,
m71.prevT,
m72.prevT,
m73.prevT,
m74.prevT,
m75.prevT,
m76.prevT,
m77.prevT,
m78.prevT,
m79.prevT,
m80.prevT,
m81.prevT,
m82.prevT,
m83.prevT,
m84.prevT,
m85.prevT,
m86.prevT,
m87.prevT,
m88.prevT,
m89.prevT,
m90.prevT,
m91.prevT,
m92.prevT,
m93.prevT,
m94.prevT,
m95.prevT,
m96.prevT,
m97.prevT,
m98.prevT,
m99.prevT,
m100.prevT,
m101.prevT,
m102.prevT,
m103.prevT,
m104.prevT,
m105.prevT,
m106.prevT,
m107.prevT,
m108.prevT,
m109.prevT,
m110.prevT,
m111.prevT,
m112.prevT,
m113.prevT,
m114.prevT,
m115.prevT,
m116.prevT,
m117.prevT,
m118.prevT,
m119.prevT,
m120.prevT,
m121.prevT,
m122.prevT,
m123.prevT,
m124.prevT,
m125.prevT,
m126.prevT,
m127.prevT,
m128.prevT,
m129.prevT,
m130.prevT,
m131.prevT,
m132.prevT,
m133.prevT,
m134.prevT,
m135.prevT,
m136.prevT,
m137.prevT,
m138.prevT,
m139.prevT,
m140.prevT,
m141.prevT,
m142.prevT,
m143.prevT,
m144.prevT,
m145.prevT,
m146.prevT,
m147.prevT,
m148.prevT,
m149.prevT,
m150.prevT,
m151.prevT,
m152.prevT,
m153.prevT,
m154.prevT,
m155.prevT,
m156.prevT,
m157.prevT,
m158.prevT,
m159.prevT,
m160.prevT,
m161.prevT,
m162.prevT,
m163.prevT,
m164.prevT,
m165.prevT,
m166.prevT,
m167.prevT,
m168.prevT,
m169.prevT,
m170.prevT,
m171.prevT,
m172.prevT,
m173.prevT,
m174.prevT,
m175.prevT,
m176.prevT,
m177.prevT,
m178.prevT,
m179.prevT,
m180.prevT,
m181.prevT,
m182.prevT,
m183.prevT,
m1a.prevT,
m2a.prevT,
m3a.prevT,
m4a.prevT,
m5a.prevT,
m6a.prevT,
m7a.prevT,
m8a.prevT,
m9a.prevT,
m10a.prevT,
m11a.prevT,
m12a.prevT,
m13a.prevT,
m14a.prevT,
m15a.prevT,
m16a.prevT,
m17a.prevT,
m18a.prevT,
m19a.prevT,
m20a.prevT,
m21a.prevT,
m22a.prevT,
m23a.prevT,
m24a.prevT,
m25a.prevT,
m26a.prevT,
m27a.prevT,
m28a.prevT,
m29a.prevT,
m30a.prevT,
m31a.prevT,
m32a.prevT,
m33a.prevT,
m34a.prevT,
m35a.prevT,
m36a.prevT,
m37a.prevT,
m38a.prevT,
m39a.prevT,
m40a.prevT,
m41a.prevT,
m42a.prevT,
m43a.prevT,
m44a.prevT,
m45a.prevT,
m46a.prevT,
m47a.prevT,
m48a.prevT,
m49a.prevT,
m50a.prevT,
m51a.prevT,
m52a.prevT,
m53a.prevT,
m54a.prevT,
m55a.prevT,
m56a.prevT,
m57a.prevT,
m58a.prevT,
m59a.prevT,
m60a.prevT,
m61a.prevT,
m62a.prevT,
m63a.prevT,
m64a.prevT,
m65a.prevT,
m66a.prevT,
m67a.prevT,
m68a.prevT,
m69a.prevT,
m70a.prevT,
m71a.prevT,
m72a.prevT,
m73a.prevT,
m74a.prevT,
m75a.prevT,
m76a.prevT,
m77a.prevT,
m78a.prevT,
m79a.prevT,
m80a.prevT,
m81a.prevT,
m82a.prevT,
m83a.prevT,
m84a.prevT,
m85a.prevT,
m86a.prevT,
m87a.prevT,
m88a.prevT,
m89a.prevT,
m90a.prevT,
m91a.prevT,
m92a.prevT,
m93a.prevT,
m94a.prevT,
m95a.prevT,
m96a.prevT,
m97a.prevT,
m98a.prevT,
m99a.prevT,
m100a.prevT,
m101a.prevT,
m102a.prevT,
m103a.prevT,
m104a.prevT,
m105a.prevT,
m106a.prevT,
m107a.prevT,
m108a.prevT,
m109a.prevT,
m110a.prevT,
m111a.prevT,
m112a.prevT,
m113a.prevT,
m114a.prevT,
m115a.prevT,
m116a.prevT,
m117a.prevT,
m118a.prevT,
m119a.prevT,
m120a.prevT,
m121a.prevT,
m122a.prevT,
m123a.prevT,
m124a.prevT,
m125a.prevT,
m126a.prevT,
m127a.prevT,
m128a.prevT,
m129a.prevT,
m130a.prevT,
m131a.prevT,
m132a.prevT,
m133a.prevT,
m134a.prevT,
m135a.prevT,
m136a.prevT,
m137a.prevT,
m138a.prevT,
m139a.prevT,
m140a.prevT,
m141a.prevT,
m142a.prevT,
m143a.prevT,
m144a.prevT,
m145a.prevT,
m146a.prevT,
m147a.prevT,
m148a.prevT,
m149a.prevT,
m150a.prevT,
m151a.prevT,
m152a.prevT,
m153a.prevT,
m154a.prevT,
m155a.prevT,
m156a.prevT,
m157a.prevT,
m158a.prevT,
m159a.prevT,
m160a.prevT,
m161a.prevT,
m162a.prevT,
m163a.prevT,
m164a.prevT,
m165a.prevT,
m166a.prevT,
m167a.prevT,
m168a.prevT,
m169a.prevT,
m170a.prevT,
m171a.prevT,
m172a.prevT,
m173a.prevT,
m174a.prevT,
m175a.prevT,
m176a.prevT,
m177a.prevT,
m178a.prevT,
m179a.prevT,
m180a.prevT,
m181a.prevT,
m182a.prevT,
m183a.prevT,
m1g.prevT,
m2g.prevT,
m3g.prevT,
m4g.prevT,
m5g.prevT,
m6g.prevT,
m7g.prevT,
m8g.prevT,
m9g.prevT,
m10g.prevT,
m11g.prevT,
m12g.prevT,
m13g.prevT,
m14g.prevT,
m15g.prevT,
m16g.prevT,
m17g.prevT,
m18g.prevT,
m19g.prevT,
m20g.prevT,
m21g.prevT,
m22g.prevT,
m23g.prevT,
m24g.prevT,
m25g.prevT,
m26g.prevT,
m27g.prevT,
m28g.prevT,
m29g.prevT,
m30g.prevT,
m31g.prevT,
m32g.prevT,
m33g.prevT,
m34g.prevT,
m35g.prevT,
m36g.prevT,
m37g.prevT,
m38g.prevT,
m39g.prevT,
m40g.prevT,
m41g.prevT,
m42g.prevT,
m43g.prevT,
m44g.prevT,
m45g.prevT,
m46g.prevT,
m47g.prevT,
m48g.prevT,
m49g.prevT,
m50g.prevT,
m51g.prevT,
m52g.prevT,
m53g.prevT,
m54g.prevT,
m55g.prevT,
m56g.prevT,
m57g.prevT,
m58g.prevT,
m59g.prevT,
m60g.prevT,
m61g.prevT,
m62g.prevT,
m63g.prevT,
m64g.prevT,
m65g.prevT,
m66g.prevT,
m67g.prevT,
m68g.prevT,
m69g.prevT,
m70g.prevT,
m71g.prevT,
m72g.prevT,
m73g.prevT,
m74g.prevT,
m75g.prevT,
m76g.prevT,
m77g.prevT,
m78g.prevT,
m79g.prevT,
m80g.prevT,
m81g.prevT,
m82g.prevT,
m83g.prevT,
m84g.prevT,
m85g.prevT,
m86g.prevT,
m87g.prevT,
m88g.prevT,
m89g.prevT,
m90g.prevT,
m91g.prevT,
m92g.prevT,
m93g.prevT,
m94g.prevT,
m95g.prevT,
m96g.prevT,
m97g.prevT,
m98g.prevT,
m99g.prevT,
m100g.prevT,
m101g.prevT,
m102g.prevT,
m103g.prevT,
m104g.prevT,
m105g.prevT,
m106g.prevT,
m107g.prevT,
m108g.prevT,
m109g.prevT,
m110g.prevT,
m111g.prevT,
m112g.prevT,
m113g.prevT,
m114g.prevT,
m115g.prevT,
m116g.prevT,
m117g.prevT,
m118g.prevT,
m119g.prevT,
m120g.prevT,
m121g.prevT,
m122g.prevT,
m123g.prevT,
m124g.prevT,
m125g.prevT,
m126g.prevT,
m127g.prevT,
m128g.prevT,
m129g.prevT,
m130g.prevT,
m131g.prevT,
m132g.prevT,
m133g.prevT,
m134g.prevT,
m135g.prevT,
m136g.prevT,
m137g.prevT,
m138g.prevT,
m139g.prevT,
m140g.prevT,
m141g.prevT,
m142g.prevT,
m143g.prevT,
m144g.prevT,
m145g.prevT,
m146g.prevT,
m147g.prevT,
m148g.prevT,
m149g.prevT,
m150g.prevT,
m151g.prevT,
m152g.prevT,
m153g.prevT,
m154g.prevT,
m155g.prevT,
m156g.prevT,
m157g.prevT,
m158g.prevT,
m159g.prevT,
m160g.prevT,
m161g.prevT,
m162g.prevT,
m163g.prevT,
m164g.prevT,
m165g.prevT,
m166g.prevT,
m167g.prevT,
m168g.prevT,
m169g.prevT,
m170g.prevT,
m171g.prevT,
m172g.prevT,
m173g.prevT,
m174g.prevT,
m175g.prevT,
m176g.prevT,
m177g.prevT, 
m178g.prevT,
m179g.prevT,
m180g.prevT,
m181g.prevT,
m182g.prevT,
m183g.prevT)
str(selec1)
selec1[order(selec1[,2]),][1:10,]

#melhores models
summary(m5.prevT)
summary(m19.prevT)
summary(m5g.prevT)
summary(m35.prevT)
summary(m35g.prevT)


#Inferência
yhat.m5prevT<-predict.lm(m5.prevT,baseS.4)
yhat.m19prevT<-predict.lm(m19.prevT,baseS.4)
yhat.m5gprevT<-predict.gam(m5g.prevT,baseS.4)
yhat.m35prevT <- predict.lm(m35.prevT,baseS.4)
yhat.m35gprevT<-predict.gam(m35g.prevT,baseS.4)

#Modelo de consenso
m.cons<-as.matrix(cbind(
yhat.m5prevT,
yhat.m19prevT,
yhat.m5gprevT,
yhat.m35prevT,
yhat.m35gprevT
))

m.cons.fit<-apply(m.cons,1,mean)
m.cons.fit.sd<-apply(m.cons,1,sd)
IC.inf<-m.cons.fit-(1.96*m.cons.fit.sd)
IC.sup<-m.cons.fit+(1.96*m.cons.fit.sd)

#Validação interna
base.val <- as.data.frame(cbind(
round(basePrevT$PrevT,4),
round(m.cons.fit[as.numeric(rownames(basePrevT))],4),
round(IC.inf[as.numeric(rownames(basePrevT))],4),
round(IC.sup[as.numeric(rownames(basePrevT))],4)))

base.val[c(1,26,32),1] <- 0
cor.test(base.val[,1],base.val[,2], method = "spearman")

#exportar para arcgis
m.dca<-as.matrix(cbind(baseTDCApop2$IncDCA,m.cons.fit))
colnames(m.dca) <- c("IncDCA","m.cons.fit")
m.dca[m.dca[,1]>0,2]<-m.dca[m.dca[,1]>0,1]
m.dca<-as.data.frame(m.dca)
m.cons.fit<-m.dca$m.cons.fit
exp.prev<-as.data.frame(cbind(baseTDCApop2[,c(1,23,24,28,27,29)],m.cons.fit,IC.inf,IC.sup))
write.dbf(exp.prev,"prevMS1.dbf")

#estimativa de desfechos
est.f<-subset(exp.prev, subset=  (m.cons.fit>=0 & IC.inf>=0) | (IC.inf==IC.inf[5570]) | (IncDCA>0 & IC.inf < 0))
str(est.f)  
mean(est.f$m.cons.fit)
sd(est.f$m.cons.fit)
sum(round(est.f$m.cons.fit*est.f[,4],0))
