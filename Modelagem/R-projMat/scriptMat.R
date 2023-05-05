library(deSolve)

parameters <- c(a = 0.0111*exp(0.025),
b = 0.0111*exp(0.025),
c = 0.0143*exp(0.0252),
d = 0.0111*exp(0.025),
r = 0.02,
g = 0.5,
p = 0.1)

state <- c(M = 760000,
F = 1,
C = 1,
R = 1)


massad<-function(t, state, parameters) {
with(as.list(c(state, parameters)),{
# rate of change
dM <- -a*M -(1-p)*r*M - p*r*M
dF <- (1-p)*r*M - b*F
dC <- p*r*M -(c+g)*C
dR <- g*C- d*R
# return the rate of change
list(c(dM, dF, dC, dR))
}) # end with(as.list ...
}

times <- seq(0, 30, by = 1)


out <- ode(y = state, times = times, func = massad, parms = parameters)
head(out)
x<-out[,1]
z<-out[,2]

png("Fig2_new.png", height=120, width=150, units="mm",res=600)
par(mar=c(4,4,3,0.1))
plot(x, z, xlab = "Anos após a eliminação do vetor domiciliado (ano 1=2007)", ylab = "Número de mulheres em idade fértil com DC", type="l", col="gray", lwd=2, main = "Projeção matemática", xlim=c(1,20), 
ylim=c(400000,760000))
polygon(c(x[x>=0], max(x), 0), c(z[x>=0], 0, 0), col="lightgray")
points(c(9,10),c(573002,555300),pch=c(19,15),col=c("darkblue","salmon"))
points(9.5,583143, pch=17, col = "darkgreen" )
text(12,583143, label=c("este trabalho"))
legend(13,750000,pch=c(17,19,15), col=c("darkgreen","darkblue","salmon"),legend=c("2015-2016","2015","2016"), title = "Período de referência \n da estimativa", bty = "n")
dev.off()
