#erste Betrachtung der Daten
summary (data1$pH[which(data1$Baumart=="Fi")])
sd (data1$pH[which(data1$Baumart=="Fi")]) # Standardabweichung
var (data1$pH[which(data1$Baumart=="Fi")])
sem <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))
sem (data1$pH[which(data1$Baumart=="Fi")]) # Standardfehler des Mittelwertes 
CV <- function(x) sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
CV (data1$pH[which(data1$Baumart=="Fi")])

summary (data1$pH[which(data1$Baumart=="Bu")])
sd (data1$pH[which(data1$Baumart=="Bu")]) # Standardabweichung

# Test auf Normalverteilung -Voraussetzung der ANOVA
# visuell
hist(data1$pH, las=1) # sieht ganz gut aus

#Fitten der Daten an eine Normalverteilung
library(MASS)
fit.norm <- fitdistr(data1$pH,"normal")
fit.norm

#kumulative Darstellung und Vergleich mit Normalverteilung
par(mfrow=c(1,2), mar=c(4,5,4,0.5))
plot(ecdf(data1$pH), pch="", verticals=T, las=1,main="Normalverteilung", xlab="BHD [cm]", cex.lab=1.5)
curve(pnorm(x, mean=fit.norm$estimate[1], sd=fit.norm$estimate[2]), add=T,lwd=3, col="grey")
lines(ecdf(data1$pH), pch="", verticals=T) # passt gut !

#KStest
ks.test(data1$pH, "pnorm", fit.norm$estimate[1],+ fit.norm$estimate[2]) # interpretation ?


# Visuelle Darstellung
boxplot (data1$pH[which(data1$Baumart=="Fi")],data1$pH[which(data1$Baumart=="Bu")], ylab="pH-Wert", names=c("Fichte","Buche"))

# Test auf Varianzhomogenität
var(data1$pH[which(data1$Baumart=="Fi")]) 
var(data1$pH[which(data1$Baumart=="Bu")]) #groeßerer Wert
diff <- var(data1$pH[which(data1$Baumart=="Bu")])/var(data1$pH[which(data1$Baumart=="Fi")])
diff
pf(diff, 32, 26, lower.tail=F) #größer als 0,025 --> Varianzen der Gruppen unterscheiden sich signifikant ? 
var.test(data1$pH[which(data1$Baumart=="Bu")],data1$pH[which(data1$Baumart=="Fi")]) #p-value sind nicht signifikant heterogen

# ANOVA
fm <- aov(pH ~ Baumart, data=data1)
summary(aov(pH ~ Baumart, data=data1)) #signifikant unterschiedlich ! Jipiie
plot(fm, 1) #visuell sehr homogen

#Kruskal-Wallis-Test
data1$Baumart <- as.factor(data1$Baumart)
kruskal.test(pH ~ Baumart, data=data1)
library(pgirmess)
#install.packages("pgirmess")
kruskalmc(pH ~ Baumart, data=data1)
