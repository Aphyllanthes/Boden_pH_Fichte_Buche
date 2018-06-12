

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
