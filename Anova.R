

# Visuelle Darstellung
boxplot (data1$pH[which(data1$Baumart=="Fi")],data1$pH[which(data1$Baumart=="Bu")], ylab="pH-Wert", names=c("Fichte","Buche"))

# Test auf Varianzhomogenität
var(data1$pH[which(data1$Baumart=="Fi")]) 
var(data1$pH[which(data1$Baumart=="Bu")]) #groeßerer Wert
diff <- var(data1$pH[which(data1$Baumart=="Bu")])/var(data1$pH[which(data1$Baumart=="Fi")])
pf(diff, 29, 29, lower.tail=F) #Varianzen der Gruppen unterscheiden sich signifikant :-/ Voraussetzung ANOVA nicht erf?llt
var.test(data1$pH[which(data1$Baumart=="Bu")],data1$pH[which(data1$Baumart=="Fi")])

# ANOVA
fm <- aov(pH ~ Baumart, data=data1)
summary(aov(pH ~ Baumart, data=data1)) #signifikant unterschiedlich !!! JIpiie
plot(fm, 1)
