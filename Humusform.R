#Zusammenhang Baumart Humusform

# Anzahl bestimmen
auswahl <- data1 %>% 
  filter(humusform == "L-Mull" & Baumart == "Bu") 
nrow(auswahl) #31

auswahl <- data1 %>% 
  filter(humusform == "F-Mull" & Baumart == "Fi") 
nrow(auswahl) #21

# die anderen Werte von Hand ausgerechnet

# Darstellung in Form einer Matrix
Humusform <- matrix (c(31,6,2,21), nrow=2, byrow=FALSE)
Humusform

#Fischer Test (x²Test geht nicht da ein Wert <5)
fisher.test (Humusform)
