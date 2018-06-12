# Vegetation
vegetation1 <- vegetation[,-c(8, 17)] 
zeigerwerte1 <- zeigerwerte %>% 
  mutate(R = ifelse(R %in% c("x", "NaN"), NA, as.numeric(R))) %>% 
  mutate(L = ifelse(L %in% c("x", "NaN") , NA, as.numeric(L)))

vegetation1 <- vegetation1 %>% 
  tidyr::gather("Art", "Deckung", 7:44) %>% 
  left_join(zeigerwerte1, by=c("Art"="Krautschicht")) %>% 
  mutate(R = ifelse(Deckung %in% c("0", NA), NA, R)) %>% 
  mutate(L = ifelse(Deckung %in% c("0", NA), NA, L)) 

# Für jeden Punkt den Mittelwert der Zeigerwerte berechnen:
mittlere_zeigerwerte <- vegetation1 %>% 
  group_by(id) %>% 
  summarise(R = mean(R, na.rm=T), L = mean(L, na.rm = T))

data1 <- left_join(data1, mittlere_zeigerwerte, by="id")

##Reaktionszahl
geodat <- filter(data1, !is.na(R))
coordinates(geodat) <- ~x_coord + y_coord
Reaktionszahl <- gstat(id = "R", formula = R~1, data = geodat)
#formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name z, for ordinary and simple kriging use the formula z~1; for simple kriging also define beta (see below); for universal kriging, suppose z is linearly dependent on x and y, use the formula z~x+y
plot(variogram(Reaktionszahl))
Reaktionszahl1 <- gstat(id = "R", formula = R~x_coord+y_coord, data= geodat)
plot(variogram(Reaktionszahl1))

