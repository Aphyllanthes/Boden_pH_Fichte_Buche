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

dataveg <- left_join(data1, mittlere_zeigerwerte, by="id")

##Reaktionszahl
reaktdat <- filter(dataveg, !is.na(R))
coordinates(reaktdat) <- ~x_coord + y_coord

## fit a model to the variogram:
vgm_R <- variogram(R~1, data=reaktdat)
fit_R <- fit.variogram(vgm_R, vgm(model = "Exp")) 
## fit variogram: model = gaussian (?) (Lin wäre linear, ... vgm())
plot(vgm_R, fit_R)

##Lichtzahl
lichtdat <- filter(dataveg, !is.na(L))
coordinates(lichtdat) <- ~x_coord + y_coord

## fit a model to the variogram:
vgm_L <- variogram(L~1, data=lichtdat)
fit_L <- fit.variogram(vgm_L, vgm(model = "Exp")) 
## fit variogram: model = gaussian (?) (Lin wäre linear, ... vgm())
plot(vgm_L, fit_L)

cor.test(dataveg$pH, dataveg$R, use= "pairwise.complete.obs")
## keine signifikanz im t-test, keine korrelation
cor.test(dataveg$pH, dataveg$L)
## leicht signifikant im t-test, kaum korrelation

