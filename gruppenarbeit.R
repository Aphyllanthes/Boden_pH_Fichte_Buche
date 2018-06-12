library(dplyr)
library(ggplot2)
library(readxl)

## Veränderung des BodenpH und der Vegetation im Übergang von Nadel zu Laubwald
set.seed(1)
## zufällige punkte generieren:
grad <- runif(84, min = 0, max = 360)
dist <- runif(84, min = 0, max = 5)
x <- rep(rep(1:7, each= 4), each= 3)
y <- rep(rep(1:4, times =  7), each=  3)
punkt <- rep(letters[1:3], 28)
id <- paste0(x,".", y, punkt)

tab <- data.frame(id, x, y, punkt, grad = round(grad), dist = round(dist, 2))
rm(id, x, y, punkt, grad, dist)

## zwei vertauschte Punkte: 5.1a mit 5.4a die grad- und dist-Werte wurden im Feld vertauscht.
punkt51a <- filter(tab, id=="5.1a")
punkt54a <- filter(tab, id=="5.4a")
tab[which(tab$id=="5.1a"),c("grad", "dist")] <- punkt54a[,c("grad","dist")]
tab[which(tab$id=="5.4a"),c("grad", "dist")] <- punkt51a[,c("grad","dist")]
rm(punkt51a, punkt54a)

tab <- tab %>% 
  mutate(y_coord = round(10*y + sin(2*pi*(183-grad)/360)*dist,2)-5) %>% 
  mutate(x_coord = round(10*x + cos(2*pi*(183-grad)/360)*dist,2)-15) %>% 
  filter(x > 1 & x < 7)

#write.csv(tab, "tabelle1.csv")


### Im Feld aufgenommene Daten einlesen:
data <- read_excel("tabelle1.xlsx")
data <- data %>% 
  select(id, humusform, pH, Fichte, Buche, Bergahorn,
         Sommerlinde, Esche, Lärche, Anmerkung)

data1 <- left_join(tab, data, by="id")
data1 <- data1 %>% 
  mutate(Baumart = ifelse( as.numeric(paste0(x, y))<=42, "Fi", "Bu" )) %>% 
  mutate(Nadelbaum = Fichte + Lärche, 
         Laubbaum = Buche + Bergahorn + Sommerlinde + Esche) %>% 
  mutate(Anteil_Nadelbaum = Nadelbaum/(Nadelbaum + Laubbaum)) %>% 
  mutate(pH = as.numeric(pH))

vegetation <- read_excel("vegetation1.xlsx")
zeigerwerte <- read_excel("vegetation1.xlsx", sheet = 2)

#pdf("punkte.pdf", 7, 4)
#ggplot(tab, aes(x_coord, y_coord, col = punkt)) + geom_point() 
#dev.off()
# ggplot(data1, aes(x_coord, y_coord, col = pH)) + 
#   geom_point() +
#   scale_color_gradient(low="blue", high="orange") +
#   #scale_color_gradientn(colours = topo.colors(40)) + 
#   theme_bw() + coord_equal()



