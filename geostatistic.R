library(gstat)
library(sp)

source("gruppenarbeit.R")

geodat <- data1
coordinates(geodat) <- ~x_coord + y_coord

g <- gstat(id = "pH", formula = pH~1, data = geodat)
#formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name z, for ordinary and simple kriging use the formula z~1; for simple kriging also define beta (see below); for universal kriging, suppose z is linearly dependent on x and y, use the formula z~x+y
plot(variogram(g))
k <- gstat(id = "pH", formula = pH~x_coord+y_coord, data= geodat)
plot(variogram(k))

## fit a model to the variogram:
vgm_pH <- variogram(pH~1, data=geodat)
fit_vgm <- fit.variogram(vgm_pH, vgm(model = "Exp")) 
## fit variogram: model = gaussian (?) (Lin wäre linear, ... vgm())
plot(vgm_pH, fit_vgm)

grid <- makegrid(geodat, cellsize = 0.5)
#grid <- SpatialPoints(grid)
coordinates(grid) <- ~x1 + x2
pH_kriged <- krige(pH ~ 1, geodat, grid, model=fit_vgm)

pH_kriged %>% as.data.frame %>% 
  mutate(pH_interpolation = var1.pred) %>% 
  ggplot(aes(x=x1, y=x2)) + 
  geom_tile(aes(fill=pH_interpolation)) + coord_equal() +
  scale_fill_gradient(low = "blue", high="orange") +
  #scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() + xlab("x") + ylab("y") +
  geom_point(data = data1, aes(x_coord, y_coord, size = Anteil_Nadelbaum)) 

