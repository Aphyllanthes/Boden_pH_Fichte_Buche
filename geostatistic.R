library(gstat)
library(sp)
coordinates(data1) <- ~x_coord + y_coord

g <- gstat(id = "pH", formula = pH~1, data = data1)
#formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name z, for ordinary and simple kriging use the formula z~1; for simple kriging also define beta (see below); for universal kriging, suppose z is linearly dependent on x and y, use the formula z~x+y
plot(variogram(g))
k <- gstat(id = "pH", formula = pH~x_coord+y_coord, data= data1)
plot(variogram(k))
