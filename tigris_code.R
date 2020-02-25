install.packages('tigris')

library(tigris)
library(sp)

dfw <- tracts(state = 'TX', county = c('Dallas', 'Tarrant'))

dfw <- tracts(state = 'TX', county = c('Dallas', 'Tarrant'))

plot(dfw)

dfw$GEOID[[1]]
test <- dfw@polygons[[1]]@Polygons[[1]]@coords



plot(test, type = "l")


dfw2 <- tracts(state = 'IL', county = c('Cook',"Dupage"))

dfw2
test <- dfw2@polygons[[1]]@Polygons[[1]]@coords

which(dfw2$GEOID == "17031810000")
which(dfw2$GEOID == "17031809900")

plot(dfw2@polygons[[22]]@Polygons[[1]]@coords, type = "l", ylim = c(42.000, 42.045) ,xlim = c(-87.685, -87.65))
points(dfw2@polygons[[22]]@Polygons[[1]]@coords, col = "red", pch = 16)

points(dfw2@polygons[[21]]@Polygons[[1]]@coords, type = "l")
points(dfw2@polygons[[21]]@Polygons[[1]]@coords, col = "blue", pch = 16)


dfw2@polygons[[22]]@Polygons[[1]]@coords

dat <- rbind(dfw2@polygons[[22]]@Polygons[[1]]@coords, dfw2@polygons[[21]]@Polygons[[1]]@coords)
points(dat[duplicated(dat),],pch = 16, col = "darkgreen")

greg <- dfw2@polygons[[22]]@Polygons[[1]]@coords
points(greg[duplicated(greg),1],greg[duplicated(greg),2],pch = 16, col = "black")

greg2 <- dfw2@polygons[[21]]@Polygons[[1]]@coords
points(greg2[duplicated(greg2),1],greg2[duplicated(greg2),2],pch = 16, col = "yellow")


44.93
44.94


