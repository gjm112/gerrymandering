#install.packages('tigris')
library(Matrix)
library(tigris)
library(sp)

dfw <- tracts(state = 'IL')
plot(dfw)

#number of geoids
n <- length(dfw$GEOID)

#create the adjacency matrix
adj_mat <- Matrix(0, n, n, sparse = TRUE)

shapes <- list()
for (i in 1:n){
  shapes[[i]] <- dfw@polygons[[i]]@Polygons[[1]]@coords
}

#Small test
#shapes <- list(shapes[[1]],shapes[[2]], shapes[[3]], shapes[[21]],shapes[[22]])


#remove the duplicated points in each
shapes <- lapply(shapes,function(x){x[!duplicated(x),]})

border <- function(shape_one,shape_two){any(duplicated(rbind(shape_one, shape_two)))}
border2 <- function(x){unlist(lapply(shapes, border, shape_two = x))}
# for (i in 1:n){print(i)
#   for (j in i:n){print(j)
# adj_mat[i,j] <- adj_mat[j,i] <- any(duplicated(rbind(shapes[[i]], shapes[[j]]))) 
#   }
# }
start <- Sys.time()
adj_list <- lapply(shapes,border2)
adj_mat <- Matrix(do.call(rbind,adj_list), sparse = TRUE)
end <- Sys.time()
(end - start)
adj_mat@Dimnames[[1]] <- dfw$GEOID
adj_mat@Dimnames[[2]] <- dfw$GEOID

#adj_mat[diag(adj_mat)] <- 0


library(igraph)
g1 <- graph_from_adjacency_matrix(adj_mat[1:10,1:10])
plot.igraph(g1, arrow.mode = 0)
plot(g1, vertex.size=10,
     vertex.label.dist=10, vertex.color="red", edge.arrow.size=0)

###############################
#Scrap
###############################


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


