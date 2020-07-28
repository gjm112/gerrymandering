library(Matrix)
library(tigris)
library(sp)
library(sf)
library(dplyr)
library(tidyverse)
library(rgeos)

dfw <- tracts(state = 'VT')
dfw <- dfw[order(dfw$TRACTCE),]
names(dfw)
#plot(dfw)

#number of geoids
n <- length(dfw$GEOID)
n

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
adj_mat@Dimnames[[1]] <- dfw$TRACTCE
adj_mat@Dimnames[[2]] <- dfw$TRACTCE



# A block
# Find the tract ID for that block
# Check adjacency matrix if tract is adjacent for another tract
# If they are compare all blocks between the tracts
# Insert in the correct spot in the big matrix
# Else do nothing


##Block Level
VT_blocks <- tigris::blocks(state = 'VT')
#Ordering blocks by tract number and then block number
VT_blocks <- VT_blocks[order(VT_blocks$TRACTCE10, VT_blocks$BLOCKCE10),]

#Labeling each block with a unique number
VT_blocks@data <- cbind(VT_blocks@data, orderno = 1:length(VT_blocks$GEOID10))
#plot(VT_blocks)

#number of geoids
no_blocks <- length(VT_blocks$GEOID10)
m <- no_blocks

#Creating list with the block shapes
block_shapes <- list()
for (i in 1:no_blocks){
  block_shapes[[i]] <- VT_blocks@polygons[[i]]@Polygons[[1]]@coords
}

#Remove duplicated points from each block
block_shapes <- lapply(block_shapes,function(x){x[!duplicated(x),]})

#didn't use this function
#border5 <- function(x){unlist(lapply(block_shapes, border, shape_two = x))}

#Make the border2 function generic by using lapply on any y instead of a determined list
border6 <- function(x,y){unlist(lapply(y, border, shape_two = x))}

#Creating list of tract names
tractnos <- list()
tractnos <- dfw$TRACTCE
tractnos

# Determine the number of blocks in each tract
blocks_in_tract <- list()
for (i in 1:n) {
  blocks_in_tract[[i]] <- sum(VT_blocks$TRACTCE10 == tractnos[[i]])
}

#Assign each block name/number to blocks_in_tract list items, 
#so the number of blocks in a tract and the name of that block are paired
names(blocks_in_tract) <- tractnos
blocks_in_tract

#Vectorize the list
vector_blocks_in_tract <- unlist(blocks_in_tract)

#Cumulative Blocks in Tract
cumulative_blocks_in_tract <- list()
cumulative_blocks_in_tract[[1]] <- 0
for(i in 1:n) {
  cumulative_blocks_in_tract[[i+1]] <- sum(vector_blocks_in_tract[1:i])
}

#Subset tracts in case it's useful later
subset_tracts <- list()
for(i in 1:184) {
  subset_tracts[[i]] <- subset(VT_blocks, TRACTCE10 == tractnos[[i]])
}
str(subset_tracts[[1]],2)

#Subsetting the blocks shapes in each tract
subset_block_shapes <- list()
for(i in 1:184){
  subset_block_shapes[[i]] <- block_shapes[(cumulative_blocks_in_tract[[i]] + 1): cumulative_blocks_in_tract[[i+1]]]
}

#----------------
#PREVIOUS METHOD 
#----------------
# #Comparing for Adjacency
# #Create a data frame to store i,j values for the sparse matrix we'll use
# adjdf <- data.frame(ival = integer(), jval = integer())
# adjdf
# #m= number of blocks
# for (i in 1:m) {
#   #n = number of tracts
#   for (j in 1:n) {
#     #Look at tract adjacency matrix to determine if the current block is in a tract that is adjacent to another tract
#     if (adj_mat[VT_blocks$TRACTCE10[[i]], j]) {
#       #Compare all the blocks in tract j (as a list) to current block using border6
#       tx <- which(border6(block_shapes[[i]], subset_block_shapes[[j]]))
#       #If any blocks are adjacent to current block
#       if(length(tx) != 0) {
#         #Add the i and j positions of the adjacent blocks relying on the cumulative_blocks_in_tract list
#         for(k in 1:length(tx)) {
#           adjdf <- adjdf %>% add_row(ival = i, jval = cumulative_blocks_in_tract[[j]] + tx[[k]])}
#       }
#     }
#   }
# }
#--------------------
# END PREVIOUS METHOD
#--------------------

#Comparing for Adjacency
#Look at tract adjacency matrix to determine if the current block is in a tract that is adjacent to another tract
#i is a block and j is a tract
adj_fn <- function(i,j){
  adjdf <- data.frame()
  if (adj_mat[VT_blocks$TRACTCE10[[i]], j]) {
    #Compare all the blocks in tract j (as a list) to current block using border6
    tx <- which(border6(block_shapes[[i]], subset_block_shapes[[j]]))
    #If any blocks are adjacent to current block
    if(length(tx) != 0) {
      #Add the i and j positions of the adjacent blocks relying on the cumulative_blocks_in_tract list
      adjdf <- do.call(rbind, lapply(tx, function(x){return(c(i,cumulative_blocks_in_tract[[j]] + x))}))
    }
  }
  return(adjdf)
}

registerDoParallel()

start <- Sys.time()
greg <- foreach(x = rep(1:2000, each = n), y = rep(1:n, 2000)) %dopar% adj_fn(x,y)
end <- Sys.time()
end - start

adjacdf <- do.call(rbind, greg)

write.csv(adjacdf, "adjacdf.csv")
#At this point, with a more efficient technique, we're pretty much 
#done with creating the block adjacency matrix


##Trying to develop a gerrymandering score for districts
#All the congressional districs
cong_dist <- congressional_districts()
#The congressional districts in vermont, there's only 1 so this is not very useful
vermont_districts <- cong_dist[cong_dist$STATEFP == "50",]

#The districts in the Vermont state senate
vt_state_sen <- state_legislative_districts("VT")
vt_state_cong <- state_legislative_districts("VT", house = "lower")
plot(vt_state_sen, col = "red")
plot(vt_state_cong, col = "forestgreen")

#Plot the tracts in Vermont onto its state congressional districts
par(mfrow = c(1,2))
plot(dfw)
plot(dfw, reset = FALSE)
plot(vt_state_cong, add = TRUE,border = rgb(0,1,0,0.25), lwd = 2)
par(mfrow = c(1,1))


#Focus on one specific congressional district
addison = vt_state_sen[vt_state_sen$SLDUST == "ADD",]
addison1 = vt_state_cong[vt_state_cong$SLDLST == "A-1",]
addison2 = vt_state_cong[vt_state_cong$SLDLST == "A-2",]
franklin1 = vt_state_cong[vt_state_cong$SLDLST == "F-1",]

plot(dfw, reset = FALSE)
plot(addison, add =TRUE, border = "green", lwd = 2, reset = FALSE)
plot(addison1, add =TRUE, border = "violetred", lwd = 2, reset = FALSE)
plot(addison2, add =TRUE, border = "red", lwd = 2, reset = FALSE)
plot(franklin1, add =TRUE, border = "orange", lwd = 2)

#Converting sp to sf objects (idk if this is actually necessary)
#This is the way I know how to check for intersection between polygons
addison_sf <- st_as_sf(addison)
addison1_sf <- st_as_sf(addison1)
addison2_sf <- st_as_sf(addison2)
franklin1_sf <- st_as_sf(franklin1)

#Check to see if the polygons intersect
st_within(addison1_sf, addison_sf)
st_within(addison2_sf, addison_sf)
st_within(franklin1_sf, addison_sf)

st_intersects(addison1_sf, addison2_sf)
st_intersects(addison1_sf, franklin1_sf)



#Plan in the case that there is no other method to do this:
#Convert df into an sf object
#Convert the districts into sf object
#Taking one block at a time, determine which block is in which district
#Store the information of which blocks are in which district in some form
#Perform the gerrymandering scoring operations on sets of blocks representing districts
#moving forward


#Pseudocode for scoring algorithm
# Working on the district level for gerrymandering score
# Take a district
# For that district go through the blocks one by one
# For block x, check adjacency matrix for adjacent blocks
# Check if adjacent blocks are in this current district
  ## If they're not add 1 to the district score
