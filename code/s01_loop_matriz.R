library(terra)

r_to_crop<-terra::rast("./data/covariaveis_50m.tif")
plot(r_to_crop$dem)
r<-r_to_crop[[c("dem","saga_wetness_index")]]
names(r)
r <- terra::aggregate(r, 10)

n <- 10

# Get the starting cells of interest

rows <- seq(1, nrow(r), by=n)
cols <- seq(1, ncol(r), by=n)    
cells <- cellFromRowColCombine(r, rows, cols)

# Get the coordinates

# upper-left coordinates of the starting cells 
xy <- terra::xyFromCell(r, cells)
rs <- res(r)
xy[,1] <- xy[,1] - rs[1]/2
xy[,2] <- xy[,2] + rs[2]/2

# add the lower-right coordinates of the end cell
xy <- cbind(xy[,1], xy[,1] + n*rs[1], xy[,2] - n*rs[2], xy[,2])

# And loop

x <- lapply(1:nrow(xy), function(i) {
  
  crop(r, xy[i,])

})

# Verify

e <- lapply(x, \(i) ext(i) |> as.polygons()) |> vect()

plot(r[[1]])

lines(e, col="blue", lwd=2)

gower_list <- vector(mode='list', length = length(x))

for (i in 1:length(x)) {
  
  gower_list[[i]] <- try(mean(gower::gower_dist(as.data.frame(x[[i]], xy =T),
                                                as.data.frame(r, xy = T)), na.rm = TRUE),silent = FALSE)
  
}

gower_list_df <- matrix(unlist(as.list(gower_list)), dimnames = list(1:length(x),"Cropped raster"))
gower_list_df

gower_list_df[order(gower_list_df[,1],decreasing=F),]




