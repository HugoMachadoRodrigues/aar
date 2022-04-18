# Lendo pacote
library(terra)

# Carregando raster de covariaveis
r_to_crop<-terra::rast("./data/covariaveis_50m.tif")
names(r_to_crop)
# Plotando o MDE
plot(r_to_crop$dem)

# Separando os rasteres de MDE e SAGA no objeto "r"
r<-r_to_crop[[c("dem","saga_wetness_index","amb_pedo","aspect")]]

# Chacando os nomes
names(r)

# Trocando a resolucao de 50 para 500 m
r <- terra::aggregate(r, 10)

# Setando variavel "n" como 10. Esse valor refere-se ao numero de linhas e colunas
n <- 5

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

# Plotando o raster de MDE
plot(r[[3]])

# Plotando o bloco de linhas
lines(e, col="blue", lwd=2)

# Criando lista com mesmo tamanho do objeto "r" para receber as variaveis calculadas
gower_list <- vector(mode='list', length = length(x))

# Calculando o indice gower para cada subbloco e comparando isso ao valor do indice gower do bloco maior
for (i in 1:length(x)) {
  
  gower_list[[i]] <- try(mean(gower::gower_dist(as.data.frame(x[[i]], xy =T),
                                                as.data.frame(r, xy = T)), na.rm = TRUE),silent = T)
  
}

# Transformando objeto das metricas calculadas em objeto do tipo matriz
gower_list_df <- matrix(unlist(as.list(gower_list)))#, dimnames = list(1:length(x),"Cropped raster"))

# Transformando as entradas da lista em classe de numero
gower_list_df<-as.numeric(gower_list_df[,])

# Chechando os valores de cada entrada
gower_list_df

x_gower_ordered <- sort(unlist(gower_list_df), decreasing = FALSE,na.last = NA,index.return = TRUE)

# Juntando os subrasters "x" com os valores de gower respectivos em um objeto
x_gower_2<- cbind(x, x_gower_ordered$x,x_gower_ordered$ix)
x_gower_2[,c(2,3)]

candidatos <- (x_gower_2[,c(1)][x_gower_2[,c(2)] <= 0.25])

candidatos<-candidatos[!sapply(candidatos,is.null)]

teste <- do.call(terra::mosaic, candidatos[c(1:length(candidatos))])

par(mfrow = c(1,1))
terra::plot(r$saga_wetness_index,range = c(5,14))
terra::plot(teste$saga_wetness_index, range = c(5,14), add = T, col =grDevices::topo.colors((50) ))

gower_list
