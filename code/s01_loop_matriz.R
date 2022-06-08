# Lendo pacote
library(terra)

# Carregando dados e rasteres de covariaveis

ar_manual <- terra::vect("./data/AR_satiro_dias.shp")
satiro_dias <- terra::vect("./data/Satiro_dias.shp")
perfis <- terra::vect("./data/Satiro_dias.shp")
dados_in <- terra::vect(read.table("./data/dados_in.txt", sep = "\t", dec = ".", header = T),
                        geom = c("X","Y"))

dados_out <- terra::vect(read.table("./data/dados_out.txt", sep = "\t", dec = ".", header = T),
                        geom = c("X","Y"))

r_to_crop<-terra::rast("./data/covariaveis_10m_cut_fator.tif")
r_to_crop<-terra::rast("./data/covariaveis_10m_cut_fator.tif")

names(r_to_crop)

# Separando os rasteres que serao continuos
# r_continuo<-r_to_crop[[c("precip","temp")]]
r_continuo <- r_to_crop[[c("slope","dem")]]

# Separando os rasteres que serao categoricos
r_categoria<-r_to_crop[[c("geology","geomorphology","pedology")]]

# Convertendo os dados de continuos para fator
r_categoria$geology<-sjlabelled::as_factor(r_categoria$geology[])
r_categoria$pedology<-sjlabelled::as_factor(r_categoria$pedology[])
r_categoria$geomorphology<-sjlabelled::as_factor(r_categoria$geomorphology[])

# Checando se os dados categoricos estao como fator
is.factor(r_categoria["geology"])
is.factor(r_categoria["pedology"])
is.factor(r_categoria["geomorphology"])

# Reamostrando os rasteres para poder processar durante etapa de teste
r_continuo<-terra::aggregate(r_continuo,50, fun = "mean")
r_categoria <- terra::aggregate(r_categoria,50, fun = "modal")

plot(r_categoria)
plot(r_continuo)

# Dividindo cada variavel em dummys
geologia <- data.frame(varhandle::to.dummy(terra::as.data.frame(r_categoria$geology), "geology"))
geomorfologia <- data.frame(varhandle::to.dummy(as.data.frame(r_categoria$geomorphology), "geomorphology"))
pedologia <- data.frame(varhandle::to.dummy(as.data.frame(r_categoria$pedology), "pedology"))

# Associando as vairiáveis às coordenadas
geologia$x <- terra::as.data.frame(r_categoria$geology, xy = T)[,c(1)]
geologia$y <- terra::as.data.frame(r_categoria$geology, xy = T)[,c(2)]

geomorfologia$x <- terra::as.data.frame(r_categoria$geomorphology, xy = T)[,c(1)]
geomorfologia$y <- terra::as.data.frame(r_categoria$geomorphology, xy = T)[,c(2)]

pedologia$x <- terra::as.data.frame(r_categoria$pedology, xy = T)[,c(1)]
pedologia$y <- terra::as.data.frame(r_categoria$pedology, xy = T)[,c(2)]

# Separando as variáveis de interesse
geologia <- geologia[, c(6, 7, 1:5)]
geomorfologia <- geomorfologia[, c(5, 6, 1:4)]
pedologia <- pedologia[, c(8, 9, 1:7)]

geologia<-terra::rast(geologia)
geomorfologia<-terra::rast(geomorfologia)
pedologia<-terra::rast(pedologia)

plot(geologia)
plot(geomorfologia)
plot(pedologia)

# Juntando as covariaveis dummy em um stack
r_variaveis_categoricas_dummy <- c(geologia,geomorfologia,pedologia)

# Juntando o stack de covariaveis dummy e as contínuas em um mesmo stack
r <- c(r_continuo,r_variaveis_categoricas_dummy)

str(as.data.frame(r))

# Checando os nomes
names(r)

#Plotando o banco de rasteres
plot(r)

# Setando variavel "n" como 10. Esse valor refere-se ao numero de linhas e colunas para fazer a busca no bloco de rasteres
n <- 10

# Get the starting cells of interest
rows <- seq(1, nrow(r), by=n)
cols <- seq(1, ncol(r), by=n)    
cells <- terra::cellFromRowColCombine(r, rows, cols)

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
plot(r[["dem"]])

# Plotando o bloco de linhas
lines(e, col="blue", lwd=2)

# Criando lista com mesmo tamanho do objeto "r" para receber as variaveis calculadas
gower_list <- vector(mode='list', length = length(x))

# Calculando o indice gower para cada subbloco e comparando isso ao valor do indice gower do bloco maior
for (i in 1:length(x)) {
  
  gower_list[[i]] <- try(mean(gower::gower_dist(as.data.frame(x[[i]]),
                                                as.data.frame(r)), na.rm = TRUE),silent = T)
  
}

# Transformando objeto das metricas calculadas em objeto do tipo matriz
gower_list_df <- matrix(unlist(as.list(gower_list)))#, dimnames = list(1:length(x),"Cropped raster"))

# Transformando as entradas da lista em classe de numero
gower_list_df<-as.numeric(gower_list_df[,])

# Chechando os valores de cada entrada
gower_list_df

# Ordenando os dados
x_gower_ordered <- unlist(gower_list_df)
# x_gower_ordered <- sort(unlist(gower_list_df), decreasing = FALSE,na.last = NA,index.return = TRUE)

# Juntando os subrasters "x" com os valores de gower respectivos em um objeto
x_gower_2<- cbind(x, x_gower_ordered)
x_gower_2

# Selecionando as subseções de rasteres com índice gower menor e igual que 0.25
candidatos <- (x_gower_2[,c(1)][x_gower_2[,c(2)] <= 0.135])
candidatos_ruins <- (x_gower_2[,c(1)][x_gower_2[,c(2)] > 0.135])

# Removendo os objetos da lista que estao vazios
candidatos<-candidatos[!sapply(candidatos,is.null)]
candidatos_ruins<-candidatos_ruins[!sapply(candidatos_ruins,is.null)]

# Juntando em mosaico as subseções menores que 0.25 em gower
mosaico <- do.call(terra::mosaic, candidatos[c(1:length(candidatos))])
mosaico_ruins <- do.call(terra::mosaic, candidatos_ruins[c(1:length(candidatos_ruins))])

pe <- as.polygons(ext(mosaico))
pr <- as.polygons(mosaico > -Inf)

pe_ruins <- as.polygons(ext(mosaico_ruins))
pr_ruins <- as.polygons(mosaico_ruins > -Inf)

par(mfrow=c(1,2))
plot(r$dem)
# plot(pe, lwd=5, border='red', add=TRUE)
plot(pr, lwd=3, border='blue', add=TRUE) 
plot(ar_manual, lwd=3, border='yellow', add=TRUE) 
plot(dados_in, pch=19, col='black', add=TRUE, cex = 0.8)
plot(dados_out, pch=17, col='red', add=TRUE, cex = 0.8)

plot(r$dem)
# plot(pe, lwd=5, border='red', add=TRUE)
plot(pr_ruins, lwd=3, border='blue', add=TRUE)
# plot(pr, lwd=3, border='blue', add=TRUE)
plot(ar_manual, lwd=3, border='yellow', add=TRUE) 
plot(dados_in, pch=19, col='black', add=TRUE, cex = 0.8)
plot(dados_out, pch=17, col='red', add=TRUE, cex = 0.8)

# calculando area

expanse(pr, unit = "km")
expanse(satiro_dias, unit = "km")
expanse(ar_manual, unit = "km")
expanse(pr_ruins, unit = "km")

# Calculando área manual a porcentagem de área para área de referência manualmente delineada
expanse(ar_manual, unit = "km") / expanse(satiro_dias, unit = "km") * 100 # equiviale a 23% da área de interesse

# Calculando a porcentagem de área automatizada classificada com os menores valores de índice gower <= 0.135
expanse(pr, unit = "km") / expanse(satiro_dias, unit = "km") * 100 

# Calculando a porcentagem de área automatizada classificada como "ruim" ou com índice gower > 0.135
expanse(pr_ruins, unit = "km") / expanse(satiro_dias, unit = "km") * 100

#####################################################################################
##################### Removendo areas candidatas e refazendo busca  #################
#####################################################################################


# A partir desse momento removeremos a área considerada como "Boa" e refaremos a busca pelos menores índices

candidata_1 <- terra::mask(r,pr)
plot(candidata_1)

candidata_2 <- terra::mask(r,candidata_1, inverse = T)
plot(candidata_2)

# Get the starting cells of interest
rows_2 <- seq(1, nrow(candidata_2), by=n)
cols_2 <- seq(1, ncol(candidata_2), by=n)    
cells_2 <- terra::cellFromRowColCombine(candidata_2, rows, cols)

# Get the coordinates

# upper-left coordinates of the starting cells 
xy_2 <- terra::xyFromCell(candidata_2, cells)
rs_2 <- res(candidata_2)
xy_2[,1] <- xy_2[,1] - rs_2[1]/2
xy_2[,2] <- xy_2[,2] + rs_2[2]/2

# add the lower-right coordinates of the end cell
xy_2 <- cbind(xy_2[,1], xy_2[,1] + n*rs_2[1], xy_2[,2] - n*rs_2[2], xy_2[,2])

# And loop
x_2 <- lapply(1:nrow(xy_2), function(i) {
  
  crop(candidata_2, xy_2[i,])
  
})

# Verify
e_2 <- lapply(x_2, \(i) ext(i) |> as.polygons()) |> vect()

# Plotando o raster de MDE
plot(candidata_2[["dem"]])

# Plotando o bloco de linhas
lines(e_2, col="blue", lwd=2)

# Criando lista com mesmo tamanho do objeto "candidata_2" para receber as variaveis calculadas
gower_list_2 <- vector(mode='list', length = length(x_2))

# Calculando o indice gower para cada subbloco e comparando isso ao valor do indice gower do bloco maior
for (i in 1:length(x_2)) {
  
  gower_list_2[[i]] <- try(mean(gower::gower_dist(as.data.frame(x_2[[i]]),
                                                as.data.frame(candidata_2)), na.rm = TRUE),silent = T)
  
}

# Transformando objeto das metricas calculadas em objeto do tipo matriz
gower_list_df_2 <- matrix(unlist(as.list(gower_list_2)))#, dimnames = list(1:length(x),"Cropped raster"))

# Transformando as entradas da lista em classe de numero
gower_list_df_2<-as.numeric(gower_list_df_2[,])

# Chechando os valores de cada entrada
gower_list_df_2

# Ordenando os dados
x_gower_ordered_2 <- unlist(gower_list_df_2)

# Juntando os subrasters "x" com os valores de gower respectivos em um objeto
x_gower_2_2<- cbind(x, x_gower_ordered_2)
x_gower_2_2

# Selecionando as subseções de rasteres com índice gower menor e igual que 0.17
candidatos_2 <- (x_gower_2_2[,c(1)][x_gower_2_2[,c(2)] <= 0.17])
candidatos_ruins_2 <- (x_gower_2_2[,c(1)][x_gower_2_2[,c(2)] > 0.17])

# Removendo os objetos da lista que estao vazios
candidatos_2<-candidatos_2[!sapply(candidatos_2,is.null)]
candidatos_ruins_2<-candidatos_ruins_2[!sapply(candidatos_ruins_2,is.null)]

# Juntando em mosaico as subseções menores que 0.25 em gower
mosaico_2 <- do.call(terra::mosaic, candidatos_2[c(1:length(candidatos_2))])
mosaico_ruins_2 <- do.call(terra::mosaic, candidatos_ruins_2[c(1:length(candidatos_ruins_2))])

pe_2 <- as.polygons(ext(mosaico_2))
pr_2 <- as.polygons(mosaico_2 > -Inf)

pe_ruins_2 <- as.polygons(ext(mosaico_ruins_2))
pr_ruins_2 <- as.polygons(mosaico_ruins_2 > -Inf)

# Plotando os mosaicos da segunda rodada de buscas para os melhores candidatos de índice gower
par(mfrow=c(1,2))
plot(candidata_2$dem)
# plot(pe, lwd=5, border='red', add=TRUE)
plot(pr_2, lwd=3, border='blue', add=TRUE) 
plot(ar_manual, lwd=3, border='yellow', add=TRUE) 
plot(dados_in, pch=19, col='black', add=TRUE, cex = 0.8)
plot(dados_out, pch=17, col='red', add=TRUE, cex = 0.8)

# Plotando os mosaicos da segunda rodada de buscas para os piores candidatos de índice gower
plot(candidata_2$dem)
# plot(pe, lwd=5, border='red', add=TRUE)
plot(pr_ruins_2, lwd=3, border='blue', add=TRUE)
# plot(pr, lwd=3, border='blue', add=TRUE)
plot(ar_manual, lwd=3, border='yellow', add=TRUE) 
plot(dados_in, pch=19, col='black', add=TRUE, cex = 0.8)
plot(dados_out, pch=17, col='red', add=TRUE, cex = 0.8)

# Exportando os arquivos de "Bons" candidatos para o primeiro e segunda loop de buscas
# terra::writeVector(pr,"../data/ar_loop_300m.shp" )
# terra::writeVector(pr_2,"../data/ar_loop_2_300m.shp" )

#1º e 2º loop de 100 metros foi feito com valor <0.115
#1º e 2º loop de 200 metros foi feito com valor <0.115
#1º e 2º loop de 300 metros foi feito com valor <0.0.2

teste_100 <-terra::vect("../data/ar_loop_100m.shp" )
teste_2_100 <-terra::vect("../data/ar_loop_2_100m.shp" )

teste_200 <-terra::vect("../data/ar_loop_200m.shp" )
teste_2_200 <-terra::vect("../data/ar_loop_2_200m.shp" )

teste_300 <-terra::vect("../data/ar_loop_300m.shp" )
teste_2_300 <-terra::vect("../data/ar_loop_2_300m.shp" )

library(terra)
plot(teste_100)
plot(teste_2_100, add = T, col = "red")

plot(teste_200)
plot(teste_2_200, col = "red", add=T)

plot(teste_300)
plot(teste_2_300, col = "red")

