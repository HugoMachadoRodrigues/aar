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

plot(dados_in)
plot(dados_out, add = T)

perfis <- terra::union(dados_in, dados_out)

plot(perfis)
plot(pr, add=T)

pontos_AR_automatizada <- terra::intersect(pr,perfis)
pontos_AR_automatizada

plot(pontos_AR_automatizada)
plot(pr,add=T)

pontos_valida_AR_automatizada <- perfis - pontos_AR_automatizada

plot(pontos_valida_AR_automatizada)
plot(pontos_AR_automatizada, add= T, pch = 17, col="blue")
plot(pr, add= T)

# buscando perfis na AR original
pontos_AR_manual <- terra::intersect(ar_manual,perfis)
pontos_AR_manual

pontos_valida_AR_manual <- perfis - pontos_AR_manual


plot(r$geomorphology)
plot(pontos_AR_automatizada, col = "blue",add=T, pch=17)
plot(pr,add=T)
plot(ar_manual,add=T)
plot(pontos_AR_manual, col="black",  add=T)
plot(pontos_valida_AR_automatizada, col="brown", add=T,pch =1)
plot(pontos_valida_AR_manual,col="yellow", add=T,pch =1 )

pontos_AR_automatizada # 66
pontos_AR_manual # 147

pontos_valida_AR_automatizada #120
pontos_valida_AR_manual #39
