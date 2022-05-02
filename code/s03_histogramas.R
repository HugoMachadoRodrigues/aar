ar_candidata <- vector(mode='list', length = length(x))

# Calculando o indice gower para cada subbloco e comparando isso ao valor do indice gower do bloco maior
for (i in 1:length(x)) {
  
  ar_candidata[[i]] <- try(gower::gower_dist(as.data.frame(mosaico[[i]], xy =T),
                                           as.data.frame(r, xy = T)))
  
}

gower_freq <- list() 
gower_freq$precip <- try(mean(gower::gower_dist(as.data.frame(mosaico$precip),
                                                as.data.frame(r$precip))))
  
gower_freq$temp <- try(mean(gower::gower_dist(as.data.frame(mosaico$temp),
                                                as.data.frame(r$temp))))

gower_freq$geology <- try(mean(gower::gower_dist(as.data.frame(mosaico$geology),
                                              as.data.frame(r$geology))))

gower_freq$geomorphology <- try(mean(gower::gower_dist(as.data.frame(mosaico$geomorphology),
                                                 as.data.frame(r$geomorphology))))

gower_freq$pedology <- try(mean(gower::gower_dist(as.data.frame(mosaico$pedology),
                                                       as.data.frame(r$pedology))))

teste[] <- lapply(teste, function(x) as.numeric(as.character(x)))

teste_t<-t(teste)

par(mar = c(3, 3, 3, 3))
barplot(t(teste_t), main = "Índice Gower", names.arg =c("Precipitação", "Temperatura", "Geologia", "Geomorfologia", "Pedologia") )


