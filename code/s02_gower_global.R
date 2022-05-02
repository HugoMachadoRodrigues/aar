mosaico$geology<-sjlabelled::as_numeric(mosaico$geology[])
mosaico$pedology<-sjlabelled::as_numeric(mosaico$pedology[])
mosaico$geomorphology<-sjlabelled::as_numeric(mosaico$geomorphology[])

r$geology<-sjlabelled::as_numeric(r$geology[])
r$pedology<-sjlabelled::as_numeric(r$pedology[])
r$geomorphology<-sjlabelled::as_numeric(r$geomorphology[])


area_total <- terra::as.data.frame(r)
tibble::tibble(area_total)
tibble::tibble(as.data.frame(mosaico))

gower_total <- gower::gower_dist(terra::as.data.frame(mosaico, na.rm = T),
                                     area_total)

area_total$gower <- gower_total

coordenadas <- terra::as.data.frame(r, xy=T)
area_total$x <- coordenadas$x
area_total$y <- coordenadas$y

sp::coordinates(area_total) <-~ x + y
sp::gridded(area_total) <- T
sp::plot(area_total["gower"])
