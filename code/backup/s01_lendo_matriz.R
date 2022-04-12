# carregando dado de MDE
library(terra);library(spatstat.geom);library(Corbi);library(gower);library(st)
library(tidyr);library(purrr);library(dplyr)

r_to_crop<-terra::rast("./data/covariaveis_50m.tif")
plot(r_to_crop$dem)
r<-r_to_crop[[c("dem","saga_wetness_index")]]
names(r)
# dt_raster <- data.table::data.table(as.data.frame(raster_completo, xy = T))

r <- terra::set.ext(rast(volcano), terra::ext(0, nrow(volcano), 0, ncol(volcano)))
plot(r)

xmin <- xmin(r)
ymax <- ymax(r)
rs <- res(r)

n <- 10
xmax <- xmin + n * rs[1] 
ymin <- ymax - n * rs[2]

x <- crop(r, c(xmin, xmax, ymin, ymax))
plot(x)


dt_raster <- as.data.frame(r, xy = T)

# create a nested data frame where we have created 10 lists
# for rows 1..10, 11..20, etc
df <- dt_raster |>  
  mutate(row_id = (row_number()-1) %/% 20 + 1) |>  
  group_by(row_id) |> 
  nest()

# create cartesian product
crossing(a = df, b = df) |> 
  
  # compute gdist for each combo
  mutate(gdist = map2(a$data, b$data, gower::gower_dist)) |> 
  
  # compute avg value for each
  mutate(gavg = map_dbl(gdist, mean)) |> 
  
  # order
  arrange(-gavg)


dt_raster |>  
  mutate(col_id = (col_number()-1) %/% 10 + 1)

