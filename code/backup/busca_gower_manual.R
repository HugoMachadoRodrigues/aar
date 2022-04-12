r_to_crop<-terra::rast("./data/covariaveis_50m.tif")
terra::plot(r_to_crop$dem)
r<-r_to_crop[[c("dem","saga_wetness_index")]]
plot(r)

dataframe<-as.data.frame(r, xy =T, na.omit = T)
dataframe

n <- 10
nr <- nrow(r)

nested_list<-split(r[,c(3:4)], rep(1:ceiling(nr/n), each=n, length.out=nr))
nested_list[[1]]

dt_1 <- nested_list$`1`[c(1:10),]
dt_2 <- nested_list$`2`[c(11:20),]

nested_list[[1]][c(1:10),]

dt_1 <- na.omit(dt_1)
dt_2 <- na.omit(dt_2)

gower_test_1 <- gower::gower_dist(dt_1,  dataframe[,-c(1:2)])
gower_test_2 <- gower::gower_dist(dt_2,  dataframe[,-c(1:2)])

mean(gower_test_1)
mean(gower_test_2)

empty_df = tidyr::nest(dplyr::group_by(nested_list[[1]][c(1:10),]))

empty_df

for (i in 1:length(nested_list)) {
    
    dado <- paste0("dt_",i)
    
    empty_df[dado] <-  tidyr::nest(dplyr::group_by(nested_list[[i]][c(1:10),]))
    
    print(empty_df)
  
}

df <- empty_df[,-1]
  
df$dt_1



