library(ncdf4)
library(dplyr)
library(raster)
library(rgdal)
gis.dir <- here::here("gis")
get_bsb <- function(){
  #strata
  strata <- readOGR(file.path(gis.dir,"strata.shp"), verbose = F)
  strata@proj4string <- CRS(crs)
  #get data
  data <- nc_open("gis/Black Sea Bassfall_2.nc")
  #longitude
  lon <- ncvar_get(data, "xi", verbose = F)
  #latitude
  lat <- ncvar_get(data, "yi", verbose = F)
  #thermal habitat projection
  z <- ncvar_get(data, "zi")
  #combine in data.frame
  proj <- data.frame(lon = lon,
                     lat = lat,
                     z = z)
  rm(data)
  z.max <- 1.353462
  proj <- proj %>% filter(z != "NA",z>0) %>% 
    mutate(z = plyr::mapvalues(z, from = (z[(z>z.max)]), to = rep(z.max,length(z[(z>z.max)]))))
  proj$z <- proj$z/max(proj$z)
  
  #turn dataframe to raster
  coordinates(proj) = ~lon+lat
  proj4string(proj)=crs # set it to lat-long
  proj <- spTransform(proj,crs)
  proj <- proj[strata,]
  gridded(proj) <- TRUE
  r <- raster(proj)
  #projection(r) <- crs#downsample
  d <- disaggregate(r, fact=5)
  # 
  # ggplot(data=r_df,aes(Var2, rev(Var1), fill = value)) +
  #   geom_tile() +
  #   scale_fill_gradient2(low = "blue",mid = "white",high = "red",midpoint = 0.5)
  r_spdf <- as(d, "SpatialPixelsDataFrame")
  r_df <- as.data.frame(r_spdf)
  m_df <- r_df %>%
    reshape2::melt(id = c("y","x")) %>%
    dplyr::rename(lat = y, long = x) %>%
    dplyr::select(-variable)# %>%
  
}


