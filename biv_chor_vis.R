
    cat1 <- SOE.data %>%
      filter(str_detect(Var,"social vulnerability MAB")) %>% 
      mutate(chr_cat = ifelse(Value > 2, "C",
                              ifelse((Value <= 2) & (Value > 1), "B", "A"))) %>% 
      pull(chr_cat)
    
    
    cat2 <- SOE.data %>%
      filter(str_detect(Var,"commercial reliance MAB")) %>% 
      mutate(num_cat = ifelse(Value > 2, 3,
                              ifelse((Value <= 2) & (Value > 1), 2, 1))) %>% 
      pull(num_cat)
    
    biv_cat <- paste0(cat1, cat2) 
    biv_cat <- factor(biv_cat, levels = c("A1","A2","A3",
                                          "B1","B2","B3",
                                          "C1","C2","C3"),
                      ordered = TRUE)  
    
    lon <- SOE.data %>% filter(Var == "choropleth longitude MAB") %>% pull(Value)
    lat <- SOE.data %>% filter(Var == "choropleth latitude MAB") %>% pull(Value)
    
    #new dataframe to turn into sp object
    mab.dat <- data.frame(lon = lon,
                          lat = lat,
                          biv_cat = biv_cat) %>% 
      arrange(desc(biv_cat))
    
    #Map limits
    xmin = -78
    xmax = -72
    ymin = 35.5
    ymax = 41.5
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
    
    
    #colors of bivariate color scheme
    base_col <- c("#7F7FFF","blue","black",
                  "#B8B8FF","#B871B8","red",
                  "#FFFFFF","#FFB8B8","#FF7F7F")
    
    #raster image to display on plot as bivariate legend
    r <- raster(xmn = -74.5, xmx = -72.4, ymn = 36.1, ymx = 37.8, nrows = 3, ncols = 3)
    r[] <- 1:9
    r_spdf <- as(r, "SpatialPixelsDataFrame")
    r_df <- as.data.frame(r_spdf)
    m_df <- r_df %>%
      reshape2::melt(id = c("y","x")) %>%
      dplyr::rename(lat = y, long = x) %>%
      dplyr::select(-variable)
    
    ggplot() + 
      geom_sf(data = new_coast)+
      geom_point(data = mab.dat, aes(x = lon, y = lat, color = biv_cat, size = biv_cat))+
      coord_sf(crs = crs, xlim = xlims, ylim = ylims,datum=NA) +
      geom_tile(data = m_df, aes(x = long, y = lat, fill = as.factor(value)), inherit.aes = F) +
      guides(fill = F, alpha = F) +
      scale_color_manual(values = base_col)+
      scale_fill_manual(values = base_col)
      
    
    
    #break up into groups for plotting - allows for effective layering
    g1 <- mab.dat %>% filter(cat3 == "A1"|cat3 == "A2"|cat3 == "A3") %>% arrange(cat3)
    g2 <- mab.dat %>% filter(cat3 == "B1"|cat3 == "B2") %>% arrange(cat3)
    g3 <- mab.dat %>% filter(cat3 == "C1") %>% arrange(cat3)
    
    
    g2_big <- mab.dat %>% filter(cat3 == "B3")
    g3_big <- mab.dat %>% filter(cat3 == "C2"|cat3 == "C3") %>% arrange(cat3)
    
    
    #turn grouped data into sp objects for overlaying on plot of Mid-Atlantic
    
    coordinates(g1) <- ~lon+lat
    g1@proj4string <- map.crs
    g1 <- spTransform(g1, map.crs)
    colo1 <- c(rep("#FFFFFF",table(g1$cat3)[1]),
               rep("#B8B8FF",table(g1$cat3)[2]),
               rep("#7F7FFF",table(g1$cat3)[3]))
    
    
    coordinates(g2) <- ~lon+lat
    g2@proj4string <- map.crs
    g2 <- spTransform(g2, map.crs)
    colo2 <- c(rep("#FFB8B8",table(g2$cat3)[4]),rep("#B871B8",table(g2$cat3)[5]))
    
    
    coordinates(g3) <- ~lon+lat
    g3@proj4string <- map.crs
    g3 <- spTransform(g3, map.crs)
    colo3 <- c("#FF7F7F")
    
    
    coordinates(g2_big) <- ~lon+lat
    g2_big@proj4string <- map.crs
    g2_big <- spTransform(g2_big, map.crs)
    
    coordinates(g3_big) <- ~lon+lat
    g3_big@proj4string <- map.crs
    g3_big <- spTransform(g3_big, map.crs)
    
    
    #plot map and dots of different size based on category
    
    plot(coast, xlim = c(-77.5,-71), ylim = c(36.5,41),col = "grey")
    plot(g1, pch = 16, col = colo1,cex = 1, add = T)
    plot(g2, pch = 16, col = colo2,cex = 1, add = T)
    plot(g3, pch = 16, col = colo3,cex = 1,add = T)
    
    
    plot(g2_big, pch = 16, col = "blue",cex = 2, add = T)
    plot(g3_big, pch = 16, col = c(rep("red",table(g3_big$cat3)[8]),rep("black",table(g3_big$cat3)[9])),cex = 2,add = T)
    
    #colors of bivariate color scheme
    base_col <- c("#7F7FFF","blue","black",
                  "#B8B8FF","#B871B8","red",
                  "#FFFFFF","#FFB8B8","#FF7F7F")
    
    #raster image to display on plot as bivariate legend
    r <- raster(xmn = -73.5, xmx = -71.4, ymn = 36, ymx = 37.8, nrows = 3, ncols = 3)
    r[] <- 1:9
    plot(r, col = base_col, legend = F, bty = "n", axes = F, yaxt = "n", frame.plot = F,
         xaxt='n', add = T)
    
    #text
    text(-73.9, 36.9, "Commercial Reliance",srt=90, cex = .75)
    text(-72.4,35.6, "Social Vulnerability", cex = .75)
    text(-77.1,42, "A", cex = 1.5)
    
    axis(1, at = c(-76,-74,-72), labels = paste( c(-76,-74,-72) * -1, 'W'),pos = 34.7,col = NA, col.ticks = 1)
    axis(2, at = axTicks(2), labels = paste(axTicks(2), 'N'), las = T, pos = -77.8,col = NA, col.ticks = 1)
    box(lty = 1, lwd = 2)
    #text
    
    #####################################################################################################
    
    cat1 <- SOE.data[SOE.data$Var == "social vulnerability MAB",]$Value
    cat2 <- SOE.data[SOE.data$Var == "recreational reliance MAB",]$Value
    
    #group data into categories
    ncat1 <- NULL
    for (i in 1:length(cat1)){
      if (cat1[i] > 2){
        ncat1[i] <- "C"
      } else if ((cat1[i] <= 2) & (cat1[i] > 1)){
        ncat1[i] <- "B"
      } else {
        ncat1[i] <- "A"
      }
    }
    
    ncat2 <- NULL
    for (i in 1:length(cat1)){
      if (cat2[i] > 2){
        ncat2[i] <- 3
      } else if ((cat2[i] <= 2) & (cat2[i] > 1)){
        ncat2[i] <- 2
      } else {
        ncat2[i] <- 1
      }
    }
    
    
    cat3 <- paste0(ncat1, ncat2)
    cat3 <- factor(cat3, levels = c("A1","A2","A3",
                                    "B1","B2","B3",
                                    "C1","C2","C3"),
                   ordered = TRUE)
    lon <- SOE.data[SOE.data$Var == "choropleth longitude MAB",]$Value
    lat <- SOE.data[SOE.data$Var == "choropleth latitude MAB",]$Value
    
    #new dataframe to turn into sp object
    mab.dat <- data.frame(lon = lon,
                          lat = lat,
                          cat3 = cat3)
    
    #break up into groups for plotting - allows for effective layering
    g1 <- mab.dat %>% filter(cat3 == "A1"|cat3 == "A2"|cat3 == "A3") %>% arrange(cat3)
    g2 <- mab.dat %>% filter(cat3 == "B1"|cat3 == "B2") %>% arrange(cat3)
    g3 <- mab.dat %>% filter(cat3 == "C1") %>% arrange(cat3)
    
    
    g2_big <- mab.dat %>% filter(cat3 == "B3")
    g3_big <- mab.dat %>% filter(cat3 == "C2"|cat3 == "C3") %>% arrange(cat3)
    
    
    #turn grouped data into sp objects for overlaying on plot of Mid-Atlantic
    
    coordinates(g1) <- ~lon+lat
    g1@proj4string <- map.crs
    g1 <- spTransform(g1, map.crs)
    colo1 <- c(rep("#FFFFFF",table(g1$cat3)[1]),
               rep("#B8B8FF",table(g1$cat3)[2]),
               rep("#7F7FFF",table(g1$cat3)[3]))
    
    
    coordinates(g2) <- ~lon+lat
    g2@proj4string <- map.crs
    g2 <- spTransform(g2, map.crs)
    colo2 <- c(rep("#FFB8B8",table(g2$cat3)[4]),rep("#B871B8",table(g2$cat3)[5]))
    
    
    coordinates(g3) <- ~lon+lat
    g3@proj4string <- map.crs
    g3 <- spTransform(g3, map.crs)
    colo3 <- c("#FF7F7F")
    
    
    coordinates(g2_big) <- ~lon+lat
    g2_big@proj4string <- map.crs
    g2_big <- spTransform(g2_big, map.crs)
    
    coordinates(g3_big) <- ~lon+lat
    g3_big@proj4string <- map.crs
    g3_big <- spTransform(g3_big, map.crs)
    
    
    #plot map and dots of different size based on category
    
    plot(coast, xlim = c(-77.5,-71), ylim = c(36.5,41),col = "grey")
    plot(g1, pch = 16, col = colo1,cex = 1, add = T)
    plot(g2, pch = 16, col = colo2,cex = 1, add = T)
    plot(g3, pch = 16, col = colo3,cex = 1,add = T)
    
    
    plot(g2_big, pch = 16, col = "blue",cex = 2, add = T)
    plot(g3_big, pch = 16, col = c(rep("red",table(g3_big$cat3)[8]),rep("black",table(g3_big$cat3)[9])),cex = 2,add = T)
    
    #colors of bivariate color scheme
    base_col <- c("#7F7FFF","blue","black",
                  "#B8B8FF","#B871B8","red",
                  "#FFFFFF","#FFB8B8","#FF7F7F")
    
    #raster image to display on plot as bivariate legend
    r <- raster(xmn = -73.5, xmx = -71.4, ymn = 36, ymx = 37.8, nrows = 3, ncols = 3)
    r[] <- 1:9
    plot(r, col = base_col, legend = F, bty = "n", axes = F, yaxt = "n", frame.plot = F,
         xaxt='n', add = T)
    axis(1, at = c(-76,-74,-72), labels = paste( c(-76,-74,-72) * -1, 'W'),pos = 34.7,col = NA, col.ticks = 1)
    axis(2, at = axTicks(2), labels = paste(axTicks(2), 'N'), las = T, pos = -77.8,col = NA, col.ticks = 1)
    box(lty = 1, lwd = 2)
    #text
    text(-73.9, 36.9, "Recreational Reliance",srt=90, cex = .75)
    text(-72.4,35.6, "Social Vulnerability", cex = .75)
    text(-77.1,42, "B", cex = 1.5)
    
    
  }
  
}

