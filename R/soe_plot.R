soe.plot <- function(data, x.var, y.var, x.label = '', y.label = '', tol = 0.1,
                     x.start = NA, x.end = NA, end.start = 2008, bg.col = background, mean_line = T,
                     end.col = recent, stacked = NA, x.line = 2.5, y.line = 3.5, scale.axis = 1,
                     rel.y.num = 1.5, rel.y.text = 1.5, suppressAxis = FALSE,status  = F,anomaly = F,
                     endshade = TRUE, full.trend = TRUE, point.cex = 1.5, lwd = 2, ymax = TRUE,ymin = TRUE,
                     y.upper = y.upper, y.lower = y.lower, extra = FALSE, x.var2 = x.var2, y.var2 = y.var2,
                     line.forward = FALSE, mean_line.2 = T, cex.stacked = 1, website = T) {

  
  #Select Data
  x <- data[Var == y.var, ]
  x <- x[order(x[, get(x.var)]), ]
  setnames(x, x.var, 'X')
  
  #Set common time step if necessary
  if(is.na(x.start)) x.start <- min(x[, X])
  if(is.na(x.end))   x.end   <- max(x[, X])
  x <- x[X >= x.start, ]
  
  #Set up plot parameters
  if (ymax == TRUE){
    y.max <- max(x[, Value]) + tol * max(x[, Value])
  } else {
    y.max <- as.numeric(y.upper)
  }
  
  if (ymin == TRUE){
    y.min <- min(x[, Value]) - tol * abs(min(x[, Value]))
  } else if (ymin == FALSE){
    y.min <- as.numeric(y.lower)
  }
  
  y.mean <- mean(x[, Value])
  y.sd <- sd(x[, Value])
  
  #Plot blank plot
  plot(x[X >= x.start, list(X, Var)]$X, xlim = c(x.start, x.end),
       ylim = c(y.min,y.max), xlab = '', ylab = '', axes = F, ty = 'n')
  
  
  #Add background
  u <- par('usr')
  rect(u[1], u[3], u[2], u[4], border = NA, col = bg.col)
  
  #Add end period shading
  if (endshade == TRUE){
    rect(end.start - 0.5, u[3], u[2], u[4], border = NA, col = end.col)
  }
  
  #Add mean line
  if (anomaly == F){
    if (mean_line == TRUE){
      abline(h = y.mean, col = 'grey', lwd = 3, lty = 2)
    } 
  } else if (anomaly == TRUE){
    abline(h = 0, col = 'grey', lwd = 3, lty = 2)
  }
  
  #Add x y lines
  abline(h = u[3], lwd=3)
  abline(v = u[1], lwd=3)
  
  #Add data points/lines
  points(x[, list(X, Value)], pch = 16, cex = point.cex)
  lines( x[, list(X, Value)], lwd = lwd)
  
  #extra lines
  if (extra == TRUE){
    x2 <- data[Var == y.var2, ]
    x2 <- x2[order(x2[, get(x.var2)]), ]
    setnames(x2, x.var2, 'X2')
    x2 <- x2[X2 >= x.start, ]
    if (mean_line.2 == TRUE){
      abline(h = mean(x2[, Value]), col = 'lightcoral', lwd = 3, lty = 2) 
    }
    points(x2[, list(X2, Value)], pch = 16, cex = point.cex, col = "indianred")
    lines( x2[, list(X2, Value)], lwd = lwd, col = "indianred")
  }
  
  
  #Add axis
  if (suppressAxis == FALSE){
    if(is.na(stacked)) axis(1, cex.axis = 1)
    if(!is.na(stacked)){
      if(stacked!= 'A') axis(3, cex.axis = 1.5, tck = 0.1, labels = F)
    }
  }
  
  #Stacked axes with 0 overlap so need to remove
  labels <- round((axTicks(2) / scale.axis), 5)
  if(labels[1] == 0) labels[1] <- ''
  axis(2, at = axTicks(2), labels = labels, cex.axis = rel.y.num,
       las = T)
  
  #Add axis labels
  if(!website){
    if(!is.na(stacked)) text(u[1], u[4], labels = stacked, cex = cex.stacked, adj = c(-0.5, 1.5))
  } else if (website){
    text(u[1], u[4], labels = "", cex = cex.stacked, adj = c(-0.5, 1.5))
  }
  if(is.na(stacked)){
    mtext(1, text = x.label, line = x.line, cex = 1)
    mtext(2, text = y.label, line = y.line, cex = rel.y.text)
  }
  
  if (full.trend == T){
    #Split data into past decade and full time series
    dat <- as.data.frame(x[, list(X, Value)])
    
    dat <- dat %>% dplyr::rename(series = Value) %>%
      mutate(time = seq(1,nrow(dat),1))
    
    # Fit linear model
    lm_out <- fit_lm(dat = dat)
    p <- lm_out$p
    if (p < .05){
      
      newtime <- seq(min(dat$time), max(dat$time), length.out=length(dat$time))
      newdata <- data.frame(time = newtime,
                            time2 = newtime^2)
      lm_pred <- AICcmodavg::predictSE(lm_out$model, 
                                       newdata = newdata,
                                       se.fit = TRUE)
      
      year <- seq(x$X[1],x$X[length(x$X)],length.out = length(dat$time))
      
      # Make plot
      if (lm_pred$fit[length(lm_pred$fit)] > lm_pred$fit[1]){
        lines(year, lm_pred$fit, col = main.pos, lwd = 7)
        points(x[, list(X, Value)], pch = 16, cex = point.cex)
        lines( x[, list(X, Value)], lwd = lwd)
        
        if (line.forward == TRUE){
          lines(year, lm_pred$fit, col = main.pos, lwd = 7)
        }
      } else if (lm_pred$fit[length(lm_pred$fit)] < lm_pred$fit[1]){
        lines(year, lm_pred$fit, col = main.neg, lwd = 7)
        points(x[, list(X, Value)], pch = 16, cex = point.cex)
        lines( x[, list(X, Value)], lwd = lwd)
        if (line.forward == TRUE){
          lines(year, lm_pred$fit, col = main.neg, lwd = 7)
        }
      }
    }
    
    if (extra == TRUE){
      
      # Second variable
      dat <- as.data.frame(x2[, list(X2, Value)])
      
      dat <- dat %>% dplyr::rename(series = Value) %>%
        mutate(time = seq(1,nrow(dat),1))
      
      # Fit linear model
      lm_out <- fit_lm(dat = dat)
      p <- lm_out$p
      points(x2[, list(X2, Value)], pch = 16, cex = point.cex, col = "indianred")
      lines( x2[, list(X2, Value)], lwd = lwd, col = "indianred")
      if (p < .05){
        
        newtime <- seq(min(dat$time), max(dat$time), length.out=length(dat$time))
        newdata <- data.frame(time = newtime,
                              time2 = newtime^2)
        lm_pred <- AICcmodavg::predictSE(lm_out$model, 
                                         newdata = newdata,
                                         se.fit = TRUE)
        
        year <- seq(x2$X2[1],x2$X2[length(x2$X2)],length.out =length(dat$time))
        
        # Make plot
        if (lm_pred$fit[length(lm_pred$fit)] > lm_pred$fit[1] ){
          lines(year, lm_pred$fit, col = main.pos, lwd = 7)
          points(x2[, list(X2, Value)], pch = 16, cex = point.cex, col = "indianred")
          lines( x2[, list(X2, Value)], lwd = lwd, col = "indianred")
        } else if (lm_pred$fit[length(lm_pred$fit)] < lm_pred$fit[1]){
          lines(year, lm_pred$fit, col = main.neg, lwd = 7)
          points(x2[, list(X2, Value)], pch = 16, cex = point.cex, col = "indianred")
          lines( x2[, list(X2, Value)], lwd = lwd, col = "indianred")
        } 
      }
    }
    
  }
  
  
  
}  


#Add axis labels for stacked plots
soe.stacked.axis <- function(x.label, y.label, x.line = 2.5,rel.x.text = 1.5,
                             y.line = 3.5, rel.y.text = 1.5, outer = TRUE){
  axis(1, cex.axis = rel.x.text)
  mtext(1, text = x.label, line = x.line, cex = rel.x.text, outer = outer)
  mtext(2, text = y.label, line = y.line, cex = rel.y.text, outer = outer)
  
}


#Background colors
background   <- 'white'
recent       <- '#E6E6E6'
main.pos <- rgb(253/255, 184/255, 99/255,  alpha = .9)
main.neg <- rgb(178/255, 171/255, 210/255, alpha = .9)