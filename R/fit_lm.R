fit_lm <- function(dat) {
  # Remove missing values first so that all models
  # use the same number of observations (important for AIC)
  # dat <- dat %>% dplyr::filter(complete.cases(.))
  
  # Constant model (null model used to calculate 
  # overall p-value)
  constant_norm <-
    nlme::gls(series ~ 1, 
              data = dat)
  
  constant_ar1 <-
    try(nlme::gls(series ~ 1,
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(constant_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,
                                 aicc  = NA,
                                 coefs..Intercept = NA,
                                 coefs.time = NA,
                                 coefs.time2 = NA,
                                 pval = NA)) 
  } 
  
  
  
  # Linear model with normal error
  linear_norm <- 
    nlme::gls(series ~ time, 
              data = dat)
  
  # Linear model with AR1 error
  linear_ar1 <- 
    try(nlme::gls(series ~ time, 
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(linear_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,
                                 aicc  = NA,
                                 coefs..Intercept = NA,
                                 coefs.time = NA,
                                 coefs.time2 = NA,
                                 pval = NA))
    
  }
  
  # Polynomial model with normal error
  dat$time2 <- dat$time^2
  poly_norm <- 
    nlme::gls(series ~ time + time2, 
              data = dat)
  
  # Polynomial model with AR1 error
  poly_ar1 <- 
    try(nlme::gls(series ~ time + time2, 
                  data = dat,
                  correlation = nlme::corAR1(form = ~time)))
  if (class(poly_ar1) == "try-error"){
    return(best_lm <- data.frame(model = NA,
                                 aicc  = NA,
                                 coefs..Intercept = NA,
                                 coefs.time = NA,
                                 coefs.time2 = NA,
                                 pval = NA))
    
  }
  
  # Calculate AICs for all models
  df_aicc <-
    data.frame(model = c("poly_norm",
                         "poly_ar1",
                         "linear_norm",
                         "linear_ar1"),
               aicc  = c(AICc(poly_norm),
                         AICc(poly_ar1),
                         AICc(linear_norm),
                         AICc(linear_ar1)),
               coefs = rbind(coef(poly_norm),
                             coef(poly_ar1),
                             c(coef(linear_norm), NA),
                             c(coef(linear_ar1),  NA)),
               # Calculate overall signifiance (need to use
               # ML not REML for this)
               pval = c(anova(update(constant_norm, method = "ML"),
                              update(poly_norm, method = "ML"))$`p-value`[2],
                        anova(update(constant_ar1, method = "ML"),
                              update(poly_ar1, method = "ML"))$`p-value`[2],
                        anova(update(constant_norm, method = "ML"),
                              update(linear_norm, method = "ML"))$`p-value`[2],
                        anova(update(constant_ar1, method = "ML"),
                              update(linear_ar1, method = "ML"))$`p-value`[2]))
  
  best_lm <-
    df_aicc %>%
    dplyr::filter(aicc == min(aicc))
  
  
  if (best_lm$model == "poly_norm") {
    model <- poly_norm
  } else if (best_lm$model == "poly_ar1") {
    model <- poly_ar1
  } else if (best_lm$model == "linear_norm") {
    model <- linear_norm
  } else if (best_lm$model == "linear_ar1") {
    model <- linear_ar1
  }
  
  return(list(p = best_lm$pval,
              model = model))
}
