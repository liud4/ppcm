library(dplyr)

ppcm <- function(data, 
                 id.var, 
                 outcome.var, 
                 time.var,
                 cov.ti,
                 cov.td,
                 method = NULL,
                 baseline.only = FALSE){
  
  if (!all(c(id.var, time.var, outcome.var, cov.ti, cov.td) %in% names(data))) {
    stop("There are variable not selected from the data!")
  }
  
  if (baseline.only == FALSE){
    method = NULL
    gee.df <- data
    cov.td.bl <- NULL
    for (var.i in cov.td) {
      new.var <- paste0(var.i, "_bl")
      gee.df[new.var] <- gee.df[var.i]
      gee.df <- gee.df %>%
        arrange(id.var, time.var) %>%
        group_by(gee.df[id.var]) %>%
        mutate_at(new.var,
                  ~ dplyr::first(.)) %>%
        ungroup() %>%
        as.data.frame()
      cov.td.bl <- c(cov.td.bl, new.var)
    }
    
    gee.formula <- paste0(outcome.var, '~', paste0(c(time.var, cov.ti, cov.td.bl), collapse = '+'))
    
    gee.fit <- rms::lrm(formula = as.formula(gee.formula), data = gee.df, x = TRUE, y = TRUE)
    
    gee.est <- stats::coef(gee.fit)[c(time.var, cov.ti, cov.td.bl)]
    
    gee.se <- sqrt(diag(stats::vcov(gee.fit))[c(time.var, cov.ti, cov.td.bl)])
    
    return(list("est" = gee.est, "se" = gee.se))
  }
  
}