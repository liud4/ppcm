##
## ppcm: Predictive Partly Conditional Model
## Copyright (C) 2024 Panpan Zhang and Dandan Liu
## Panpan Zhang <panpan.zhang@vumc.org>
##
## This file is part of the R package nonparlongdat.
##
## The R package wdnet is free software: You can redistribute it and/or
## modify it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or any later
## version (at your option). See the GNU General Public License at
## <https://www.gnu.org/licenses/> for details.
##
## The R package wdnet is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
##

#' @importFrom dplyr
#' @importFrom rms lmr
NULL

##  ppcm: Predictive Partly Conditional Model

#' Predictive Partly Conditional Model
#' 
#' This function
#'
#' @param data is a data frame in a long format
#' @param id.var is the variable name indicating ID or similar
#' @param outcome.var is the variable name of the outcome
#' @param time.var is the variable name of the follow-up time
#' @param cov.ti is the list of variable names of the time-independent covariates
#' @param cov.td is the list of variable names of the time-dependent covariates
#' @param method allows the users to select between the restricted \code{ppcm} 
#' using fixed time window only and the unrestricted \code{ppcm-u} using all
#' available data.
#' 
#'
#' @return a list of parameter estimates and standard errors.
#'
#' @references Neal, J. E., Zhang, P. & Liu, D. (2024). Predictive partly conditional
#' models for longitudinal ordinal outcomes with application to Alzheimer's disease
#' progression. Under review.
#'
#' @keywords internal
#'
#' @export

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