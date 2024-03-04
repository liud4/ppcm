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

#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr first
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate_at
#' @importFrom dplyr ungroup
#' @importFrom rms lmr
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats vcov
NULL

##  ppcm: Predictive Partly Conditional Model

#' Predictive Partly Conditional Model
#'
#' Function short description
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
#' @param baseline.only is a logistic argument determining whether or not to fit
#' the model using the baseline covariates only. The default is \code{FALSE}.
#' @param fixed.window.length is an integer specifying the fixed window lenght if
#' the \code{ppcm} method is used
#' @param int.width is the interval width
#' @param bandwidth is the bandwidth
#' @param width.subset is a logistic argument that determines whether or not to
#' subset the analysis data: \code{TRUE}, subsetting the data based on \code{int.width}
#' and \code{bandwidth}; \code{FALSE}, otherwise.
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
                 baseline.only = FALSE,
                 fixed.window.length = NULL,
                 int.width = NULL,
                 bandwidth = NULL,
                 width.subset = FALSE) {
  if (!all(c(id.var, time.var, outcome.var, cov.ti, cov.td) %in% names(data))) {
    stop("There are variable not selected from the data!")
  }
  
  if (baseline.only == TRUE) {
    method = NULL
    gee.df <- data
    cov.td.bl <- NULL
    for (var.i in cov.td) {
      new.var <- paste0(var.i, "_bl")
      gee.df[new.var] <- gee.df[var.i]
      gee.df <- gee.df %>%
        dplyr::arrange(id.var, time.var) %>%
        dplyr::group_by(gee.df[id.var]) %>%
        dplyr::mutate_at(new.var,
                         ~ dplyr::first(.)) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      cov.td.bl <- c(cov.td.bl, new.var)
    }
    
    gee.formula <-
      paste0(outcome.var, '~', paste0(c(time.var, cov.ti, cov.td.bl), collapse = '+'))
    
    gee.fit <-
      rms::lrm(
        formula = stats::as.formula(gee.formula),
        data = gee.df,
        x = TRUE,
        y = TRUE
      )
    
    gee.est <- stats::coef(gee.fit)[c(cov.ti, cov.td.bl)]
    
    gee.se <-
      sqrt(diag(stats::vcov(gee.fit))[c(cov.ti, cov.td.bl)])
    
    return(list("est" = gee.est, "se" = gee.se))
  } else {
    ppcm.full.df <-
      data_expand(
        data,
        id.var,
        outcome.var,
        time.var,
        cov.ti,
        cov.td,
        int.width,
        bandwidth,
        width.subset
      )
    
    if (method == "ppcm") {
      ppcm.df <-
        ppcm.full.df %>% dplyr::filter(time.diff == fixed.window.length) %>% as.data.frame()
      
      ppcm.formula <-
        paste0(outcome.var, '~', paste0(c(cov.ti, cov.td, "time.end"), collapse = '+'))
      
      ppcm.fit <-
        rms::lrm(
          formula = stats::as.formula(ppcm.formula),
          data = ppcm.df,
          x = TRUE,
          y = TRUE
        )
      
      ppcm.est <- stats::coef(ppcm.fit)[c(cov.ti, cov.td)]
      
      ppcm.se <-
        sqrt(diag(stats::vcov(ppcm.fit))[c(cov.ti, cov.td)])
      
      return(list("est" = ppcm.est, "se" = ppcm.se))
    } else if (method == "ppcm_u") {
      ppcm_u.formula <-
        paste0(outcome.var, '~', paste0(c(cov.ti, cov.td, "time.end"), collapse = '+'))
      
      ppcm_u.fit <-
        rms::lrm(
          formula = stats::as.formula(ppcm.formula),
          data = ppcm.full.df,
          x = TRUE,
          y = TRUE
        )
      
      ppcm_u.est <- stats::coef(ppcm_u.fit)[c(cov.ti, cov.td)]
      
      ppcm_u.se <-
        sqrt(diag(stats::vcov(ppcm_u.fit))[c(cov.ti, cov.td)])
      
      return(list("est" = ppcm_u.est, "se" = ppcm_u.se))
    }
  }
}