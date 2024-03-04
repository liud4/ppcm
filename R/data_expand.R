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

#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom utils combn
NULL

##  ppcm: Predictive Partly Conditional Model

#' Create an expanded data frame
#' 
#' Function short description
#'
#' @param data is a data frame in a long format
#' @param id.var is the variable name indicating ID or similar
#' @param outcome.var is the variable name of the outcome
#' @param time.var is the variable name of the follow-up time
#' @param cov.ti is the list of variable names of the time-independent covariates
#' @param cov.td is the list of variable names of the time-dependent covariates
#' @param int.width is the interval width
#' @param bandwidth is the bandwidth
#' @param width.subset is a logistic argument that determines whether or not to
#' subset the analysis data: \code{TRUE}, subsetting the data based on \code{int.width}
#' and \code{bandwidth}; \code{FALSE}, otherwise.
#'
#' @return an expanded data frame in long format
#'
#' @references Neal, J. E., Zhang, P. & Liu, D. (2024). Predictive partly conditional
#' models for longitudinal ordinal outcomes with application to Alzheimer's disease
#' progression. Under review.
#'
#' @keywords internal
#'
#' @export

data_expand <- function(data,
                        id.var,
                        outcome.var,
                        time.var,
                        cov.ti,
                        cov.td,
                        int.width,
                        bandwidth,
                        width.subset = FALSE) {
  id.list <- unique(data[id.var])
  id.list <- id.list[1:nrow(id.list),]
  
  expanded.df <- data.frame(NULL)
  for (current.id in id.list) {
    temp.df <- data[data[, id.var] == current.id, ]
    time.expanded <-
      data.frame(t(utils::combn(temp.df[, time.var], 2)))
    colnames(time.expanded) <- c(time.var, "time.end")
    temp.expanded.df <-
      time.expanded %>% dplyr::left_join(temp.df %>% dplyr::select(!outcome.var), by = time.var) %>%
      dplyr::rename("time.start" = time.var) %>%
      as.data.frame()
    temp.expanded.df <-
      temp.expanded.df %>% dplyr::left_join(temp.df %>% dplyr::select(c(time.var, outcome.var)) %>% dplyr::rename("time.end" = time.var),
                                            by = "time.end") %>%
      as.data.frame()
    expanded.df <- rbind(expanded.df, temp.expanded.df)
  }
  
  expanded.df <- expanded.df %>%
    dplyr::mutate(time.diff = time.end - time.start) %>%
    as.data.frame()
  
  if (width.subset == TRUE) {
    expanded.df = expanded.df %>% dplyr::filter(time.diff >= (int.width - bandwidth) &
                                                  time.diff <= (int.width + bandwidth)) %>% as.data.frame()
  } else {
    expanded.df = expanded.df
  }
  
  return(expanded.df)
}
