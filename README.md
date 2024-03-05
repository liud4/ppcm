ppcm: Predictive Partly Conditional Model
================

The $\mathtt{ppcm}$ package aims to provide user-friendly functions to
implement a predictive partly conditional model to characterize disease
progression at time $t$ with longitudinal ordinal outcomes in the
presence of time-dependent covariates at time $s$ ($s < t$) and
time-dependent effects.

## Installation

You can install the $\mathtt{ppcm}$ package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("liud4/ppcm")
```

## Import Dataset

``` r
example.df <- readr::read_csv(system.file("extdata", "example_data.csv", package = "ppcm"))
head(example.df)
```

    ## # A tibble: 6 × 6
    ##      id  time outcome    X1     X2   Z_ij
    ##   <dbl> <dbl>   <dbl> <dbl>  <dbl>  <dbl>
    ## 1     1     0       0     1 -0.561 -2.36 
    ## 2     1     1       0     1 -0.561 -2.26 
    ## 3     1     2       0     1 -0.561 -0.245
    ## 4     1     3       0     1 -0.561 -0.779
    ## 5     1     4       1     1 -0.561 -2.85 
    ## 6     2     0       1     0  0.120 18.6

## Implementation

``` r
ppcm.fit <- ppcm::ppcm(
  data = example.df,
  id.var = "id",
  outcome.var = "outcome",
  time.var = "time",
  cov.ti = c("X1", "X2"),
  cov.td = "Z_ij",
  method = "ppcm",
  baseline.only = FALSE,
  fixed.window.length = 3,
  int.width = NULL,
  bandwidth = NULL,
  width.subset = FALSE
)

ppcm.res <- as.data.frame(rbind(round(ppcm.fit$est, digits = 3),
                  round(ppcm.fit$se, digits = 3)))
rownames(ppcm.res) <- c("Est.", "S.E.")

ppcm.res %>% knitr::kable(., format = "pipe", padding = 2)
```

|      |    X1 |     X2 |  Z_ij |
|:-----|------:|-------:|------:|
| Est. | 0.468 | -0.039 | 0.510 |
| S.E. | 0.230 |  0.232 | 0.035 |
