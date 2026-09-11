# Price Elasticity Modelling with `fixest`


## Overview

Price elasticity modelling is widely used in economics and public health research to quantify how consumers respond to changes in food prices. This repository provides a workflow for estimating food price elasticities using household-level loyalty-card data in R. The analyses apply fixed-effects Poisson Pseudo-Maximum Likelihood (PPML) models implemented with the `fixest` package to examine how increases in food prices are associated with changes in energy-adjusted food purchase quantities. The framework is designed for large-scale food purchase datasets, where zero purchases are common and household purchasing behavior is observed repeatedly over time.

This repository includes:

- Code for calculating food category-specific Fisher Ideal Price Indices (folder: *fisher_index*)
- Code for estimating fixed-effects Poisson Pseudo-Maximum Likelihood (PPML) models for selected food categories, both for the full sample and separately by OECD income groups (folder: *models*)

---

## Why PPML?

Food purchase data often contain a large number of zeros because households do not purchase every food category during every observation period.

PPML was selected because it:

- Naturally accommodates zero outcomes
- Does not require log-transformation of the dependent variable
- Produces consistent estimates under heteroskedasticity
- Supports high-dimensional fixed effects
- Scales efficiently to large household panel datasets

Models are estimated using the `fepois()` function from the `fixest` package. For example:

```r

library(fixest)

#'Data includes:

#'@param "class1" Food categories as factor

#'@param "customer_id" ID to each household 

#'@param "elapsed_time" Continuous elapsed time variable

#'@param "energy_adjust_MJ" Continuous energy-adjusted food purchase quantities

#'@param "fisher_index" Continuous Fisher Ideal Price Index


#  Setup:

categories      <- levels(data$class1)
energy_vars     <- paste0("energy_adjust_MJ_", categories)
fisher_names    <- paste0("fisher_index_", categories)
price_vars      <- paste0("log(", fisher_names, ")")
fe_string       <- "factor(customer_id) + factor(elapsed_time)"


# fepois (the longitudinal data were reshaped from long to wide format prior to analysis): 

fepois_list <- lapply(seq_along(categories), function(i){
  outcome <- energy_vars[i]
  formula_str <- paste0(outcome, " ~ ", paste(price_vars, collapse = " + "), " | ", fe_string)
  fepois(
    as.formula(formula_str),
    data = data_wide,
    cluster = ~customer_id,
    fixef.rm = "none"
 )
})
names(fepois_list) <- categories

# Results:

extract_coef_se_table <- function(model_list, categories, price_vars) {

  n <- length(categories)

  coef_mat <- matrix(NA_real_, n, n)
  se_mat   <- matrix(NA_real_, n, n)

  rownames(coef_mat) <- colnames(coef_mat) <- categories
  rownames(se_mat)   <- colnames(se_mat)   <- categories

  for (i in seq_along(categories)) {

    m  <- model_list[[i]]
    ct <- coeftable(m)

    for (j in seq_along(categories)) {

      vname <- price_vars[j]

      if (vname %in% rownames(ct)) {
        coef_mat[i, j] <- ct[vname, "Estimate"]
        se_mat[i, j]   <- ct[vname, "Std. Error"]
      }
    }
  }

  list(coef = coef_mat, se = se_mat)
}


coef_se <- extract_coef_se_table(
  fepois_list,
  categories,
  price_vars
)

coef_se

```

# References:

Davies T, Saxena A, et al. (2025). Food price elasticity estimates in Australia. Nature Food 6, 725–732.

Correia S, Guimarães P, Zylkin T. (2020). Fast Poisson Estimation with High-Dimensional Fixed Effects. Stata Journal, 20(1), 95-115.

Gourieroux C, Monfort A, Trognon A. (1984). Pseudo Maximum Likelihood Methods: Applications to Poisson Models. Econometrica, 52(3), 701-720.

Santos Silva JMC, Tenreyro S. (2006). The Log of Gravity. Review of Economics and Statistics, 88(4), 641-658.

Pan W. (2001). On the Robust Variance Estimator in Generalized Estimating Equations. Biometrics 57, 901–906.


[![DOI](https://zenodo.org/badge/1356991552.svg)](https://doi.org/10.5281/zenodo.22706803)
