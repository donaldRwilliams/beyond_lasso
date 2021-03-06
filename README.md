# Beyond Lasso: A Survey of Nonconvex Regularization in Gaussian Graphical Models

This repo contains R code to reproduce the simulations in Williams (2020).

Note that you will have to install the developmental version of **GGMncv**.
The `ref` is a branch with the exact package used in the paper.

```{r}
# install.packages("devtools")
devtools::install_github("donaldRwilliams/GGMncv", ref = "reproduce")
```

## File Descriptions

* synthetic_sim.Rmd: This code will reproduce the simulations in the section titled "Synthetic Partial Correlations."
  * synthetic_conditions.csv: This file must be downloaded. It includes the simulation conditions.
  * functions.R: This file includes the function GGMncv_search. This will be in the new version of **GGMncv**, 
    but for now this file is needed to reproduce the simulations.

* empirical_sim.Rmd: This code will reproduce the simulations in the section titled "Empirical Partial Correlations."
  * functions.R: This file includes the function GGMncv_search. This will be in the new version of **GGMncv**, 
    but for now this file is needed to reproduce the simulations.

* tuning_select_sim.Rmd: This code will reproduce the simulations in the section titled "Tuning Parameter Selection."
  * functions.R: This file includes the function GGMncv_search. This will be in the new version of **GGMncv**, 
    but for now this file is needed to reproduce the simulations.


# Reference
Williams, D. R. (2020). Beyond Lasso: A Survey of Nonconvex Regularization in Gaussian Graphical Models. https://doi.org/10.31234/osf.io/ad57p

