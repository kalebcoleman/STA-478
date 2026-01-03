# STA478

Statistical computing helpers built during STA 478. The package collects
resampling, diagnostics, and Monte Carlo utilities that mirror course
assignments while remaining reusable for future projects.

## Installation

```r
# install.packages("devtools")
# devtools::install_github("kalebcoleman/STA-478")
```

## Included helpers

- Resampling: `bootstrap_performance()`, `train_test_split()`
- Model diagnostics: `compute_vif()`, `stepwise_vif()`, `best_subset_search()`
- Classification metrics: `cv_classification_report()`
- Monte Carlo sampling: `AcceptReject()`, `Inverse.CDF()`, `metropolis_1d()`
- MCMC diagnostics: `Gelman()`

## Quick example

```r
library(STA478)

set.seed(123)
parts <- train_test_split(mtcars, prop = 0.7)

glm_res <- bootstrap_performance(am ~ wt + hp, data = mtcars, B = 50, seed = 1)

glm_res$mean_error
```

## Notes

Coursework PDFs and Rmds are stored in `docs/` for reference.
