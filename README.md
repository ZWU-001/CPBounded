# CPBounded: Conformalized Regression for Bounded Outcomes

## Overview

This repository contains the R code and data used in the real data application
in:

> Wu, Z., Leisen, F. and Rubio, F.J. (2025). Conformalized Regression for
> Bounded Outcomes. *Submitted.*
> [arXiv:2507.14023](https://arxiv.org/abs/2507.14023)


## Requirements

The following R packages are required:

```r
install.packages(c("betareg", "tram", "conformalInference",
                   "ggplot2", "dplyr", "readr"))
```

> Verify exact dependencies against the `library()` calls at the top of each
> script, as the list above may be incomplete.

## Data

| File | Description |
|---|---|
| `BodyFat_data.csv` | Body fat dataset ($n = 183$) used in the analysis |
| `Combined_prediction_interval.csv` | True outcomes, point predictions, prediction intervals, and coverage indicators for all test points under all model frameworks |

The body fat dataset is bundled in this repository. It is originally from
Slack (1997) and was further analysed by Johnson (2021).

## Repository structure

The scripts are organised by model type and conformal procedure. Each script
produces prediction intervals under its specified model and conformal method.

### Core analysis scripts

| Script | Model | CP procedure | Non-conformity score |
|---|---|---|---|
| `bodyfat_transformation.R` | Transformation model | Split + Full | — |
| `bodyfat_hetero_transformation.R` | Heteroscedastic transformation model | Split + Full | — |
| `bodyfat_pearson_mu_split.R` | Beta regression ($\mu$) | Split | Pearson residual |
| `bodyfat_pearson_mu_full.R` | Beta regression ($\mu$) | Full | Pearson residual |
| `bodyfat_pearson_mu_phi_split.R` | Beta regression ($\mu$, $\phi$) | Split | Pearson residual |
| `bodyfat_pearson_mu_phi_full.R` | Beta regression ($\mu$, $\phi$) | Full | Pearson residual |
| `bodyfat_quantile_mu_split.R` | Beta regression ($\mu$) | Split | Quantile residual |
| `bodyfat_quantile_mu_full.R` | Beta regression ($\mu$) | Full | Quantile residual |
| `bodyfat_quantile_mu_phi_split.R` | Beta regression ($\mu$, $\phi$) | Split | Quantile residual |
| `bodyfat_quantile_mu_phi_full.R` | Beta regression ($\mu$, $\phi$) | Full | Quantile residual |

### Aggregation and comparison scripts

| Script | Description |
|---|---|
| `Prediction_interval_all_frameworks.R` | Visualises prediction intervals from all frameworks in a single figure using `Combined_prediction_interval.csv` |
| `Split_union_intersection.R` | Union and intersection intervals for split CP across all frameworks |
| `Full_union_intersection.R` | Union and intersection intervals for full CP across all frameworks |
| `Bootstrap_prediction.R` | Bootstrap-based prediction intervals (Espinheira et al., 2014) as a benchmark |

## Citation

If you use this code, please cite:

```bibtex
@article{wu2025cpbounded,
  author  = {Wu, Z. and Leisen, F. and Rubio, F.J.},
  title   = {Conformalized Regression for Bounded Outcomes},
  journal = {Submitted},
  year    = {2025},
  note    = {arXiv:2507.14023}
}
```

