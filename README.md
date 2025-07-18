# Conformalized Regression for Continuous Bounded Outcomes

This repository contains the R code and data used in the real data application in:

> Wu, Z., Leisen, F. and Rubio, F.J. (2025). Conformalized Regression for Continuous Bounded Outcomes. Submitted.

# Description
We apply conformal prediction techniques to model the body fat percentage of young adults using the body fat dataset originally compiled by Slack (1997) and analyzed by Johnson (2021). The outcome is continuous and bounded in (0,1), making it suitable for transformation models and beta regression.
To evaluate the performance of conformal prediction methods, we consider both split and full conformal procedures, using different model specifications: transformation models, heteroscedastic transformation models, and beta regression models with varying nonconformity scores (absolute quantile and Pearson residuals). Performance is assessed in terms of empirical coverage and average interval width across different test points.

# Files
The repository includes:

- `bodyfat_transformation.R`: Basic transformation model implementation (both split and full CP)
- `bodyfat_hetero_transformation.R`: Heteroscedastic transformation model implementation (both split and full CP)
- `bodyfat_pearson_mu[_phi]_[split/full].R`: Beta regression with the Pearson residual
- `bodyfat_quantile_mu[_phi]_[split/full].R`: Beta regression with the quantile residual

Each script produces prediction intervals under the specified model and conformal method.
