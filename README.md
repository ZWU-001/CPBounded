# Conformalized Regression for Continuous Bounded Outcomes

This repository contains the R code and data used in the real data application in:

> Wu, Z., Leisen, F. and Rubio, F.J. (2025). Conformalized Regression for Continuous Bounded Outcomes. Submitted.

# Description
We apply conformal prediction techniques to model the body fat percentage of young adults using the body fat dataset originally compiled by Slack (1997) and analyzed by Johnson (2021). One observation with an implausible age entry (age = 1) is removed, resulting in a final sample size of 183. The outcome is continuous and bounded in (0,1), making it suitable for transformation models and beta regression.

To evaluate the performance of conformal prediction methods, we consider both split and full conformal procedures, using different model specifications: transformation models, heteroscedastic transformation models, and beta regression models with varying nonconformity scores (absolute quantile and Pearson residuals). Performance is assessed in terms of empirical coverage and average interval width across different test points.

# Files
The repository includes:

- `BodyFat_data.csv`: This CSV file is the dataset of size 183 that we use.
- `bodyfat_transformation.R`: This R code implements both the split and the full conformal prediction based on the transformation regression model.
- `bodyfat_hetero_transformation.R`: This R code implements both the split and the full conformal prediction based on the heteroscedastic regression model.
- `bodyfat_pearson_mu_split.R`: This R code implements the split conformal prediction based on the beta regression model (with covariate-dependent mean) with a non-conformity measure based on Pearson residuals.
- `bodyfat_pearson_mu_full.R`: This R code implements the full conformal prediction based on the beta regression model (with covariate-dependent mean) with a non-conformity measure based on Pearson residuals.
- `bodyfat_pearson_mu_phi_split.R`: This R code implements the split conformal prediction based on the beta regression model (with covariate-dependent mean and dispersion) with a non-conformity measure based on Pearson residuals.
- `bodyfat_pearson_mu_phi_full.R`: This R code implements the full conformal prediction based on the beta regression model (with covariate-dependent mean and dispersion) with a non-conformity measure based on Pearson residuals.
- `bodyfat_quantile_mu_split.R`: This R code implements the split conformal prediction based on the beta regression model (with covariate-dependent mean) with a non-conformity measure based on quantile residuals.
- `bodyfat_quantile_mu_full.R`: This R code implements the full conformal prediction based on the beta regression model (with covariate-dependent mean) with a non-conformity measure based on quantile residuals.
- `bodyfat_quantile_mu_phi_split.R`: This R code implements the split conformal prediction based on the beta regression model (with covariate-dependent mean and dispersion) with a non-conformity measure based on quantile residuals.
- `bodyfat_quantile_mu_phi_full.R`: This R code implements the full conformal prediction based on the beta regression model (with covariate-dependent mean and dispersion) with a non-conformity measure based on quantile residuals.

- `Combined_prediction_interval.csv`: This CSV file contains the true outcome values, point predictions, prediction intervals, and coverage indicators for each test point, evaluated under all considered model frameworks.
- `Prediction_interval_all_frameworks.R`: This R code visualizes prediction results (`Combined_prediction_interval.csv`) of all model frameworks into one figure.
- 

Each script produces prediction intervals under the specified model and conformal method.
