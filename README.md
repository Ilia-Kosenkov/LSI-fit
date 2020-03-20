# Fitting LSI polarization data
[`LSI-fit/_drake.R`](LSI-fit/_drake.R) is the entry point, invoked via `drake::r_make()` from the [`LSI-fit`](LSI-fit/) directory.
- [`LSI-fit/R/data_import.R`](LSI-fit/R/data_import.R) handles data import from the `csv`s,
- [`LSI-fit/R/kepler_eq.R`](LSI-fit/R/kepler_eq.R) solves Keplerian equation problem using simple Newtonan method,
- [`LSI-fit/R/debug_plots.R`](LSI-fit/R/debug_plots.R) contains useful visualization tools to validate model/data correctness.
