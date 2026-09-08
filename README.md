# Replication Package for Trends and Heterogeneity in Interracial Marriage in the United States, 1960-2023

This is a replication package for the article "Trends and Heterogeneity in Interracial Marriage in the United States, 1960-2023" by Aaron Gullickson, forthcoming in *Socius*.

## Data Access

### Census and American Community Survey Data

The data used for this project are a subsample of the [IPUMS USA](https://usa.ipums.org/usa/) data. Any use of these data should be cited as follows:

> Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Grace Cooper, Julia A. Rivera Drew, Stephanie Richards, Renae Rodgers, Jonathan Schroeder, and Kari C.W. Williams. IPUMS USA: Version 16.0 dataset. Minneapolis, MN: IPUMS, 2025. <https://doi.org/10.18128/D010.V16.0>

The data included with this project is intended only for replication purposes. Individuals are not to redistribute the data without permission. Contact [ipums\@umn.edu](mailto:ipums@umn.edu) for redistribution requests. For all other uses of these data, please access data directly via [usa.ipums.org](https://usa.ipums.org).

Because of the size of the data, they are housed separately from this repository on Google Drive. However, the `analysis/organize_data.qmd` code will download these data to read locally when it is rendered. Furthermore the codebook for the extracts are included with the repository at at `data/data_raw/acs_census`.

### Vital Statistics Data

The vital statistics data were transcribed from hand from specific tables in PDF files of the vital statistics data reports. These PDF reports and CSV files of the transcribed trables are provided in `data/data_raw/vital_stats`. The transcribed tables are also available [here](https://docs.google.com/spreadsheets/d/1YVMMhwrIsyux6FQCPqyCqN45BLOruxLfsk60XBra1z4/edit?usp=sharing).

## Package Dependencies

To ensure all package dependencies for the project are met, users should first source in the `utils/check_packages.R` script. Below, I show the `sessionInfo()` in R for the last run of the full project:

```r
R version 4.5.0 (2025-04-11)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 22.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

time zone: America/Los_Angeles
tzcode source: system (glibc)

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods
[8] base

other attached packages:
 [1] qrcode_0.3.0           fs_1.6.6               furrr_0.3.1
 [4] future_1.58.0          progress_1.2.3         tictoc_1.2.1
 [7] tidycensus_1.7.1       gt_1.0.0               modelsummary_2.3.0
[10] mice_3.17.0            marginaleffects_0.26.0 srvyr_1.3.0
[13] ggrepel_0.9.6          broom_1.0.8            lubridate_1.9.4
[16] forcats_1.0.0          stringr_1.5.1          dplyr_1.2.1
[19] purrr_1.2.2            tidyr_1.3.1            tibble_3.3.1
[22] ggplot2_4.0.1          tidyverse_2.0.0        googledrive_2.1.1
[25] yaml_2.3.10            googlesheets4_1.1.1    haven_2.5.5
[28] readr_2.1.5            knitr_1.50             here_1.0.1

loaded via a namespace (and not attached):
 [1] Rdpack_2.6.4       DBI_1.2.3          rlang_1.3.0        magrittr_2.0.5
 [5] e1071_1.7-16       compiler_4.5.0     vctrs_0.7.3        rvest_1.0.4
 [9] crayon_1.5.3       pkgconfig_2.0.3    shape_1.4.6.1      fastmap_1.2.0
[13] backports_1.5.0    promises_1.3.2     tzdb_0.5.0         ps_1.9.1
[17] nloptr_2.2.1       xfun_0.52          glmnet_4.1-8       jomo_2.7-6
[21] jsonlite_2.0.0     later_1.4.2        uuid_1.2-1         pan_1.9
[25] prettyunits_1.2.0  R6_2.6.1           tables_0.9.31      stringi_1.8.7
[29] RColorBrewer_1.1-3 parallelly_1.45.0  boot_1.3-31        rpart_4.1.24
[33] cellranger_1.1.0   assertthat_0.2.1   Rcpp_1.1.0         iterators_1.0.14
[37] Matrix_1.7-3       splines_4.5.0      nnet_7.3-20        timechange_0.3.0
[41] tidyselect_1.2.1   dichromat_2.0-0.1  codetools_0.2-19   websocket_1.4.4
[45] processx_3.8.6     listenv_0.9.1      lattice_0.22-5     withr_3.0.3
[49] S7_0.2.1           evaluate_1.0.5     survival_3.8-3     sf_1.0-21
[53] units_0.8-7        proxy_0.4-27       survey_4.4-2       xml2_1.3.8
[57] pillar_1.11.1      tigris_2.2.1       KernSmooth_2.23-26 foreach_1.5.2
[61] reformulas_0.4.3.1 generics_0.1.4     rprojroot_2.0.4    chromote_0.5.1
[65] hms_1.1.3          scales_1.4.0       minqa_1.2.8        globals_0.18.0
[69] class_7.3-23       glue_1.8.1         tools_4.5.0        data.table_1.17.2
[73] lme4_1.1-37        grid_4.5.0         mitools_2.4        rbibutils_2.3
[77] nlme_3.1-168       cli_3.6.6          rappdirs_0.3.3     gargle_1.5.2
[81] gtable_0.3.6       digest_0.6.37      classInt_0.4-11    farver_2.1.2
[85] htmltools_0.5.8.1  lifecycle_1.0.5    httr_1.4.7         mitml_0.4-5
[89] MASS_7.3-65
```

## Running the Code

The entire project can be run as a [quarto](https://quarto.org/) project with the following command in the base directory:

``` bash
quarto render
```

The following files run the full analysis and can also be rendered separately in the specified order:

1.  `analysis/organize_data.qmd` - Read in the raw data, clean it, and organize it into a couple-level dataset.
2.  `analysis/build_model_data.qmd` - Build bootstrapped contingency table level data sets for the log-linear models. Because of the bootstrapping, this can take a substantial amount of time. See below for how to shorten that time for test runs.
3.  `analysis/estimate_models.qmd` - Estimate the actual log-linear models from the bootstrapped data.
4.  `analysis/estimate_outmarriage_props.qmd` - Estimate simple outmarriage proportions from the data.
5.  `analysis/analysis.qmd` - Analyze the model results.

Running the entire project from scratch will re-run the full bootstrapping of samples and will therefore take a significant amount of time (likely more than 24 hours on most machines). To test out the code with a smaller number of bootstrap samples, users can adjust the `bootstrap_rep` parameter in the yaml header of the `analysis/build_model_data.qmd` file to a smaller value:

``` yaml
params:
  bootstrap_rep: 1000
```

## Accessing Model Results

For convenience, I also include all of the final model estimates as RData files in the `data/data_constructed` directory. These files can be used to re-run the `analysis/analysis.qmd` quarto document and all the code contained within it. They can also be used to extract exact values for parameter estimates based on figures shown in the article.
