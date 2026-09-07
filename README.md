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

To ensure all package dependencies for the project are met, users should first source in the `utils/check_packages.R` script. Below, I show the versions of each package that were used for the final run of the project.

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

For convenience, I also include all of the final model estimates as RData files in the `data/data_constructed` directory. These files can be used to re-run the `analysis/analysis.qmd` quarto document and all the code contained within it.
