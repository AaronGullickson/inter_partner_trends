# Data Sources

## Census and American Community Survey Data

The data used for this project are a subsample of the [IPUMS USA](https://usa.ipums.org/usa/) data. Any use of these data should be cited as follows:

> Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Grace Cooper, Julia A. Rivera Drew, Stephanie Richards, Renae Rodgers, Jonathan Schroeder, and Kari C.W. Williams. IPUMS USA: Version 16.0 dataset. Minneapolis, MN: IPUMS, 2025. <https://doi.org/10.18128/D010.V16.0>

The data included with this project is intended only for replication purposes. Individuals are not to redistribute the data without permission. Contact [ipums\@umn.edu](mailto:ipums@umn.edu) for redistribution requests. For all other uses of these data, please access data directly via [usa.ipums.org](https://usa.ipums.org).

Because of the size of the data, they are housed separately from this repository on Google Drive. However, the `analysis/organize_data.qmd` code will download these data to read locally when it is rendered. Furthermore the codebook for the extracts are included with the repository at at `data/data_raw/acs_census`.

## Vital Statistics Data

The vital statistics data were transcribed from hand from specific tables in PDF files of the vital statistics data reports. These PDF reports and CSV files of the transcribed trables are provided in `data/data_raw/vital_stats`. The transcribed tables are also available [here](https://docs.google.com/spreadsheets/d/1YVMMhwrIsyux6FQCPqyCqN45BLOruxLfsk60XBra1z4/edit?usp=sharing).
