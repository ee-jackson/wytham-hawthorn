# wytham-hawthorn

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15124593.svg)](https://doi.org/10.5281/zenodo.15124593)

This repository contains the [research compendium](https://research-compendium.science) for the article:

Jackson E.E., Greenwell M.P., Bullock J.M, Oliver T.H., Topple S., Foster C.W. & Gripenberg S. (2025). **Density-dependent effects on the reproductive ecology of trees in a temperate woodland.** *Ecology & Evolution.* DOI: [10.1002/ece3.71491](https://doi.org/10.1002/ece3.71491)

Contact: eleanor.elizabeth.j@gmail.com

## Abstract

The reproductive success of plants often depends on their local conspecific densities. The degree of isolation from conspecific plants can mediate an individual’s interactions with other organisms. For example, a high density of flowers can attract pollinators and improve seed set, and a high density of seeds can attract enemies such as seed predators. It is the joint outcome of positive and negative density dependent effects which will determine the spatial distribution of a population, yet they are rarely studied simultaneously. We related two indicators of reproductive success (fruit set and fruit drop) to tree size and the density of neighbouring conspecifics for 32 *Crataegus monogyna* (Rosaceae) individuals in a temperate woodland. Overall, 26% of flowers set seed, but seed set was not density dependent. We found that 25% of fruits were dropped before reaching maturity, and 24% of mature fruits were dropped before the typical dispersal period. The drop of both immature and mature fruits increased with the density of reproductive conspecifics in this system, with potential implications for spatial patterns of seedling recruitment.

## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains two subdirectories, [`exploration/`](code/exploration/) which contains R Markdown exploratory analyses, and [`scripts/`](code/scripts/) which contains all the code for cleaning, combining, and analysing the data.

Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All `.Rmd` files in [`exploration/`](code/exploration/) are knitted to `github_documents` to make the GitHub repo browsable.

### `data/`
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`. 
The contents of `data/raw/` can be downloaded from Zenodo [10.5281/zenodo.10599206](https://doi.org/10.5281/zenodo.10599206).

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectories [`figures/`](output/figures/) and [`results/`](output/results/), which contain the figures used in the paper and other output from analyses, respectively.

### [`docs/`](docs/)
The [`docs/`](docs/) directory contains the [data dictionary](docs/data-dictionary.md), [project notebook](docs/project-notebook.md) and any other relevant documents.

## Usage
To reproduce results and figures from this project in the RStudio IDE, first open the `.Rproj` file and call `renv::restore()` to restore the project's R package library. Populate your `data/raw/` directory with the raw data files which can be downloaded from Zenodo [10.5281/zenodo.10599206](https://doi.org/10.5281/zenodo.10599206). Then, run the `.R` scripts in `code/scripts/` in the order in which they are labelled. 

## License
Code is under a [MIT license](LICENSE.md)
