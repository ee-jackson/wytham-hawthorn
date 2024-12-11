# wytham-hawthorn

This repository contains the [research compendium](https://research-compendium.science) for our in-prep manuscript:

Eleanor E. Jackson, Matthew P. Greenwell, James M. Bullock, Tom H. Oliver, Susie Topple, Christopher W. Foster, Sofia Gripenberg. **Density-dependent effects on the reproductive ecology of trees in a temperate woodland.**

## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains two subdirectories, [`exploration/`](code/exploration/) which contains R Markdown exploratory analyses, and [`scripts/`](code/scripts/) which contains all the code for cleaning, combining, and analysing the data.

Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All `.Rmd` files in [`exploration/`](code/exploration/) are knitted to `github_documents` to make the GitHub repo browsable.

### `data/`
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`. 
The contents of `data/raw/` can be downloaded from Zenodo https://doi.org/10.5281/zenodo.10599206

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectories [`figures/`](output/figures/) and [`results/`](output/results/), which contain the figures used in the paper and other output from analyses, respectively.

### [`docs/`](docs/)
The [`docs/`](docs/) directory contains the [data dictionary](docs/data-dictionary.md), [project notebook](docs/project-notebook.md) and any other relevant documents.
