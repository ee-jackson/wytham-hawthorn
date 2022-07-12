# wytham-seed-predation

This repository contains code for the Wytham Hawthorn project, lead by Sofia Gripenberg's research group at the University of Reading.

## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains two subdirectories, [`exploration/`](code/exploration/) which contains R Markdown exploratory analyses, and [`scripts/`](code/scripts/) which contains all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the `.Rproj` file lives).

Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All `.Rmd` files in [`exploration/`](code/exploration/) are knitted to `github_documents` to make the GitHub repo browsable.

### `data/`
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`. Data will be made available if/when we publish a corresponding research paper.

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectories [`figures/`](output/figures/) and [`results/`](output/results/), which contain the figures used in the paper and other output from analyses, respectively.

### [`docs/`](docs/)
The [`docs/`](docs/) directory contains the [data dictionary](docs/data-dictionary.md), [project notebook](docs/project-notebook.md) and any other relevant documents.

## To make changes to the project:
-  Fork this repository, so you have your own copy
-  Make changes
-  Create a pull request
