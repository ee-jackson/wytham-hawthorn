# wytham-seed-predation

This repository contains the code for the project: Spatial patterns of pre-dispersal seed predation in Wytham Woods

## Contents:

### data/
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`.

### code/
The `code/` directory contains two subdirectories, `exploration/` which contains R Markdown exploratory analyses, and `scripts/` which contains all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the .Rproj file lives).

Each .R script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All R Markdown files in `exploration/` are knitted to `github_documents` to make the GitHub repo browsable.

### output/
The `output/` directory contains the subdirectories `figures/` and `results/`, which contain the figures used in the paper and other output from analyses, respectively.

### docs/
The `docs/` directory contains the project notebook, the data dictionary and any other relevant documents.
