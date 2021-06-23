# bioEAT

The aim of bioEAT is to provide functionality to **E**xplore data, make gene **A**nnotation exhaustive and simplify the gene IDs **T**ranslation between different organisms.

## Installation

This package could be installed from this GitHub. 

To install a package from GitHub, you will first need to install [devtools](https://github.com/r-lib/devtools):
```R
# Install devtools from CRAN
install.packages("devtools")
```

Further you  may install bioEAT:

```R
devtools::install_github("Troshchk/bioEAT")
```

## Usage

For the data exploration:
- find the non-overlapping elements of two vectors with ```outersect()```
- get main n pca components with ```get_main_pca()```
- calculate the logFold change between two dataframe columns and order the result with ```logfc()```
- calculate and visualize Shannon entropy ```sh_entropy()```

For the data annotation:
- use OrgDB and maRt in one function with ```conv_ids_full()```

The part for the data translation is under development.

For more information about the package and the functions refer to the [documentation](https://github.com/Troshchk/bioEAT/blob/main/bioEAT.pdf). 

## Author
Ksenia Troshchenkova

ksenia.trs@gmail.com

[GitHub](https://github.com/Troshchk)

[LinkedIn](https://www.linkedin.com/in/ksenia-troshchenkova/)
