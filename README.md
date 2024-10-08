# banditr

The goal of banditr is to manage multi-armed bandit experiments. The package supports Linear Upper Confidence Bound (LinUCB), and Thompson sampling algorithms. It allows storing the resulting objects in a database management system (DBMS). Currently, the package supports one DBMS: Microsoft SQL Server, through the [`RODBC`](https://cran.r-project.org/web/packages/RODBC/index.html) package.

## Installation

You can install banditr from github with:

```{r gh-installation, eval = FALSE}
# uncomment the following line if the package devtools is not installed
# install.packages("devtools")
devtools::install_github("rferrali/banditr")
```

## Tutorial

A tutorial is available [here](http://rferrali.github.io/banditr). It is also available as a vigentte in the package. To access the vignette, make sure to compile it as you install the package, and use: 

```{r example, eval = FALSE}
# devtools::install_github("rferrali/banditr", build_vignettes = TRUE)
library(banditr)
vignette("introduction", "banditr")
```
