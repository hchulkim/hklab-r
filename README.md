# Overview

This repository is used to store R functions I employ in data analysis. 

## Acknowledgement

Lot of the functions and information in this repository is from [GSlab_r](https://github.com/gslab-econ/gslab_r/tree/master). 

For tools on package development, try these below:

| Resource | Description |
| -------- | ----------- |
| [O'Reily R Packages](http://r-pkgs.had.co.nz/) | A detailed guide of the package development process |
| [Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) | Barebones guide to package development with `devtools` |
| [R package primer](https://kbroman.org/pkg_primer/) | Another guide to package development (especially helpful when you want to put the package on CRAN) |
| [`devtools` on GitHub](https://github.com/hadley/devtools) | Easy package creation functions |
| [`roxygen2` vignette](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html) | Methods for package documentation |
| [`testthat` tutorial](https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf) | Contemporary unit testing in R |
| [`ggplot2` on GitHub](https://github.com/hadley/ggplot2) | Example of a large scale, well-designed, and reliable R package |

## Development process

I follow the process outlined in [GSlab_r](https://github.com/gslab-econ/gslab_r/tree/master). Hence I just copy/modify their README below.

### Tools

I use the `devtools`, `roxygen2`, and `testthat` packages as our primary tools in the package development process. `devtools` is a convenient wrapper for the functions in the others. The commands below are from `devtools`, but the syntax belongs to `roxygen2` and `testthat`.

The `create` function initializes a package with the `DESCRIPTION`, `R` and `NAMESPACE` fields. The `LICENSE` file must be created manually, the `DESCRIPTION` file must be edited manually.

The `document` function should be used to produce the help files in the `man` subdirectory and appropriately fill the `NAMESPACE` file. This function is a wrapper for the `roxygenise` function from the `roxygen2` package. This functions read from `DESCRIPTION` and the documentation for the files in `R`. Formatting must align with `roxygen2` standards. Each time you update the documentation, run `document()` at the root of the package directory.

During development, `devtools::check` function should be used to preform the unit tests in the `tests` subdirectory and check a large number of other package aspects. This function centralizes and adds utilities to functions in the `testthat` package. The `tests` directory must be conform to the format used by `testthat`. To just run unit tests in the `tests` subdirectory without running the rest of tests, run the script `testthat.R` at `tests` subdirectory.

To install a package from GitHub, use the `install_github` function. To install or update the package stored in a local folder, use the `install` function at the root of `gslab_r`.

### Workflow

The workflow for the creation of a package might be:
  1. Create a new branch of `hklab-r` named `development-packagename` from the`master` branch.
  2. Open R, load `devtools`, and run `create("packagename")` from the root of `gslab_r`.
  3. Write the functions, in-file documentation, and unit tests for the package.
    *  From the head of the directory, run `document` in R to add and update documentation as needed. 
    *  From the head of the directory, run `devtools::check` in R to perform unit tests as needed.
  4. When development is complete, merge the `development-packagename` branch into `master`.

The workflow to add a feature to a package that already exists might be:
  1. Create a new branch of `hklab-r` named `development-packagename-issue` from the `development-packagename` branch.
  3. Write the new functions, in-file documentation, and unit tests for the package.
    *  From the head of the directory in R, run `document` to add and update documentation as needed. 
    *  From the head of the directory in R, run `devtools::check` to perform unit tests as needed.
  4. When development is complete, merge the `development-packagename-issue` branch into `development-packagename`.

##  Anatomy of a hklabr R package

Every package contains the following structure.

### DESCRIPTION

The `DESCRIPTION` file (no extension) is required for R to reckognize a directory as a package. This file should contain complete information on the following fields. 

*  The `Package` section should contain the name of the package. 

*  The `Title` should give a descriptive package title.

*  `Version` gives the current version of the package. I use [semantic versioning](http://semver.org/).

*  `Authors@R` references hyoungchul kim as the creator.

*  `Description` is a paragraph explanation of the function.

*  `Depends` gives package dependencies. Normally, this will only contain a version of R.

*  `Imports` lists the packages that will be installed along with this pacakge. Imports provides a robust method of tracking dependencies between packages.

*  `Suggests` contains packages that may be useful but are not strictly necessary to use the pacakge. Packages used for development and testing belong in this field.

*  `License` by defauly contains a reference to a separate file LICENSE file.

### LICENSE

By default, the `LICENSE` file (no extension) should contain the standard [MIT Open Source License](https://opensource.org/licenses/MIT). 

### R 

The `R` subdirectory stores R files that contain the functions in the package. Each script should include exactly one function. Arbitrary dependencies are allowed between the function in this package. Documentation should follow documentation guidelines(#function-documentation)

### NAMESPACE

The `NAMESPACE` file (no extension) informs R which functions from the `R` subdirectory to load for use and which other packages to install.

### man

The `man` subdirectory contains help files stored in .Rd format. Each help file should closely match the documentation of the functions in R.

### tests

The `tests` subdirectory contains the unit tests.

### README.md

The `README.md` should give a brief description of the user called functions in the package. 

