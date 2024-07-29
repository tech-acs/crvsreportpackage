# crvsreportpackage

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

This package contains some useful functions to create a crvs report.

## Simple installation and Basic Usage

In order to use the `crvsreportpackage` with your data, you will need to follow these straight forward commands.
First, install the package from the GitHub repo:

### Install and Load the devtools Package

Install and load the devtools package if you haven't already.

```r
install.packages("devtools")
library(devtools)
```

### Install the `crvsreportpackage` Package from GitHub

Use the install_github() function from the devtools package to install the package.
You need to provide the repository address in the format username/repository.

```r
devtools::install_github("tech-acs/crvsreportpackage")
```

During this installation, you will be asked to install some required packages that are used within `crvsreportpackage`.
This might take a minute or two.

### Use the `crvsreportpackage` in your Script

Once you have installed the package, here is a basic example of how to use
__crvsreportpackage__:

```r
library(crvspackage)

# Example Usage
age_grp_80 <- derive_age_groups(ageinyrs,
                                        start_age = 5, max_band = 80,
                                        step_size = 5, under_1 = TRUE)
```

## Developer Installation

If you want to develop the package, you may want to consider cloning the repository and following the steps below to have the correct environment to improve the package. First, install the `crvsreportpackage` from GitHub, then ensure all package dependencies are correctly managed and installed using `renv`:

1.__Clone the repository__:

```sh
git clone https://github.com/tech-acs/crvsreportpackage.git
cd crvsreportpackage
```

2.__Initialize the `renv` environment__: Upon first entering the package directory, `renv` will activate the specific package environment defined for `crvsreportpackage`. You need to run the following R commands to restore the required packages:

```R
install.packages("renv")
renv::restore()
```

This will read the `renv.lock` file which contains the dependencies and install the correct versions of all required packages.

3.__Install the package__: Once the environment is set up, you can install the package using:

```R
devtools::install()
```

By following these steps, you will ensure that you have all the necessary dependencies and the correct versions installed, allowing `crvsreportpackage` to function as intended. It will also mean you can now open the project and develop the package further.

## Maintenance

Once you have made significant changes to the package, it might be worth considering changing the package version.
This can be done in the DESCRIPTION file, bumping up the `Version` variable.
In order to rebuild and update the `crvsreportpackage` after making some changes to the functions in it follow these steps:

```r
# Remove the installed version of the package
remove.packages("crvsreportpackage")

# Clear your R environment
rm(list = ls())

# Restart R session (do this manually or with RStudio shortcut Ctrl+Shift+F10)

# Initialize renv if not already done
if (!"renv" %in% installed.packages()) {
  install.packages("renv")
}
library(renv)

# Restore the environment
renv::restore()

# Update the lockfile
renv::snapshot()

# Recreate documentation and rebuild the package
library(devtools)
document()  # Generates the NAMESPACE file and documentation using roxygen2
build()     # Builds the package
install()   # Installs the package
```

After these steps the package should be installed with the latest changes to the functions.
This should mean the package is in the right state to be updated on the online repo.

Before you make any big changes, double check that all the unit test are working
as expected, by running them manually:

```R
testthat::test_dir("tests/testthat")
```


## Documentation

We are currently working on our documentation, this will soon be available in
website format.

## Contributing

We welcome contributions! Please have a look at our [Contributing Guidelines](CONTRIBUTING.md) for details
on the process for submitting pull requests. Also have a read at our [Contributor Code of Conduct](CODE_OF_CONDUCT.md) before interacting with the project.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

- Pamela Kakande - Contributor and Maintainer - GitHub profile
- Tesfaye Belay - Contributor and Maintainer - [GitHub profile](https://github.com/tbelay)
- Rachel Shipsey - Contributor - GitHub profile
- Liam Beardsmore - Initial work - [GitHub profile](https://github.com/beardl-ons)
- Henry Partridge (ONS) - Contributor - [GitHub profile](https://github.com/rcatlord)
- Diego Lara (ONS) - Contributor - [GitHub profile](https://github.com/diego-ons)
