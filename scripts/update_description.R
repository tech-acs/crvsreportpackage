# Load required packages
library(desc)
library(renv)

# Function to update DESCRIPTION file
update_description <- function() {
  # Load current DESCRIPTION file
  desc_file <- "DESCRIPTION"
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file does not exist in the current directory.")
  }
  desc <- description$new(desc_file)

  # Get dependencies from renv
  deps <- renv::dependencies()

  # Extract unique package names
  pkgs <- unique(deps$Package)

  # Remove base packages and suggested packages from the list
  base_pkgs <- installed.packages(priority = "base")[, "Package"]
  pkgs <- setdiff(pkgs, c(base_pkgs, desc$get_deps()$package))

  # Set Imports in DESCRIPTION
  if (length(pkgs) > 0) {
    for (pkg in pkgs) {
      desc$set_dep(pkg, "Imports")
    }
  }

  # Write back to DESCRIPTION
  desc$write(file = desc_file)
}

# Run the function to update DESCRIPTION
update_description()
