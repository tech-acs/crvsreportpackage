#'@title convert_config_args
#'
#'@description A function that takes the user defined arguments for the table functions and converts the format to include when calling the function.
#'
#'@details Given the config defined arguments for a function, it will look for the data fields and convert them to create the function call.
#'
#'@param args A list of variables that comes from the config file.
#'
#'@return A list of arguments that can be passed into the functions.
#'
#'@examples convert_config_args(args)
#'
#'@export

convert_config_args <- function(args) {
  # Convert data to the actual data frame if it's a string representing a variable name
  if (!is.null(args$data) && is.character(args$data)) {
    args$data <- get(args$data, envir = .GlobalEnv)
  }
  # Convert est_data to the actual data frame if it's a string representing a variable name
  if (!is.null(args$est_data) && is.character(args$est_data)) {
    args$est_data <- get(args$est_data, envir = .GlobalEnv)
  }
  # Convert cause to the actual data frame if it's a string representing a variable name
  if (!is.null(args$cause) && is.character(args$cause)) {
    args$cause <- get(args$cause, envir = .GlobalEnv)
  }
  # Convert pops to the actual data frame if it's a string representing a variable name
  if (!is.null(args$pops) && is.character(args$pops)) {
    args$pops <- get(args$pops, envir = .GlobalEnv)
  }
  # Convert output path to actual variable name
  if (!is.null(args$output_path) && is.character(args$output_path)) {
    args$output_path <- get(args$output_path, envir = .GlobalEnv)
  }
  return(args)
}
