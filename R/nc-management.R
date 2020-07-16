#' Tools to manage netcdf4 files
#'
#' Wrappers around nco commands .
#'
#' @param in_files character vector of files
#' @param out_file file to save output
#' @param overwrite wether to overwrite exiting files
#' @param verbose print out commands
#'
#' @export
#' @rdname nc-files
nc_append <- function(in_files, out_file, verbose = TRUE) {
  if (file.exists(out_file)) file.remove(out_file)

  in_files <- paste0("\"", in_files, "\"")
  out_file <- paste0("\"", out_file, "\"")

  for (file in in_files) {
    command <- paste("ncks -A", file, out_file)
    if (verbose) {
      message("Processing ", file, " with command:\n", command)
    }
    system(command)
  }
  return(out_file)

}

#' @export
#' @rdname nc-files
nc_concatenate <- function(in_files, out_file, verbose = TRUE, overwrite = FALSE) {

  if (file.exists(out_file) & overwrite == FALSE) {
    if (verbose) message("Non existent file: ", out_file)
    return(out_file)
  }
  if (file.exists(out_file)) file.remove(out_file)

  in_files <- paste0("\"", in_files, "\"")
  out_file <- paste0("\"", out_file, "\"")


  command <- paste("ncrcat", paste(in_files, collapse = " "), out_file)
  if (verbose) {
    message("Concatenating files with command:\n", command)
  }
  system(command)
}
