#' Read in multiple data at once and bind them together.  
#'
#' @param pattern The pattern of the data file you want to read.
#' @param path The path of the data file you want to read. 
#' @param bind TRUE if you want to bind the resulting list object into one dataframe. 
#' @param read_func Type of function you are using to read files. 
#' @param header TRUE if the header is already in the data. 
#' @param encoding The way file was encoded.
#' @param ... Additional arguments passed to the function
#' 
#' @importFrom readr locale
#' @importFrom utils read.csv
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @export

multi_read <- function(pattern, path = ".", bind = TRUE, read_func = NULL, header = TRUE, encoding = "UTF-8", ...) {
  # List files matching the pattern
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No files found matching the pattern: ", pattern)
  }
  
  # Auto-detect read function if not provided
  if (is.null(read_func)) {
    if (grepl("\\.csv$", pattern, ignore.case = TRUE)) {
      read_func <- read.csv
    } else if (grepl("\\.xlsx$", pattern, ignore.case = TRUE)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("The 'readxl' package is required for reading Excel files. Install it with install.packages('readxl').")
      }
      read_func <- readxl::read_excel
    } else {
      stop("Unsupported file type. Please specify a suitable 'read_func'.")
    }
  }
  
  # Read files with error handling
  data_list <- purrr::map(files, function(file) {
    tryCatch(
      {
        message("Reading file: ", file)
        # Handle parameters based on the read function
        if (identical(read_func, readxl::read_excel)) {
          read_func(file, ...)
        } else if ("col_names" %in% names(formals(read_func))) {
          # For functions like read_csv
          read_func(file, col_names = header, locale = locale(encoding = encoding), ...)
        } else if ("header" %in% names(formals(read_func))) {
          # For functions like read.csv
          read_func(file, header = header, fileEncoding = encoding, ...)
        } else {
          # Default behavior for other read functions
          read_func(file, ...)
        }
      },
      error = function(e) {
        warning("Error reading file: ", file, "\nDetails: ", e$message)
        NULL
      }
    )
  })
  
  # Filter out failed reads
  data_list <- data_list[!sapply(data_list, is.null)]
  if (length(data_list) == 0) {
    stop("No files were successfully read.")
  }
  
  # Combine data frames if bind = TRUE
  if (bind) {
    tryCatch(
      {
        dplyr::bind_rows(data_list)
      },
      error = function(e) {
        stop("Error combining files with bind_rows. Ensure structures are compatible.\nDetails: ", e$message)
      }
    )
  } else {
    return(data_list)
  }
}