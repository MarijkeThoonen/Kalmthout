#' @title Connect to a 32-bit access database
#'
#' @description This function makes a connection to a 32-bit access database and requires a 32-bit R version.
#'
#' @param db_file_path The path to the access database file.
#'
#' @return An open connection to the database.
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc

connect_to_access_dbi <- function(db_file_path) {
  require(DBI)
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }
  # make sure that a 32-bit R version is running
  if (R.Version()$arch == "x86_64") {
    stop("connection to 32-bit access dbi requires a 32-bit version of R")
  }
  # Assemble connection strings
  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  myconn <- dbConnect(odbc(),
                      .connection_string = db_connect_string)
  return(myconn)
}
