#' Creates an ODBC connection to an SQL Server instance
#'
#' @param server SQL server name
#' @param database Database name (optional)
#' @param timezone The Server time zone. Default is 'Europe/Helsinki'. See [odbc::dbConnect()]
#' @param timezone_out The time zone returned to R. Default is 'Europe/Helsinki'. See [odbc::dbConnect()]
#'
#' @seealso [close_conn()]
#' @export
get_conn <- function(server, database = NULL, timezone = "Europe/Helsinki", timezone_out = "Europe/Helsinki") {
  con <- odbc::dbConnect(
    drv = odbc::odbc(), timezone = timezone,
    timezone_out = timezone_out,
    encoding = ifelse(l10n_info()$`UTF-8`, "UTF-8", "ISO-8859-13"),
    # TODO: driver = ,
    server = server, database = database, trusted_connection = "Yes"
  )

  return(con)
}

#' Closes an ODBC connection
#'
#' @param conn An ODBC connection object
#'
#' @seealso [get_conn()]
#'
#' @export
close_conn <- function(conn) {
  DBI::dbDisconnect(conn = conn)
}
