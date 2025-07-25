# Parse SDMX dates
# TIME_FORMAT codelist taken from https://sdmx.org/?page_id=3215
parse_dates <- \(data) {
  formatter <- function(date, format) {
    fn <- switch(format,
      # Daily
      P1D = lubridate::ymd,
      # Monthly
      P1M = lubridate::ym,
      # Annual
      P1Y = as.integer,
      # Quarterly
      P3M = \(date) lubridate::add_with_rollback(lubridate::ceiling_date(lubridate::yq(date), "quarter"), -months(1)),
      # Semester / semi-annually
      P6M = \(date) {
        date <- stringr::str_split(date, "-S", simplify = TRUE)
        date <- lubridate::ym(paste(sep = "-", date[, 1], as.integer(date[, 2]) * 6))
      }
    )

    fn(date)
  }

  data <- unname(split(data, ~ TIME_FORMAT))
  data <- lapply(data, \(x) {
    x$TIME_PERIOD <- formatter(x$TIME_PERIOD, unique(x$TIME_FORMAT))
    x
  })
  data <- do.call(rbind, data)

  data
}

reformat_data <- function(data) {
  data_format <- getOption("loaders.data_format", "raw")

  if (!data_format %in% c("raw", "rsdmx")) {
    stop("Invalid data format. Must be one of 'raw' or 'rsdmx'.")
  }

  if (data_format == "rsdmx") {
    names(data)[names(data) == "TIME_PERIOD"] <- "obsTime"
    names(data)[names(data) == "OBS_VALUE"] <- "obsValue"
  }

  data
}

# Generate an individual auth token
# Takes the username and password from the Windows credential store or a user-specified username and password
# Keep the token value safe and do not distribute it in plain text
get_auth_token <- \(target_name = "", username = NULL, password = NULL) {
    # Extract password as a character vector
    pass_extr <- \(x) {
      rawToChar(x[x != "00"])
    }

    auth <- if (!(is.null(username) || is.null(password))) {
      # Make login from the passed username and password
      paste(username, password, sep = ":")
    } else {
      # Read login and pass from Credential Manager
      cred <- oskeyring::windows_item_read(target_name = target_name)
      paste(cred$username, pass_extr(cred$credential_blob), sep = ":")
    }

    # Return login as password base64 encoded
    RCurl::base64(txt = auth)
}

# Setup request endpoint, configure auth for confidential SDW
setup_endpoint <- function(confidential, auth_token_id, username, password) {
  if (confidential) {
    stop("Not implemented")
    query_url <- ""

    auth_token <- get_auth_token(auth_token_id, username, password)

    headers <- list(Authorization = paste("Basic", auth_token))
  } else {
    query_url <- "https://data-api.ecb.europa.eu/service/data/"

    headers <- list()
  }

  req <- httr2::request(query_url) %>%
    httr2::req_headers(!!!headers)

  req
}

# Performs request to the selected SDW endpoint
make_request <- function(req, dataset, key, start, end, first_n_obs, last_n_obs, last_modified, verbose) {
  query <- list(
    startPeriod = start,
    endPeriod = end,
    firstNObservations = first_n_obs,
    lastNObservations = last_n_obs
  )

  req <- req %>%
    httr2::req_url_path_append(dataset, key) %>%
    httr2::req_url_query(!!!query) %>%
    httr2::req_headers("Accept" = "application/vnd.sdmx.structurespecificdata+xml;version=2.1",
                       "Accept-Encoding" = "gzip, deflate")

  # In case of SSL issues, use this
  # req <- httr2::req_options(req, ssl_verifypeer = 0L, ssl_verifyhost = 0L)

  if(!is.null(last_modified)) {
    req <- httr2::req_headers(req, "If-Modified-Since" = last_modified)
  }

  # Add automatic request retries
  req <- httr2::req_retry(req, is_transient = \(res) httr2::resp_status(res) %in% c(429, 500, 503), max_tries = 3)

  message(paste("Requesting", req$url))
  res <- httr2::req_perform(req, verbosity = if (verbose) 1 else 0)
}

#' Get data from an SDW dataset
#'
#' @param dataset Name of the queried SDW dataset
#' @param key An SDMX key string (without the dataset) or a list of individual SDMX key items
#' @param parse_dates Whether to cast TIME_PERIOD strings to R dates
#' @param start Starting time period
#' @param end Ending time period
#' @param first_n_obs Number of observations from starting period onwards
#' @param last_n_obs Number of observations from ending period backwards
#' @param confidential Whether to use authentication
#' @param auth_token_id Windows keyring ID with the authentication information
#' @param username Alternative username to use for authenticating
#' @param password Alternative password to use for authenticating
#' @param verbose Whether the print request debug information
#' @param use_cache Whether to use caching
#' @param cache_dir Directory for storing cache files
#' @param refresh_cache Whether to enable automatically updating the cache
#' @param update_cache Whether to force updating the cache
#'
#' @return A dataframe containing the data from the SDW request
#' @export
#'
#' @seealso [clean_sdw_cache()]
#'
#' @examples
#' get_sdw("BSI", key = list("M", "", "N", "A", "T00", "A", "1", "Z5", "0000", "Z01", "E"))
#' get_sdw("BSI", key = list("M.U2.N.A.T00.A.1.Z5.0000.Z01.E"))
get_sdw <- \(
  dataset,
  key,
  parse_dates = TRUE,
  start = NULL,
  end = NULL,
  first_n_obs = NULL,
  last_n_obs = NULL,
  confidential = FALSE,
  auth_token_id = "",
  username = NULL,
  password = NULL,
  verbose = FALSE,
  use_cache = TRUE,
  cache_dir = NULL,
  refresh_cache = TRUE,
  update_cache = FALSE
) {
  # Validate inputs
  if (!is.character(dataset) || length(dataset) != 1) {
    stop("dataset must be a single string")
  }

  if (!is.character(key) && !is.list(key) && length(key) > 0) {
    stop("key must be a single string or a list/vector of strings")
  }

  if (length(key) > 1) {
    key <- paste0(key, collapse = ".")
  }

  # Initialize cache if needed
  cache <- NULL
  if (use_cache) {
    if (is.null(cache_dir)) {
      cache_dir <- normalizePath(getOption("loaders.sdw_cache_dir", file.path(tempdir(), "sdw")), winslash = "/", mustWork = FALSE)
    }

    cache <- storr::storr_rds(cache_dir)
  }

  # Generate unique cache key
  cache_key <- digest::digest(list(
    dataset = dataset,
    key = key,
    parse_dates = parse_dates,
    start = start,
    end = end,
    first_n_obs = first_n_obs,
    last_n_obs = last_n_obs,
    confidential = confidential
  ))

  # Load from cache if enabled and possible
  last_modified <- NULL
  if (use_cache && !update_cache && cache$exists(cache_key, "timestamp") && cache$exists(cache_key, "data")) {
    if (refresh_cache) {
      message("Refreshing enabled. Found cached timestamp.")
      last_modified <- cache$get(cache_key, "timestamp")
    } else {
      message("Refreshing disabled. Using cached data from ", cache_dir)
      return(cache$get(cache_key, "data"))
    }
  }

  # Setup endpoint & auth
  req <- setup_endpoint(confidential, auth_token_id, username, password)

  res <- make_request(req, dataset, key, start, end, first_n_obs, last_n_obs, last_modified, verbose)

  # Handle response
  if (httr2::resp_status(res) == 304) {
    message("No new data available. Using cached data from ", cache_dir)
    return(cache$get(cache_key, "data"))
  }

  # Data is new or has been modified, parse it
  tmp <- tempfile(fileext = ".xml")
  writeBin(httr2::resp_body_raw(res), tmp)

  data <- readsdmx::read_sdmx(tmp)
  unlink(tmp)

  data$OBS_VALUE[data$OBS_VALUE == "NaN"] <- NA
  data$OBS_VALUE <- as.numeric(data$OBS_VALUE)
  row.names(data) <- NULL

  # Reformat dates
  if (parse_dates) {
    data <- parse_dates(data)
  }

  # Adjust naming of time and value fields
  data <- reformat_data(data)

  # Update cache if needed
  if (use_cache) {
    cache$set(cache_key, data)
    cache$set(cache_key, httr2::resp_header(res, "Last-modified"), "timestamp")
    cache$set(cache_key, data, "data")
    message("Query cached in ", cache_dir)
  }

  data
}

#' Clean SDW Cache
#'
#' Delete the sdw cache directory.
#'
#' @param cache_dir A path to cache directory. If `NULL` (default) tries to clean the default temporary cache directory.
#'
#' @seealso [get_sdw()]
#'
#' @examples
#' \dontrun{
#' clean_sdw_cache()
#' }
#' @export
clean_sdw_cache <- function(cache_dir = NULL) {

  if (is.null(cache_dir)) {
    cache_dir <- normalizePath(getOption("loaders.sdw_cache_dir", file.path(tempdir(), "sdw")), winslash = "/", mustWork = FALSE)
  }

  if (!file.exists(cache_dir)) {
    stop("The cache folder ", cache_dir," does not exist")
  }

  unlink(cache_dir, recursive = TRUE, force = TRUE)
  message("Deleted the cache folder in ", cache_dir)

  invisible(TRUE)
}
