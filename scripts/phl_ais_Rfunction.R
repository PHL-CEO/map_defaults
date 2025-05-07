library(httr2)
library(jsonlite)
library(furrr)

get_coordinates <- function(address) {
  api_key <- Sys.getenv("PHILA_API_KEY")
  base_url <- "https://api.phila.gov/ais/v2/search/"
  url <- paste0(base_url, URLencode(address))

  response <- tryCatch(
    {
      req <- request(url) %>%
        req_headers(Authorization = paste("Gatekeeper-Key", api_key))
      resp <- req_perform(req)
      resp_body_string(resp)
    },
    error = function(e) {
      message("Error querying address: ", address)
      return(NULL)
    }
  )

  if (!is.null(response) && grepl("Rate Limit Exceeded", response)) {
    message("Rate limit exceeded for address: ", address)
    return(NULL)
  }

  if (!is.null(response)) {
    parsed_response <- tryCatch(
      {
        jsonlite::fromJSON(response, simplifyVector = FALSE)
      },
      error = function(e) {
        message("Error parsing JSON for address: ", address)
        return(NULL)
      }
    )

    if (!is.null(parsed_response) &&
        "features" %in% names(parsed_response) &&
        length(parsed_response$features) > 0) {
      
      feature <- parsed_response$features[[1]]
      coords <- feature$geometry$coordinates
      props <- feature$properties

      if (!is.null(coords) && length(coords) == 2) {
        return(list(
          lat = coords[2],
          lon = coords[1],
          zip_code = props$zip_code,
          council_district = props$council_district_2024
        ))
      }
    }
  }

  return(list(lat = NA, lon = NA, zip_code = NA, council_district = NA))
}

get_batch_coordinates <- function(df, address_column) {
  if (!address_column %in% colnames(df)) {
    stop("Address column not found in the dataframe.")
  }

  batch_start_time <- Sys.time()

  plan(multisession, workers = 6)

  batch_results <- future_map(df[[address_column]], function(address) {
    if (grepl("PO BOX", address, ignore.case = TRUE) || is.na(address) || address == "") {
      return(list(lat = NA, lon = NA, zip_code = NA, council_district = NA))
    }
    get_coordinates(address)
  })

  batch_results_df <- purrr::map_dfr(batch_results, identity)

  batch_end_time <- Sys.time()
  message("Total time taken for batch processing: ", batch_end_time - batch_start_time)

  return(batch_results_df)
}
geocode_dataframe <- function(df, address_column) {
  # Validate address column exists
  if (!address_column %in% colnames(df)) {
    stop("Address column not found in the dataframe.")
  }
  
  # Get batch geocoded results
  batch_results_df <- get_batch_coordinates(df, address_column)  # Call the get_batch_coordinates function
  
  # Combine the original dataframe with the geocoded results
  df_with_coords <- cbind(df, batch_results_df)  # Add lat and lon columns to the original dataframe
  
  return(df_with_coords)  # Return the data frame with coordinates added
}
