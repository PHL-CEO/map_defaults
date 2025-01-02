library(httr2)
library(jsonlite)
library(furrr)

get_coordinates <- function(address) {
  api_key <- Sys.getenv("PHILA_API_KEY")  # Retrieve API key from environment variable
  base_url <- "https://api.phila.gov/ais/v2/search/"  # Base URL for the API
  url <- paste0(base_url, URLencode(address))  # Create full URL by encoding the address
  
  response <- tryCatch(
    {
      req <- request(url) %>%
        req_headers(Authorization = paste("Gatekeeper-Key", api_key))  # Prepare the HTTP request
      resp <- req_perform(req)  # Perform the HTTP request
      resp_body_string(resp)  # Extract the response body as a string
    },
    error = function(e) {
      message("Error querying address: ", address)  # Error handling if request fails
      return(NULL)  # Return NULL in case of error
    }
  )
  
  if (!is.null(response) && grepl("Rate Limit Exceeded", response)) {
    message("Rate limit exceeded for address: ", address)  # Handle rate-limited responses
    return(NULL)  # Return NULL if rate limit is exceeded
  }
  
  if (!is.null(response)) {
    parsed_response <- tryCatch(
      {
        jsonlite::fromJSON(response, simplifyVector = FALSE)  # Parse the JSON response
      },
      error = function(e) {
        message("Error parsing JSON for address: ", address)  # Error handling if parsing fails
        return(NULL)  # Return NULL in case of error
      }
    )
    
    if (!is.null(parsed_response) && 
        "features" %in% names(parsed_response) && 
        length(parsed_response$features) > 0) {
      
      geometry <- parsed_response$features[[1]]$geometry  # Extract geometry data
      if (!is.null(geometry) && "coordinates" %in% names(geometry)) {
        coords <- geometry$coordinates  # Extract coordinates (longitude, latitude)
        if (!is.null(coords) && length(coords) == 2) {
          return(c(lat = coords[2], lon = coords[1]))  # Return latitude and longitude as a named vector
        }
      }
    }
  }
  
  return(c(lat = NA, lon = NA))  # Return NA if no valid coordinates found
}

get_batch_coordinates <- function(df, address_column) {
  # Validate if the address column exists in the dataframe
  if (!address_column %in% colnames(df)) {
    stop("Address column not found in the dataframe.")
  }
  
  batch_start_time <- Sys.time()
  
  plan(multisession, workers = 6)  # Use multiple workers (adjust workers as needed)
  
  # Use furrr::future_map to perform geocoding in parallel
  batch_results <- future_map(df[[address_column]], function(address) {
    if (grepl("PO BOX", address, ignore.case = TRUE) || is.na(address) || address == "") {
      return(c(lat = NA, lon = NA))  # Skip PO Boxes, empty or NA addresses
    }
    get_coordinates(address)  # Call the geocoding function to get coordinates
  })
  
  # Convert list of results into a data frame
  batch_results_df <- purrr::map_dfr(batch_results, ~ data.frame(lat = .x[1], lon = .x[2]))
  
  batch_end_time <- Sys.time()
  
  batch_time_taken <- batch_end_time - batch_start_time
  message("Total time taken for batch processing: ", batch_time_taken)
  
  return(batch_results_df)  # Return the data frame with coordinates
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
