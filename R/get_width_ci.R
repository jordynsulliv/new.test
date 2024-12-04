
library(dplyr)

get_width_ci <- function(est, iso_code, coverage = 95) {
  if (!coverage %in% c(95, 80)) {
    stop("Coverage must be either 95 or 80.")
  }
    country_data <- subset(est, iso == iso_code)
    if (nrow(country_data) == 0) {
    stop(paste("No data found for iso_code:", iso_code))
  }
    required_columns <- c("Year", "L95", "U95", "L80", "U80")
  if (!all(required_columns %in% colnames(country_data))) {
    stop("The required columns (Year, L95, U95, L80, U80) are missing from the data.")
  }
    country_data <- country_data %>%
    filter(!is.na(L95) & !is.na(U95) & !is.na(L80) & !is.na(U80))
    if (nrow(country_data) == 0) {
    stop("No data available after removing NA values from L95, U95, L80, and U80.")
  }
    if (coverage == 95) {
    country_data <- country_data %>%
      mutate(interval_width = U95 - L95)
  } else if (coverage == 80) {
    country_data <- country_data %>%
      mutate(interval_width = U80 - L80)
  }
    result <- country_data %>%
    select(Year, interval_width)
  
  return(result)
}

