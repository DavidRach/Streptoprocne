#' Water Temp and Wind Speed from the NOAA stations in the Harbor
#'
#' @param x An URL for the particular NOAA buoy
#'
#' @importFrom lubridate ymd_hm
#' @importFrom magrittr %>%
#' @importFrom dplyr relocate
#'
#' @return A dataframe of relavent information
#' @export
#'
#' @examples
#'
#' url <- "https://www.ndbc.noaa.gov/data/realtime2/BLTM2.txt"
#' FtMcHenry <- NOAA_Buoys(x="https://www.ndbc.noaa.gov/data/realtime2/BLTM2.txt")
NOAA_Buoys <- function(x){
  data <- readLines(x)
  data <- gsub("    ", " ", data)
  data <- gsub("   ", " ", data)
  data <- gsub("  ", " ", data)
  headers1 <- strsplit(data[1], " ")[[1]]
  headers1 <- gsub("#", "", fixed = TRUE, headers1)
  headers2 <- strsplit(data[2], " ")[[1]]
  headers2 <- gsub("#", "", fixed = TRUE, headers2)

  # Remove the first line (headers) from the data
  data_subset <- data[-1:-2]

  data_matrix <- do.call(rbind, strsplit(data_subset, " "))
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(df) <- headers1
  cols_to_convert <- c(1:5)
  df[, cols_to_convert] <- sapply(df[, cols_to_convert], as.numeric)
  #write.csv(df, "4-21-24-FtMcHenry.csv", row.names = FALSE)

  df$datetime <- ymd_hm(paste(df$YY, df$MM, df$DD, df$hh, df$mm, sep = " "))
  df <- df[, -(1:5)]
  df <- df %>% relocate(datetime, .before="WDIR")

  return(df)
}
