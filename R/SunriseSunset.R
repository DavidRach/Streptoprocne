#' Return sunrise sunset times from naval academy website
#'
#' @param URL to desired locations sunrise sunset table
#'
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_node
#' @importFrom rvest html_text
#' @importFrom dplyr filter
#' @importFrom lubridate hm
#'
#' @return A dataframe of sunrise sunset times
#' @export
#'
#' @examples NULL
SunriseSunset <- function(x){
  html <- read_html(x)
  nodes <- html %>% html_nodes('main.usa-layout-docs#main-content')
  pre_element <- nodes %>% html_node('div > pre')
  text <- pre_element %>% html_text()

  lines <- strsplit(text, "\n")[[1]]
  lines <- lines[-1:-7]
  lines <- lines[-35:-36]
  lines <- lines[-3] #No need for hour minutes

  newlines <- lines[-c(1, 2)]
  WorkAround <- matrix(newlines)
  WorkAround <- gsub("             ", " NA NA ", WorkAround)
  split_columns <- strsplit(WorkAround, "\\s+")
  result <- do.call(rbind, split_columns)

  num_cols <- ncol(result)
  small_matrices <- list()

  for (i in seq(2, num_cols, by = 2)) {
    small_matrix <- result[, c(1, i, i + 1)]
    small_matrices[[length(small_matrices) + 1]] <- small_matrix
  }

  TheMonths <- c("January", "February", "March", "April", "May",
                 "June", "July", "August", "September", "October",
                 "November", "December")

  matrices_with_month <- list()

  for (i in seq_along(small_matrices)) {
    small_matrix <- small_matrices[[i]]
    month <- TheMonths[i]
    small_matrix_with_month <- cbind(month, small_matrix)
    matrices_with_month[[i]] <- small_matrix_with_month
  }

  combined_matrix <- do.call(rbind, matrices_with_month)
  combined_df <- as.data.frame(combined_matrix)
  colnames(combined_df) <- c("Month", "Day", "Sunrise", "Sunset")

  Data <- combined_df %>% filter(Sunrise != "NA")
  Data$Sunrise <- paste0(substr(Data$Sunrise, 1, 2), ":",
                         substr(Data$Sunrise, 3, 4))
  Data$Sunset <- paste0(substr(Data$Sunset, 1, 2), ":",
                        substr(Data$Sunset, 3, 4))

  Data$Sunrise <- hm(Data$Sunrise)
  Data$Sunset <- hm(Data$Sunset)
  return(Data)
}
