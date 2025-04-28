#' Processes Tide Data
#' 
#' @param url Link to the marineweather.net data
#' 
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom lubridate mdy
#' @importFrom lubridate hm
#' @importFrom lubridate ymd_hms
#' 
#' @return A data.frame
#' 
#' @export
Tides <- function(url){
  year <- as.integer(format(Sys.Date(), "%Y"))

  html <- read_html(url)
  nodes <- html |> html_nodes('table')
  table <- nodes |> html_table()

  Data <- data.frame(table)
  Data <- Data |> mutate(Date = sub("^[A-Za-z]+\\s+", "", Date))
  Data$Date <- paste0(Data$Date, ", ", year)
  Data$Date <- mdy(Data$Date)

  Data$Time <- lubridate::parse_date_time(
    Data$Time, 
    orders = "%I:%M %p"  # %I = 12-hour hour, %M = minute, %p = AM/PM
  )
  Data$Time <- format(Data$Time, "%H:%M:%S")

  Data$Feet <- gsub(" ft", "", Data$Feet)
  Data$Feet <- as.numeric(Data$Feet)

  Data$Tide <- gsub(" ", "", Data$Tide)
  Data$Tide <- gsub("Tide", "", Data$Tide)

  Data$Tide <- factor(Data$Tide)
  Data <- Data |> mutate(datetime = paste(Date, Time))
  Data$datetime <- ymd_hms(Data$datetime)
  Data <- Data |> relocate(datetime, .before = Date)
  return(Data)
}